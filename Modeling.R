library(tidyverse)
library(tidymodels)
library(yardstick)

# EDA ---------------------------------------------------------------------

df_final |> 
  ggplot(aes(x = spread_line)) +
  geom_histogram()

mean(df_final$spread_line)
median(df_final$spread_line)
max(df_final$spread_line)
min(df_final$spread_line)

# Splitting data ----------------------------------------------------------

## create a training set
df_train <- df_final |> 
  filter(season <= 2018)

## create a testing set
df_test <- df_final |> 
  filter(season > 2018)


# Model Setup --------------------------------------------------------

## predict cover team and create an indicator that prediction is either true or false
predict_cover_team <- function(model_obj, df){
  
  predictions <- augment(model_obj, new_data = df) |> 
    select(game_id, spread_line, .pred, .resid) |> 
    left_join(schedules |> select(result, game_id:home_score), by = "game_id") |> 
    mutate(pred_cover_team = case_when(
      .pred < spread_line ~ home_team, 
      .pred > spread_line ~ away_team
    )) |> 
    relocate(pred_cover_team, .after = .resid) |>
    left_join(team_cover_final, by = "game_id") |> 
    relocate(actual_cover_team, .after = pred_cover_team) |> 
    mutate(actual_cover_team = replace_na(actual_cover_team, "PUSH")) |> 
    filter(actual_cover_team != "PUSH") |> 
    mutate(pred_away = ifelse(pred_cover_team == away_team, 1, 0),
           actual_away = ifelse(actual_cover_team == away_team, 1, 0)) |> 
    mutate(pred_away_true = ifelse(pred_away == actual_away, 1, 0),
           pred_straight = ifelse(pred_cover_team == actual_cover_team, 1, 0))
  
  return(predictions)
}

all_models_test_results |> 
  filter(pred_away == 1) |> 
  group_by(model) |> 
  summarize(total_away_predictions = sum(pred_away),
            total_away_correct_predicitions = sum(actual_away)) |> 
  ungroup() |> 
  mutate(total_away_correct_predicitions / total_away_predictions)

## evaluate the binary predictor variable
evaluate_predictions <- function(df_w_predictions, title) {
  
  ### confusion matrix
  conf_table <- conf_mat(table(df_w_predictions$actual_away, df_w_predictions$pred_away))
  conf_summary <- summary(conf_table, event_level = 'second')
  
  density_chart <- df_w_predictions |> 
    select(game_id, spread_line, .pred, result) |> 
    pivot_longer(!game_id, names_to = "type", values_to = "value") |> 
    ggplot(aes(value, fill = type, color = type)) +
    geom_density(alpha = 0.1) +
    theme_minimal() +
    labs(title = title)
  
  return(list(conf_table, conf_summary, density_chart))
}

## get the away team's accuracy
get_away_team_accuracy <- function(df_w_predictions, model){
  
  conf_table <- conf_mat(table(df_w_predictions$pred_away_true, df_w_predictions$actual_away))
  conf_summary <- summary(conf_table, event_level = 'second')
  conf_summary |> filter(.metric == 'accuracy') |> mutate(model = model)
}

## model formula
fmla <- formula("spread_line ~ 
                   rolling_rush_epa_avg_rank_away_team + 
                   rolling_rush_epa_avg_rank_home_team + 
                   
                   rolling_pass_epa_avg_rank_away_team + 
                   rolling_pass_epa_avg_rank_home_team +
                   
                   rolling_rush_def_epa_avg_rank_away_team +
                   rolling_rush_def_epa_avg_rank_home_team +
                   
                   rolling_pass_def_epa_avg_rank_away_team +
                   rolling_pass_def_epa_avg_rank_home_team
                   ")

# Linear Regression -------------------------------------------------------

## specify an LM model object
lm_spec <- linear_reg() |> 
  set_mode('regression') |> 
  set_engine('lm')

## fit the model
lm_fit <- lm_spec |> fit(formula = fmla, data = df_train)
lm_fit |> pluck("fit") |> summary()

## make predictions on df_train
lm_predictions <- predict_cover_team(lm_fit, df_train)
lm_predictions|> summarize(accuracy = mean(pred_straight))
evaluate_predictions(lm_predictions, title = "Linear Regression Density Chart")

# Random Forest -----------------------------------------------------------

## specify the random forest object
rf_spec <- rand_forest(
  mtry = tune(), 
  trees = 1000,
  min_n = tune()
) |> 
  set_mode('regression') |> 
  set_engine('ranger')

## create a workflow
rf_tune_wf <- workflow() |> 
  add_formula(fmla) |> 
  add_model(rf_spec)

## train hyperparameters
set.seed(234)
trees_folds <- vfold_cv(df_train)

### choose grid = 20 to choose 20 grid points automatically
doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  rf_tune_wf,
  resamples = trees_folds,
  grid = 20
)

## collect metrics
tune_res |> 
  collect_metrics() |> 
  filter(.metric == 'rmse') |> 
  select(mean, min_n, mtry) |> 
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) |> 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE") # lower values of mtry and 10-20 range for min_n is better

## set a range of hyperparameters to try based on the initial result
rf_grid <- grid_regular(
  mtry(range = c(10, 30)),
  min_n(range = c(2, 8)),
  levels = 5
)

rf_grid

## tune one more time
set.seed(456)
regular_res <- tune_grid(
  rf_tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

## collect metrics
regular_res |> 
  collect_metrics() |> 
  filter(.metric == 'rmse') |> 
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(x = NULL, y = "RMSE")

## choose the best model and finalize
final_rf <- finalize_model(
  rf_spec,
  select_best(regular_res, "rmse")
)

final_rf

## explore the model a little bit

### variable importance plot
library(vip)

final_rf |> 
  set_engine("ranger", importance = "permutation") |> 
  fit(fmla, 
      data = df_train) |> 
  vip(geom = "point")

## fit the model 
rf_fit <- final_rf |> fit(formula = fmla, data = df_train)
rf_fit

## make predictions on df_train
rf_predictions <- predict_cover_team(rf_fit, df_train)
rf_predictions |> summarize(accuracy = mean(pred_straight))
evaluate_predictions(rf_predictions, title = "Random Forest Density Chart")

# Gradient Boosting -------------------------------------------------------

## model specification
xgboost_model <- boost_tree(
  mode       = "regression", 
  trees      = 1000, 
  min_n      = tune(), 
  tree_depth = tune(), 
  learn_rate = tune()
) |> 
  set_engine("xgboost", objective = "reg:squarederror")

xgboost_model

## create a workflow
xgf_tune_wf <- workflow() |> 
  add_formula(fmla) |> 
  add_model(xgboost_model)

## grid spec
xgboost_params <- parameters(min_n(range = c(2, 8)), tree_depth(), learn_rate())
xgboost_params

set.seed(123)
xgboost_grid <- grid_max_entropy(xgboost_params, size = 30)
xgboost_grid

## tune model
doParallel::registerDoParallel()
xgboost_stage_1_cv_results_tbl <- tune_grid(
  xgf_tune_wf,
  resamples = trees_folds,
  grid      = xgboost_grid,
  metrics   = metric_set(mae, mape, rmse, rsq),
  control   = control_grid(verbose = TRUE)
)

xgboost_stage_1_cv_results_tbl %>% show_best("rmse", n = 10, maximize = FALSE)

## select the best xgboost model
params_xgboost_best <- xgboost_stage_1_cv_results_tbl %>% 
  select_best("rmse", maximize = FALSE)

params_xgboost_best

## finalize the model
xgboost_stage_2_model <- xgboost_model %>% 
  finalize_model(params_xgboost_best)

xgboost_stage_2_model

## fit the model 
xgb_fit <- xgboost_stage_2_model |> fit(formula = fmla, data = df_train)
xgb_fit

## make predictions on df_train
xgb_predictions <- predict_cover_team(xgb_fit, df_train)
xgb_predictions |> summarize(accuracy = mean(pred_straight))
evaluate_predictions(xgb_predictions, title = "XGBoost Density Chart")

# Compare all three models ------------------------------------------------

all_models <- bind_rows(
  lm_predictions |> mutate(model = "lm"),
  rf_predictions |> mutate(model = "rf"),
  xgb_predictions |> mutate(model = "xgb")
)

all_models |> 
  group_by(season, model) |> 
  summarize(accuracy = mean(pred_straight)) |> 
  pivot_wider(names_from = model, values_from = accuracy)

# make predictions on the test data ---------------------------------------

## linear model
lm_test_preds <- predict_cover_team(lm_fit, df_test)
lm_test_preds |> summarize(accuracy = mean(pred_straight))

## random forest model
rf_test_preds <- predict_cover_team(rf_fit, df_test)
rf_test_preds |> summarize(accuracy = mean(pred_straight))

## gradient boosting model
xgb_test_preds <- predict_cover_team(xgb_fit, df_test)
xgb_test_preds |> summarize(accuracy = mean(pred_straight))

## compare all three models
all_models_test_results <- bind_rows(
  lm_test_preds |> mutate(model = "lm"),
  rf_test_preds |> mutate(model = "rf"),
  xgb_test_preds |> mutate(model = "xgb")
)

## compare the away team's accuracy
get_away_team_accuracy(lm_test_preds, "lm")
get_away_team_accuracy(rf_test_preds, "rf")
get_away_team_accuracy(xgb_test_preds, "xgb")

## straight up predictions
all_models_test_results |> 
  group_by(season, model) |> 
  summarize(accuracy = mean(pred_straight)) |> 
  pivot_wider(names_from = model, values_from = accuracy)




