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

## evaluate the binary predictor variable
evaluate_predictions <- function(df_w_predictions, title) {
  
  ### confusion matrix
  conf_table <- conf_mat(table(df_w_predictions$pred_away_true, df_w_predictions$actual_away))
  conf_summary <- summary(conf_table, event_level = 'second')
  
  density_chart <- lm_predictions |> 
    select(game_id, spread_line, .pred, result) |> 
    pivot_longer(!game_id, names_to = "type", values_to = "value") |> 
    ggplot(aes(value, fill = type, color = type)) +
    geom_density(alpha = 0.1) +
    theme_minimal() +
    labs(title = title)
  
  return(list(conf_table, conf_summary, density_chart))
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
                   rolling_pass_def_epa_avg_rank_home_team +
                   
                   roof +
                   surface + 
                   div_game
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
evaluate_predictions(lm_predictions, title = "Linear Regression Density Chart")




