library(tidyverse)
library(tidymodels)

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

# Linear Regression -------------------------------------------------------

## specify an LM model object
lm_spec <- linear_reg() |> 
  set_mode('regression') |> 
  set_engine('lm')

## specify the formula
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

## fit the model
lm_fit <- lm_spec |> fit(formula = fmla, data = df_train)
lm_fit |> pluck("fit") |> summary()

## make predictions on df_train
augment(lm_fit, new_data = df_train) |> 
  select(game_id, spread_line, .pred, .resid) |> 
  left_join(schedules |> select(result, game_id:home_score), by = "game_id") |> 
  mutate(pred_cover_team = case_when(
    .pred < spread_line ~ home_team, 
    .pred > spread_line ~ away_team
  )) |> 
  relocate(pred_cover_team, .after = .resid) |> 
  View()







