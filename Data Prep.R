library(tidyverse)
library(nflreadr)

# Load Data ---------------------------------------------------------------


# schedules

### load and clean schedules data from 2010
schedules_clean <- clean_homeaway(load_schedules(seasons = 2010:2022), 
                                  invert = c("result","spread_line"))

### build ats results data
ats_df <- schedules_clean |> 
  group_by(game_id, team) |> 
  slice_head(n = 1) |> 
  ungroup() |> 
  mutate(team_win = case_when(
    team_score > opponent_score ~ 1, 
    team_score == opponent_score ~ 0.5, 
    team_score < opponent_score ~ 0
  )) |> 
  mutate(team_cover = case_when(
    result > spread_line ~ 1, 
    TRUE ~ 0
  ))
