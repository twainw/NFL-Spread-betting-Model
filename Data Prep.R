library(tidyverse)
library(nflreadr)

# Load Data ---------------------------------------------------------------

seasons <- 2010:2022

## schedules

### load and clean schedules data from 2010
schedules_clean <- clean_homeaway(load_schedules(seasons = seasons), 
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

## pbp

### load play-by-play data
pbp <- load_pbp(seasons = seasons)

### calculate rushing and passing avg EPA for each gameID, and posteam
off_epa <- pbp |> 
  filter(rush == 1, !is.na(epa)) |> 
  group_by(game_id, posteam) |> 
  summarize(rush_off_epa = mean(epa)) |> 
  ungroup() |> 
  
  # join pass epa 
  inner_join(
    pbp |> 
      filter(pass == 1, !is.na(epa)) |> 
      group_by(game_id, posteam) |> 
      summarize(pass_off_epa = mean(epa)) |> 
      ungroup(),
    
    by = c("game_id", "posteam")
  )

### calculate rushing and passing defensive EPA for each gameID and posteam
def_epa <- pbp |> 
  filter(rush == 1, !is.na(epa)) |> 
  group_by(game_id, defteam) |> 
  summarize(rush_def_epa = mean(epa)) |> 
  ungroup() |> 
  
  # join pass epa 
  inner_join(
    pbp |> 
      filter(pass == 1, !is.na(epa)) |> 
      group_by(game_id, defteam) |> 
      summarize(pass_def_epa = mean(epa)) |> 
      ungroup(),
    
    by = c("game_id", "defteam")
  )

### join both offensive and defensive EPA stats on gameID and team
epa_df <- off_epa |> 
  inner_join(def_epa, by = c("game_id", "posteam" = "defteam")) |> 
  rename(team = posteam)




