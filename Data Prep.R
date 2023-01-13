library(tidyverse)
library(nflreadr)

# Load Data ---------------------------------------------------------------

seasons <- 2010:2022

## schedules

schedules <- load_schedules(seasons = seasons)

## EPA and total plays

pbp <- load_pbp(seasons = seasons)

### calculate rushing and passing avg EPA for each gameID, and posteam
off_epa <- pbp |> 
  filter(rush == 1, !is.na(epa)) |> 
  group_by(game_id, posteam) |> 
  summarize(total_rush_epa = sum(epa),
            total_rush_plays = n()) |> 
  ungroup() |> 
  
  # join pass epa 
  inner_join(
    pbp |> 
      filter(pass == 1, !is.na(epa)) |> 
      group_by(game_id, posteam) |> 
      summarize(total_pass_epa = sum(epa),
                total_pass_plays = n()) |> 
      ungroup(),
    
    by = c("game_id", "posteam")
  )

### calculate rushing and passing defensive EPA for each gameID and posteam
def_epa <- pbp |> 
  filter(rush == 1, !is.na(epa)) |> 
  group_by(game_id, defteam) |> 
  summarize(total_rush_def_epa = sum(epa),
            total_rush_def_plays = n()) |> 
  ungroup() |> 
  
  # join pass epa 
  inner_join(
    pbp |> 
      filter(pass == 1, !is.na(epa)) |> 
      group_by(game_id, defteam) |> 
      summarize(total_pass_def_epa = sum(epa),
                total_pass_def_plays = n()) |> 
      ungroup(),
    
    by = c("game_id", "defteam")
  )

### join both offensive and defensive EPA stats on gameID and team
epa_df <- off_epa |> 
  inner_join(def_epa, by = c("game_id", "posteam" = "defteam")) |> 
  rename(team = posteam) |> 
  mutate(team = clean_team_abbrs(team))

# Join Data ---------------------------------------------------------------

# Calculate Rolling Means -------------------------------------------------

# EDA ---------------------------------------------------------------------







