library(tidyverse)
library(nflreadr)
library(slider)

# Prepare Data ------------------------------------------------------------

seasons <- 2010:2022

## schedules
schedules <- load_schedules(seasons = seasons)

## EPA and total plays
pbp <- load_pbp(seasons = seasons)

### calculate rushing and passing avg EPA for each gameID, and posteam
off_epa <- pbp |> 
  filter(rush == 1, !is.na(epa)) |> 
  group_by(game_id, season, week, posteam) |> 
  summarize(total_rush_epa = sum(epa),
            total_rush_plays = n()) |> 
  ungroup() |> 
  
  # join pass epa 
  inner_join(
    pbp |> 
      filter(pass == 1, !is.na(epa)) |> 
      group_by(game_id, season, week, posteam) |> 
      summarize(total_pass_epa = sum(epa),
                total_pass_plays = n()) |> 
      ungroup(),
    
    by = c("game_id", "season", "week", "posteam")
  )

### calculate rushing and passing defensive EPA for each gameID and posteam
def_epa <- pbp |> 
  filter(rush == 1, !is.na(epa)) |> 
  group_by(game_id, season, week, defteam) |> 
  summarize(total_rush_def_epa = sum(epa),
            total_rush_def_plays = n()) |> 
  ungroup() |> 
  
  # join defensive pass epa 
  inner_join(
    pbp |> 
      filter(pass == 1, !is.na(epa)) |> 
      group_by(game_id, season, week, defteam) |> 
      summarize(total_pass_def_epa = sum(epa),
                total_pass_def_plays = n()) |> 
      ungroup(),
    
    by = c("game_id", "season", "week", "defteam")
  )

### join both offensive and defensive EPA stats on gameID, team, and season
epa_df <- off_epa |> 
  inner_join(def_epa, by = c("game_id", "season", "week", "posteam" = "defteam")) |> 
  rename(team = posteam) |> 
  mutate(team = clean_team_abbrs(team))

# Calculate Rolling Means -------------------------------------------------

## let's try calculating 5-game rolling rush epa/play
## rollin mean = average of the current + the previous n rows.
## Thus, the epa roll mean of week 6 will include week 6 + epa in the previous 5 (n) games.
## what we want is the epa of the previous 5 games only when going into week 6. 
## we don't want to include week 6 epa cause that will be unknown going into week 6.
## thus, n=4. Thus, week 5 roll avg will include week 5 + epa in the previous 4 (n) games.
## than we lag this result to get the first 5 games avg epa for week 6

n=4 

epa_df |> 
  select(game_id:total_rush_plays) |>
  group_by(season, team) |> 
  mutate(
    rolling_rush_epa_per_play = lag(slide_sum(total_rush_epa, before=n, complete = T) /
      slide_sum(total_rush_plays, before=n, complete = T))
      ) |> 
  arrange(season, team, week) |> 
  View()





