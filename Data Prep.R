library(tidyverse)
library(nflreadr)
library(slider)

# Prepare Data ------------------------------------------------------------

seasons <- 2007:2022

## preliminary calculations

### number of rows the model data should have
(32*16*(length(seasons)-2))+(32*17*2)-2

### number of missing values in the rolling averages column
32*(n+1)*length(seasons)

### % missing
(32*(n+1)*length(seasons)) / ((32*16*(length(seasons)-2))+(32*17*2)-2) * 100

### number of rows left after removing missing values
((32*16*(length(seasons)-2))+(32*17*2)-2) - 32*(n+1)*length(seasons)

### increasing the number of seasons won't make a difference to the % missing unless n changes.
### but will have more games to build and test the model.

## schedules
schedules <- load_schedules(seasons = seasons) |> 
  mutate(away_team = clean_team_abbrs(away_team),
         home_team = clean_team_abbrs(home_team))

schedules_long <- schedules |> 
  select(game_id, away_team, home_team) |> 
  pivot_longer(!game_id, names_to = "type", values_to = "team")
  
## EPA and total plays
pbp <- load_pbp(seasons = seasons) |> filter(season_type == 'REG')

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

### figure out which teams covered the spread
team_cover_df <- schedules |> 
  clean_homeaway(invert = c("spread_line", "result")) |> 
  select(game_id, team, team_score, opponent, opponent_score, result, spread_line) |> 
  mutate(team_cover = case_when(
    result > spread_line ~ 1, 
    TRUE ~ 0
  )) |> 
  rename(actual_cover_team = team)

### qc team_cover_df
team_cover_df |> distinct(game_id) |> count() # total number of games
team_cover_df |> summarize(sum(team_cover)) # total number of games where a team covered the spread

team_cover_df |> group_by(game_id) |> 
  summarize(count = sum(team_cover)) |> 
  ungroup() |> 
  filter(count == 0) |> 
  View() # number of games where the spread bet was pushed

## final changes to team_cover_df
team_cover_final <- team_cover_df |> 
  filter(team_cover == 1) |> 
  select(game_id, actual_cover_team)

# Calculate Rolling Means -------------------------------------------------

## let's try calculating 5-game rolling rush epa/play
## rollin mean = average of the current + the previous n rows.
## Thus, the epa roll mean of week 6 will include week 6 + epa in the previous 5 (n) games.
## what we want is the epa of the previous 5 games only when going into week 6. 
## we don't want to include week 6 epa cause that will be unknown going into week 6.
## thus, n=4. Thus, week 5 roll avg will include week 5 + epa in the previous 4 (n) games.
## than we lag this result to get the first 5 games avg epa for week 6
## or the previous five game average going into any week after wk 6.

n=3

## calculate the previous 5 game average of the efficiency metrics
roll_epa_df <- epa_df |> 
  group_by(season, team) |> 
  mutate(
    
    ### rush EPA
    rolling_rush_epa_avg = lag(slide_sum(total_rush_epa, before = n, complete = T)/
                                 slide_sum(total_rush_plays, before = n, complete = T)),
    ### pass EPA
    rolling_pass_epa_avg = lag(slide_sum(total_pass_epa, before = n, complete = T) /
                                 slide_sum(total_pass_plays, before = n, complete = T)),
    
    ### rush def EPA
    rolling_rush_def_epa_avg = lag(slide_sum(total_rush_def_epa, before = n, complete = T) /
                                     slide_sum(total_rush_def_plays, before = n, complete = T)),
    
    ### pass def EPA
    rolling_pass_def_epa_avg = lag(slide_sum(total_pass_def_epa, before = n, complete = T) /
                                     slide_sum(total_pass_def_plays, before = n, complete = T))
      ) |> 
  arrange(season, team, week)

## check missing values
sapply(roll_epa_df, function(x) sum(is.na(x))) # <- actual missing
32*(n+1)*length(seasons) # <- expected missing
((32*16*(length(seasons)-2))+(32*17*2)-2) - 32*(n+1)*length(seasons) # <- expected number of rows after removing NAs

## calculate ranks
df_final <- roll_epa_df |> 
  filter(!is.na(rolling_rush_epa_avg)) |> 
  group_by(season, week) |> 
  mutate(rolling_rush_epa_avg_rank = rank(desc(rolling_rush_epa_avg), ties.method = 'average'),
         rolling_pass_epa_avg_rank = rank(desc(rolling_pass_epa_avg), ties.method = 'average'),
         rolling_rush_def_epa_avg_rank = rank(rolling_rush_def_epa_avg, ties.method = 'average'),
         rolling_pass_def_epa_avg_rank = rank(rolling_pass_def_epa_avg, ties.method = 'average')
         ) |> 
  ungroup() |>
  left_join(schedules_long, by = c("game_id", "team")) |> 
  relocate(type, .after = team) |>
  select(season,game_id, type, total_rush_epa:last_col()) |>
  pivot_wider(names_from = type, 
              values_from = c(total_rush_epa:last_col())) |> 
  left_join(schedules |> select(game_id, weekday, away_team, home_team, spread_line, 
                                div_game, roof, surface, away_coach, home_coach, stadium), 
            by = "game_id") |> 
  filter(!is.na(total_rush_epa_home_team), !is.na(total_rush_epa_away_team))

sapply(df_final, function(x) sum(is.na(x)))

