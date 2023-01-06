library(tidyverse)
library(nflreadr)

# Load Data ---------------------------------------------------------------


# schedules

### load schedules data
schedules <- load_schedules(seasons = 2010:2022)

### clean it
schedules_clean <- clean_homeaway(schedules, invert = c("result","spread_line"))
