library(tidyverse)

nba_rosters_raw <- read_rds("rosters/rosters_1980_to_2024/nba_rosters_1980_to_2024.rds") 

teams_per_season <- nba_rosters_raw |> 
  group_by(yearSeason) |> 
  distinct(nameTeam)

num_teams_per_season <- teams_per_season |> 
  ungroup() |> 
  count(yearSeason)
