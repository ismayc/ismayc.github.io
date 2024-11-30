library(tidyverse)

nba_rosters_raw <- read_rds("rosters/rosters_1980_to_2024/nba_rosters_1980_to_2024.rds") 

teams_per_season <- nba_rosters_raw |> 
  group_by(yearSeason) |> 
  distinct(nameTeam)

num_teams_per_season <- teams_per_season |> 
  ungroup() |> 
  count(yearSeason)

first_season_teams <- nba_rosters_raw |> 
  filter(yearSeason == min(yearSeason)) |> 
  filter(!str_detect(nameTeam, "San Diego|Kansas City|Bullets|Seattle|New Jersey")) |> 
  distinct(nameTeam) |> 
  pull()

first_season_players <- nba_rosters_raw |> 
  filter(yearSeason == min(yearSeason)) |> 
  filter(!str_detect(nameTeam, "San Diego|Kansas City|Bullets|Seattle|New Jersey"))

# Check that players are duplicated in the older team names and current team names
old_teams_1980 <- nba_rosters_raw |> 
  filter(yearSeason == min(yearSeason)) |>
  filter(nameTeam %in% c("San Diego Clippers", "Kansas City Kings", "Washington Bullets", "Seattle SuperSonics", "New Jersey Nets")) |> 
  select(-nameTeam)

# Check that players are duplicated in the older team names and current team names
current_teams_1980 <- nba_rosters_raw |> 
  filter(yearSeason == min(yearSeason)) |>
  filter(nameTeam %in% c("Los Angeles Clippers", "Sacramento Kings", "Washington Wizards", "Oklahoma City Thunder", "Brooklyn Nets")) |> 
  select(-nameTeam)

all.equal(old_teams_1980, current_teams_1980)
