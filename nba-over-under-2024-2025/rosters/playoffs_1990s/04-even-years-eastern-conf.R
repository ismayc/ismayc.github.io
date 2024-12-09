library(tidyverse)

team_info <- read_csv("team_info.csv")

playoff_starters <- read_rds("playoff_starters_all_enriched.rds") |> 
  mutate(team_full_name = str_replace_all(team_full_name, 
                                          " Basic and Advanced Stats Table",
                                          "")) |> 
  mutate(team_abbr = str_replace_all(team_abbr, 
                                     "BRK", "BKN")) |> 
  left_join(team_info, by = c("team_abbr" = "abbreviation"))

east_even <- playoff_starters |> 
  filter(conference == "East", season %% 2 == 0, season >= 1996)

# Playoff appearances for each team
team_playoff_appearances <- east_even |> 
  distinct(season, team_full_name) |> 
  count(team_full_name)
