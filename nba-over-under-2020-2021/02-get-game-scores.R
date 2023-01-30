#install.packages("devtools")
#devtools::install_github("abresler/nbastatR")

library(tidyverse)
library(nbastatR)
library(here)
library(glue)

if(!file.exists(here(
  "rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
) {
  season_2021 <- game_logs(seasons = 2021)
  
  game_results_raw <- season_2021 %>% 
    distinct(slugSeason, nameTeam, dateGame, numberGameTeamSeason, 
             slugMatchup, slugTeam, slugOpponent, slugTeamLoser)
  
  write_rds(game_results_raw, 
            here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
} else {
  game_results_raw <- read_rds(
    here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds"))
  )
}

slug_lookup <- game_results_raw %>% 
  distinct(nameTeam, slugTeam) %>% 
  arrange(slugTeam)

.get_slug_year <- function() {
  current_date <- Sys.Date()
  current_year <- lubridate::year(current_date)
  current_month <- lubridate::month(current_date)
  slug_year <- dplyr::case_when(current_month >= 10 ~  current_year,
                                TRUE ~ current_year - 1)
  slug_year
}

scores <- nbastatR::current_schedule() %>% 
  filter(dateGame >= "2020-12-22") %>% 
  filter(!is.na(scoreAway)) %>% 
  #  mutate(is_home_winner = (isWinnerHome == 1),
  #         is_away_winner = (isWinnerAway == 1)) %>% 
  select(game_date = dateGame,
         game_id = idGame,
         slug_away_team = slugTeamAway,
         away_team = nameTeamAway,
         slug_home_team = slugTeamHome,
         home_team = nameTeamHome,
         away_score = scoreAway,
         home_score = scoreHome#,
         #         is_home_winner,
         #         is_away_winner
  )



#standings <- current_standings()
if(!file.exists(here("rds", glue("standings_through_{Sys.Date() - 1}.rds")))) {
  standings_url <- "https://data.nba.net/prod/v1/current/standings_all_no_sort_keys.json"
  standings_raw <- jsonlite::fromJSON(standings_url) %>% 
    pluck("league", "standard", "teams") %>% 
    mutate(teamId = as.integer(teamId))
  team_lookup <- nba_teams()
  
  standings_temp <- standings_raw %>% 
    inner_join(team_lookup, by = c("teamId" = "idTeam")) %>% 
    select(team_name = nameTeam, wins = win, losses = loss) %>% 
    inner_join(meta, by = c("team_name" = "team")) %>% 
    mutate(wins = as.integer(wins), losses = as.integer(losses)) %>% 
    mutate(`Winning Pct` = round(wins / (wins + losses), 3)) %>% 
    mutate(differential = wins - losses)
  
  top_differentials <- standings_temp %>% 
    group_by(conference) %>% 
    slice_max(n = 1, order_by = differential) %>% 
    select(team_name, differential) %>% 
    ungroup()
  
  standings <- standings_temp %>% 
    mutate(top_diff = ifelse(
      conference == "West",
      top_differentials %>% filter(conference == "West") %>% pull(differential),
      top_differentials %>% filter(conference == "East") %>% pull(differential))
    ) %>% 
    group_by(conference) %>% 
    mutate(`Games Back` = (top_diff - differential) / 2) %>% 
    select(-top_diff) %>% 
    ungroup() %>% 
    mutate(`Games Back` = if_else(
      `Games Back` == 0, 
      "-", 
      sprintf("%.1f", round(`Games Back`, 1)))
    )
  
  write_rds(standings, 
            here("rds", glue("standings_through_{Sys.Date() - 1}.rds")))
} else {
  standings <- read_rds(
    here("rds", glue("standings_through_{Sys.Date() - 1}.rds"))
  )
}