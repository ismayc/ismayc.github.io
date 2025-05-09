#install.packages("devtools")
#devtools::install_github("abresler/nbastatR")

library(tidyverse)
library(nbastatR)
library(here)
library(glue)

nba_season_start_date <- "2021-10-19"
nba_season_end_date <- "2022-04-10"
all_star_game_date <- "2022-02-20"
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
season <- 2022

if(!file.exists(here(
  "rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
) {
  season <- game_logs(seasons = season)   
  
  game_results_raw <- season %>% 
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

current_schedule <-
  function() {
    slug_year <-
      .get_slug_year()
    json <-
      glue::glue("https://data.nba.net/prod/v2/{slug_year}/schedule.json") %>%
      as.character() %>%
      jsonlite::fromJSON()
    
    json_data <- json$league$standard
    
    df_season_games <-
      json_data[!json_data %>% names() %in% c("period", "nugget", "hTeam", "vTeam", "watch", "playoffs")] %>%
      dplyr::as_tibble()
    
    df_season_games <-
      df_season_games %>%
      select(1:10) %>%
      purrr::set_names(
        c(
          "slugGame",
          "idStageGame",
          "slugGameCode",
          "idGameStatus",
          "hasExtendedStatus",
          "isUnknownStartTime",
          "datetimeGame",
          "dateSlugGame",
          "timeEasternGame",
          "hasBuzzerBeater"
        )
      ) %>%
      tidyr::separate(slugGameCode,
                      into = c("idGame", "slugTeams"),
                      sep = "/")
    season <-
      df_season_games$idGame[[1]] %>% substr(1, 4) %>% as.numeric() + 1
    
    df_season_games <-
      df_season_games %>%
      mutate(
        yearSeason = season,
        idGame = idGame %>% as.numeric(),
        slugTeamHome = slugTeams %>% substr(4, 6),
        slugTeamAway = slugTeams %>% substr(1, 3)
      ) %>%
      mutate(
        idGame = slugGame %>% as.numeric(),
        urlNBAGameBook = glue::glue(
          "https://data.nba.net/prod/v1/{dateSlugGame}/{dateSlugGame}_Book.pdf"
        ) %>% as.character(),
        datetimeGame = readr::parse_datetime(datetimeGame),
        dateGame = lubridate::ymd(dateSlugGame)
      ) %>%
      mutate(idRow = 1:n()) %>%
      select(idGame, everything()) %>%
      select(yearSeason,
             dateGame,
             slugTeamAway,
             slugTeamHome,
             everything())
    
    df_periods <-
      json_data$period %>%
      as_tibble() %>%
      purrr::set_names(c("quarterMaxPlayed", "idSeasonType", "maxQuartersRegular")) %>%
      mutate(
        hasOvertime = quarterMaxPlayed > 4,
        countOTQuarters =  quarterMaxPlayed - maxQuartersRegular,
        isComplete = !quarterMaxPlayed == 1
      ) %>%
      mutate(idRow = 1:n())
    
    df_descriptions <-
      tibble(descriptionGame = json_data$nugget$text) %>%
      mutate(idRow = 1:n()) %>%
      mutate_all(funs(ifelse(. == "", NA, .)))
    
    df_home <-
      json_data$hTeam %>% flatten() %>% dplyr::as_tibble() %>%
      purrr::set_names(c('idTeamHome', 'scoreHome', 'isWinnerHome', 'isLoserHome')) %>%
      mutate_all(as.numeric) %>%
      mutate(idRow = 1:n()) %>%
      left_join(nba_teams() %>% select(idTeamHome = idTeam, nameTeamHome = nameTeam)) %>%
      select(idTeamHome, nameTeamHome, everything()) %>%
      mutate(idRow = 1:n()) %>%
      suppressMessages()
    
    df_away <-
      json_data$vTeam %>% flatten() %>% dplyr::as_tibble() %>%
      purrr::set_names(c('idTeamAway', 'scoreAway', 'isWinnerAway', 'isLoserAway')) %>%
      mutate_all(as.numeric) %>%
      mutate(idRow = 1:n()) %>%
      left_join(nba_teams() %>% select(idTeamAway = idTeam, nameTeamAway = nameTeam)) %>%
      select(idTeamAway, nameTeamAway, everything()) %>%
      mutate(idRow = 1:n()) %>%
      suppressMessages()
    
    data <-
      list(df_season_games,
           df_periods,
           df_home,
           df_away,
           df_descriptions) %>%
      purrr::reduce(left_join) %>%
      suppressMessages() %>%
      select(-idRow) %>%
      dplyr::select(idSeasonType,
                    dateGame,
                    timeEasternGame,
                    idGame,
                    everything())
    
    data
  }

# scores_not_filled_in <- nbastatR::current_schedule() %>% 
#   filter(dateGame >= nba_season_start_date, dateGame < Sys.Date()) %>% 
#   filter(is.na(scoreAway))
# 
# if(nrow(scores_not_filled_in))
#   stop("Scores not updated. Please review the `scores_not_filled_in` tibble.")

scores <- nbastatR::current_schedule() %>% 
  filter(dateGame >= nba_season_start_date, dateGame < Sys.Date()) %>% 
#  filter(!is.na(scoreAway)) %>% 
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