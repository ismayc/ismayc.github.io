library(readr)
library(dplyr)
library(nbastatR)
library(stringr)
library(retry)  
library(httr)
library(glue)
library(lubridate)
library(purrr)

# Define the season
season <- 2025

curl_chinazi <- function(url = "https://stats.nba.com/stats/leaguegamelog?Counter=1000&Season=2019-20&Direction=DESC&LeagueID=00&PlayerOrTeam=P&SeasonType=Regular%20Season&Sorter=DATE") {
  
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv=72.0) Gecko/20100101 Firefox/72.0',
    `Accept` = 'application/json, text/plain, */*',
    `Accept-Language` = 'en-US,en;q=0.5',
    `Accept-Encoding` = 'gzip, deflate, br',
    `x-nba-stats-origin` = 'stats',
    `x-nba-stats-token` = 'true',
    `Connection` = 'keep-alive',
    `Referer` = 'https://stats.nba.com/',
    `Pragma` = 'no-cache',
    `Cache-Control` = 'no-cache'
  )
  
  res <- GET(url, add_headers(.headers = headers))
  
  # Check for successful response
  if (status_code(res) != 200) {
    stop("Failed to fetch data: HTTP ", status_code(res))
  }
  
  # Parse JSON content
  json <- content(res, as = "parsed", simplifyVector = TRUE)
  
  # Validate the structure of the received JSON
  if (is.null(json$resultSets)) {
    stop("Invalid data structure received from the NBA Stats API.")
  }
  
  return(json)
}


# Define the team_season_roster function with retry logic
team_season_roster <- function(team = "Denver Nuggets", season = season, 
                               return_message = TRUE) {
  
  # Validate input
  if (missing(team) || !is.character(team) || nchar(team) == 0) {
    stop("Please enter a valid team name.")
  }
  
  season_start <- season - 1
  slugSeason <- paste(season_start, substr(as.character(season), 3, 4), sep = "-")
  
  if (!exists("df_dict_team_history", envir = .GlobalEnv)) {
    df_dict_team_history <- nba_franchise_history(only_active = TRUE)
    assign("df_dict_team_history", df_dict_team_history, envir = .GlobalEnv)
  }
  
  team_id <- df_dict_team_history %>%
    filter(str_detect(nameTeam, fixed(team, ignore_case = TRUE))) %>%
    pull(idTeam) %>%
    unique()
  
  if (length(team_id) == 0) {
    stop("No team ID found for team: ", team)
  }
  
  json_url <- glue("https://stats.nba.com/stats/commonteamroster?LeagueID=00&Season={slugSeason}&TeamID={team_id}") %>%
    as.character()
  
  # Define a function to fetch data with retries
  fetch_data_with_retry <- function(url, max_attempts = 5, delay = 5) {
    retry::retry(
      expr = {
        json_data <- nbastatR:::.curl_chinazi(url)
        if (is.null(json_data$resultSets)) {
          stop("Invalid data structure received.")
        }
        json_data
      },
      when = function(e) {
        message("Attempt failed: ", e$message)
        TRUE  # Retry on any error
      },
      max_tries = max_attempts,
      interval = delay
    )
  }
  
  # Fetch data with retry logic
  json_data <- fetch_data_with_retry(json_url)
  
  Sys.sleep(2) # Pause to avoid rate limiting
  
  names_roster <- unlist(json_data$resultSets$headers[[1]]) %>% str_to_lower()
  data_roster <- json_data$resultSets$rowSet[[1]] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    tibble::as_tibble()
  
  actual_names <- c("idTeam", "yearSeason", "idLeague", "namePlayer",
                    "firstNamePlayer", "playerHandle",
                    "numberJerseySeason", "groupPosition", "height",
                    "weightLBS", "dateBirth", "agePlayerSeason", "countSeasons",
                    "nameSchool", "idPlayer", "transaction")[seq_along(names(data_roster))]
  
  data_roster <- data_roster %>%
    purrr::set_names(actual_names) %>%
    mutate(slugSeason = slugSeason, nameTeam = team) %>%
    select(slugSeason, yearSeason, nameTeam, everything())
  
  data_roster <- data_roster %>%
    mutate(across(c("idTeam", "yearSeason",
                    "idLeague", "weightLBS", "agePlayerSeason", "countSeasons",
                    "idPlayer"), ~ as.character(.) %>% readr::parse_number())) %>%
    mutate(
      dateBirth = lubridate::mdy(dateBirth),
      countSeasons = if_else(is.na(countSeasons), 0, countSeasons),
      isRookie = countSeasons == 0
    ) %>%
    select(-idLeague) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  if (return_message) {
    message("Successfully fetched ", team, "'s roster for the ", slugSeason, " season.")
  }
  
  return(data_roster)
}

# Load teams data
teams <- read_rds("nba-over-under-2024-2025/rosters/teams.rds")

# Fetch player seasons data with error handling
players_season_pulled <- purrr::map_dfr(
  teams,
  ~ {
    tryCatch(
      team_season_roster(team = ., season = season, return_message = TRUE),
      error = function(e) {
        message("Failed to fetch data for team: ", ., " - ", e$message)
        NULL  # Skip this team on error
      }
    )
  }
)

# Save the results
write_rds(players_season_pulled, 
          "nba-over-under-2024-2025/rosters/players_season_pulled.rds")
