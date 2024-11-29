library(readr)
library(dplyr)
library(nbastatR)
library(stringr)
library(retry)  
library(httr)
library(glue)
library(lubridate)
library(purrr)

# Define the seasons
seasons <- 1979:2025

# Custom function to fetch data from the NBA Stats API
curl_chinazi <- function(url) {
  headers <- c(
    `Host` = 'stats.nba.com',
    `User-Agent` = 'Mozilla/5.0',
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
team_season_roster <- function(team = NULL, team_id = NULL, season, return_message = TRUE) {
  # Validate input
  if (is.null(team_id) && (missing(team) || !is.character(team) || nchar(team) == 0)) {
    stop("Please enter a valid team name or team_id.")
  }
  
  season_start <- season - 1
  slugSeason <- paste(season_start, substr(as.character(season), 3, 4), sep = "-")
  
  if (is.null(team_id)) {
    if (!exists("df_dict_team_history", envir = .GlobalEnv)) {
      df_dict_team_history <- nba_franchise_history(only_active = FALSE)
      assign("df_dict_team_history", df_dict_team_history, envir = .GlobalEnv)
    }
    
    team_id <- df_dict_team_history %>%
      filter(str_detect(nameTeam, fixed(team, ignore_case = TRUE))) %>%
      pull(idTeam) %>%
      unique()
    
    if (length(team_id) == 0) {
      stop("No team ID found for team: ", team)
    }
  }
  
  json_url <- glue("https://stats.nba.com/stats/commonteamroster?LeagueID=00&Season={slugSeason}&TeamID={team_id}")
  
  # Define a function to fetch data with retries
  fetch_data_with_retry <- function(url, max_attempts = 5, delay = 5) {
    retry::retry(
      expr = {
        json_data <- curl_chinazi(url)
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
    mutate(slugSeason = slugSeason, nameTeam = team, season = season) %>%
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

# Load the full franchise history
df_dict_team_history <- nba_franchise_history(only_active = FALSE)
assign("df_dict_team_history", df_dict_team_history, envir = .GlobalEnv)

# Function to get teams active in a given season
get_teams_for_season <- function(season) {
  teams_in_season <- df_dict_team_history %>%
    filter((yearStart <= season) & (is.na(yearEnd) | yearEnd >= season)) %>%
    select(nameTeam, idTeam) %>%
    unique()
  return(teams_in_season)
}

# Directory to save season data
output_dir <- "nba_rosters_by_season"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Fetch rosters for all teams across all seasons
for (season in seasons) {
  message("Processing season: ", season)
  
  teams_in_season <- get_teams_for_season(season)
  
  season_rosters <- purrr::map_dfr(1:nrow(teams_in_season), function(i) {
    team_id <- teams_in_season$idTeam[i]
    team_name <- teams_in_season$nameTeam[i]
    
    tryCatch(
      team_season_roster(team = team_name, team_id = team_id, season = season, return_message = TRUE),
      error = function(e) {
        message("Failed to fetch data for team: ", team_name, " - ", e$message)
        NULL  # Skip this team on error
      }
    )
  })
  
  # Save the season's data to an RDS file
  output_file <- file.path(output_dir, paste0("nba_rosters_", season, ".rds"))
  write_rds(season_rosters, output_file)
  message("Saved data for season ", season, " to ", output_file)
  
  # Optional: Pause between seasons to respect rate limits
  Sys.sleep(5)
}
