library(rvest)
library(tidyverse)
library(jsonlite)

season <- "2025-26"
year <- 2026
nba_cup_champ_date <- "2025-12-16"

# https://www.basketball-reference.com/leagues/NBA_2025_games-october.html

base_url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                   "_games-")
months <- c("october", "november", "december", "january", "february", "march", "april")

# scrape_month <- function(month) {
#   url <- paste0(base_url, month, ".html")
#   page <- read_html(url)
#   
#   # Extract the JSON-LD script
#   json_ld <- page %>%
#     html_element("script[type='application/ld+json']") %>%
#     html_text()
#   
#   # Parse the JSON-LD into a list
#   games_list <- fromJSON(json_ld, simplifyVector = FALSE)
#   
#   # Filter for objects of type "SportsEvent"
#   sports_events <- keep(games_list, ~ .x$`@type` == "SportsEvent")
#   
#   # Convert each event into a tidy data frame
#   games_df <- map_dfr(sports_events, ~{
#     tibble(
#       home_team = .x$competitor[[2]]$name,
#       away_team = .x$competitor[[1]]$name,
#       description = .x$description,
#       date = .x$startDate,
#       location = .x$location$name,
#       url = .x$url
#     )
#   })
#   
#   Sys.sleep(5)
#   return(games_df)
# }

scrape_month <- function(month) {
  url <- paste0(base_url, month, ".html")
  page <- read_html(url)
  
  # Extract the table
  table_raw <- page %>%
    html_table(header = TRUE) %>%
    .[[1]]  # Assuming the table of interest is the first
  
  # Rename all columns explicitly
  table <- table_raw %>%
    rename(
      game_date = Date,
      start_time = `Start (ET)`,
      away_team = `Visitor/Neutral`,
      visitor_pts = 4,
      home_team = `Home/Neutral`,
      home_pts = 6,
      box_score = 7,
      overtime = 8,
      attendance = Attend.,
      game_duration = LOG,
      arena = Arena,
      notes = 12
    ) # |> 
 #   mutate(attendance = as.numeric(attendance))

  message("Scraping schedule for month of ", str_to_sentence(month))
  Sys.sleep(3)
  
  table |> select(game_date, start_time, away_team, home_team)
}

nba_schedule_raw <- map_dfr(months, scrape_month)

# Example: Convert date column to Date type
nba_schedule <- nba_schedule_raw %>%
  mutate(
    game_date = as.Date(game_date, format = "%a, %b %d, %Y"),
    start_time = as.character(start_time)
  ) |> 
  # Filter out in-season tournament championship game
  filter(!game_date == as.Date(nba_cup_champ_date))

# Check for correct number of home and away games
home <- nba_schedule %>%
  group_by(home_team) %>%
  summarise(n_home_games = n()) %>%
  arrange(desc(n_home_games)) 

away <- nba_schedule %>%
  group_by(away_team) %>%
  summarise(n_away_games = n()) %>%
  arrange(desc(n_away_games))

# Check that each team has 41 home and 41 away
home %>% filter(n_home_games != 41)
away %>% filter(n_away_games != 41)

#write_csv(nba_schedule, paste0("schedule-", season, "_initial.csv"))
write_csv(nba_schedule, paste0("schedule-", season, "-after-ist.csv"))
file.copy(paste0("schedule-", season, "-after-ist.csv"),
          to = "..")
