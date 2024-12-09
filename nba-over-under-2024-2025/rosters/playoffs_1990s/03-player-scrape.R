library(rvest)
library(tidyverse)
library(jsonlite)  # Added for JSON parsing

# Function to scrape individual player information
scrape_player_info <- function(player_link) {
  # Base URL
  base_url <- "https://www.basketball-reference.com"
  
  message("Scraping data for", player_link, "\n")
  
  # Full URL for the player page
  player_url <- paste0(base_url, player_link)
  
  # Read the HTML content of the page
  page <- read_html(player_url)
  
  # Extract and clean JSON content
  birth_data_raw <- page %>%
    html_nodes("script[type='application/ld+json']") %>%
    html_text(trim = TRUE)
  
  # Identify the correct JSON block containing "@type": "Person"
  birth_data_json <- birth_data_raw[grep('"@type": "Person"', birth_data_raw)]
  
  # Safely parse JSON content if valid JSON is found
  birth_data <- tryCatch(
    fromJSON(birth_data_json, flatten = TRUE),
    error = function(e) list(birthDate = NA, birthPlace = NA)
  )
  
  birth_date <- birth_data$birthDate
  birth_place <- birth_data$birthPlace
  
  # Extract the JSON-LD script data
  metadata_raw <- page %>%
    html_nodes("script[type='application/ld+json']") %>%
    html_text(trim = TRUE)
  
  # Find the JSON block containing "@type": "Person"
  player_metadata <- metadata_raw[grep('"@type": "Person"', metadata_raw)]
  
  # Parse JSON safely
  metadata <- tryCatch(
    fromJSON(player_metadata, flatten = TRUE),
    error = function(e) list(height = NA, weight = NA)
  )
  
  height <- metadata$height$value %>% coalesce(NA)
  weight <- metadata$weight$value %>% coalesce(NA)
  
  draft_info <- page %>%
    html_node(xpath = "//p[strong[contains(text(), 'Draft')]]") %>%
    html_text(trim = TRUE)
  
  # Extract per-game stats table
  per_game_stats <- page %>%
    html_node("#per_game_stats") %>%
    {if (!is.null(.)) html_table(., header = TRUE, fill = TRUE) else tibble()}
  
  
  # Add player link to the per-game stats
  per_game_stats <- per_game_stats %>%
    mutate(player_link = player_link, .before = 1)
  
  # Combine all extracted data into a single tidy tibble
  player_info <- tibble(
    player_link = player_link,
    birthDate = birth_date,
    birthPlace = birth_place,
    height = height,
    weight = weight,
    draft_info = draft_info
  )
  
  Sys.sleep(10)
  
  return(list(player_info = player_info, per_game_stats = per_game_stats))
}

player_links <- unique(playoff_starters_cleaned$player_link)

# Using scrape_player_info to get player info and stats
get_player_data <- function(player_link) {
  result <- scrape_player_info(player_link)
  list(
    player_info = result$player_info,
    per_game_stats = result$per_game_stats
  )
}

# Create a tibble with player links and scraped data
player_data_raw <- tibble(
  player_links = player_links,
  player_stats = map(player_links, get_player_data)
)

write_rds(player_data_raw, "player_data_raw.rds")

# Extracting structured columns for convenience
player_data <- player_data_raw %>%
  mutate(
    player_info = map(player_data, "player_info"),
    player_stats = map(player_data, "per_game_stats")
  ) %>%
  select(-player_data)
