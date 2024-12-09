# Load necessary libraries
library(rvest)
library(httr)
library(dplyr)
library(purrr)

year_start <- 1994

playoff_starters_by_game_by_year <- function(starting_year) {
  
  cat("Season: ", starting_year - 1, "-", starting_year, "\n")
  
  # Set the URL
  playoffs_url <- paste0("https://www.basketball-reference.com/playoffs/NBA_", 
                         starting_year, ".html")
  
  # Define function
  safe_read_html <- function(url) {
    tryCatch({
      res <- httr::GET(url, httr::user_agent("Mozilla/5.0"))
      if (httr::status_code(res) == 200) {
        content <- httr::content(res, as = "text", encoding = "UTF-8")
        message("Successfully read URL: ", url)
        return(read_html(content, encoding = "UTF-8"))
      } else {
        message("Failed to retrieve URL (HTTP ", httr::status_code(res), "): ", url)
        return(NULL)
      }
    }, error = function(e) {
      message("Error reading URL: ", url, "\n", e)
      return(NULL)
    })
  }
  
  # # Use httr's GET function with a user agent
  # response <- httr::GET(playoffs_url, httr::user_agent("Mozilla/5.0"))
  # 
  # # Check if the request was successful
  # if (httr::status_code(response) == 200) {
  #   # Parse the content
  #   playoffs_page <- httr::content(response, as = "text", encoding = "UTF-8")
  #   playoffs_html <- read_html(playoffs_page, encoding = "UTF-8")
  #   message("Successfully read the page.")
  # } else {
  #   stop("Failed to retrieve the page. Status code: ", httr::status_code(response))
  # }
  
  playoffs_html <- safe_read_html(playoffs_url)
  
  # Extract all game links from the page within tables
  game_links <- playoffs_html %>%
    html_nodes('table a[href^="/boxscores/"]') %>%
    html_attr('href') %>%
    unique()
  
  # Display the first few game links
  head(game_links)
  
  ### Starting Lineups
  
  # Initialize a data frame to store starting lineup data
  starting_lineups_df <- data.frame()
  
  for (game_link in game_links) {
    game_url <- paste0("https://www.basketball-reference.com", game_link)
    game_page <- safe_read_html(game_url)
    
    # Extract the title of the page
    game_title <- game_page %>%
      html_node("title") %>%
      html_text(trim = TRUE)
    
    if (!is.null(game_page)) {
      # Find team abbreviations used in table ids
      team_ids <- game_page %>%
        html_nodes('table[id^="box-"][id$="-game-basic"]') %>%
        html_attr("id") %>%
        unique()
      
      for (team_id in team_ids) {
        # Extract team abbreviation from table id
        team_abbr <- sub("box-(.*)-game-basic", "\\1", team_id)
        team_full_name <- game_page %>%
          html_node(paste0("#", team_id, " caption")) %>%
          html_text(trim = TRUE)
        
        # Extract player rows
        player_rows <- game_page %>%
          html_node(paste0("#", team_id, " tbody")) %>%
          html_nodes("tr")
        
        # Initialize starters vector
        starters <- c()
        
        # Loop through the player rows
        for (player_row in player_rows) {
          # Get the class attribute
          class_attr <- html_attr(player_row, "class")
          # Check if the row is the separator for reserves
          if (!is.na(class_attr) && class_attr == "thead") {
            # We've reached the reserves section
            break
          }
          # Get player name from the <th> element
          player_name <- player_row %>%
            html_node('th[data-stat="player"]') %>%
            html_text(trim = TRUE)
          starters <- c(starters, player_name)
        }
        
        
        # Store the data
        team_lineup <- data.frame(
          season = starting_year,
          game_link = game_link,
          team_abbr = toupper(team_abbr),
          team_full_name = team_full_name,
          player_name = starters,
          stringsAsFactors = FALSE
        ) |> mutate(game_info = game_title)
        
        starting_lineups_df <- rbind(starting_lineups_df, team_lineup)
      }
      # Be polite to the server
      Sys.sleep(5)
    }
  }
  
  write_rds(starting_lineups_df, paste0("starting_lineups_", starting_year, ".rds"))
}

# playoff_starters_by_game_by_year(starting_year = 1990)

# purrr::walk(year_start:1999, playoff_starters_by_game_by_year)

# Read in each of the RDS files and stack them in a long data frame
playoff_starters_1990s_raw <- bind_rows(
  map(1990:1999, ~ readRDS(paste0("starting_lineups_", .x, ".rds")))
)

##################
# Function to extract player links from a game page
extract_player_links <- function(game_link) {
  game_url <- paste0("https://www.basketball-reference.com", game_link)
  game_page <- safe_read_html(game_url)
  
  if (!is.null(game_page)) {
    # Find team table IDs (e.g., box-LAL-game-basic)
    team_ids <- game_page %>%
      html_nodes('table[id^="box-"][id$="-game-basic"]') %>%
      html_attr("id") %>%
      unique()
    
    # Initialize a list to store player links
    player_links_list <- list()
    
    # Iterate over each team
    for (team_id in team_ids) {
      # Extract player rows (both starters and reserves)
      player_rows <- game_page %>%
        html_node(paste0("#", team_id, " tbody")) %>%
        html_nodes("tr")
      
      # Iterate over each player row
      for (player_row in player_rows) {
        # Get the class attribute
        class_attr <- html_attr(player_row, "class")
        
        # Check if the row is the separator for reserves
        if (isTRUE(class_attr == "thead")) {
          # We've reached the reserves section
          next
        }
        
        # Extract player name and link
        player_node <- player_row %>%
          html_node('th[data-stat="player"] a')
        
        if (!is.na(player_node)) {
          player_name <- player_node %>%
            html_text(trim = TRUE)
          player_link <- player_node %>%
            html_attr('href')
          
          # Append to the list
          player_links_list[[length(player_links_list) + 1]] <- list(
            game_link = game_link,
            player_name = player_name,
            player_link = player_link
          )
        }
      }
    }
    
    # Convert the list to a data frame
    if (length(player_links_list) > 0) {
      player_links_df <- bind_rows(player_links_list)
      return(player_links_df)
    } else {
      return(NULL)
    }
  } else {
    # Return NULL if the game page couldn't be read
    return(NULL)
  }
}

fill_player_links <- function(playoff_starters_df = playoff_starters_1990s_raw) {
  # Extract unique game links from the data frame
  unique_game_links <- unique(playoff_starters_df$game_link)
  
  # Initialize an empty data frame to store player links
  player_links_df <- data.frame(
    game_link = character(),
    player_name = character(),
    player_link = character(),
    stringsAsFactors = FALSE
  )
  
  # Iterate over each unique game link to extract player links
  for (game_link in unique_game_links) {
    message("Processing game link: ", game_link)
    
    extracted_links <- extract_player_links(game_link)
    
    if (!is.null(extracted_links)) {
      player_links_df <- bind_rows(player_links_df, extracted_links)
    } else {
      message("No player links extracted for game: ", game_link)
    }
    
    # Be polite to the server
    Sys.sleep(5)
  }
  
  # Check for any duplicate entries
  player_links_df %>%
    distinct(game_link, player_name, .keep_all = TRUE)
  
}

player_links_1990s_df <- fill_player_links()

# Merge the player_links_df with playoff_starters_1990s_raw
playoff_starters_1990s_enriched <- playoff_starters_1990s_raw %>%
  left_join(player_links_1990s_df, by = c("game_link", "player_name"))

# Save the enriched data frame
write_rds(playoff_starters_1990s_enriched, "playoff_starters_1990s_enriched.rds")







## Do the same for 2000s on
purrr::walk(2000:2024, playoff_starters_by_game_by_year)


# Read in each of the RDS files and stack them in a long data frame
playoff_starters_2000s_on_raw <- bind_rows(
  map(2000:2024, ~ readRDS(paste0("starting_lineups_", .x, ".rds")))
)

# Extract player links for the 2000s-on data
player_links_2000s_on_df <- fill_player_links(playoff_starters_2000s_on_raw)

# Merge the player_links_df with playoff_starters_1990s_raw
playoff_starters_2000s_on_enriched <- playoff_starters_2000s_on_raw %>%
  left_join(player_links_2000s_on_df, by = c("game_link", "player_name"))

# Save the enriched data frame
write_rds(playoff_starters_2000s_on_enriched, "playoff_starters_2000s_on_enriched.rds")

# Combine the enriched data frames
playoff_starters_all_enriched <- bind_rows(playoff_starters_1990s_enriched, 
                                           playoff_starters_2000s_on_enriched)

# Save the combined data frame
write_rds(playoff_starters_all_enriched, "playoff_starters_all_enriched.rds")
