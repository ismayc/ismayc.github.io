# Load necessary libraries
library(rvest)
library(httr)
library(dplyr)
library(purrr)

year_start <- 1993

playoff_starters_by_game_by_year <- function(starting_year) {
  
  cat("Season: ", starting_year, "-", starting_year + 1, "\n")
  
  # Set the URL
  playoffs_url <- paste0("https://www.basketball-reference.com/playoffs/NBA_", 
                         year_start, ".html")
  
  # Define function
  safe_read_html <- function(url) {
    tryCatch({
      res <- httr::GET(url, httr::user_agent("Mozilla/5.0"))
      if (httr::status_code(res) == 200) {
        content <- httr::content(res, as = "text", encoding = "UTF-8")
        read_html(content, encoding = "UTF-8")
        #      message("Successfully read URL: ", url)
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
          season = year_start,
          game_link = game_link,
          team_abbr = toupper(team_abbr),
          team_full_name = team_full_name,
          player_name = starters,
          stringsAsFactors = FALSE
        ) |> mutate(game_info = game_title)
        
        starting_lineups_df <- rbind(starting_lineups_df, team_lineup)
      }
      # Be polite to the server
      Sys.sleep(10)
    }
  }
  
  write_rds(starting_lineups_df, paste0("starting_lineups_", starting_year, ".rds"))
}

purrr::walk(year_start:1999, playoff_starters_by_game_by_year)
