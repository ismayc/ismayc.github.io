library(shiny)
library(shinyjs)
library(tibble)
library(readr)
library(dplyr)

# Load player data by position
players <- read_rds("players_ballot.rds")

# Assign colors based on position
players$color <- ifelse(players$position == "Guard", "orange", 
                        ifelse(players$position == "Wing", "lightblue", "purple"))

players$text_color <- ifelse(players$color %in% c("purple"), "white", "black")

draft_order <- c("Phil", "Steve", "Chester", "Jake", "Ryan", "Mary")

# Snake draft order
snake_order <- tibble(
  pick_number = 1:60,
  round = rep(1:10, each = 6),
  player = unlist(lapply(1:10, function(r) {
    if (r %% 2 == 1) {
      draft_order
      #c("Mary", "Jake", "Steve", "Chester", "Ryan", "Phil")
    } else {
      rev(draft_order)
      #c("Phil", "Ryan", "Chester", "Steve", "Jake", "Mary")
    }
  }))
)