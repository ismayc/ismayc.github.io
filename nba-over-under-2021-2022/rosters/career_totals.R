library(nbastatR)
library(tidyverse)
library(purrr)
library(tictoc)

Sys.setenv("VROOM_CONNECTION_SIZE" = 100000000000)

nba_players <- nbastatR::nba_players()

cat("\n")
cat(paste("Starting at", Sys.time()))
cat("\n")

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer,
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-", Sys.Date(), ".rds")
)
