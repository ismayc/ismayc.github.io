library(nbastatR)
library(tidyverse)
library(purrr)
library(tictoc)

Sys.setenv("VROOM_CONNECTION_SIZE" = 100000000000)

#nba_players <- nbastatR::nba_players()
#readr::write_rds(nba_players, "rosters/nba_players.rds")

nba_players <- read_rds("rosters/nba_players.rds")

cat("\n")
cat(paste("Starting at", Sys.time()))
cat("\n")

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[501:1000],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-501to1000-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[1001:1500],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-1001to1500-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[1501:2000],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-1501to2000-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[2001:2500],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-2001to2500-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[2501:3000],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-2501to3000-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[3001:3500],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-3001to3500-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[3501:4000],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-3501to4000-", Sys.Date(), ".rds")
)

tic()
career_stats <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[4001:4730],
  modes = "Totals"
)
toc()

readr::write_rds(
  dataPlayerCareerTotalsRegularSeason,
  paste0("~/Desktop/regular_season_career-4001to4730-", Sys.Date(), ".rds")
)