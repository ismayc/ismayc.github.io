library(nbastatR)
library(tidyverse)
library(purrr)
library(tictoc)

Sys.setenv("VROOM_CONNECTION_SIZE" = 100000000000)

#nba_players <- nbastatR::nba_players()
#readr::write_rds(nba_players, "rosters/nba_players.rds")

nba_players <- read_rds("rosters/nba_players.rds")

rm(dataPlayerCareerTotalsRegularSeason)
career_stats_list <- nbastatR::players_careers(
  player_ids = nba_players$idPlayer[1],
  modes = "Totals"
)#read_rds("~/Desktop/regular_season_career-1to500-2022-03-26.rds")

#career_stats <- dataPlayerCareerTotalsRegularSeason
#write_rds(career_stats, "~/Desktop/regular_season_career-1to1562-2022-03-26.rds")
career_stats <- read_rds("~/Desktop/regular_season_career-1to1562-2022-03-26.rds")

#for(i in 2:nrow(nba_players)) {
for(i in 1617:nrow(nba_players)) {
  career_stats_temp <- nbastatR::players_careers(
    player_ids = nba_players$idPlayer[i],
    modes = "Totals"
  )
  Sys.sleep(5)
  if(exists("dataPlayerCareerTotalsRegularSeason")){
    career_stats <- dplyr::bind_rows(career_stats, dataPlayerCareerTotalsRegularSeason)
    rm(dataPlayerCareerTotalsRegularSeason)
  }
  write_rds(career_stats, "~/Desktop/regular_season_career-iterating-2022-03-28.rds")
}

# Old way
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