# Build the example data the help pages and articles use.
#   R CMD INSTALL . && Rscript data-raw/make-datasets.R   (from ouanalytics/)
# The pool example is the small fixture the tests use: three teams, two days.
# The seeding example is the 19-team league the seeding tests use.

suppressMessages({library(dplyr); library(tibble); library(ouanalytics)})
source("tests/testthat/helper-fixtures.R", local = TRUE)
source("tests/testthat/helper-league.R", local = TRUE)

pool_example <- list(
  game_results_raw = game_results_raw,
  scores = scores,
  projections = projections,
  picks = picks,
  standings = tibble(team_name = c("Alpha", "Bravo", "Los Angeles Clippers"),
                     wins = c(1, 0, 1), losses = c(1, 1, 0)),
  params = list(yesterday = d2, today = d2 + 1, season_start = d1,
                num_games = 82)
)

seeding_example <- list(
  meta = league_meta %>% select(-team),
  standings = league_standings,
  scores_tidy = league_scores
)

save(pool_example, file = "data/pool_example.rda", compress = "bzip2", version = 2)
save(seeding_example, file = "data/seeding_example.rda", compress = "bzip2", version = 2)
cat("wrote data/pool_example.rda and data/seeding_example.rda\n")
