# Small fixtures shared by the tests. Three teams and two days:
#   Oct 20: Alpha beats Bravo.
#   Oct 21: Clippers beat Alpha (Clippers' first game; Bravo does not play).
# Every team has a 50% line, so Alpha is OVER after game 1 and PUSH after
# game 2, Bravo is UNDER, and the Clippers are OVER.

library(dplyr)
library(tibble)
library(testthat)

# Load the code under test. covr::file_coverage() has already defined these
# when it runs the tests, and tests/run-tests.R depends on that.
if (!exists("compute_game_results")) {
  source(test_path("..", "..", "R", "standings_logic.R"))
}
if (!exists("apply_tiebreakers")) {
  source(test_path("..", "..", "R", "tiebreakers.R"))
}

d1 <- as.Date("2026-10-20")
d2 <- as.Date("2026-10-21")

game_results_raw <- tibble(
  slugSeason = "2026-27",
  dateGame = c(d1, d1, d2, d2),
  numberGameTeamSeason = c(1L, 1L, 2L, 1L),
  nameTeam = c("Alpha", "Bravo", "Alpha", "LA Clippers"),
  slugMatchup = c("ALP vs. BRA", "BRA @ ALP", "ALP @ CLI", "CLI vs. ALP"),
  slugTeam = c("ALP", "BRA", "ALP", "CLI"),
  slugOpponent = c("BRA", "ALP", "CLI", "ALP"),
  slugTeamLoser = c("BRA", "BRA", "ALP", "ALP")
)

# One row per game, as 02c builds it
scores <- tibble(
  game_id = c("g1", "g2"),
  game_date = c(d1, d2),
  away_team = c("Bravo", "Alpha"),
  home_team = c("Alpha", "LA Clippers"),
  slug_away_team = c("BRA", "ALP"),
  slug_home_team = c("ALP", "CLI"),
  away_score = c(90, 100),
  home_score = c(95, 110)
)

projections <- tibble(
  team = c("Alpha", "Bravo", "Los Angeles Clippers"),
  win_projection = c(41, 41, 41),
  percentage_projection = c(50, 50, 50)
)

picks <- tribble(
  ~team,                  ~player, ~wage, ~choice,
  "Alpha",                "P1",    15,    "OVER",
  "Bravo",                "P1",    14,    "UNDER",
  "Alpha",                "P2",    15,    "UNDER",
  "Los Angeles Clippers", "P2",    13,    "OVER"
)

# The pipeline up to the scored picks, built once for the tests that need it
game_results <- compute_game_results(game_results_raw)
scores_tidy <- tidy_scores(scores)
updated_schedule <- add_game_scores(game_results, scores_tidy)
join_with_projections <- add_over_under(updated_schedule, projections, d2)
standings_grid <- build_standings_grid(join_with_projections, d1, d2)
scored_picks <- score_picks(standings_grid, picks)
