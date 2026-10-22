pool_standings <- tibble(team_name = c("Alpha", "Bravo", "Los Angeles Clippers"),
                         wins = c(1, 0, 1), losses = c(1, 1, 0))

run_pool <- function() {
  compute_pool(game_results_raw, scores, projections, picks, pool_standings,
               yesterday = d2, today = d2 + 1, season_start = d1, num_games = 82)
}

test_that("compute_pool returns every table the page uses", {
  pool <- run_pool()
  expect_setequal(
    names(pool),
    c("game_results", "scores_tidy", "join_with_projections", "standings_grid",
      "scored_picks", "changes_in_player_projections", "rows_today",
      "projected_score", "player_projections_by_team", "current_rankings",
      "standings_with_projected", "wins_needed")
  )
})

test_that("compute_pool matches the steps run one at a time", {
  pool <- run_pool()
  expect_equal(pool$standings_grid, standings_grid)
  expect_equal(pool$scored_picks, scored_picks)
  expect_equal(pool$changes_in_player_projections, add_changes(scored_picks))
  expect_equal(pool$projected_score, projected_score_by_player(scored_picks))
})

test_that("compute_pool finds yesterday's changes and the ranking", {
  pool <- run_pool()
  expect_equal(nrow(pool$rows_today), 3)
  expect_equal(pool$current_rankings, c("P1", "P2"))
  expect_equal(nrow(pool$wins_needed), 3)
})

test_that("export_pool_case writes the inputs, the params, and every output", {
  dir <- withr::local_tempdir()
  pool <- run_pool()
  params <- list(yesterday = d2, today = d2 + 1, season_start = d1, num_games = 82)
  export_pool_case(dir, game_results_raw, scores, projections, picks,
                   pool_standings, params, pool)
  expect_setequal(
    list.files(file.path(dir, "inputs")),
    c("game_results_raw.csv", "scores.csv", "projections.csv", "picks.csv",
      "standings.csv", "params.csv"))
  expect_true(all(c(paste0(pool_output_names, ".csv"), "current_rankings.csv") %in%
                    list.files(file.path(dir, "outputs"))))
  p <- read.csv(file.path(dir, "inputs", "params.csv"))
  expect_equal(p$value[p$name == "yesterday"], "2026-10-21")
  expect_equal(p$value[p$name == "num_games"], "82")
})

test_that("exported tables keep dates as text, factors as text, and NA as empty", {
  dir <- withr::local_tempdir()
  pool <- run_pool()
  params <- list(yesterday = d2, today = d2 + 1, season_start = d1, num_games = 82)
  export_pool_case(dir, game_results_raw, scores, projections, picks,
                   pool_standings, params, pool)
  grid <- read.csv(file.path(dir, "outputs", "standings_grid.csv"),
                   colClasses = "character")
  expect_true(all(grepl("^2026-10-2[01]$", grid$date)))
  expect_equal(sum(grid$over_under == ""), 1)
  outcome <- read.csv(file.path(dir, "outputs", "player_projections_by_team.csv"))
  expect_true(all(outcome$Outcome.Determined %in% c("not yet", "OVER", "UNDER")))
  ranks <- read.csv(file.path(dir, "outputs", "current_rankings.csv"))
  expect_equal(ranks$Player, c("P1", "P2"))
  expect_equal(ranks$rank, 1:2)
})

test_that("export_seeding_case writes the seeding inputs and result", {
  dir <- withr::local_tempdir()
  seeded <- seed_conference("West", league_standings, league_scores, league_meta)
  export_seeding_case(dir, league_meta, league_standings, league_scores, seeded)
  expect_setequal(list.files(file.path(dir, "inputs")),
                  c("meta.csv", "standings.csv", "scores_tidy.csv"))
  out <- read.csv(file.path(dir, "outputs", "seeding.csv"), check.names = FALSE)
  expect_equal(out$abbreviation, seeded$abbreviation)
  expect_equal(out$seed, seeded$seed)
})
