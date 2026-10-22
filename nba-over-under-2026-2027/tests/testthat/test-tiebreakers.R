# scores_tidy has one row per team per game (two rows per game), as
# tidy_scores() builds it, so these fixtures do the same.
tb_scores <- function(games) {
  bind_rows(
    games %>% transmute(slug_away_team = away, slug_home_team = home,
                        team_slug = away, opponent_slug = home,
                        score = away_score, opponent_score = home_score),
    games %>% transmute(slug_away_team = away, slug_home_team = home,
                        team_slug = home, opponent_slug = away,
                        score = home_score, opponent_score = away_score)
  )
}

games <- tribble(
  ~away, ~home, ~away_score, ~home_score,
  "A",   "B",   100,         90,   # A beats B
  "B",   "C",   100,         90,   # B beats C
  "C",   "A",   100,         90,   # C beats A
  "A",   "D",   100,         90,   # A beats D
  "E",   "B",   100,         90    # E beats B
)
tb <- tb_scores(games)

wins_of <- function(x, team, col = "winner") x$wins[x[[col]] == team]

test_that("calculate_head_to_head counts wins among the tied teams only", {
  out <- calculate_head_to_head(c("A", "B", "C"), tb)
  # A, B, and C each beat one other tied team; every game has two rows
  expect_equal(sort(out$winner), c("A", "B", "C"))
  expect_equal(out$wins, c(2, 2, 2))
  out2 <- calculate_head_to_head(c("A", "B"), tb)
  expect_equal(out2$winner, "A")
})

test_that("is_tie_resolved is TRUE only when no two teams share a win count", {
  expect_true(is_tie_resolved(tibble(wins = c(3, 2, 1))))
  expect_false(is_tie_resolved(tibble(wins = c(2, 2, 1))))
  expect_true(is_tie_resolved(tibble(wins = integer())))
})

record_games <- tb_scores(tribble(
  ~away, ~home, ~away_score, ~home_score,
  "A",   "D",   100,         90,   # A beats D
  "E",   "B",   100,         90    # E beats B
))

test_that("record_against gives wins, losses, and win percentage against a set of teams", {
  out <- record_against(c("A", "B"), record_games, c("D", "E"))
  row <- function(team) out[out$winner == team, ]
  expect_equal(row("A")$win_pct, 1)
  expect_equal(row("E")$win_pct, 1)
  expect_equal(row("D")$win_pct, 0)
  expect_equal(row("B")$win_pct, 0)
  expect_equal(row("B")$losses, 2)
  expect_equal(row("A")$losses, 0)
  expect_equal(row("A")$total_games, row("A")$wins + row("A")$losses)
})

test_that("record_against sorts by win percentage and ignores other games", {
  out <- record_against(c("A", "B"), record_games, c("D"))
  expect_setequal(out$winner, c("A", "D"))
  expect_equal(out$win_pct, c(1, 0))
})

test_that("record_against does not need a loser column on the scores", {
  # The first version grouped by a `loser` column that scores never had
  expect_false("loser" %in% names(record_games))
  expect_no_error(record_against(c("A", "B"), record_games, c("D", "E")))
})

test_that("each record wrapper uses its own opponent set", {
  info <- list(division_teams = "D", conference_teams = "E")
  playoff <- list(conference_teams = "D", opposing_conference_teams = "E")
  expect_setequal(calculate_division_record(c("A", "B"), record_games, info)$winner,
                  c("A", "D"))
  expect_setequal(calculate_conference_record(c("A", "B"), record_games, info)$winner,
                  c("E", "B"))
  expect_setequal(
    calculate_winning_pct_against_playoff_teams_conference(c("A", "B"), record_games, playoff)$winner,
    c("A", "D"))
  expect_setequal(
    calculate_winning_pct_against_playoff_teams_opposing_conference(c("A", "B"), record_games, playoff)$winner,
    c("E", "B"))
})

test_that("calculate_point_differential returns one row per tied team", {
  out <- calculate_point_differential(c("A", "B", "C"), tb)
  expect_setequal(out$team, c("A", "B", "C"))
  expect_equal(names(out), c("team", "total_point_diff"))
})

test_that("KNOWN BUG: calculate_point_differential nets to 0 on one-row-per-team scores", {
  # Each game's two rows give a-h and h-a for the same team, so they cancel.
  # When this is fixed, change this test to expect the real margins.
  out <- calculate_point_differential(c("A", "B", "C"), tb)
  expect_true(all(out$total_point_diff == 0))
})

test_that("finalize_ranking adds the tie-break columns for teams whose wins match", {
  standings <- tibble(team_name = c("A", "B"), wins = c(2, 5))
  result <- tibble(winner = c("A", "B"), wins = c(2, 9), losses = c(1, 1))
  out <- finalize_ranking(result, standings)
  expect_equal(out$losses, c(1, NA))
  expect_equal(nrow(out), 2)
})

test_that("finalize_ranking returns the standings unchanged with no wins column", {
  standings <- tibble(team_name = c("A", "B"), points = c(1, 2))
  result <- tibble(team = "A", total_point_diff = 3)
  expect_identical(finalize_ranking(result, standings), standings)
})

test_that("KNOWN BUG: finalize_ranking stops on a point-differential result", {
  # The result has no `winner` or `wins` column to join on
  standings <- tibble(team_name = c("A", "B"), wins = c(2, 5))
  result <- tibble(team = "A", total_point_diff = 3)
  expect_error(finalize_ranking(result, standings))
})

# apply_tiebreakers: say which step resolves the tie by passing a checker
# that answers FALSE until its nth call
resolves_at <- function(n) {
  calls <- 0
  function(...) {
    calls <<- calls + 1
    calls >= n
  }
}

tb_standings <- tibble(team_name = c("A", "B", "C"), wins = c(2, 2, 2))
tb_info <- list(division_teams = c("D", "E"), conference_teams = c("D", "E"))
tb_playoff <- list(conference_teams = c("D", "E"), opposing_conference_teams = c("D", "E"))
tb_tied <- c("A", "B")
tb_scores_all <- tb_scores(tribble(
  ~away, ~home, ~away_score, ~home_score,
  "A",   "B",   100,         90,
  "A",   "D",   100,         90,
  "B",   "E",   100,         90,
  "D",   "A",   100,         90,
  "E",   "B",   100,         90
))

run_tb <- function(is_resolved) {
  apply_tiebreakers(tb_tied, tb_scores_all, tb_info, tb_playoff, tb_standings,
                    is_resolved = is_resolved)
}

test_that("apply_tiebreakers stops at head-to-head when it settles the tie", {
  out <- run_tb(resolves_at(1))
  expect_equal(names(out), c("team_name", "wins"))
})

test_that("apply_tiebreakers moves on to the division record", {
  expect_true("losses" %in% names(run_tb(resolves_at(2))))
})

test_that("apply_tiebreakers moves on to the conference record", {
  expect_true("losses" %in% names(run_tb(resolves_at(3))))
})

test_that("apply_tiebreakers moves on to the record against playoff teams", {
  expect_true("losses" %in% names(run_tb(resolves_at(4))))
  expect_true("losses" %in% names(run_tb(resolves_at(5))))
})

test_that("KNOWN BUG: apply_tiebreakers stops at the point-differential step", {
  # finalize_ranking cannot join a point-differential result (see above)
  expect_error(run_tb(resolves_at(6)))
})

test_that("apply_tiebreakers returns the standings when no step settles the tie", {
  expect_identical(run_tb(function(...) FALSE), tb_standings)
})

test_that("apply_tiebreakers uses is_tie_resolved by default", {
  # Head-to-head between A and B has one winner, so the real check settles it
  out <- apply_tiebreakers(tb_tied, tb_scores_all, tb_info, tb_playoff, tb_standings)
  expect_equal(nrow(out), 3)
})
