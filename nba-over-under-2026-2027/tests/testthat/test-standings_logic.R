test_that("compute_game_results keeps running wins, losses, and percentage per team", {
  alpha <- game_results %>% filter(`Team Name` == "Alpha") %>% arrange(`Game Number`)
  expect_equal(alpha$current_wins, c(1, 1))
  expect_equal(alpha$current_losses, c(0, 1))
  expect_equal(alpha$current_win_perc, c(100, 50))
})

test_that("compute_game_results renames columns and spells out Los Angeles", {
  expect_true(all(c("Game Number", "Team Name", "Game Date") %in% names(game_results)))
  expect_true("Los Angeles Clippers" %in% game_results$`Team Name`)
  expect_false("LA Clippers" %in% game_results$`Team Name`)
})

test_that("compute_game_results sorts by team name", {
  expect_equal(as.character(game_results$`Team Name`),
               sort(as.character(game_results$`Team Name`)))
})

test_that("tidy_scores makes one row per team and pairs each score with the opponent's", {
  expect_equal(nrow(scores_tidy), 4)
  expect_equal(scores_tidy$team_slug, c("BRA", "ALP", "ALP", "CLI"))
  expect_equal(scores_tidy$opponent_slug, c("ALP", "BRA", "CLI", "ALP"))
  expect_equal(scores_tidy$score, c(90, 95, 100, 110))
  expect_equal(scores_tidy$opponent_score, c(95, 90, 110, 100))
})

test_that("tidy_scores spells out the Clippers", {
  expect_equal(scores_tidy$team_name,
               c("Bravo", "Alpha", "Alpha", "Los Angeles Clippers"))
})

test_that("add_game_scores joins scores, record, and the result text", {
  alpha_g1 <- updated_schedule %>%
    filter(`Team Name` == "Alpha", `Game Number` == 1)
  expect_equal(alpha_g1$final_score, "95-90")
  expect_equal(alpha_g1$`Current Record`, "1-0")
  expect_equal(alpha_g1$`Game Result`,
               "\nGame Number: 1\nGame Date: 2026-10-20\nGame Matchup: ALP vs. BRA\nFinal Score: 95-90")
  expect_equal(names(updated_schedule)[which(names(updated_schedule) == "Game Date") + 1],
               "game_id")
})

test_that("add_game_scores leaves the score blank for a game with no score row", {
  no_scores <- tidy_scores(scores[1, ])
  sched <- add_game_scores(game_results, no_scores)
  clippers <- sched %>% filter(`Team Name` == "Los Angeles Clippers")
  expect_equal(clippers$final_score, "NA-NA")
})

test_that("classify_over_under compares the percentage to the line", {
  expect_equal(classify_over_under(c(60, 40, 50, NA), 50),
               c("OVER", "UNDER", "PUSH", "PUSH"))
})

test_that("add_over_under classifies each game and marks flips", {
  alpha <- join_with_projections %>%
    filter(`Team Name` == "Alpha") %>% arrange(`Game Number`)
  expect_equal(alpha$over_under, c("OVER", "PUSH"))
  expect_equal(alpha$flipped, c(NA, TRUE))
  bravo <- join_with_projections %>% filter(`Team Name` == "Bravo")
  expect_equal(bravo$over_under, "UNDER")
  expect_equal(bravo$flipped, NA)
})

test_that("add_over_under flags only yesterday's games as flipped_today", {
  expect_equal(join_with_projections %>% arrange(`Team Name`, `Game Number`) %>%
                 pull(flipped_today),
               c(NA, TRUE, NA, TRUE))
})

test_that("add_over_under drops a team that has no projection", {
  fewer <- add_over_under(updated_schedule, projections[projections$team != "Bravo", ], d2)
  expect_false("Bravo" %in% fewer$`Team Name`)
})

test_that("build_standings_grid has one row per team per day", {
  expect_equal(nrow(standings_grid), 3 * 2)
  expect_setequal(standings_grid$date, c(d1, d2))
})

test_that("build_standings_grid shows 0-0 and no status before a team's first game", {
  clippers_d1 <- standings_grid %>%
    filter(team == "Los Angeles Clippers", date == d1)
  expect_equal(clippers_d1$`Current Record`, "0-0")
  expect_equal(clippers_d1$current_wins, 0)
  expect_true(is.na(clippers_d1$over_under))
  expect_true(is.na(clippers_d1$`Current Win %`))
})

test_that("build_standings_grid carries the last status over a day off", {
  bravo_d2 <- standings_grid %>% filter(team == "Bravo", date == d2)
  expect_equal(bravo_d2$over_under, "UNDER")
  expect_equal(bravo_d2$`Current Record`, "0-1")
  expect_equal(bravo_d2$current_losses, 1)
})

test_that("build_standings_grid fills the line backward for the first day", {
  clippers_d1 <- standings_grid %>%
    filter(team == "Los Angeles Clippers", date == d1)
  expect_equal(clippers_d1$`Vegas Win Projection %`, 50)
})

test_that("build_standings_grid stops at yesterday", {
  one_day <- build_standings_grid(join_with_projections, d1, d1)
  expect_equal(max(one_day$date), d1)
})

test_that("score_picks gives the wage for a right pick and minus the wage for a wrong one", {
  pts <- function(player, team, day) {
    scored_picks %>%
      filter(Player == player, Team == team, Date == day) %>%
      pull(current_projected_points)
  }
  expect_equal(pts("P1", "Alpha", d1), 15)
  expect_equal(pts("P2", "Alpha", d1), -15)
  expect_equal(pts("P1", "Bravo", d1), 14)
})

test_that("score_picks gives 0 for a push and NA before a team's first game", {
  pts <- function(player, team, day) {
    scored_picks %>%
      filter(Player == player, Team == team, Date == day) %>%
      pull(current_projected_points)
  }
  expect_equal(pts("P1", "Alpha", d2), 0)
  expect_true(is.na(pts("P2", "Los Angeles Clippers", d1)))
  expect_equal(pts("P2", "Los Angeles Clippers", d2), 13)
})

test_that("score_picks reports the gap to the line", {
  gap <- scored_picks %>%
    filter(Player == "P1", Team == "Alpha", Date == d1) %>%
    pull(`Win % - Vegas`)
  expect_equal(gap, 50)
})

test_that("add_changes reports a pick's movement from the day before", {
  changes <- add_changes(scored_picks)
  alpha_p1 <- changes %>% filter(Player == "P1", Team == "Alpha", Date == d2)
  expect_equal(alpha_p1$`Change in Points Since Yesterday`, -15)
  alpha_p2 <- changes %>% filter(Player == "P2", Team == "Alpha", Date == d2)
  expect_equal(alpha_p2$`Change in Points Since Yesterday`, 15)
})

test_that("add_changes counts a team's first game as a change (the October 21 bug)", {
  changes <- add_changes(scored_picks)
  clippers <- changes %>% filter(Team == "Los Angeles Clippers")
  expect_equal(clippers$Date, d2)
  expect_equal(clippers$`Change in Points Since Yesterday`, 13)
})

test_that("add_changes counts a pick's opening day as a change from 0", {
  changes <- add_changes(scored_picks)
  bravo_d1 <- changes %>% filter(Player == "P1", Team == "Bravo", Date == d1)
  expect_equal(bravo_d1$`Change in Points Since Yesterday`, 14)
})

test_that("add_changes leaves out picks whose points did not move", {
  changes <- add_changes(scored_picks)
  expect_equal(nrow(changes %>% filter(Team == "Bravo", Date == d2)), 0)
  expect_false(any(changes$`Change in Points Since Yesterday` == 0))
})

test_that("rows_for_day keeps one day and returns no rows for a quiet day", {
  changes <- add_changes(scored_picks)
  expect_equal(nrow(rows_for_day(changes, d2)), 3)
  expect_equal(nrow(rows_for_day(changes, as.Date("2026-10-25"))), 0)
})

test_that("projected_score_by_player totals points and counts correct picks", {
  score <- projected_score_by_player(scored_picks)
  p1_d1 <- score %>% filter(Player == "P1", Date == d1)
  expect_equal(p1_d1$`Projected Total Points`, 29)
  expect_equal(p1_d1$`Number Correct`, 2)
  expect_equal(p1_d1$`Number (Wage 15) Correct`, 1)
  expect_equal(p1_d1$`Number (Wage 14) Correct`, 1)
  expect_equal(p1_d1$`Number (Wage 13) Correct`, 0)
})

test_that("projected_score_by_player counts 13s and 12s", {
  wages <- tibble(Date = d1, Player = "P", current_projected_points = c(13, 12, 12, -13))
  s <- projected_score_by_player(wages)
  expect_equal(s$`Number (Wage 13) Correct`, 1)
  expect_equal(s$`Number (Wage 12) Correct`, 2)
  expect_equal(s$`Projected Total Points`, 24)
})

test_that("projected_score_by_player treats an unplayed team as 0 points", {
  score <- projected_score_by_player(scored_picks)
  p2_d1 <- score %>% filter(Player == "P2", Date == d1)
  expect_equal(p2_d1$`Projected Total Points`, -15)
})

outcome_input <- function() {
  tibble(
    current_projected_points = c(1, 1, 1),
    current_wins = c(41, 0, 10),
    current_losses = c(0, 42, 10),
    `Vegas Win Projection %` = 50
  )
}

test_that("add_outcome_columns settles a team that has reached its OVER", {
  out <- add_outcome_columns(outcome_input(), 82)
  expect_equal(as.character(out$`Outcome Determined`[1]), "OVER")
  expect_true(is.na(out$`Wins To Go Over Vegas`[1]))
  expect_true(is.na(out$`Losses To Go Under Vegas`[1]))
})

test_that("add_outcome_columns settles a team that can no longer reach its OVER", {
  out <- add_outcome_columns(outcome_input(), 82)
  expect_equal(as.character(out$`Outcome Determined`[2]), "UNDER")
  expect_true(is.na(out$`Winning % In Remaining Games Needed`[2]))
  expect_true(is.na(out$`Wins To Go Over Vegas`[2]))
})

test_that("add_outcome_columns leaves an open race with its numbers", {
  out <- add_outcome_columns(outcome_input(), 82)
  expect_equal(as.character(out$`Outcome Determined`[3]), "not yet")
  expect_equal(out$`Wins To Go Over Vegas`[3], 31)
  expect_equal(out$`Losses To Go Under Vegas`[3], 31)
  expect_equal(out$`Winning % In Remaining Games Needed`[3], 50)
})

test_that("add_outcome_columns renames the points column and drops wins and losses", {
  out <- add_outcome_columns(outcome_input(), 82)
  expect_true("Current Projected Points" %in% names(out))
  expect_false(any(c("Current Wins", "Current Losses") %in% names(out)))
  expect_equal(levels(out$`Outcome Determined`), c("not yet", "OVER", "UNDER"))
})

test_that("rank_players orders by points, then correct picks, then 15s", {
  s <- tibble(
    Date = rep(c(d1, d2), each = 4),
    Player = rep(c("A", "B", "C", "D"), 2),
    `Projected Total Points` = c(99, 99, 99, 99, 10, 10, 10, 20),
    `Number Correct` = c(0, 0, 0, 0, 5, 6, 6, 1),
    `Number (Wage 15) Correct` = c(0, 0, 0, 0, 1, 1, 2, 0)
  )
  expect_equal(rank_players(s, d2 + 1), c("D", "C", "B", "A"))
})

test_that("rank_players uses the most recent date before today", {
  s <- tibble(
    Date = c(d1, d1, d2, d2),
    Player = c("A", "B", "A", "B"),
    `Projected Total Points` = c(5, 1, 1, 5),
    `Number Correct` = 1,
    `Number (Wage 15) Correct` = 0
  )
  expect_equal(rank_players(s, d2 + 1), c("B", "A"))
  expect_equal(rank_players(s %>% filter(Date == d1), d2), c("A", "B"))
})

test_that("add_projected_to_standings computes wins needed for the OVER", {
  standings <- tibble(team_name = c("Alpha", "Bravo"), wins = c("30", "10"),
                      losses = c("10", "30"))
  proj <- tibble(team = c("Alpha", "Bravo"), win_projection = c("41.5", "41.5"),
                 percentage_projection = c(50.6, 50.6))
  out <- add_projected_to_standings(standings, proj, d2, 82)
  alpha <- out %>% filter(team_name == "Alpha")
  expect_equal(alpha$date, d2)
  expect_equal(alpha$current_win_perc, 75)
  expect_equal(alpha$remaining_games, 42)
  expect_equal(alpha$wins_needed_for_over, 12)
  expect_equal(alpha$winning_perc_for_over, 12 / 42 * 100)
  expect_equal(alpha$differential, 75 - 12 / 42 * 100)
  expect_false("percentage_projection" %in% names(out))
})

wins_needed_input <- function() {
  tibble(
    Date = c(d2, d2, d2, d2, d1),
    Team = c("Alpha", "Alpha", "Bravo", "Clippers", "Alpha"),
    `Outcome Determined` = factor(c("not yet", "not yet", "OVER", "UNDER", "not yet"),
                                  levels = c("not yet", "OVER", "UNDER")),
    `Current Record` = c("10-10", "10-10", "41-0", "0-42", "9-10"),
    `Wins To Go Over Vegas` = c(31, 31, 0, 41, 32),
    `Winning % In Remaining Games Needed` = c(50, 50, NA, 102.5, 50),
    `Losses To Go Under Vegas` = c(31, 31, NA, -1, 31)
  )
}

test_that("build_wins_needed keeps one row per team for the day", {
  out <- build_wins_needed(wins_needed_input(), d2, 82)
  expect_equal(out$Team, c("Alpha", "Bravo", "Clippers"))
  expect_true(all(out$Date == d2))
})

test_that("build_wins_needed splits the record and counts games left", {
  out <- build_wins_needed(wins_needed_input(), d2, 82)
  alpha <- out %>% filter(Team == "Alpha")
  expect_equal(alpha$Wins, 10L)
  expect_equal(alpha$Losses, 10L)
  expect_equal(alpha$`Remaining Games`, 62)
  expect_false("Winning % In Remaining Games Needed" %in% names(out))
})

test_that("build_wins_needed blanks the numbers for a settled team", {
  out <- build_wins_needed(wins_needed_input(), d2, 82)
  over <- out %>% filter(Team == "Bravo")
  expect_true(is.na(over$`Wins To Go Over Vegas`))
  expect_true(is.na(over$`Losses To Go Under Vegas`))
  under <- out %>% filter(Team == "Clippers")
  expect_true(is.na(under$`Wins To Go Over Vegas`))
  expect_true(is.na(under$`Losses To Go Under Vegas`))
  open <- out %>% filter(Team == "Alpha")
  expect_equal(open$`Wins To Go Over Vegas`, 31)
  expect_equal(open$`Losses To Go Under Vegas`, 31)
})

test_that("a rebuild of the whole pipeline on the fixture is stable", {
  # The result a person reads on the page for Oct 21: three picks moved
  changes <- add_changes(scored_picks)
  expect_equal(
    rows_for_day(changes, d2) %>% arrange(Player, Team) %>%
      select(Player, Team, `Change in Points Since Yesterday`) %>% as.data.frame(),
    data.frame(Player = c("P1", "P2", "P2"),
               Team = c("Alpha", "Alpha", "Los Angeles Clippers"),
               `Change in Points Since Yesterday` = c(-15, 15, 13),
               check.names = FALSE)
  )
})
