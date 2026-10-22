# Pure calculations behind the over/under page, split out of
# 03-data-wrangling.R so tests can run them on fixture data.
# Nothing here reads a file, calls the network, or looks at Sys.Date():
# every date and constant comes in as an argument.

# One row per team game, with running wins, losses, and win percentage
#' Game results and scores
#'
#' Turn the saved game list into one row per team game with a running record,
#' and the scores table into one row per team per game.
#'
#' @param game_results_raw One row per team game: `dateGame`, `numberGameTeamSeason`,
#'   `nameTeam`, `slugMatchup`, `slugTeam`, `slugTeamLoser`.
#' @param scores One row per game: `game_id`, `game_date`, `away_team`, `home_team`,
#'   `slug_away_team`, `slug_home_team`, `away_score`, `home_score`.
#' @param game_results The result of `compute_game_results()`.
#' @param scores_tidy The result of `tidy_scores()`.
#' @return `compute_game_results()` returns a grouped tibble with `current_wins`,
#'   `current_losses`, and `current_win_perc` (a percentage from 0 to 100) per team,
#'   "LA " spelled out as "Los Angeles ", sorted by team.
#'   `tidy_scores()` returns one row per team per game with `team_slug`, `opponent_slug`,
#'   `score`, and `opponent_score`. `add_game_scores()` adds each game's score, record,
#'   and a readable result text to `game_results`.
#' @examples
#' gr <- compute_game_results(pool_example$game_results_raw)
#' st <- tidy_scores(pool_example$scores)
#' add_game_scores(gr, st)
#' @export
compute_game_results <- function(game_results_raw) {
  game_results_raw %>%
    mutate(win = if_else(slugTeam != slugTeamLoser, 1, 0),
           loss = 1 - win) %>%
    group_by(nameTeam) %>%
    mutate(
      current_wins = cumsum(win),
      current_losses = cumsum(loss),
      current_win_perc = current_wins / (current_wins + current_losses) * 100
    ) %>%
    rename(`Game Number` = numberGameTeamSeason,
           `Team Name` = nameTeam,
           `Game Date` = dateGame) %>%
    mutate(`Team Name` = str_replace_all(`Team Name`, "LA ", "Los Angeles ")) %>%
    arrange(`Team Name`)
}

# One row per team per game from the one-row-per-game scores table. Rows
# alternate away, home, so the opponent's score is the neighboring row.
#' @rdname compute_game_results
#' @export
tidy_scores <- function(scores) {
  scores %>%
    pivot_longer(cols = away_score:home_score,
                 names_to = "location",
                 values_to = "score") %>%
    mutate(location = str_replace_all(location, "_score", "")) %>%
    mutate(
      team_slug = if_else(location == "away", slug_away_team, slug_home_team),
      opponent_slug = if_else(location == "away", slug_home_team, slug_away_team),
      team_name = if_else(location == "away", away_team, home_team)
    ) %>%
    mutate(team_name = str_replace_all(
      team_name,
      "LA Clippers",
      "Los Angeles Clippers")) %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname)) %>%
    mutate(opponent_score = if_else(rowname %% 2 == 1,
                                    lead(score),
                                    lag(score)))
}

# Attach each game's final score and a readable result string
#' @rdname compute_game_results
#' @export
add_game_scores <- function(game_results, scores_tidy) {
  game_results %>%
    left_join(
      scores_tidy %>%
        select(game_id, game_date, team_name, score, opponent_score),
      by = c("Game Date" = "game_date", "Team Name" = "team_name")
    ) %>%
    relocate(game_id, .after = `Game Date`) %>%
    mutate(final_score = paste0(score, "-", opponent_score)) %>%
    mutate(`Current Record` = paste0(current_wins, "-", current_losses)) %>%
    mutate(`Game Result` = paste0("\nGame Number: ", `Game Number`, "\n",
                                  "Game Date: ", `Game Date`, "\n",
                                  "Game Matchup: ", slugMatchup, "\n",
                                  "Final Score: ", final_score))
}

# OVER when a team's winning percentage is above the line, UNDER below it,
# PUSH when level (or when either side is missing)
#' Over, under, or push against the win total
#'
#' A team is OVER when its winning percentage is above its line, UNDER when it is
#' below, and PUSH when the two are equal or either is missing.
#'
#' @param current_win_pct,vegas_pct Winning percentages (0 to 100).
#' @param updated_schedule The result of `add_game_scores()`.
#' @param projections Win totals: `team`, `win_projection`, and `percentage_projection`
#'   (the win total as a percentage of the season).
#' @param yesterday The last day with results (a `Date`).
#' @param join_with_projections The result of `add_over_under()`.
#' @param season_start The first day of the season (a `Date`).
#' @return `classify_over_under()` returns "OVER", "UNDER", or "PUSH".
#'   `add_over_under()` returns one row per team game with `over_under`, `flipped`
#'   (the status changed from the game before), and `flipped_today`.
#'   `build_standings_grid()` returns one row per team per day from `season_start`
#'   to `yesterday`. A team keeps its last status on days it does not play, and has
#'   no status (and a record of 0-0) before its first game.
#' @examples
#' classify_over_under(c(60, 50, 40), 50)
#'
#' gr <- compute_game_results(pool_example$game_results_raw)
#' sched <- add_game_scores(gr, tidy_scores(pool_example$scores))
#' ou <- add_over_under(sched, pool_example$projections, as.Date("2026-10-21"))
#' build_standings_grid(ou, as.Date("2026-10-20"), as.Date("2026-10-21"))
#' @export
classify_over_under <- function(current_win_pct, vegas_pct) {
  case_when(
    current_win_pct > vegas_pct ~ "OVER",
    current_win_pct < vegas_pct ~ "UNDER",
    TRUE                        ~ "PUSH"
  )
}

# Each game row with the team's status against its line, and whether the
# status flipped from the game before
#' @rdname classify_over_under
#' @export
add_over_under <- function(updated_schedule, projections, yesterday) {
  updated_schedule %>%
    inner_join(projections, by = c("Team Name" = "team")) %>%
    mutate(`Current Win %` = round(current_win_perc, 2),
           `Vegas Win Projection %` = round(percentage_projection, 2)) %>%
    mutate(over_under = classify_over_under(`Current Win %`,
                                            `Vegas Win Projection %`)) %>%
    group_by(`Team Name`) %>%
    mutate(flipped = if_else(lag(over_under) != over_under, TRUE, NA),
           flipped_today = if_else(as.Date(`Game Date`) == yesterday,
                                   TRUE,
                                   NA))
}

# One row per team per day of the season so far. A team keeps its last
# status on days it does not play, and has no status before its first game.
#' @rdname classify_over_under
#' @export
build_standings_grid <- function(join_with_projections, season_start,
                                 yesterday) {
  days_in_season_to_today <- seq(from = as.Date(season_start),
                                 to = as.Date(yesterday),
                                 by = "days")
  teams <- unique(join_with_projections$`Team Name`)

  crossing(team = teams, date = days_in_season_to_today) %>%
    left_join(join_with_projections %>%
                select(
                  `Team Name`, `Game Date`, starts_with("current"),
                  `Vegas Win Projection %`, over_under,
                  -current_win_perc
                ),
              by = c("team" = "Team Name", "date" = "Game Date")) %>%
    group_by(team) %>%
    fill(c(current_wins, current_losses, `Current Record`, `Current Win %`,
           over_under),
         .direction = "down") %>%
    fill(`Vegas Win Projection %`, .direction = "downup") %>%
    replace_na(replace = list(current_wins = 0,
                              current_losses = 0,
                              `Current Record` = "0-0",
                              `Current Win %` = NA_real_))
}

# Each player's points for each team they picked, every day
#' Score the picks
#'
#' Each player's points for each team they picked, every day: the wage when the
#' pick matches the team's status, minus the wage when it does not, 0 for a push,
#' and missing before the team's first game.
#'
#' @param standings_grid The result of `build_standings_grid()`.
#' @param picks One row per pick: `team`, `player`, `wage`, and `choice` ("OVER" or
#'   "UNDER").
#' @param player_projections_by_team The result of `score_picks()`.
#' @param changes The result of `add_changes()`.
#' @param day A `Date`.
#' @param projected_score The result of `projected_score_by_player()`.
#' @param today The day the page is built (a `Date`).
#' @return `score_picks()` returns one row per player, team, and day.
#'   `add_changes()` returns the rows where a pick's points moved from the day before.
#'   A team's points are missing until its first game, so a missing value counts as 0:
#'   otherwise every team's first game would drop out of the changes.
#'   `rows_for_day()` keeps one day of those. `projected_score_by_player()` returns each
#'   player's total points and counts of correct picks by day (missing points count
#'   as 0 and as not correct). `rank_players()` returns the players, best first, by
#'   points, then correct picks, then correct 15-point picks, on the date closest to
#'   `today`.
#' @examples
#' pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
#'   picks, standings, params$yesterday, params$today, params$season_start,
#'   params$num_games))
#' scored <- pool$scored_picks
#' ch <- add_changes(scored)
#' rows_for_day(ch, as.Date("2026-10-21"))
#' ps <- projected_score_by_player(scored)
#' rank_players(ps, today = as.Date("2026-10-22"))
#' @export
score_picks <- function(standings_grid, picks) {
  standings_grid %>%
    inner_join(picks, by = "team") %>%
    mutate(current_projected_points = case_when(
      (over_under == "PUSH") ~ 0,
      (over_under == choice) ~ wage,
      (over_under != choice) ~ -wage
    )) %>%
    select(Date = date, Player = player, Team = team, starts_with("current"),
           `Vegas Win Projection %`,
           `Current Over/Under` = over_under,
           `Over/Under Pick` = choice) %>%
    mutate(
      `Win % - Vegas` = round(`Current Win %` - `Vegas Win Projection %`, 2)
    ) %>%
    relocate(current_projected_points,
             .after = `Win % - Vegas`)
}

# Rows where a pick's points moved from the day before. A team's points are
# NA until its first game, so NA counts as 0: otherwise every team's first
# game drops out and the page says nothing changed.
#' @rdname score_picks
#' @export
add_changes <- function(player_projections_by_team) {
  player_projections_by_team %>%
    group_by(Player, Team) %>%
    mutate(
      `Change in Points Since Yesterday` =
        coalesce(current_projected_points, 0) -
        coalesce(lag(current_projected_points), 0)
    ) %>%
    ungroup() %>%
    arrange(Date, Team, Player) %>%
    filter(`Change in Points Since Yesterday` != 0) %>%
    select(Date, Player, Team, `Current Record`, `Current Win %`,
           `Vegas Win Projection %`, `Change in Points Since Yesterday`)
}

#' @rdname score_picks
#' @export
rows_for_day <- function(changes, day) {
  changes %>%
    filter(Date == day)
}

# Each player's total and counts of correct picks, every day
#' @rdname score_picks
#' @export
projected_score_by_player <- function(player_projections_by_team) {
  player_projections_by_team %>%
    group_by(Date, Player) %>%
    summarize(
      `Projected Total Points` = sum(current_projected_points, na.rm = TRUE),
      # na.rm: a team that has not played has no points yet. Without it a
      # player with one unplayed team had an NA count, which sorted last.
      `Number Correct` = sum(current_projected_points > 0, na.rm = TRUE),
      `Number (Wage 15) Correct` = sum(
        current_projected_points == 15, na.rm = TRUE
      ),
      `Number (Wage 14) Correct` = sum(
        current_projected_points == 14, na.rm = TRUE
      ),
      `Number (Wage 13) Correct` = sum(
        current_projected_points == 13, na.rm = TRUE
      ),
      `Number (Wage 12) Correct` = sum(
        current_projected_points == 12, na.rm = TRUE
      ),
      .groups = "drop"
    )
}

# Wins and losses each team still needs, and whether the outcome is settled
#' Wins and losses still needed
#'
#' How many more wins a team needs to finish OVER, how many more losses would make
#' it finish UNDER, and whether the outcome is already settled.
#'
#' @param player_projections_by_team For `add_outcome_columns()`, the result of
#'   `score_picks()`. For `build_wins_needed()`, the result of `add_outcome_columns()`.
#' @param num_games Games in a season (82).
#' @param standings Official standings: `team_name`, `wins`, `losses`.
#' @param projections Win totals, as for `add_over_under()`.
#' @param yesterday The last day with results (a `Date`).
#' @return `add_outcome_columns()` adds `Wins To Go Over Vegas`,
#'   `Losses To Go Under Vegas`, `Winning % In Remaining Games Needed`, and
#'   `Outcome Determined` ("not yet", "OVER", or "UNDER"); the "to go" columns are
#'   missing once the outcome is settled. `add_projected_to_standings()` adds the wins
#'   needed for an OVER to the official standings (no percentage when no games are
#'   left). `build_wins_needed()` returns one row per team for `yesterday`.
#' @examples
#' pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
#'   picks, standings, params$yesterday, params$today, params$season_start,
#'   params$num_games))
#' add_outcome_columns(pool$scored_picks, num_games = 82)
#' build_wins_needed(pool$player_projections_by_team, as.Date("2026-10-21"), 82)
#' @export
add_outcome_columns <- function(player_projections_by_team, num_games) {
  player_projections_by_team %>%
    rename(
      `Current Projected Points` = current_projected_points,
      `Current Wins` = current_wins,
      `Current Losses` = current_losses
    ) %>%
    mutate(
      `Wins To Go Over Vegas` =
        ceiling(`Vegas Win Projection %` / 100 * num_games) - `Current Wins`,
      `Winning % In Remaining Games Needed` = round(
        `Wins To Go Over Vegas` /
          (num_games - `Current Wins` - `Current Losses`) * 100, 2),
      .before = `Current Projected Points`
    ) %>%
    mutate(`Outcome Determined` = factor(case_when(
      `Wins To Go Over Vegas` <= 0 ~ "OVER",
      `Winning % In Remaining Games Needed` > 100 ~ "UNDER",
      TRUE ~ "not yet"
    ), levels = c("not yet", "OVER", "UNDER")
    )) %>%
    mutate(
      `Losses To Go Under Vegas` =
        num_games - ceiling(`Vegas Win Projection %` / 100 * num_games) -
        `Current Losses`,
      .before = `Current Projected Points`
    ) %>%
    mutate(`Winning % In Remaining Games Needed` = if_else(
      `Losses To Go Under Vegas` < 0 |
        `Winning % In Remaining Games Needed` > 100 |
        `Wins To Go Over Vegas` <= 0,
      NA_real_,
      `Winning % In Remaining Games Needed`
    )) %>%
    mutate(`Wins To Go Over Vegas` = if_else(
      `Losses To Go Under Vegas` < 0 |
        `Winning % In Remaining Games Needed` > 100 |
        `Wins To Go Over Vegas` <= 0,
      NA_real_,
      `Wins To Go Over Vegas`
    )) %>%
    mutate(`Losses To Go Under Vegas` = if_else(
      `Losses To Go Under Vegas` < 0 |
        `Winning % In Remaining Games Needed` > 100 |
        `Wins To Go Over Vegas` <= 0,
      NA_real_,
      `Losses To Go Under Vegas`
    )) %>%
    select(-`Current Wins`, -`Current Losses`)
}

# The players' scores on the most recent date, best first
#' @rdname score_picks
#' @export
rank_players <- function(projected_score, today) {
  most_recent_results <- projected_score %>%
    mutate(days_from_today = as.numeric(difftime(today,
                                                 Date,
                                                 units = c("days")))) %>%
    filter(days_from_today == min(days_from_today))

  most_recent_results %>%
    arrange(desc(`Projected Total Points`),
            desc(`Number Correct`),
            desc(`Number (Wage 15) Correct`)) %>%
    pull(Player)
}

# Official standings with the wins each team needs for its OVER
#' @rdname add_outcome_columns
#' @export
add_projected_to_standings <- function(standings, projections, yesterday,
                                       num_games) {
  standings %>%
    mutate(date = yesterday) %>%
    mutate(wins = as.numeric(wins),
           losses = as.numeric(losses)) %>%
    mutate(current_win_perc = wins / (wins + losses) * 100) %>%
    inner_join(projections, by = c("team_name" = "team")) %>%
    select(-percentage_projection) %>%
    mutate(
      remaining_games = num_games - (wins + losses),
      wins_needed_for_over = ceiling(as.numeric(win_projection)) - wins,
      # No games left means no percentage (not Inf or NaN)
      winning_perc_for_over = if_else(
        remaining_games > 0,
        wins_needed_for_over / remaining_games * 100,
        NA_real_
      )
    ) %>%
    mutate(differential = current_win_perc - winning_perc_for_over)
}

# One row per team for yesterday: record, wins to go, games left
#' @rdname add_outcome_columns
#' @export
build_wins_needed <- function(player_projections_by_team, yesterday,
                              num_games) {
  player_projections_by_team %>%
    filter(Date == yesterday) %>%
    select(Date, Team, `Outcome Determined`,
           `Current Record`, `Wins To Go Over Vegas`,
           `Winning % In Remaining Games Needed`,
           `Losses To Go Under Vegas`) %>%
    distinct() %>%
    mutate(`Wins To Go Over Vegas` = if_else(
      `Wins To Go Over Vegas` <= 0 |
        `Winning % In Remaining Games Needed` > 100,
      NA_real_,
      `Wins To Go Over Vegas`
    )) %>%
    mutate(`Losses To Go Under Vegas` = if_else(
      `Losses To Go Under Vegas` < 0 |
        is.na(`Wins To Go Over Vegas`),
      NA_real_,
      `Losses To Go Under Vegas`
    )) %>%
    separate(
      col = `Current Record`,
      into = c("Wins", "Losses"),
      sep = "-",
      remove = FALSE) %>%
    mutate(Wins = as.integer(Wins),
           Losses = as.integer(Losses)) %>%
    mutate(`Remaining Games` = num_games - Wins - Losses) %>%
    select(-`Winning % In Remaining Games Needed`)
}
