# The whole pool calculation in one call, so the page build and the tests
# run the same code path in the same order.

#' Run the whole pool calculation
#'
#' Runs every step from the saved game data to the tables the page uses, in order.
#' The page build and the tests both use this function.
#'
#' @param game_results_raw,scores See [compute_game_results()].
#' @param projections Win totals, as for [add_over_under()].
#' @param picks One row per pick, as for [score_picks()].
#' @param standings Official standings, as for [add_projected_to_standings()].
#' @param yesterday The last day with results (a `Date`).
#' @param today The day the page is built (a `Date`).
#' @param season_start The first day of the season (a `Date`).
#' @param num_games Games in a season (82).
#' @return A named list: `game_results`, `scores_tidy`, `join_with_projections`,
#'   `standings_grid`, `scored_picks`, `changes_in_player_projections`, `rows_today`,
#'   `projected_score`, `player_projections_by_team`, `current_rankings`,
#'   `standings_with_projected`, and `wins_needed`.
#' @examples
#' pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
#'   picks, standings, params$yesterday, params$today, params$season_start,
#'   params$num_games))
#' pool$rows_today
#' pool$current_rankings
#' @export
compute_pool <- function(game_results_raw, scores, projections, picks,
                         standings, yesterday, today, season_start,
                         num_games) {
  game_results <- compute_game_results(game_results_raw)
  scores_tidy <- tidy_scores(scores)
  updated_schedule <- add_game_scores(game_results, scores_tidy)
  join_with_projections <- add_over_under(updated_schedule, projections,
                                          yesterday)
  standings_grid <- build_standings_grid(join_with_projections, season_start,
                                         yesterday)
  scored_picks <- score_picks(standings_grid, picks)
  changes <- add_changes(scored_picks)
  projected_score <- projected_score_by_player(scored_picks)
  player_projections_by_team <- add_outcome_columns(scored_picks, num_games)

  list(
    game_results = game_results,
    scores_tidy = scores_tidy,
    join_with_projections = join_with_projections,
    standings_grid = standings_grid,
    scored_picks = scored_picks,
    changes_in_player_projections = changes,
    rows_today = rows_for_day(changes, yesterday),
    projected_score = projected_score,
    player_projections_by_team = player_projections_by_team,
    current_rankings = rank_players(projected_score, today),
    standings_with_projected = add_projected_to_standings(
      standings, projections, yesterday, num_games),
    wins_needed = build_wins_needed(player_projections_by_team, yesterday,
                                    num_games)
  )
}
