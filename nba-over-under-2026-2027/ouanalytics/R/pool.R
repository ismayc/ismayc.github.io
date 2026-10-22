# The whole pool calculation in one call, so the page build and the tests
# run the same code path in the same order.

#' Run the pool calculation from saved game data to every table the page uses.
#'
#' @param game_results_raw One row per team game (02c).
#' @param scores One row per game with away and home scores (02c).
#' @param projections Win totals: `team`, `win_projection`, `percentage_projection`.
#' @param picks `team`, `player`, `wage`, `choice`.
#' @param standings Official standings with `team_name`, `wins`, `losses`.
#' @param yesterday Last day with results.
#' @param today The day the page is built.
#' @param season_start First day of the season.
#' @param num_games Games in a season.
#' @return A named list of tables.
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
