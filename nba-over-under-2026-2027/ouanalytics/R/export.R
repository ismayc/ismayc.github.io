# Write a case (inputs and the tables computed from them) as CSV files in a
# fixed layout. The Python package ouanalytics-py reads the same layout, so
# it can recompute every table independently and report any difference:
#
#   <dir>/inputs/*.csv    what went in
#   <dir>/outputs/*.csv   what the R code computed
#
# Missing values are written as empty fields, dates as YYYY-MM-DD, factors
# as text.

#' Write one table as CSV (internal)
#' @noRd
write_table <- function(x, path) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x[] <- lapply(x, function(col) if (is.factor(col)) as.character(col) else col)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

#' Create the inputs and outputs folders (internal)
#' @noRd
make_case_dirs <- function(dir) {
  dir.create(file.path(dir, "inputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "outputs"), recursive = TRUE, showWarnings = FALSE)
}

# Tables compute_pool() returns that the Python package recomputes
#' Names of the tables a pool case exports (internal)
#' @noRd
pool_output_names <- c(
  "game_results", "join_with_projections", "standings_grid", "scored_picks",
  "changes_in_player_projections", "rows_today", "projected_score",
  "player_projections_by_team", "standings_with_projected", "wins_needed"
)

#' Export a case as CSV files
#'
#' Writes the inputs and the computed tables as CSV files in a fixed layout
#' (`inputs/` and `outputs/`), so the Python package can recompute every table and
#' compare. Missing values are empty fields, dates are `YYYY-MM-DD`, and factors are
#' text.
#'
#' @param dir The folder to write into; created if needed.
#' @param game_results_raw,scores,projections,picks,standings The inputs given to
#'   [compute_pool()].
#' @param params A named list: `yesterday`, `today`, `season_start`, `num_games`.
#' @param pool The result of [compute_pool()].
#' @param meta,scores_tidy The inputs given to [compute_playoff_seeding()]. In
#'   `export_seeding_case()`, `standings` is the standings given to it too.
#' @param seeded The result of [compute_playoff_seeding()].
#' @return `dir`, invisibly.
#' @examples
#' dir <- tempfile()
#' pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
#'   picks, standings, params$yesterday, params$today, params$season_start,
#'   params$num_games))
#' with(pool_example, export_pool_case(dir, game_results_raw, scores, projections,
#'   picks, standings, params, pool))
#' list.files(dir, recursive = TRUE)
#' @export
export_pool_case <- function(dir, game_results_raw, scores, projections, picks,
                             standings, params, pool) {
  make_case_dirs(dir)
  inputs <- list(game_results_raw = game_results_raw, scores = scores,
                 projections = projections, picks = picks,
                 standings = standings)
  for (nm in names(inputs)) {
    write_table(inputs[[nm]], file.path(dir, "inputs", paste0(nm, ".csv")))
  }
  write_table(data.frame(name = names(params),
                         value = vapply(params, as.character, character(1)),
                         row.names = NULL),
              file.path(dir, "inputs", "params.csv"))
  for (nm in pool_output_names) {
    write_table(pool[[nm]], file.path(dir, "outputs", paste0(nm, ".csv")))
  }
  write_table(data.frame(rank = seq_along(pool$current_rankings),
                         Player = pool$current_rankings),
              file.path(dir, "outputs", "current_rankings.csv"))
  invisible(dir)
}

#' @rdname export_pool_case
#' @export
export_seeding_case <- function(dir, meta, standings, scores_tidy, seeded) {
  make_case_dirs(dir)
  write_table(meta, file.path(dir, "inputs", "meta.csv"))
  write_table(standings, file.path(dir, "inputs", "standings.csv"))
  write_table(scores_tidy, file.path(dir, "inputs", "scores_tidy.csv"))
  write_table(seeded, file.path(dir, "outputs", "seeding.csv"))
  invisible(dir)
}
