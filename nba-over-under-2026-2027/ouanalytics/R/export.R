# Write a case (inputs and the tables computed from them) as CSV files in a
# fixed layout. The Python package ouanalytics-py reads the same layout, so
# it can recompute every table independently and report any difference:
#
#   <dir>/inputs/*.csv    what went in
#   <dir>/outputs/*.csv   what the R code computed
#
# Missing values are written as empty fields, dates as YYYY-MM-DD, factors
# as text.

write_table <- function(x, path) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x[] <- lapply(x, function(col) if (is.factor(col)) as.character(col) else col)
  utils::write.csv(x, path, row.names = FALSE, na = "")
  invisible(path)
}

make_case_dirs <- function(dir) {
  dir.create(file.path(dir, "inputs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(dir, "outputs"), recursive = TRUE, showWarnings = FALSE)
}

# Tables compute_pool() returns that the Python package recomputes
pool_output_names <- c(
  "game_results", "join_with_projections", "standings_grid", "scored_picks",
  "changes_in_player_projections", "rows_today", "projected_score",
  "player_projections_by_team", "standings_with_projected", "wins_needed"
)

#' Export a pool case: the inputs given to compute_pool() and its tables.
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

#' Export a seeding case: the inputs given to compute_playoff_seeding() and
#' the seeded standings.
export_seeding_case <- function(dir, meta, standings, scores_tidy, seeded) {
  make_case_dirs(dir)
  write_table(meta, file.path(dir, "inputs", "meta.csv"))
  write_table(standings, file.path(dir, "inputs", "standings.csv"))
  write_table(scores_tidy, file.path(dir, "inputs", "scores_tidy.csv"))
  write_table(seeded, file.path(dir, "outputs", "seeding.csv"))
  invisible(dir)
}
