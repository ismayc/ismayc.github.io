#' Example pool: three teams, two days
#'
#' A tiny pool used in the examples and tests: three teams (Alpha, Bravo, and the
#' Los Angeles Clippers), two players, and two days of games. Alpha beats Bravo on
#' October 20, 2026; the Clippers beat Alpha on October 21, the Clippers' first
#' game. Every team has a 50 percent line.
#'
#' @format A list with the inputs of [compute_pool()]:
#' \describe{
#'   \item{game_results_raw}{One row per team game.}
#'   \item{scores}{One row per game, with scores.}
#'   \item{projections}{Win totals.}
#'   \item{picks}{Two players' picks.}
#'   \item{standings}{Official standings.}
#'   \item{params}{A list: `yesterday`, `today`, `season_start`, `num_games`.}
#' }
#' @examples
#' str(pool_example, max.level = 1)
"pool_example"

#' Example league for playoff seeding
#'
#' A small league used in the examples and tests: 13 East teams in three divisions
#' and 6 West teams in two, with a few games, so that some teams tie on winning
#' percentage.
#'
#' @format A list with the inputs of [compute_playoff_seeding()]:
#' \describe{
#'   \item{meta}{`abbreviation`, `division`, and `conference` for each team.}
#'   \item{standings}{Standings with `Winning Pct`.}
#'   \item{scores_tidy}{One row per team per game.}
#' }
#' @examples
#' str(seeding_example, max.level = 1)
"seeding_example"
