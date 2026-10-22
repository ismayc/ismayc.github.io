#' ouanalytics: calculations behind the NBA over/under pool page
#'
#' Pure functions: nothing here reads a file, calls the network, or looks at
#' `Sys.Date()`. Every date and constant comes in as an argument.
#'
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import stringr
"_PACKAGE"

# Column names used through dplyr's non-standard evaluation
utils::globalVariables(c(
  ".", "Date", "Game Date", "Game Number", "Team Name", "Team", "Player",
  "Current Record", "Current Win %", "Current Wins", "Current Losses",
  "Current Projected Points", "Vegas Win Projection %", "Win % - Vegas",
  "Wins To Go Over Vegas", "Losses To Go Under Vegas",
  "Winning % In Remaining Games Needed", "Outcome Determined",
  "Remaining Games", "Wins", "Losses", "Number Correct",
  "Number (Wage 15) Correct", "Projected Total Points",
  "Change in Points Since Yesterday", "Winning Pct", "Games Back",
  "away_score", "home_score", "away_team", "home_team", "slug_away_team",
  "slug_home_team", "slugTeam", "slugTeamLoser", "slugMatchup", "nameTeam",
  "numberGameTeamSeason", "dateGame", "win", "loss", "location", "score",
  "opponent_score", "team_slug", "opponent_slug", "team_name", "rowname",
  "game_id", "game_date", "final_score", "current_wins", "current_losses",
  "current_win_perc", "current_projected_points", "percentage_projection",
  "win_projection", "over_under", "choice", "wage", "player", "team",
  "date", "flipped", "flipped_today", "days_from_today", "wins", "losses",
  "remaining_games", "wins_needed_for_over", "winning_perc_for_over",
  "current_win_perc", "abbreviation", "conference", "division", "pct", "pd",
  "seed", "seed_note"
))
