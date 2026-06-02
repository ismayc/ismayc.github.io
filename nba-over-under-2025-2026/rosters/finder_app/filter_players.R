# Core logic for the NBA Player Finder, extracted so it can be unit-tested
# independently of the Shiny UI (see ../tests/testthat/). app.R sources this file;
# the reactive forwards input values to filter_players(), and the "Focus on one
# team" observer uses focus_team_plan() to decide which inputs to update.

# Given a team, return the plan for the "Focus on one team" control: the team's
# conference, division, and the selection each conference team-group should get
# (only the team in its own group; every other group cleared). Returns NULL for
# an unknown/empty team. Pure function -> directly testable.
focus_team_plan <- function(team, team_meta, div_group, groups_by_conf) {
  if (is.null(team) || is.na(team) || team == "") return(NULL)
  row <- team_meta[team_meta$team == team, , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  conf   <- row$conference[1]
  grp    <- div_group[[row$division[1]]]
  groups <- groups_by_conf[[conf]]
  selected <- stats::setNames(
    lapply(groups, function(g) if (identical(g, grp)) team else character(0)),
    groups
  )
  list(conference = conf, division = row$division[1], selected = selected)
}

#
# Returns the filtered, display-ready data frame (the same columns the table
# shows). Players whose height/age/jersey is NA are dropped by the range
# comparisons, matching dplyr::filter's treatment of NA as FALSE.

filter_players <- function(players_data, conf, divisions, teams,
                           known_height = FALSE, chosen_feet = NULL, chosen_inches = NULL,
                           min_feet = NULL, min_inches = NULL,
                           max_feet = NULL, max_inches = NULL,
                           known_age = FALSE, age_value = NULL, age_range = NULL,
                           known_jersey = FALSE, jersey_value = NULL, jersey_range = NULL) {
  players_data |>
    dplyr::filter(
      conference == conf,
      division %in% divisions,
      team %in% teams,
      if (isTRUE(known_height)) {
        height_total_inches == as.integer(chosen_feet) * 12 + as.integer(chosen_inches)
      } else {
        dplyr::between(
          height_total_inches,
          as.integer(min_feet) * 12 + as.integer(min_inches),
          as.integer(max_feet) * 12 + as.integer(max_inches)
        )
      },
      if (isTRUE(known_age)) {
        age == age_value
      } else {
        dplyr::between(age, age_range[1], age_range[2])
      },
      if (isTRUE(known_jersey)) {
        suppressWarnings(as.integer(number_jersey)) == jersey_value
      } else {
        dplyr::between(suppressWarnings(as.integer(number_jersey)),
                       jersey_range[1], jersey_range[2])
      }
    ) |>
    dplyr::mutate(height = paste0(height_feet, "-", height_inches)) |>
    dplyr::select(player, team, conference, division, height, age, number_jersey)
}
