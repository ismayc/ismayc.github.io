# Playoff tie-breakers, split out of 03-data-wrangling.R so tests/testthat
# can run them on fixture data. Each takes the tied teams and the tidy scores
# and returns a table the next step uses to rank the tie.

library(dplyr)

calculate_head_to_head <- function(tied_teams, scores_tidy) {
  head_to_head_games <- scores_tidy %>%
    filter(team_slug %in% tied_teams & opponent_slug %in% tied_teams) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug)) %>%
    group_by(winner) %>%
    summarise(wins = n(), .groups = 'drop') %>%
    arrange(desc(wins))

  return(head_to_head_games)
}

# Games where one side is a tied team and the other is in `opponent_teams`,
# as each team's wins, losses, and win percentage. Wins and losses both
# come from those games. (The first version counted losses over all games
# and grouped by a `loser` column the scores table never had, so it
# stopped the build whenever a tie got past head-to-head.)
record_against <- function(tied_teams, scores, opponent_teams) {
  games <- scores %>%
    filter((slug_away_team %in% tied_teams & slug_home_team %in% opponent_teams) |
             (slug_home_team %in% tied_teams & slug_away_team %in% opponent_teams)) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug),
           loser = if_else(score > opponent_score, opponent_slug, team_slug))

  wins <- games %>% count(winner, name = "wins")
  losses <- games %>% count(loser, name = "losses")

  wins %>%
    full_join(losses, by = c("winner" = "loser")) %>%
    mutate(wins = coalesce(wins, 0L),
           losses = coalesce(losses, 0L),
           total_games = wins + losses,
           win_pct = wins / total_games) %>%
    arrange(desc(win_pct))
}

calculate_division_record <- function(tied_teams, scores, teams_info) {
  record_against(tied_teams, scores, teams_info$division_teams)
}

calculate_conference_record <- function(tied_teams, scores, teams_info) {
  record_against(tied_teams, scores, teams_info$conference_teams)
}

calculate_winning_pct_against_playoff_teams_conference <- function(tied_teams, scores, playoff_teams) {
  record_against(tied_teams, scores, playoff_teams$conference_teams)
}

calculate_winning_pct_against_playoff_teams_opposing_conference <- function(tied_teams, scores, playoff_teams) {
  record_against(tied_teams, scores, playoff_teams$opposing_conference_teams)
}

calculate_point_differential <- function(tied_teams, scores) {
  point_diff <- scores %>%
    filter(slug_away_team %in% tied_teams | slug_home_team %in% tied_teams) %>%
    mutate(point_diff = if_else(slug_away_team %in% tied_teams, score - opponent_score, opponent_score - score),
           team = if_else(slug_away_team %in% tied_teams, slug_away_team, slug_home_team)) %>%
    group_by(team) %>%
    summarise(total_point_diff = sum(point_diff)) %>%
    arrange(desc(total_point_diff))

  return(point_diff)
}

is_tie_resolved <- function(tiebreaker_results) {
  all_unique <- all(duplicated(tiebreaker_results$wins) == FALSE)
  return(all_unique)
}

finalize_ranking <- function(tiebreaker_results, standings) {
  if("wins" %in% names(standings) | "wins" %in% names(tiebreaker_results)) {
    standings <- standings %>%
      left_join(tiebreaker_results, by = c("team_name" = "winner", "wins"))
  }
  return(standings)
}

# `is_resolved` is a seam for tests. With the real check the last step always
# resolves: the point-differential table has no `wins` column, so
# is_tie_resolved() is TRUE for it.
apply_tiebreakers <- function(tied_teams, scores_tidy, teams_info, playoff_teams, standings,
                              is_resolved = is_tie_resolved) {
  # Head-to-Head
  head_to_head_result <- calculate_head_to_head(tied_teams, scores_tidy)
  if(is_resolved(head_to_head_result)) {
    return(finalize_ranking(head_to_head_result, standings))
  }

  # Division Record
  division_record_result <- calculate_division_record(tied_teams, scores_tidy, teams_info)
  if(is_resolved(division_record_result)) {
    return(finalize_ranking(division_record_result, standings))
  }

  # Conference Record
  conference_record_result <- calculate_conference_record(tied_teams, scores_tidy, teams_info)
  if(is_resolved(conference_record_result)) {
    return(finalize_ranking(conference_record_result, standings))
  }

  # Winning Percentage Against Playoff Teams in Own Conference
  wpct_against_playoff_teams_conference_result <- calculate_winning_pct_against_playoff_teams_conference(tied_teams, scores_tidy, playoff_teams)
  if(is_resolved(wpct_against_playoff_teams_conference_result)) {
    return(finalize_ranking(wpct_against_playoff_teams_conference_result, standings))
  }

  # Winning Percentage Against Playoff Teams in Opposing Conference
  wpct_against_playoff_teams_opposing_result <- calculate_winning_pct_against_playoff_teams_opposing_conference(tied_teams, scores_tidy, playoff_teams)
  if(is_resolved(wpct_against_playoff_teams_opposing_result)) {
    return(finalize_ranking(wpct_against_playoff_teams_opposing_result, standings))
  }

  # Point Differential
  point_differential_result <- calculate_point_differential(tied_teams, scores_tidy)
  if(is_resolved(point_differential_result)) {
    return(finalize_ranking(point_differential_result, standings))
  }

  return(standings)
}
