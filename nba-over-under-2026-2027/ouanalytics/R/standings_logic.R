# Pure calculations behind the over/under page, split out of
# 03-data-wrangling.R so tests can run them on fixture data.
# Nothing here reads a file, calls the network, or looks at Sys.Date():
# every date and constant comes in as an argument.

# One row per team game, with running wins, losses, and win percentage
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
classify_over_under <- function(current_win_pct, vegas_pct) {
  case_when(
    current_win_pct > vegas_pct ~ "OVER",
    current_win_pct < vegas_pct ~ "UNDER",
    TRUE                        ~ "PUSH"
  )
}

# Each game row with the team's status against its line, and whether the
# status flipped from the game before
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

rows_for_day <- function(changes, day) {
  changes %>%
    filter(Date == day)
}

# Each player's total and counts of correct picks, every day
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
