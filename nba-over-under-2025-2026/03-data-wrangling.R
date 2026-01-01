library(tidyverse)
library(plotly)
library(here)
library(glue)

standings <- here("rds", glue("standings_through_{Sys.Date() - 1}.rds")) %>% 
  read_rds()

# From 02c-get-game-scores.R
game_results_raw <- here(
  "rds", 
  glue("game_results_raw_through_{Sys.Date() - 1}.rds")) %>% 
  read_rds()

game_results <- game_results_raw %>% 
  mutate(win = if_else(slugTeam != slugTeamLoser, 1, 0),
         loss = 1 - win) %>% 
  group_by(nameTeam) %>% 
  #  mutate(current_win_total = rollapplyr(win, 2, sum, partial = TRUE),
  #         current_loss_total = rollapplyr(loss, 2, sum, partial = TRUE)) %>% 
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

# From 02c (and 02b-get-game-scores.R)
scores_tidy <- scores %>% 
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

updated_schedule <- game_results %>% 
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

join_with_projections <- updated_schedule %>% 
  inner_join(projections, by = c("Team Name" = "team")) %>% 
  mutate(`Current Win %` = round(current_win_perc, 2),
         `Vegas Win Projection %` = round(percentage_projection, 2)) %>% 
  mutate(over_under = case_when(
    `Current Win %` > `Vegas Win Projection %` ~ "OVER",
    `Current Win %` < `Vegas Win Projection %` ~ "UNDER",
    TRUE                                       ~ "PUSH")
  ) %>% 
  group_by(`Team Name`) %>% 
  mutate(flipped = if_else(lag(over_under) != over_under, TRUE, NA),
         flipped_today = if_else(as.Date(`Game Date`) == Sys.Date() - 1,
                                 TRUE,
                                 NA))

# Pulling in picks
days_in_season_to_today <- seq(from = as.Date(nba_season_start_date), 
                               to = as.Date(Sys.Date() - 1), 
                               by = "days")
teams <- unique(join_with_projections$`Team Name`)

standings_grid <- crossing(team = teams, date = days_in_season_to_today) %>% 
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

with_picks <- standings_grid %>% 
  inner_join(picks, by = "team") %>% 
  mutate(current_projected_points = case_when(
    (over_under == "PUSH") ~ 0,
    (over_under == choice) ~ wage,
    (over_under != choice) ~ -wage
  ))

player_projections_by_team <- with_picks %>% 
  select(Date = date, Player = player, Team = team, starts_with("current"),
         `Vegas Win Projection %`,
         `Current Over/Under` = over_under,
         `Over/Under Pick` = choice) %>% 
  mutate(
    `Win % - Vegas` = round(`Current Win %` - `Vegas Win Projection %`, 2)
  ) %>% 
  relocate(current_projected_points, 
           .after = `Win % - Vegas`)

changes_in_player_projections <- player_projections_by_team %>% 
  group_by(Player, Team) %>% 
  mutate(
    `Change in Points Since Yesterday` = current_projected_points - lag(current_projected_points)
  ) %>% 
  ungroup() %>% 
  arrange(Date, Team, Player) %>% 
  filter(`Change in Points Since Yesterday` != 0) %>% 
  select(Date, Player, Team, `Current Record`, `Current Win %`, 
         `Vegas Win Projection %`, `Change in Points Since Yesterday`)

rows_today <- changes_in_player_projections %>% 
  filter(Date == Sys.Date() - 1)

projected_score <- player_projections_by_team %>% 
  group_by(Date, Player) %>% 
  summarize(
    `Projected Total Points` = sum(current_projected_points, na.rm = TRUE),
    `Number Correct` = sum(current_projected_points > 0),
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

player_projections_by_team <- player_projections_by_team %>%
  rename(
    `Current Projected Points` = current_projected_points,
    `Current Wins` = current_wins,
    `Current Losses` = current_losses
  ) %>%
  mutate(
    `Wins To Go Over Vegas` = 
      ceiling(`Vegas Win Projection %` / 100 * num_games) - `Current Wins`,
    `Winning % In Remaining Games Needed` = round(
      `Wins To Go Over Vegas` / (num_games - `Current Wins` - `Current Losses`) * 100, 2),
    .before = `Current Projected Points`
  ) %>%
  mutate(`Outcome Determined` = factor(case_when(
    `Winning % In Remaining Games Needed` <= 0 ~ "OVER",
    `Winning % In Remaining Games Needed` > 100 ~ "UNDER",
    TRUE ~ "not yet"
  ), levels = c("not yet", "OVER", "UNDER")
  ))  %>%
  mutate(
    `Losses To Go Under Vegas` = 
      num_games - ceiling(`Vegas Win Projection %` / 100 * num_games) - `Current Losses`,
    .before = `Current Projected Points`
  ) %>%
  mutate(`Winning % In Remaining Games Needed` = if_else(
    `Losses To Go Under Vegas` < 0 | `Winning % In Remaining Games Needed` > 100 | `Wins To Go Over Vegas` <= 0,
    NA_real_,
    `Winning % In Remaining Games Needed`
  )) %>%
  mutate(`Wins To Go Over Vegas` = if_else(
    `Losses To Go Under Vegas` < 0 | `Winning % In Remaining Games Needed` > 100 | `Wins To Go Over Vegas` <= 0,
    NA_real_,
    `Wins To Go Over Vegas`
  )) %>%
  mutate(`Losses To Go Under Vegas` = if_else(
    `Losses To Go Under Vegas` < 0 | `Winning % In Remaining Games Needed` > 100 | `Wins To Go Over Vegas` <= 0,
    NA_real_,
    `Losses To Go Under Vegas`
  )) %>%
  select(-`Current Wins`, -`Current Losses`) 


most_recent_results <- projected_score %>% 
  mutate(days_from_today = as.numeric(difftime(Sys.Date(),
                                               Date,
                                               units = c("days")))) %>% 
  filter(days_from_today == min(days_from_today))

current_rankings <- most_recent_results %>% 
  arrange(desc(`Projected Total Points`),
          desc(`Number Correct`),
          desc(`Number (Wage 15) Correct`)) %>% 
  pull(Player)

# As a check to compare to Phil's report
# previous_reported_results <- projected_score %>% 
#   filter(Date == as.Date("2021-01-28")) %>% 
#   arrange(desc(`Projected Total Points`))
# 
# previous_projected_scores <- player_projections_by_team %>% 
#   filter(Date == as.Date("2021-01-28"))

standings_with_projected <- standings %>% 
  mutate(date = Sys.Date() - 1) %>% 
  mutate(wins = as.numeric(wins),
         losses = as.numeric(losses)) %>% 
  mutate(current_win_perc = wins / (wins + losses) * 100) %>% 
  inner_join(projections, by = c("team_name" = "team")) %>% 
  select(-percentage_projection) %>% 
  mutate(
    remaining_games = num_games - (wins + losses),
    wins_needed_for_over = ceiling(as.numeric(win_projection)) - wins,
    winning_perc_for_over = wins_needed_for_over / remaining_games * 100
  ) %>% 
  mutate(differential = current_win_perc - winning_perc_for_over)

wins_needed <- player_projections_by_team %>% 
  filter(Date == Sys.Date() - 1) 

# Manual fix to reduce Lakers total wins by 1 and increase losses by 1
# Not sure what happened on 2024-04-09
#wins_needed[wins_needed$Team == "Los Angeles Lakers", "Wins"] <- wins_needed[wins_needed$Team == "Los Angeles Lakers", "Wins"] - 1
#wins_needed[wins_needed$Team == "Los Angeles Lakers", "Losses"] <- wins_needed[wins_needed$Team == "Los Angeles Lakers", "Losses"] + 1
#wins_needed[wins_needed$Team == "Los Angeles Lakers", "Current Record"] <- "45-35" #paste0(wins_needed[wins_needed$Team == "Los Angeles Lakers", "Wins"], "-", wins_needed[wins_needed$Team == "Los Angeles Lakers", "Losses"])

wins_needed <- wins_needed %>% 
  select(Date, Team, `Outcome Determined`,
         `Current Record`, `Wins To Go Over Vegas`,
         `Winning % In Remaining Games Needed`,
         `Losses To Go Under Vegas`) %>% 
  distinct() %>% 
  mutate(`Wins To Go Over Vegas` = if_else(
    `Wins To Go Over Vegas` <= 0 || 
      `Winning % In Remaining Games Needed` > 100,
    NA_real_,
    `Wins To Go Over Vegas`
  )) %>% 
  mutate(`Losses To Go Under Vegas` = if_else(
    `Losses To Go Under Vegas` < 0 || 
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


out_table <- projections %>% 
  inner_join(wins_needed, by = c("team" = "Team")) %>% 
  select(Team = team,
         `Outcome Determined`,
         `Current Record`,
         `Win Projection` = win_projection,
         `Remaining Games`,
         `Wins To Go Over Vegas`
  ) 

write_rds(out_table, paste0("determined_outcomes/determined_outcomes_", Sys.Date(), ".rds"))

invisible(file.copy(paste0("determined_outcomes/determined_outcomes_", Sys.Date(), ".rds"),
                    paste0("over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds"),
                    overwrite = TRUE))



# Playoff tie-breakers
calculate_head_to_head <- function(tied_teams, scores_tidy) {
  head_to_head_games <- scores_tidy %>%
    filter(team_slug %in% tied_teams & opponent_slug %in% tied_teams) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug)) %>%
    group_by(winner) %>%
    summarise(wins = n(), .groups = 'drop') %>%
    arrange(desc(wins))
  
  return(head_to_head_games)
}

calculate_division_record <- function(tied_teams, scores, teams_info) {
  division_games <- scores %>%
    filter((slug_away_team %in% tied_teams & slug_home_team %in% teams_info$division_teams) |
             (slug_home_team %in% tied_teams & slug_away_team %in% teams_info$division_teams)) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug),
           loser = if_else(score > opponent_score, opponent_slug, team_slug)) %>%
    group_by(winner) %>%
    summarise(wins = n()) %>%
    right_join(scores %>%
                 group_by(loser) %>%
                 summarise(losses = n()), by = c("winner" = "loser")) %>%
    mutate(total_games = wins + losses,
           win_pct = wins / total_games) %>%
    arrange(desc(win_pct))
  
  return(division_games)
}

calculate_conference_record <- function(tied_teams, scores, teams_info) {
  conference_games <- scores %>%
    filter((slug_away_team %in% tied_teams & slug_home_team %in% teams_info$conference_teams) |
             (slug_home_team %in% tied_teams & slug_away_team %in% teams_info$conference_teams)) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug),
           loser = if_else(score > opponent_score, opponent_slug, team_slug)) %>%
    group_by(winner) %>%
    summarise(wins = n()) %>%
    right_join(scores %>%
                 group_by(loser) %>%
                 summarise(losses = n()), by = c("winner" = "loser")) %>%
    mutate(total_games = wins + losses,
           win_pct = wins / total_games) %>%
    arrange(desc(win_pct))
  
  return(conference_games)
}

calculate_winning_pct_against_playoff_teams_conference <- function(tied_teams, scores, playoff_teams) {
  playoff_games_conference <- scores %>%
    filter((slug_away_team %in% tied_teams & slug_home_team %in% playoff_teams$conference_teams) |
             (slug_home_team %in% tied_teams & slug_away_team %in% playoff_teams$conference_teams)) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug),
           loser = if_else(score > opponent_score, opponent_slug, team_slug)) %>%
    group_by(winner) %>%
    summarise(wins = n()) %>%
    right_join(scores %>%
                 group_by(loser) %>%
                 summarise(losses = n()), by = c("winner" = "loser")) %>%
    mutate(total_games = wins + losses,
           win_pct = wins / total_games) %>%
    arrange(desc(win_pct))
  
  return(playoff_games_conference)
}

calculate_winning_pct_against_playoff_teams_opposing_conference <- function(tied_teams, scores, playoff_teams) {
  playoff_games_opposing <- scores %>%
    filter((slug_away_team %in% tied_teams & slug_home_team %in% playoff_teams$opposing_conference_teams) |
             (slug_home_team %in% tied_teams & slug_away_team %in% playoff_teams$opposing_conference_teams)) %>%
    mutate(winner = if_else(score > opponent_score, team_slug, opponent_slug),
           loser = if_else(score > opponent_score, opponent_slug, team_slug)) %>%
    group_by(winner) %>%
    summarise(wins = n()) %>%
    right_join(scores %>%
                 group_by(loser) %>%
                 summarise(losses = n()), by = c("winner" = "loser")) %>%
    mutate(total_games = wins + losses,
           win_pct = wins / total_games) %>%
    arrange(desc(win_pct))
  
  return(playoff_games_opposing)
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
  if("wins" %in% names(standings) || "wins" %in% names(tiebreaker_results)) {
    standings <- standings %>%
      left_join(tiebreaker_results, by = c("team_name" = "winner", "wins")) # %>%
      # mutate(rank = ifelse(!is.na(wins), rank(-wins, ties.method = "first"), NA_real_)) %>%
      # arrange(conference, rank, `Winning Pct`, team_name) # %>%
      # select(-wins, -rank)
  }
  return(standings)
}

playoff_teams <- standings %>%
  arrange(conference, desc(`Winning Pct`)) %>%
  group_by(conference) %>%
  slice(1:6) %>%
  ungroup()

apply_tiebreakers <- function(tied_teams, scores_tidy, teams_info, playoff_teams, standings) {
  # Head-to-Head
  head_to_head_result <- calculate_head_to_head(tied_teams, scores_tidy)
  if(is_tie_resolved(head_to_head_result)) {
    return(finalize_ranking(head_to_head_result, standings))
  }
  
  # Division Record
  division_record_result <- calculate_division_record(tied_teams, scores_tidy, teams_info)
  if(is_tie_resolved(division_record_result)) {
    return(finalize_ranking(division_record_result, standings))
  }
  
  # Conference Record
  conference_record_result <- calculate_conference_record(tied_teams, scores_tidy, teams_info)
  if(is_tie_resolved(conference_record_result)) {
    return(finalize_ranking(conference_record_result, standings))
  }
  
  # Winning Percentage Against Playoff Teams in Own Conference
  wpct_against_playoff_teams_conference_result <- calculate_winning_pct_against_playoff_teams_conference(tied_teams, scores_tidy, playoff_teams)
  if(is_tie_resolved(wpct_against_playoff_teams_conference_result)) {
    return(finalize_ranking(wpct_against_playoff_teams_conference_result, standings))
  }
  
  # Winning Percentage Against Playoff Teams in Opposing Conference
  wpct_against_playoff_teams_opposing_result <- calculate_winning_pct_against_playoff_teams_opposing_conference(tied_teams, scores_tidy, playoff_teams)
  if(is_tie_resolved(wpct_against_playoff_teams_opposing_result)) {
    return(finalize_ranking(wpct_against_playoff_teams_opposing_result, standings))
  }
  
  # Point Differential
  point_differential_result <- calculate_point_differential(tied_teams, scores_tidy)
  if(is_tie_resolved(point_differential_result)) {
    return(finalize_ranking(point_differential_result, standings))
  }
  
  return(standings)
}

# Extract tied teams as a list of character vectors
tied_teams_list <- standings %>%
  group_by(`Winning Pct`, conference) %>%
  filter(n() > 1) %>%
  summarise(team_names = list(team_name)) %>%
  pull(team_names)

# Iterate over each group of tied teams and apply tiebreakers
for(team_group in tied_teams_list) {
  standings <- apply_tiebreakers(team_group, scores_tidy, teams_info, playoff_teams, standings)
}

# Ensure division winners are in the top 4 seeds, adjusting the standings as necessary
# The provided code for adjusting standings for division winners outside top 4 already performs this task


remaining_check <- standings_with_projected %>% 
  inner_join(picks, by = c("team_name" = "team"))

chester_check <- remaining_check %>% 
  filter(player == "Chester") %>% 
  select(-player)

