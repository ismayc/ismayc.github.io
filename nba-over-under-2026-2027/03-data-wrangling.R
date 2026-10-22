library(tidyverse)
library(plotly)
library(here)
library(glue)

# The calculations live in R/ so tests/testthat can cover them; this script
# reads the saved data, calls them, and writes the results.
source(here("R", "standings_logic.R"))
source(here("R", "tiebreakers.R"))

yesterday <- Sys.Date() - 1

standings <- here("rds", glue("standings_through_{yesterday}.rds")) %>%
  read_rds()

# From 02c-get-game-scores.R
game_results_raw <- here(
  "rds",
  glue("game_results_raw_through_{yesterday}.rds")) %>%
  read_rds()

game_results <- compute_game_results(game_results_raw)

# From 02c (and 02b-get-game-scores.R)
scores_tidy <- tidy_scores(scores)

updated_schedule <- add_game_scores(game_results, scores_tidy)

join_with_projections <- add_over_under(updated_schedule, projections,
                                        yesterday)

teams <- unique(join_with_projections$`Team Name`)

# Pulling in picks
standings_grid <- build_standings_grid(join_with_projections,
                                       nba_season_start_date, yesterday)
write_rds(standings_grid,
          here("rds", glue("standings_grid_through_{yesterday}.rds")))

player_projections_by_team <- score_picks(standings_grid, picks)

changes_in_player_projections <- add_changes(player_projections_by_team)

rows_today <- rows_for_day(changes_in_player_projections, yesterday)

projected_score <- projected_score_by_player(player_projections_by_team)

player_projections_by_team <- add_outcome_columns(player_projections_by_team,
                                                  num_games)

current_rankings <- rank_players(projected_score, Sys.Date())

standings_with_projected <- add_projected_to_standings(
  standings, projections, yesterday, num_games)

wins_needed <- build_wins_needed(player_projections_by_team, yesterday,
                                 num_games)

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

# Playoff tie-breakers are in R/tiebreakers.R
playoff_teams <- standings %>%
  arrange(conference, desc(`Winning Pct`)) %>%
  group_by(conference) %>%
  slice(1:6) %>%
  ungroup()

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

remaining_check <- standings_with_projected %>%
  inner_join(picks, by = c("team_name" = "team"))

chester_check <- remaining_check %>%
  filter(player == "Chester") %>%
  select(-player)
