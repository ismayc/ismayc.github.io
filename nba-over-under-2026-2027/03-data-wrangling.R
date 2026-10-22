library(tidyverse)
library(plotly)
library(here)
library(glue)

# The calculations live in the ouanalytics package (ouanalytics/), tested at
# 100% coverage. Install it once, and again after a change to its code:
#   R CMD INSTALL ouanalytics
# This script reads the saved data, calls them, and writes the results.
library(ouanalytics)

yesterday <- Sys.Date() - 1

standings <- here("rds", glue("standings_through_{yesterday}.rds")) %>%
  read_rds()

# From 02c-get-game-scores.R
game_results_raw <- here(
  "rds",
  glue("game_results_raw_through_{yesterday}.rds")) %>%
  read_rds()

# Every table the page uses, from the saved data (02c, 02b)
pool <- compute_pool(
  game_results_raw, scores, projections, picks, standings,
  yesterday = yesterday, today = Sys.Date(),
  season_start = nba_season_start_date, num_games = num_games
)
game_results <- pool$game_results
scores_tidy <- pool$scores_tidy
join_with_projections <- pool$join_with_projections
standings_grid <- pool$standings_grid
changes_in_player_projections <- pool$changes_in_player_projections
rows_today <- pool$rows_today
projected_score <- pool$projected_score
player_projections_by_team <- pool$player_projections_by_team
current_rankings <- pool$current_rankings
standings_with_projected <- pool$standings_with_projected
wins_needed <- pool$wins_needed

teams <- unique(join_with_projections$`Team Name`)

write_rds(standings_grid,
          here("rds", glue("standings_grid_through_{yesterday}.rds")))

# The inputs and the tables, for the Python cross-check (ouanalytics-py).
# Not published: check-export/ is gitignored.
export_pool_case(
  here("check-export", "pool"), game_results_raw, scores, projections, picks,
  standings,
  params = list(yesterday = yesterday, today = Sys.Date(),
                season_start = nba_season_start_date, num_games = num_games),
  pool = pool
)

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

# Playoff tie-breakers are applied by compute_playoff_seeding() in the
# package. A second, older tie-break loop was here until October 22, 2026:
# it never changed `standings` on a path that ran, and the paths past
# head-to-head stopped with an error.

remaining_check <- standings_with_projected %>%
  inner_join(picks, by = c("team_name" = "team"))

chester_check <- remaining_check %>%
  filter(player == "Chester") %>%
  select(-player)
