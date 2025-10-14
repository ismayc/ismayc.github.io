library(tidyverse)

tic()

# Players
players <- c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")

# Column name helper functions
choice_col_for <- function(player) paste0(tolower(player), "_choice")
points_col_for <- function(player) paste0(player, "_points")

# Read data
picks_wide_new <- readr::read_rds("picks_wide_new.rds") %>% 
  rename(Team = team)

determined_so_far <- readr::read_rds(
  paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
)

# Join to get outcomes with picks
picks_joined <- picks_wide_new %>%
  left_join(determined_so_far %>% select(Team, `Outcome Determined`), by = "Team")

# ---- MANUAL OVERRIDES HERE ----
manual <- FALSE
manual_overrides <- c(
  "Atlanta Hawks" = "OVER"#,  # Manually lock Atlanta Hawks to OVER
#  "New York Knicks" = "UNDER" # You can comment out or change any line
)
# --------------------------------

if (manual) {
# Apply overrides to determined outcomes
picks_joined <- picks_joined %>%
  mutate(
    `Outcome Determined` = if_else(
      Team %in% names(manual_overrides),
      manual_overrides[Team],
      `Outcome Determined`
    )
  )
}

# Matrix setup
team_names <- picks_joined$Team
num_teams <- length(team_names)
num_players <- length(players)

# Build pick and points matrices
pick_matrix <- matrix(NA, nrow = num_teams, ncol = num_players, dimnames = list(team_names, players))
points_matrix <- matrix(NA, nrow = num_teams, ncol = num_players, dimnames = list(team_names, players))

for (j in seq_along(players)) {
  pick_matrix[, j] <- ifelse(picks_joined[[choice_col_for(players[j])]] == "OVER", 1, -1)
  points_matrix[, j] <- picks_joined[[points_col_for(players[j])]]
}

# Create outcome vector (+1, -1, or 0)
outcome_vector <- ifelse(
  picks_joined$`Outcome Determined` == "OVER", 1,
  ifelse(picks_joined$`Outcome Determined` == "UNDER", -1, 0)
)

# Identify undecided teams
not_determined_idx <- which(outcome_vector == 0)
num_not_determined <- length(not_determined_idx)

# Generate all combinations
all_combos <- expand.grid(rep(list(c(-1, 1)), num_not_determined))
n_scen <- nrow(all_combos)

scenario_outcomes <- matrix(rep(outcome_vector, times = n_scen), nrow = n_scen, byrow = TRUE)
scenario_outcomes[, not_determined_idx] <- as.matrix(all_combos)

# Corrected broadcasting
n_teams <- ncol(scenario_outcomes)

arr_outcome <- array(rep(scenario_outcomes, times = num_players),
                     dim = c(n_scen, n_teams, num_players))

arr_picks <- array(rep(pick_matrix, each = n_scen),
                   dim = c(n_scen, n_teams, num_players))

arr_points <- array(rep(points_matrix, each = n_scen),
                    dim = c(n_scen, n_teams, num_players))

# Now multiply elementwise
arr_scores <- arr_outcome * arr_picks * arr_points

# Compute scenario scores
scores <- apply(arr_scores, c(1, 3), sum)
colnames(scores) <- players

# Compute rankings
scenario_ranks <- t(apply(scores, 1, function(x) rank(-x, ties.method = "min")))
made_playoffs <- scenario_ranks <= 4

# Summarize
summary_df <- data.frame(
  player = players,
  median_rank = apply(scenario_ranks, 2, median),
  mean_rank   = colMeans(scenario_ranks),
  highest_rank    = apply(scenario_ranks, 2, min),
  lowest_rank    = apply(scenario_ranks, 2, max),
  num_sims    = n_scen,
  prob_playoffs = 100 * colMeans(made_playoffs),
  prob_first  = 100 * colMeans(scenario_ranks == 1)
) %>% arrange(median_rank)

rownames(summary_df) <- NULL

# Save results
readr::write_rds(summary_df, "scenarios_final_13_matrix.rds")


print(summary_df)