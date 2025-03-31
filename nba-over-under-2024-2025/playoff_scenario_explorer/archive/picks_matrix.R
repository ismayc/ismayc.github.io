# Suppose you have picks_wide_new like:
#   Team                adonis_choice   Adonis_points   andy_choice  ...
#   "Boston Celtics"    "OVER"          15             "UNDER"   ...
#   "Boston Celtics"    ...
#   etc.

# We'll build a matrix with row = "Boston Celtics_OVER", row = "Boston Celtics_UNDER", etc.
# Columns = player names: e.g. c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")

picks_wide_new <- readRDS("picks_wide_new.rds")

player_names <- c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")  # adjust as needed

# Initialize an empty list to store row vectors
row_list <- list()

for (i in seq_len(nrow(picks_wide_new))) {
  row_data <- picks_wide_new[i, ]
  team_name <- row_data$team
  
  # For each possible outcome: OVER or UNDER
  # We'll check each player's pick to see if it matches the outcome
  for (outcome in c("OVER", "UNDER")) {
    row_label <- paste0(team_name, "_", outcome)
    
    # Create a numeric named vector
    row_vec <- c(
      Adonis   = if (outcome == row_data$adonis_choice) row_data$Adonis_points else -row_data$Adonis_points,
      Andy     = if (outcome == row_data$andy_choice)   row_data$Andy_points   else -row_data$Andy_points,
      Chester  = if (outcome == row_data$chester_choice) row_data$Chester_points else -row_data$Chester_points,
      Jake     = if (outcome == row_data$jake_choice) row_data$Jake_points else -row_data$Jake_points,
      Mary     = if (outcome == row_data$mary_choice) row_data$Mary_points else -row_data$Mary_points,
      Mike     = if (outcome == row_data$mike_choice) row_data$Mike_points else -row_data$Mike_points,
      Phil     = if (outcome == row_data$phil_choice) row_data$Phil_points else -row_data$Phil_points,
      Ryan     = if (outcome == row_data$ryan_choice) row_data$Ryan_points else -row_data$Ryan_points
    )
    
    row_list[[row_label]] <- row_vec
  }
}

# Convert the list of named vectors into a matrix
picks_matrix <- do.call(rbind, row_list)
# remove the first errant two rows [1:2,
picks_matrix <- picks_matrix[!grepl("^_", rownames(picks_matrix)), ]
#picks_matrix <- do.call(rbind, row_list)[-c(1:2), ]  
# rownames(picks_matrix) = e.g. "Boston Celtics_OVER", "Boston Celtics_UNDER", ...
# colnames(picks_matrix) = player_names

# Save it for use in Shiny
saveRDS(picks_matrix, "picks_matrix.rds")
