library(data.table)
library(readxl)
library(tidyverse)

# If you haven't already, convert your data frames to data.tables
picks_wide_new <- read_rds("picks_wide_new.rds")
determined_so_far <- read_rds(paste0("determined_outcomes_", Sys.Date(), ".rds"))

# You can convert a data.frame to a data.table using setDT() if needed
setDT(picks_wide_new)
setDT(determined_so_far)

# Prepare your data similar to your current process
teams <- determined_so_far[order(Team), .(Team)]

outcome_not_determined_teams <- determined_so_far[`Outcome Determined` == "not yet", .(Team)]
setorder(outcome_not_determined_teams, Team)

determined_teams <- determined_so_far[`Outcome Determined` != "not yet", .(Team, `Outcome Determined`)]
# Add a dummy id column to your data.table
determined_teams[, dummy := 1]

# Use dcast to spread the data to a wide format
determined_teams_wide <- dcast(determined_teams, dummy ~ Team, value.var = "Outcome Determined")

# Optionally, you can remove the dummy column afterwards if it's not needed
determined_teams_wide[, dummy := NULL]

# Generate all possible outcomes
num_not_determined <- nrow(outcome_not_determined_teams)
cols <- paste0("V", seq_len(num_not_determined))

# Use CJ() for cross join in data.table to generate combinations, much like expand.grid()
possible_combinations_out <- do.call(CJ, replicate(n = num_not_determined, c("UNDER", "OVER"), simplify = FALSE))

setnames(possible_combinations_out, old = cols, new = outcome_not_determined_teams$Team)

possible_combinations <- cbind(possible_combinations_out, determined_teams_wide) |> 
  rownames_to_column(var = "sim")

# For long format, use melt() from data.table, which is similar to pivot_longer() from tidyr
# long_outcomes <- melt(possible_combinations, id.vars = "sim", variable.name = "Team", value.name = "outcome")

# Determine the number of chunks
n_chunks <- 10
chunk_size <- ceiling(nrow(possible_combinations) / n_chunks)

# Initialize an empty list to store results
results <- list()

for (i in 1:n_chunks) {
  # Calculate row indices for the current chunk
  chunk_start <- (i - 1) * chunk_size + 1
  chunk_end <- min(i * chunk_size, nrow(possible_combinations))
  
  # Subset the data.table for the current chunk
  chunk <- possible_combinations[chunk_start:chunk_end]
  
  # Perform the melt operation on the current chunk
  melted_chunk <- melt(chunk, id.vars = "sim", variable.name = "Team", value.name = "outcome")
  
  # Store the result
  results[[i]] <- melted_chunk
}
