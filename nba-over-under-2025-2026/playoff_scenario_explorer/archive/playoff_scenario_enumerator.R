source("expand_grid-database.R")

#############################################################################
# 0. LIBRARIES & INITIAL SETUP
#############################################################################
library(tidyverse)
library(rlang)
library(gtools)
library(DBI)
library(RSQLite)
library(glue)
library(furrr)

# We assume you already have these in your environment:
# - outcome_not_determined_teams: character vector of teams
# - possible_combinations: wide tibble with all sim combos
# - populated_sum_long: data for scoring, etc.

teams_left <- outcome_not_determined_teams

# We'll create a final combined database named scenarios.sqlite
final_db <- "scenarios.sqlite"

# OPTIONAL: fresh start if desired
if (file.exists(final_db)) file.remove(final_db)

#############################################################################
# 1. BUILD ALL UNIQUE SCENARIO IDs
#############################################################################
# Let's generate each subset and each OVER/UNDER combination,
# in a canonical sorted form, so there are no duplicates.

# Calculate the total # of combos for progress (1..n)
total_scenarios <- sum(map_int(1:length(teams_left), ~ choose(length(teams_left), .x) * 2^.x))
message(glue("Total scenario combos to enumerate: {formatC(total_scenarios, big.mark = ',', format = 'd')}"))

# Function to build scenario_id in a canonical (alphabetical) way
build_scenario_id <- function(team_subset, outcome_vector) {
  # outcome_vector is like c("Boston Celtics"="OVER", ...)
  team_labels <- map_chr(names(outcome_vector), ~ str_extract(.x, "\\w+$"))
  named_outcomes <- set_names(outcome_vector, team_labels)
  sorted_outcomes <- named_outcomes[order(names(named_outcomes))]
  label_outcomes <- paste0(names(sorted_outcomes), "_", sorted_outcomes)
  scenario_id <- paste(label_outcomes, collapse = "-")
  scenario_id
}

# Time and counter for progress in ID generation
start_time <- Sys.time()
scenario_counter <- 0

message("Enumerating all scenario IDs...")

# Check to see if file already generated for the number of teams remaining
scenario_file_name <- paste0("unique_scenario_ids_for_", length(teams_left), "_teams.rds")
if (file.exists(scenario_file_name)) {
  unique_scenario_ids <- read_rds(scenario_file_name)
} else {
  unique_scenario_ids <- map_dfr(1:length(teams_left), function(k) {
    combn(teams_left, k, simplify = FALSE) %>%
      map_dfr(function(team_subset) {
        outcome_combos <- expand_grid(
          !!!set_names(rep(list(c("OVER", "UNDER")), length(team_subset)), team_subset)
        )
        outcome_combos %>%
          pmap_chr(function(...) {
            scenario_counter <<- scenario_counter + 1
            # Optional progress printing
            if (scenario_counter %% 10000 == 0) {
              elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
              pct <- round(scenario_counter / total_scenarios * 100, 2)
              message(glue("Built scenario ID {scenario_counter}/{total_scenarios} ({pct}%) | Elapsed: {elapsed}m"))
            }
            this_outcome <- list(...)
            names(this_outcome) <- team_subset
            build_scenario_id(team_subset, this_outcome)
          }) %>%
          tibble(scenario_id = .)
      })
  }) 
  write_rds(unique_scenario_ids, scenario_file_name)
}

message(glue("Done enumerating the {formatC(total_scenarios, big.mark = ',', format = 'd')} combos."))
# Takes about 20 minutes for 13 teams

# Convert to a data frame with a row index for parallel chunk processing
df_scenarios <- unique_scenario_ids %>%
  mutate(idx = row_number()) %>%
  select(idx, scenario_id)

#############################################################################
# 2. PARSE A SCENARIO ID BACK INTO FIXED OUTCOMES
#############################################################################
# E.g.: "Celtics_OVER-Hornets_UNDER" -> c("Boston Celtics"="OVER", "Charlotte Hornets"="UNDER")
team_label_map <- set_names(teams_left, map_chr(teams_left, ~ str_extract(.x, "\\w+$")))

parse_scenario_id <- function(sid) {
  parts <- str_split(sid, "-", simplify = TRUE)
  # str_match each part: "Celtics_OVER" => group1=Celtics, group2=OVER
  mo <- str_match(parts, "^([^_]+)_([A-Z]+)$")
  labels <- mo[,2]
  outcomes <- mo[,3]
  full_team_names <- unname(team_label_map[labels])
  names(outcomes) <- full_team_names
  outcomes
}

#############################################################################
# 3. BUILD A FUNCTION TO PROCESS A CHUNK OF SCENARIOS -> PARTIAL DB
#############################################################################
# Each worker will call this function with a subset of scenarios
# and create its own partial DB (scenarios_partX.sqlite).

process_chunk_to_db <- function(chunk_df, part_index,
                                possible_combinations, populated_sum_long,
                                db_prefix = "scenarios_part") {
  db_file <- paste0(db_prefix, part_index, ".sqlite")
  
  # Overwrite if exists
  if (file.exists(db_file)) file.remove(db_file)
  
  con_part <- dbConnect(SQLite(), db_file)
  on.exit(dbDisconnect(con_part), add = TRUE)
  
  # We'll do a local progress counter for chunk_df
  chunk_start <- Sys.time()
  
  for (i in seq_len(nrow(chunk_df))) {
    scenario_id <- chunk_df$scenario_id[i]
    index <- chunk_df$idx[i]
    
    # optional progress message
    if (i %% 50 == 0) {
      chunk_elapsed <- round(difftime(Sys.time(), chunk_start, units = "secs"), 1)
      message(glue("Worker {part_index}: {i}/{nrow(chunk_df)} in chunk. Elapsed: {chunk_elapsed}s"))
    }
    
    # parse scenario
    fixed_outcomes <- parse_scenario_id(scenario_id)
    
    # Check local partial DB if scenario is present
    if (dbExistsTable(con_part, "scenarios")) {
      existing <- dbGetQuery(con_part, glue("SELECT 1 FROM scenarios WHERE scenario = '{scenario_id}' LIMIT 1"))
      if (nrow(existing) > 0) next
    }
    
    # Build filter expression
    filter_exprs <- map2(names(fixed_outcomes), fixed_outcomes, ~ expr(!!sym(.x) == !!.y))
    combined_expr <- reduce(filter_exprs, ~ expr((!!.x) & (!!.y)))
    
    sims_to_keep <- possible_combinations %>%
      filter(!!combined_expr) %>%
      pull(sim)
    if (length(sims_to_keep) == 0) next
    
    df <- populated_sum_long %>%
      filter(sim %in% sims_to_keep)
    if (nrow(df) == 0) next
    
    # Compute scenario results
    scenarios <- df %>%
      group_by(sim) %>%
      arrange(desc(total), desc(number_correct), desc(number_15_correct), .by_group = TRUE) %>%
      ungroup() %>%
      rownames_to_column(var = "rank") %>%
      mutate(rank = as.numeric(rank) %% num_players, .before = player) %>%
      mutate(rank = if_else(rank == 0, num_players, rank)) %>%
      mutate(playoffs = rank <= 4) %>%
      select(sim, everything())
    
    scenarios_final <- scenarios %>%
      group_by(player) %>%
      summarize(
#        median_expected_total = median(total),
#        mean_expected_total = mean(total),
#        sd_expected_total = sd(total),
        median_rank = median(rank),
        mean_rank = mean(rank),
        highest_rank = min(rank),
        lowest_rank = max(rank),
        num_sims = n(),
        num_times_playoffs = sum(playoffs),
        prob_playoffs = mean(playoffs) * 100,
        prob_one_rank = mean(rank == 1) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(median_expected_total)) %>%
      mutate(scenario = scenario_id)
    
    dbWriteTable(con_part, "scenarios", scenarios_final, append = TRUE)
  }
  
  # Return path to partial DB
  db_file
}

#############################################################################
# 4. PARALLELIZE: SPLIT SCENARIOS INTO CHUNKS, PROCESS SEPARATE DBs
#############################################################################
# We'll create e.g. #chunks = #workers. 
# Then each chunk is processed in a separate R session -> separate .sqlite file.

n_workers <- 5
plan(multisession, workers = n_workers)

n_scenarios <- nrow(df_scenarios)
formatted_n_scenarios <- formatC(n_scenarios, big.mark = ',', format = 'd')
chunk_size <- ceiling(n_scenarios / n_workers)

scenario_chunks <- split(df_scenarios, ceiling(seq_len(n_scenarios) / chunk_size))
message(glue("Creating {length(scenario_chunks)} chunks for {formatted_n_scenarios} scenarios."))

partial_dbs <- future_map2(
  scenario_chunks,
  seq_along(scenario_chunks),
  ~ process_chunk_to_db(.x, .y, possible_combinations, populated_sum_long),
  .progress = TRUE
)

#############################################################################
# 5. MERGE PARTIAL DB FILES INTO A SINGLE 'scenarios.sqlite'
#############################################################################
message("Merging partial DB files into final scenarios.sqlite")

con_final <- dbConnect(SQLite(), final_db)
on.exit(dbDisconnect(con_final), add = TRUE)

for (pdb in partial_dbs) {
  if (!file.exists(pdb)) next
  con_part <- dbConnect(SQLite(), pdb)
  
  if (!dbExistsTable(con_part, "scenarios")) {
    dbDisconnect(con_part)
    next
  }
  
  # read partial
  df_part <- dbReadTable(con_part, "scenarios")
  dbDisconnect(con_part)
  
  # write to final
  dbWriteTable(con_final, "scenarios", df_part, append = TRUE)
  
  # optionally remove partial to save space
  # file.remove(pdb)
}

message("All done! Combined DB is scenarios.sqlite.")


#############
# RDS option
## Running through all scenarios
# library(tidyverse)
# library(rlang)
# library(gtools)
# 
# # Inputs
# teams_left <- outcome_not_determined_teams
# 
# # Ensure output directory
# dir.create("whatif_outputs", showWarnings = FALSE)
# 
# # Function to get all OVER/UNDER combinations for a given team subset
# enumerate_outcomes <- function(team_subset) {
#   expand_grid(
#     !!!set_names(rep(list(c("OVER", "UNDER")), length(team_subset)), team_subset)
#   )
# }
# 
# # Loop through subset sizes (1 to 13 teams fixed)
# for (subset_size in 1:length(teams_left)) {
#   team_subsets <- combn(teams_left, subset_size, simplify = FALSE)
#   
#   for (team_subset in team_subsets) {
#     outcome_combos <- enumerate_outcomes(team_subset)
#     
#     for (i in 1:nrow(outcome_combos)) {
#       fixed_outcomes <- as.list(outcome_combos[i, ])
#       names(fixed_outcomes) <- team_subset
#       
#       filter_exprs <- map2(team_subset, fixed_outcomes, ~ expr(!!sym(.x) == !!.y))
#       combined_expr <- reduce(filter_exprs, ~ expr((!!.x) & (!!.y)))
#       
#       sims_to_keep <- possible_combinations %>%
#         filter(!!combined_expr) %>%
#         pull(sim)
#       
#       if (length(sims_to_keep) == 0) next
#       
#       df <- populated_sum_long %>%
#         filter(sim %in% sims_to_keep)
#       
#       if (nrow(df) == 0) next
#       
#       scenarios <- df %>%
#         group_by(sim) %>%
#         arrange(desc(total), desc(number_correct), desc(number_15_correct), .by_group = TRUE) %>%
#         ungroup() %>%
#         rownames_to_column(var = "rank") %>%
#         mutate(rank = as.numeric(rank) %% num_players, .before = player) %>%
#         mutate(rank = if_else(rank == 0, num_players, rank)) %>%
#         mutate(playoffs = rank <= 4) %>%
#         select(sim, everything())
#       
#       scenarios_final <- scenarios %>%
#         group_by(player) %>%
#         summarize(
#           median_expected_total = median(total),
#           mean_expected_total = mean(total),
#           sd_expected_total = sd(total),
#           median_rank = median(rank),
#           mean_rank = mean(rank),
#           highest_rank = min(rank),
#           lowest_rank = max(rank),
#           num_sims = n(),
#           num_times_playoffs = sum(playoffs),
#           prob_playoffs = mean(playoffs) * 100,
#           prob_one_rank = mean(rank == 1) * 100,
#           .groups = "drop"
#         ) %>%
#         arrange(desc(median_expected_total))
#       
#       team_labels <- map_chr(names(fixed_outcomes), ~ str_extract(.x, "\\w+$"))
#       label_outcomes <- paste0(team_labels, "_", fixed_outcomes)
#       filename <- paste0("whatif_outputs/scenarios_final-", paste(label_outcomes, collapse = "-"), ".rds")
#       
#       write_rds(scenarios_final, filename)
#     }
#   }
# }
# toc(log = FALSE)


####

## Arrow option instead #####################################################
# library(tidyverse)
# library(rlang)
# library(gtools)
# library(arrow)
# 
# tic()
# 
# # Inputs
# teams_left <- outcome_not_determined_teams
# 
# # Ensure output directory
# dir.create("scenarios_parquet", showWarnings = FALSE)
# 
# # Helper: get all OVER/UNDER combinations for a team subset
# enumerate_outcomes <- function(team_subset) {
#   expand_grid(
#     !!!set_names(rep(list(c("OVER", "UNDER")), length(team_subset)), team_subset)
#   )
# }
# 
# # Loop through team subsets
# for (subset_size in 1:length(teams_left)) {
#   team_subsets <- combn(teams_left, subset_size, simplify = FALSE)
#   
#   for (team_subset in team_subsets) {
#     outcome_combos <- enumerate_outcomes(team_subset)
#     
#     for (i in 1:nrow(outcome_combos)) {
#       fixed_outcomes <- as.list(outcome_combos[i, ])
#       names(fixed_outcomes) <- team_subset
#       
#       # Scenario ID for partition folder: e.g. Celtics_OVER-Nuggets_UNDER
#       team_labels <- map_chr(names(fixed_outcomes), ~ str_extract(.x, "\\w+$"))
#       label_outcomes <- paste0(team_labels, "_", fixed_outcomes)
#       scenario_id <- paste(label_outcomes, collapse = "-")
#       
#       # Filter sims for scenario
#       filter_exprs <- map2(team_subset, fixed_outcomes, ~ expr(!!sym(.x) == !!.y))
#       combined_expr <- reduce(filter_exprs, ~ expr((!!.x) & (!!.y)))
#       
#       sims_to_keep <- possible_combinations %>%
#         filter(!!combined_expr) %>%
#         pull(sim)
#       
#       if (length(sims_to_keep) == 0) next
#       
#       df <- populated_sum_long %>%
#         filter(sim %in% sims_to_keep)
#       
#       if (nrow(df) == 0) next
#       
#       scenarios <- df %>%
#         group_by(sim) %>%
#         arrange(desc(total), desc(number_correct), desc(number_15_correct), .by_group = TRUE) %>%
#         ungroup() %>%
#         rownames_to_column(var = "rank") %>%
#         mutate(rank = as.numeric(rank) %% num_players, .before = player) %>%
#         mutate(rank = if_else(rank == 0, num_players, rank)) %>%
#         mutate(playoffs = rank <= 4) %>%
#         select(sim, everything())
#       
#       scenarios_final <- scenarios %>%
#         group_by(player) %>%
#         summarize(
#           median_expected_total = median(total),
#           mean_expected_total = mean(total),
#           sd_expected_total = sd(total),
#           median_rank = median(rank),
#           mean_rank = mean(rank),
#           highest_rank = min(rank),
#           lowest_rank = max(rank),
#           num_sims = n(),
#           num_times_playoffs = sum(playoffs),
#           prob_playoffs = mean(playoffs) * 100,
#           prob_one_rank = mean(rank == 1) * 100,
#           .groups = "drop"
#         ) %>%
#         arrange(desc(median_expected_total)) %>%
#         mutate(scenario = scenario_id)  # add for partitioning
#       
#       # Write to Parquet (partitioned by scenario)
#       arrow::write_dataset(
#         scenarios_final,
#         path = "scenarios_parquet",
#         partitioning = "scenario",
#         format = "parquet",
#         existing_data_behavior = "delete_matching"
#       )
#     }
#   }
# }
# 
# toc(log = FALSE)



###########################################################################

# Parallelized (takes a long time to create scenario_rows)
# Not sure how long but it's been running for 10 minutes

# library(tidyverse)
# library(rlang)
# library(gtools)
# library(furrr)
# library(future)
# 
# plan(multisession, workers = parallel::detectCores() - 1)
# 
# # Inputs
# teams_left <- outcome_not_determined_teams
# num_players <- 8
# 
# dir.create("whatif_outputs", showWarnings = FALSE)
# 
# # Helper: expand all OVER/UNDER combinations for any subset
# enumerate_outcomes <- function(team_subset) {
#   expand_grid(
#     !!!set_names(rep(list(c("OVER", "UNDER")), length(team_subset)), team_subset)
#   )
# }
# 
# # Step 1: Generate the power set of scenarios
# scenario_df <- map_dfr(1:length(teams_left), function(k) {
#   combn(teams_left, k, simplify = FALSE) %>%
#     map_dfr(enumerate_outcomes)
# })
# 
# # Step 2: Convert to list of rows for parallel processing
# scenario_rows <- scenario_df %>%
#   mutate(row_id = row_number()) %>%
#   split(.$row_id) %>%
#   map(~ select(.x, -row_id))
# 
# process_scenario <- function(scenario_row) {
#   fixed_outcomes <- scenario_row %>%
#     compact() %>%
#     select(where(~ all(!is.na(.)))) %>%
#     as.list()
#   
#   if (length(fixed_outcomes) == 0) return(NULL)
#   
#   team_subset <- names(fixed_outcomes)
#   
#   filter_exprs <- map2(team_subset, fixed_outcomes, ~ expr(!!sym(.x) == !!.y))
#   combined_expr <- reduce(filter_exprs, ~ expr((!!.x) & (!!.y)))
#   
#   sims_to_keep <- possible_combinations %>%
#     filter(!!combined_expr) %>%
#     pull(sim)
#   
#   if (length(sims_to_keep) == 0) return(NULL)
#   
#   df <- populated_sum_long %>%
#     filter(sim %in% sims_to_keep)
#   
#   if (nrow(df) == 0) return(NULL)
#   
#   scenarios <- df %>%
#     group_by(sim) %>%
#     arrange(desc(total), desc(number_correct), desc(number_15_correct), .by_group = TRUE) %>%
#     ungroup() %>%
#     rownames_to_column(var = "rank") %>%
#     mutate(rank = as.numeric(rank) %% num_players, .before = player) %>%
#     mutate(rank = if_else(rank == 0, num_players, rank)) %>%
#     mutate(playoffs = rank <= 4) %>%
#     select(sim, everything())
#   
#   scenarios_final <- scenarios %>%
#     group_by(player) %>%
#     summarize(
#       median_expected_total = median(total),
#       mean_expected_total = mean(total),
#       sd_expected_total = sd(total),
#       median_rank = median(rank),
#       mean_rank = mean(rank),
#       highest_rank = min(rank),
#       lowest_rank = max(rank),
#       num_sims = n(),
#       num_times_playoffs = sum(playoffs),
#       prob_playoffs = mean(playoffs) * 100,
#       prob_one_rank = mean(rank == 1) * 100,
#       .groups = "drop"
#     ) %>%
#     arrange(desc(median_expected_total))
#   
#   team_labels <- map_chr(names(fixed_outcomes), ~ str_extract(.x, "\\w+$"))
#   label_outcomes <- paste0(team_labels, "_", fixed_outcomes)
#   filename <- paste0("whatif_outputs/scenarios_final-", paste(label_outcomes, collapse = "-"), ".rds")
#   
#   write_rds(scenarios_final, filename)
#   
#   return(filename)
# }
# 
# future_map(scenario_rows[1:3], process_scenario, .progress = TRUE)
