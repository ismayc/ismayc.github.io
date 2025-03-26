library(tictoc)

tic()

library(tidyverse)
library(readxl)

#manual <- TRUE
manual <- FALSE

## UNCOMMENT AND RUN TO START SIMULATIONS BUT THEN COMMENT AGAIN
#source("expand-grid-database-prep.R")

num_players <- 8

picks_wide_new <- readr::read_rds("../picks_wide_new.rds") %>% 
  rename(Team = team)

# Assign the column names to each remaining NBA team
# using things computed in the make_plots.Rmd file
determined_so_far <- read_rds(
  paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
)

if (manual) {
  # determined_so_far[determined_so_far$Team == "Minnesota Timberwolves", "Outcome Determined"] <- "UNDER"
  # determined_so_far[determined_so_far$Team == "Miami Heat", "Outcome Determined"] <- "UNDER"
  # determined_so_far[determined_so_far$Team == "Milwaukee Bucks", "Outcome Determined"] <- "UNDER"
  # determined_so_far[determined_so_far$Team == "Houston Rockets", "Outcome Determined"] <- "OVER"
  
  # Very confident
  determined_so_far <- determined_so_far %>%
    mutate(`Outcome Determined` = case_when(
      Team %in% c("Charlotte Hornets") ~ "UNDER",
      Team %in% c("Memphis Grizzlies", "Atlanta Hawks") ~ "OVER",
      TRUE ~ `Outcome Determined`
    ))
  
  # Confident
  determined_so_far <- determined_so_far %>%
    mutate(`Outcome Determined` = case_when(
      Team %in% c("Washington Wizards") ~ "UNDER",
      Team %in% c("Los Angeles Lakers", 
                  "Golden State Warriors") ~ "OVER",
      TRUE ~ `Outcome Determined`
    ))
  
  # So so
  determined_so_far <- determined_so_far %>%
    mutate(`Outcome Determined` = case_when(
      Team %in% c("Toronto Raptors") ~ "UNDER",
      Team %in% c("Los Angeles Clippers") ~ "OVER",
      TRUE ~ `Outcome Determined`
    ))
  # 5 teams left
  # "Denver Nuggets"
  # "Indiana Pacers"
  # "New York Knicks"
  # "Boston Celtics"
  # "San Antonio Spurs"
}

teams <- determined_so_far %>% 
  arrange(Team) %>% 
  pull(Team)

outcome_not_determined_teams <- determined_so_far %>% 
  filter(`Outcome Determined` == "not yet") %>% 
  arrange(Team) %>% 
  pull(Team)

determined_teams <- determined_so_far %>% 
  filter(`Outcome Determined` != "not yet") %>% 
  select(Team, `Outcome Determined`)

determined_teams_wide <- determined_teams %>% 
  pivot_wider(names_from = Team, values_from = `Outcome Determined`)

testing <- FALSE
if (testing) {
  outcome_not_determined_teams <- outcome_not_determined_teams[
    !(outcome_not_determined_teams %in% c(
      #    "San Antonio Spurs",
      #    "Washington Wizards"#,
    ))]
}

num_not_determined <- length(outcome_not_determined_teams)
num_determined <- 30 - num_not_determined

# Assuming possible_combinations_out is a data frame or tibble
# First, create the new names vector, starting with "sim" and then the team names
new_names <- c("sim", outcome_not_determined_teams)

# Generate all possible results of flipping coins
possible_combinations_out <- expand_grid(
  !!!replicate(n = num_not_determined, 
               expr = c("UNDER", "OVER"), 
               simplify = FALSE)) %>% 
  rownames_to_column() 

# Now, create a named vector for renaming, where names are current and values are new names
# Since the first name is "rowname" and should be mapped to "sim", we start with that
names_map <- setNames(new_names, names(possible_combinations_out))

possible_combinations_out <- possible_combinations_out |> 
  rename_with(~ names_map[.], everything())

possible_combinations <- cbind(possible_combinations_out, determined_teams_wide)

arrow::write_parquet(possible_combinations, "possible_combinations.parquet")

# Open the dataset
dataset <- arrow::open_dataset("possible_combinations.parquet")

# Column names to be reshaped, excluding 'sim'
team_columns <- names(possible_combinations)[!names(possible_combinations) %in% "sim"]

# # Function to process and write one column
# process_and_write_column <- function(column_name, dataset, output_file) {
#   # Select 'sim' and the current team column
#   temp_df <- dataset %>%
#     select(sim, !!column_name) %>%
#     collect() %>%
#     mutate(Team = column_name, outcome = get(column_name)) %>%
#     select(sim, Team, outcome)
#   
#   # Write/Append the data to a Parquet file
#   arrow::write_dataset(temp_df, output_file, format = "parquet", append = TRUE)
# }
# 
# # Loop over each team column and process
# for (team_col in team_columns) {
#   process_and_write_column(team_col, dataset, "long_outcomes.parquet")
# }

# Function to process one column and return the transformed chunk
process_column <- function(column_name, dataset) {
  # Select 'sim' and the current team column
  temp_df <- dataset %>%
    select(sim, !!column_name) %>%
    collect() %>%
    mutate(Team = column_name, outcome = get(column_name)) %>%
    select(sim, Team, outcome)
  
  return(temp_df)
}

# Initialize a list to store all chunks
all_chunks <- list()

# Loop over each team column, process it, and store the chunk in the list
for (team_col in team_columns) {
  chunk <- process_column(team_col, dataset)
  all_chunks[[team_col]] <- chunk
}

#readr::write_rds(all_chunks, "all_chunks.rds")

# Function to process each chunk
process_chunk <- function(chunk, picks_wide_new, con) {
  processed_chunk <- chunk %>%
    inner_join(picks_wide_new, by = c("Team" = "Team")) %>%
    mutate(sim = as.numeric(sim)) %>%
    
    # Compute the score for each player (repeat for each player as needed)
    mutate(Adonis_proj_points = if_else(outcome == adonis_choice,
                                        Adonis_points,
                                        -Adonis_points)) %>% 
    mutate(Andy_proj_points = if_else(outcome == andy_choice,
                                      Andy_points,
                                      -Andy_points)) %>% 
    mutate(Chester_proj_points = if_else(outcome == chester_choice,
                                         Chester_points,
                                         -Chester_points)) %>% 
    mutate(Jake_proj_points = if_else(outcome == jake_choice,
                                      Jake_points,
                                      -Jake_points)) %>% 
    # mutate(Jenelle_proj_points = if_else(outcome == jenelle_choice,
    #                                      Jenelle_points,
    #                                      -Jenelle_points)) %>% 
    mutate(Mary_proj_points = if_else(outcome == mary_choice,
                                      Mary_points,
                                      -Mary_points)) %>% 
    mutate(Mike_proj_points = if_else(outcome == mike_choice,
                                      Mike_points,
                                      -Mike_points)) %>% 
    mutate(Phil_proj_points = if_else(outcome == phil_choice,
                                      Phil_points,
                                      -Phil_points)) %>% 
    mutate(Ryan_proj_points = if_else(outcome == ryan_choice,
                                      Ryan_points,
                                      -Ryan_points)) #%>% 
  
  # Write the processed chunk to the SQLite database
  dbWriteTable(con, "populated", processed_chunk, 
               append = TRUE, row.names = FALSE)
  
}

#picks_wide_new <- readr::read_rds("picks_wide_new.rds")
#all_chunks <- readr::read_rds("all_chunks.rds")

library(DBI)
library(RSQLite)

# Create a new SQLite database or open a connection to an existing one
con <- dbConnect(RSQLite::SQLite(), dbname = "nba_scenarios.sqlite")

# If the table already exists and you want to overwrite or append, consider dropping it first
dbExecute(con, "DROP TABLE IF EXISTS populated")

for (i in 1:length(all_chunks)) {
  process_chunk(all_chunks[[i]], picks_wide_new, con)
}

# Old way
long_outcomes <- possible_combinations |> 
  pivot_longer(cols = -sim, names_to = "Team", values_to = "outcome")

# Compute the score for each player (adjust with your specific logic)
# Note: The operations will be executed when the data is collected or explicitly evaluated

# When you need to collect the results, for example, to view or save them, use:
# collected_results <- collect(populated)

# If the result is too large to fit into memory, consider summarizing it in chunks,
# or saving the disk.frame to disk and analyzing it in sections.

# Join with 'picks_wide_new', assuming 'picks_wide_new' can fit into memory
# Enter each player pick in a column next to these (OVER/UNDER)
# Enter each player wager in a column next to these (Could get from picks_wide)
# populated <- long_outcomes %>% 
#   inner_join(picks_wide_new) %>% 
#   mutate(sim = as.numeric(sim))



# Compute the score for each player
# populated <- populated %>% 
#   mutate(Adonis_proj_points = if_else(outcome == adonis_choice,
#                                       Adonis_points,
#                                       -Adonis_points)) %>% 
#   mutate(Andy_proj_points = if_else(outcome == andy_choice,
#                                     Andy_points,
#                                     -Andy_points)) %>% 
#   mutate(Chester_proj_points = if_else(outcome == chester_choice,
#                                        Chester_points,
#                                        -Chester_points)) %>% 
#   mutate(Jake_proj_points = if_else(outcome == jake_choice,
#                                     Jake_points,
#                                     -Jake_points)) %>% 
#   # mutate(Jenelle_proj_points = if_else(outcome == jenelle_choice,
#   #                                      Jenelle_points,
#   #                                      -Jenelle_points)) %>% 
#   mutate(Mary_proj_points = if_else(outcome == mary_choice,
#                                     Mary_points,
#                                     -Mary_points)) %>% 
#   mutate(Mike_proj_points = if_else(outcome == mike_choice,
#                                     Mike_points,
#                                     -Mike_points)) %>% 
#   mutate(Phil_proj_points = if_else(outcome == phil_choice,
#                                     Phil_points,
#                                     -Phil_points)) %>% 
#   mutate(Ryan_proj_points = if_else(outcome == ryan_choice,
#                                     Ryan_points,
#                                     -Ryan_points))
# old `populated` created up through here

library(dbplyr)

# Reference the 'populated' table from the SQLite database
populated <- tbl(con, "populated")

# Perform the operation using dplyr syntax
populated_summarized_tbl <- populated %>%
  group_by(sim) %>%
  summarize(
    Adonis = sum(Adonis_proj_points, na.rm = TRUE),
    Andy = sum(Andy_proj_points, na.rm = TRUE),
    Chester = sum(Chester_proj_points, na.rm = TRUE),
    Jake = sum(Jake_proj_points, na.rm = TRUE),
    # Jenelle = sum(Jenelle_proj_points, na.rm = TRUE),
    Mary = sum(Mary_proj_points, na.rm = TRUE),
    Mike = sum(Mike_proj_points, na.rm = TRUE),
    Phil = sum(Phil_proj_points, na.rm = TRUE),
    Ryan = sum(Ryan_proj_points, na.rm = TRUE)
  ) %>%
  arrange(sim)

# Retrieve the result (if it's of manageable size)
populated_summarized <- collect(populated_summarized_tbl)

# populated_summarized <- populated %>% 
#   group_by(sim) %>% 
#   summarize(Adonis = sum(Adonis_proj_points),
#             Andy = sum(Andy_proj_points),
#             Chester = sum(Chester_proj_points),
#             Jake = sum(Jake_proj_points),
#             #            Jenelle = sum(Jenelle_proj_points),
#             Mary = sum(Mary_proj_points),
#             Mike = sum(Mike_proj_points),
#             Phil = sum(Phil_proj_points),
#             Ryan = sum(Ryan_proj_points)) %>% 
#   arrange(sim)

populated_num_correct_tbl <- populated %>% 
  group_by(sim) %>% 
  summarize(Adonis_num_correct = sum(Adonis_proj_points > 5),
            Andy_num_correct = sum(Andy_proj_points > 5),
            Chester_num_correct = sum(Chester_proj_points > 5),
            Jake_num_correct = sum(Jake_proj_points > 5),
            # Jenelle_num_correct = sum(Jenelle_proj_points > 5),
            Mary_num_correct = sum(Mary_proj_points > 5),
            Mike_num_correct = sum(Mike_proj_points > 5),
            Phil_num_correct = sum(Phil_proj_points > 5),
            Ryan_num_correct = sum(Ryan_proj_points > 5)) %>% 
  arrange(sim)

# Retrieve the result (if it's of manageable size)
populated_num_correct <- collect(populated_num_correct_tbl)

populated_15_correct_tbl <- populated %>% 
  group_by(sim) %>% 
  summarize(Adonis_15_correct = sum(Adonis_proj_points == 15),
            Andy_15_correct = sum(Andy_proj_points == 15),
            Chester_15_correct = sum(Chester_proj_points == 15),
            Jake_15_correct = sum(Jake_proj_points == 15),
            # Jenelle_15_correct = sum(Jenelle_proj_points == 15),
            Mary_15_correct = sum(Mary_proj_points == 15),
            Mike_15_correct = sum(Mike_proj_points == 15),
            Phil_15_correct = sum(Phil_proj_points == 15),
            Ryan_15_correct = sum(Ryan_proj_points == 15)) %>% 
  arrange(sim)

# Retrieve the result (if it's of manageable size)
populated_15_correct <- collect(populated_15_correct_tbl)

populated_summarized_long <- populated_summarized %>% 
  pivot_longer(-sim, names_to = "player", values_to = "total")

populated_num_correct_long <- populated_num_correct %>% 
  pivot_longer(-sim, names_to = "player", values_to = "number_correct") %>% 
  mutate(player = str_replace_all(player, "_num_correct", ""))

populated_15_correct_long <- populated_15_correct %>% 
  pivot_longer(-sim, names_to = "player", values_to = "number_15_correct") %>% 
  mutate(player = str_replace_all(player, "_15_correct", ""))

populated_sum_long <- populated_summarized_long %>% 
  left_join(populated_num_correct_long, by = c("sim", "player")) %>% 
  left_join(populated_15_correct_long, by = c("sim", "player")) 

scenarios <- populated_sum_long %>% 
  #  slice(1:(9*10)) %>% 
  group_by(sim) %>% 
  arrange(desc(total),  
          desc(number_correct),
          desc(number_15_correct), .by_group = TRUE) %>% 
  ungroup() %>% 
  rownames_to_column(var = "rank") %>% 
  mutate(rank = as.numeric(rank) %% num_players, .before = player)  %>% 
  mutate(rank = if_else(rank == 0, num_players, rank)) %>% 
  mutate(playoffs = rank <= 4) %>% 
  select(sim, everything())

scenarios_final <- scenarios %>%
  group_by(player) %>% 
  summarize(median_expected_total = median(total),
            mean_expected_total = mean(total),
            sd_expected_total = sd(total),
            median_rank = median(rank),
            mean_rank = mean(rank),
            highest_rank = min(rank),
            lowest_rank = max(rank),
            num_sims = n(),
            num_times_playoffs = sum(playoffs == TRUE),
            prob_playoffs = mean(playoffs == TRUE) * 100,
            prob_one_rank = mean(rank == 1) * 100) %>% 
  arrange(desc(median_expected_total))

View(scenarios_final)

toc(log = FALSE)
# 19 teams - 279.734 sec elapsed
# dbDisconnect(con)

## Running through all scenarios
tic()
library(tidyverse)
library(rlang)
library(gtools)

# Inputs
teams_left <- outcome_not_determined_teams

# Ensure output directory
dir.create("whatif_outputs", showWarnings = FALSE)

# Function to get all OVER/UNDER combinations for a given team subset
enumerate_outcomes <- function(team_subset) {
  expand_grid(
    !!!set_names(rep(list(c("OVER", "UNDER")), length(team_subset)), team_subset)
  )
}

# Loop through subset sizes (1 to 13 teams fixed)
for (subset_size in 1:length(teams_left)) {
  team_subsets <- combn(teams_left, subset_size, simplify = FALSE)
  
  for (team_subset in team_subsets) {
    outcome_combos <- enumerate_outcomes(team_subset)
    
    for (i in 1:nrow(outcome_combos)) {
      fixed_outcomes <- as.list(outcome_combos[i, ])
      names(fixed_outcomes) <- team_subset
      
      filter_exprs <- map2(team_subset, fixed_outcomes, ~ expr(!!sym(.x) == !!.y))
      combined_expr <- reduce(filter_exprs, ~ expr((!!.x) & (!!.y)))
      
      sims_to_keep <- possible_combinations %>%
        filter(!!combined_expr) %>%
        pull(sim)
      
      if (length(sims_to_keep) == 0) next
      
      df <- populated_sum_long %>%
        filter(sim %in% sims_to_keep)
      
      if (nrow(df) == 0) next
      
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
          median_expected_total = median(total),
          mean_expected_total = mean(total),
          sd_expected_total = sd(total),
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
        arrange(desc(median_expected_total))
      
      team_labels <- map_chr(names(fixed_outcomes), ~ str_extract(.x, "\\w+$"))
      label_outcomes <- paste0(team_labels, "_", fixed_outcomes)
      filename <- paste0("whatif_outputs/scenarios_final-", paste(label_outcomes, collapse = "-"), ".rds")
      
      write_rds(scenarios_final, filename)
    }
  }
}
toc(log = FALSE)



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
message(glue("Total scenario combos to enumerate: {total_scenarios}"))

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
}) %>%
  distinct(scenario_id)

message(glue("\\nDone enumerating. Found {nrow(unique_scenario_ids)} unique scenario_ids of {total_scenarios} combos."))

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
        median_expected_total = median(total),
        mean_expected_total = mean(total),
        sd_expected_total = sd(total),
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

n_workers <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_workers)

n_scenarios <- nrow(df_scenarios)
chunk_size <- ceiling(n_scenarios / n_workers)

scenario_chunks <- split(df_scenarios, ceiling(seq_len(n_scenarios) / chunk_size))
message(glue("Creating {length(scenario_chunks)} chunks for {n_scenarios} scenarios."))

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
