library(tictoc)

tic()

library(tidyverse)
library(readxl)
#library(disk.frame)

manual <- TRUE

# Setup disk.frame and allow it to use multiple cores
#setup_disk.frame(workers = 8)
#options(future.globals.maxSize = Inf)

## UNCOMMENT AND RUN TO START SIMULATIONS BUT THEN COMMENT AGAIN
# picks <- readxl::read_excel("picks.xlsx", sheet = "picks")
# picks_wide_points <- picks %>%
#   select(team:wage) %>%
#   pivot_wider(names_from = player,
#               values_from = wage)
# picks_wide_choice <- picks %>%
#   select(team, player, choice) %>%
#   pivot_wider(names_from = player,
#               values_from = choice)
# points_names <- names(picks_wide_points)[names(picks_wide_points) != "team"]
# choice_names <- names(picks_wide_choice)[names(picks_wide_choice) != "team"]
# 
# points_names <- paste0(points_names, "_points")
# choice_names <- paste0(stringr::str_to_lower(choice_names), "_choice")
# 
# names(picks_wide_points) <- c("team", points_names)
# names(picks_wide_choice) <- c("team", choice_names)
# 
# picks_wide_new <- picks_wide_points %>%
#   inner_join(picks_wide_choice)
# 
# picks_wide_new <- picks_wide_new[ , order(names(picks_wide_new))] %>%
#   relocate(team, everything())
# 
# readr::write_rds(
#   picks_wide_new, "picks_wide_new.rds")

num_players <- 8

picks_wide_new <- readr::read_rds("picks_wide_new.rds") %>% 
  rename(Team = team)

# Assign the column names to each remaining NBA team
# using things computed in the make_plots.Rmd file
determined_so_far <- read_rds(
  paste0("determined_outcomes_", Sys.Date(), ".rds")
)

if (manual) {
# determined_so_far[determined_so_far$Team == "Indiana Pacers", "Outcome Determined"] <- "OVER"
# determined_so_far[determined_so_far$Team == "Boston Celtics", "Outcome Determined"] <- "OVER"
# determined_so_far[determined_so_far$Team == "Toronto Raptors", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Brooklyn Nets", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "New Orleans Pelicans", "Outcome Determined"] <- "OVER"
#  determined_so_far[determined_so_far$Team == "Phoenix Suns", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Philadelphia 76ers", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Los Angeles Clippers", "Outcome Determined"] <- "OVER"
#  determined_so_far[determined_so_far$Team == "Dallas Mavericks", "Outcome Determined"] <- "OVER"

  #  determined_so_far[determined_so_far$Team == "Golden State Warriors", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Los Angeles Lakers", "Outcome Determined"] <- "OVER"
#  determined_so_far[determined_so_far$Team == "New York Knicks", "Outcome Determined"] <- "OVER"
#  determined_so_far[determined_so_far$Team == "Denver Nuggets", "Outcome Determined"] <- "OVER"
  #  determined_so_far[determined_so_far$Team == "Atlanta Hawks", "Outcome Determined"] <- "UNDER"
  #  determined_so_far[determined_so_far$Team == "Cleveland Cavaliers", "Outcome Determined"] <- "UNDER"
  
#  determined_so_far[determined_so_far$Team == "Portland Trail Blazers", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Miami Heat", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Chicago Bulls", "Outcome Determined"] <- "OVER"
  determined_so_far[determined_so_far$Team == "Los Angeles Lakers", "Outcome Determined"] <- "UNDER"
#  determined_so_far[determined_so_far$Team == "Sacramento Kings", "Outcome Determined"] <- "OVER"
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