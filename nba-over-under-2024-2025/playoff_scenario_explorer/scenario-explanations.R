library(DBI)
library(dbplyr)
library(tidyverse)

teams_remaining <- num_not_determined

# Create a new SQLite database or open a connection to an existing one
con <- dbConnect(RSQLite::SQLite(), dbname = "nba_scenarios.sqlite")

# Figure out how many scenarios there are including each of the OVER/UNDER
# options for each team for each player

scenarios <- read_rds("scenarios.rds")

# Any ties for 4th and 5th positions?
# Select rows where rank is 4 or rank is 5 and the total column is the same
# scenarios %>%
#   filter(rank %in% c(4, 5)) %>%
#   group_by(sim) %>%
#   filter(n_distinct(total) == 1)

# Reference the 'populated' table from the SQLite database
populated <- tbl(con, "populated")
populated_collected <- collect(populated)

#Play time
phil_1_sims <- scenarios %>%
  filter(player == "Phil") %>%
  filter(playoffs == TRUE, rank == 1) %>%
  pull(sim)

phil_1_scenarios <- populated_collected %>%
  filter(sim %in% phil_1_sims) %>%
  select(sim, Team, outcome, Phil_points, Phil_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

phil_needs_for_1 <- phil_1_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

phil_playoff_sims <- scenarios %>%
  filter(player == "Phil") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

phil_playoff_scenarios <- populated_collected %>%
  filter(sim %in% phil_playoff_sims) %>%
  select(sim, Team, outcome, Phil_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

phil_nonplayoff_scenarios <- populated_collected %>%
  filter(!(sim %in% phil_playoff_sims)) %>%
  select(sim, Team, outcome, Phil_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

phil_needs <- phil_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

phil_outcome_percentages <- phil_playoff_scenarios %>%
  group_by(Team, outcome) %>%
  summarise(outcome_count = n(), .groups = 'drop') %>%
  mutate(outcome_percentage = (outcome_count / sum(outcome_count)) * 100) %>%
  arrange(Team, desc(outcome_percentage))

phil_outcome1_percentages <- phil_1_scenarios %>%
  group_by(Team, outcome) %>%
  summarise(outcome_count = n(), .groups = 'drop') %>%
  mutate(outcome_percentage = (outcome_count / sum(outcome_count)) * 100) %>%
  arrange(Team, desc(outcome_percentage))

# Adonis
adonis_playoff_sims <- scenarios %>%
  filter(player == "Adonis") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

adonis_playoff_scenarios <- populated_collected %>%
  filter(sim %in% adonis_playoff_sims) %>%
  select(sim, Team, outcome, Adonis_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

adonis_needs <- adonis_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Chester
chester_playoff_sims <- scenarios %>%
  filter(player == "Chester") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

chester_playoff_scenarios <- populated_collected %>%
  filter(sim %in% chester_playoff_sims) %>%
  select(sim, Team, outcome, Chester_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

chester_nonplayoff_scenarios <- populated_collected %>%
  filter(!(sim %in% chester_playoff_sims)) %>%
  select(sim, Team, outcome, Chester_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

chester_needs <- chester_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

chester_1_sims <- scenarios %>%
  filter(player == "Chester") %>%
  filter(playoffs == TRUE, rank == 1) %>%
  pull(sim)

chester_1_scenarios <- populated_collected %>%
  filter(sim %in% chester_1_sims) %>%
  select(sim, Team, outcome, Chester_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

chester_needs_for_1 <- chester_1_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Calculate the percentage of each outcome in the playoff scenarios
chester_outcome_percentages <- chester_playoff_scenarios %>%
  group_by(Team, outcome) %>%
  summarise(outcome_count = n(), .groups = 'drop') %>%
  mutate(outcome_percentage = (outcome_count / sum(outcome_count)) * 100) %>%
  arrange(Team, desc(outcome_percentage))

chester_outcome1_percentages <- chester_1_scenarios %>%
  group_by(Team, outcome) %>%
  summarise(outcome_count = n(), .groups = 'drop') %>%
  mutate(outcome_percentage = (outcome_count / sum(outcome_count)) * 100) %>%
  arrange(Team, desc(outcome_percentage))

# View the percentages of each outcome 
View(chester_outcome_percentages)
View(chester_outcome1_percentages)

# Andy
andy_playoff_sims <- scenarios %>%
  filter(player == "Andy") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

andy_playoff_scenarios <- populated_collected %>%
  filter(sim %in% andy_playoff_sims) %>%
  select(sim, Team, outcome, Andy_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

andy_needs <- andy_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Mary
mary_playoff_sims <- scenarios %>%
  filter(player == "Mary") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

mary_playoff_scenarios <- populated_collected %>%
  filter(sim %in% mary_playoff_sims) %>%
  select(sim, Team, outcome, Mary_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

mary_needs <- mary_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Mike
mike_playoff_sims <- scenarios %>%
  filter(player == "Mike") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

mike_playoff_scenarios <- populated_collected %>%
  filter(sim %in% mike_playoff_sims) %>%
  select(sim, Team, outcome, Mike_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

mike_needs <- mike_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Ryan
ryan_playoff_sims <- scenarios %>%
  filter(player == "Ryan") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

ryan_playoff_scenarios <- populated_collected %>%
  filter(sim %in% ryan_playoff_sims) %>%
  select(sim, Team, outcome, Ryan_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

ryan_needs <- ryan_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Jake
jake_playoff_sims <- scenarios %>%
  filter(player == "Jake") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

jake_playoff_scenarios <- populated_collected %>%
  filter(sim %in% jake_playoff_sims) %>%
  select(sim, Team, outcome, Jake_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

jake_needs <- jake_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

# Calculate the percentage of each outcome in the playoff scenarios
jake_outcome_percentages <- jake_playoff_scenarios %>%
  group_by(Team, outcome) %>%
  summarise(outcome_count = n(), .groups = 'drop') %>%
  mutate(outcome_percentage = (outcome_count / sum(outcome_count)) * 100) %>%
  select(-outcome_count) %>%
  arrange(Team, desc(outcome_percentage))

# View the percentages of each outcome 
# print(jake_outcome_percentages)

print(all(nrow(chester_needs) == 2 * teams_remaining, 
    nrow(andy_needs) == 2 * teams_remaining, 
    nrow(mary_needs) == 2 * teams_remaining, 
    nrow(mike_needs) == 2 * teams_remaining, 
    nrow(ryan_needs) == 2 * teams_remaining, 
    nrow(jake_needs) == 2 * teams_remaining, 
    nrow(phil_needs) == 2 * teams_remaining, 
    nrow(adonis_needs) == 2 * teams_remaining))

library(dplyr)

# Assuming your dataframe is called scenarios
result <- scenarios %>%
  filter(playoffs == TRUE) %>% # Step 1: Filter for playoffs == TRUE
  group_by(sim) %>% # Step 2: Group by simulation
  summarise(players_in_playoffs = toString(player)) %>% # Collect players in playoffs per simulation
filter(grepl("Andy", players_in_playoffs) &
         grepl("Chester", players_in_playoffs) &
         grepl("Mary", players_in_playoffs) &
         grepl("Adonis", players_in_playoffs))  %>% # Step 3: Check for all desired players
 nrow() # Step 4: Count the simulations

print(result)


dbDisconnect(con)

# con <- dbConnect(RSQLite::SQLite(), dbname = "nba_scenarios.sqlite")