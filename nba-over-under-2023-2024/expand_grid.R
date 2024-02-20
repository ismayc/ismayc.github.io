library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)

#phil_probs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=136453584&single=true&output=csv"
# picks <- readxl::read_excel(
#   here::here("nba-over-under-2022-2023", "picks.xlsx"), 
#   sheet = "picks")
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
#   picks_wide_new, 
#   here::here("nba-over-under-2022-2023", "picks_wide_new.rds"))

num_players <- 9

picks_wide_new <- readr::read_rds(
  here::here("nba-over-under-2022-2023", "picks_wide_new.rds")) %>% 
  rename(Team = team)

# Assign the column names to each remaining NBA team
# using things computed in the make_plots.Rmd file
determined_so_far <- read_rds(
  here::here("nba-over-under-2022-2023", 
             paste0("determined_outcomes_", Sys.Date(), ".rds"))
)

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

# Generate all possible results of flipping  coins
possible_combinations_out <- expand_grid(
  !!!replicate(n = num_not_determined, 
               expr = c("UNDER", "OVER"), 
               simplify = FALSE)) %>% 
  rownames_to_column()

names(possible_combinations_out) <- c("sim", outcome_not_determined_teams)

possible_combinations <- possible_combinations_out %>%                                     
  bind_cols(determined_teams_wide) #%>%
  #  mutate(`Washington Wizards` = "OVER", .after = `Toronto Raptors`) #%>%
  #  mutate(`Washington Wizards` = "UNDER", .after = `Toronto Raptors`)  #29

# determined_teams <- sort(setdiff(teams, outcome_not_determined_teams))

# Might need to make it long then?
long_outcomes <- possible_combinations %>% 
  pivot_longer(cols = -sim, names_to = "Team", values_to = "outcome")

# Enter each player pick in a column next to these (OVER/UNDER)
# Enter each player wager in a column next to these (Could get from picks_wide)
populated <- long_outcomes %>% 
  inner_join(picks_wide_new) %>% 
  mutate(sim = as.numeric(sim))

# Compute the score for each player
populated <- populated %>% 
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
  mutate(Jenelle_proj_points = if_else(outcome == jenelle_choice,
                                       Jenelle_points,
                                       -Jenelle_points)) %>% 
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
                                    -Ryan_points))

populated_summarized <- populated %>% 
  group_by(sim) %>% 
  summarize(Adonis = sum(Adonis_proj_points),
            Andy = sum(Andy_proj_points),
            Chester = sum(Chester_proj_points),
            Jake = sum(Jake_proj_points),
            Jenelle = sum(Jenelle_proj_points),
            Mary = sum(Mary_proj_points),
            Mike = sum(Mike_proj_points),
            Phil = sum(Phil_proj_points),
            Ryan = sum(Ryan_proj_points)) %>% 
  arrange(sim)

populated_num_correct <- populated %>% 
  group_by(sim) %>% 
  summarize(Adonis_num_correct = sum(Adonis_proj_points > 5),
            Andy_num_correct = sum(Andy_proj_points > 5),
            Chester_num_correct = sum(Chester_proj_points > 5),
            Jake_num_correct = sum(Jake_proj_points > 5),
            Jenelle_num_correct = sum(Jenelle_proj_points > 5),
            Mary_num_correct = sum(Mary_proj_points > 5),
            Mike_num_correct = sum(Mike_proj_points > 5),
            Phil_num_correct = sum(Phil_proj_points > 5),
            Ryan_num_correct = sum(Ryan_proj_points > 5)) %>% 
  arrange(sim)

populated_15_correct <- populated %>% 
  group_by(sim) %>% 
  summarize(Adonis_15_correct = sum(Adonis_proj_points == 15),
            Andy_15_correct = sum(Andy_proj_points == 15),
            Chester_15_correct = sum(Chester_proj_points == 15),
            Jake_15_correct = sum(Jake_proj_points == 15),
            Jenelle_15_correct = sum(Jenelle_proj_points == 15),
            Mary_15_correct = sum(Mary_proj_points == 15),
            Mike_15_correct = sum(Mike_proj_points == 15),
            Phil_15_correct = sum(Phil_proj_points == 15),
            Ryan_15_correct = sum(Ryan_proj_points == 15)) %>% 
  arrange(sim)

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

# Play time
phil_1_sims <- scenarios %>%
  filter(player == "Phil") %>%
  filter(playoffs == TRUE, rank == 1) %>%
  pull(sim)

phil_1_scenarios <- populated %>%
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

phil_playoff_scenarios <- populated %>%
  filter(sim %in% phil_playoff_sims) %>%
  select(sim, Team, outcome, Phil_points, Phil_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

phil_nonplayoff_scenarios <- populated %>%
  filter(!(sim %in% phil_playoff_sims)) %>%
  select(sim, Team, outcome, Phil_points, Phil_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

phil_needs <- phil_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

adonis_playoff_sims <- scenarios %>%
  filter(player == "Adonis") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

adonis_playoff_scenarios <- populated %>%
  filter(sim %in% adonis_playoff_sims) %>%
  select(sim, Team, outcome, Adonis_points, Adonis_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

adonis_needs <- adonis_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

chester_playoff_sims <- scenarios %>%
  filter(player == "Chester") %>%
  filter(playoffs == TRUE) %>%
  pull(sim)

chester_playoff_scenarios <- populated %>%
  filter(sim %in% chester_playoff_sims) %>%
  select(sim, Team, outcome, Chester_proj_points) %>%
  filter(Team %in% outcome_not_determined_teams)

chester_needs <- chester_playoff_scenarios %>%
  select(-sim) %>%
  distinct() %>%
  arrange(Team)

View(scenarios_final)