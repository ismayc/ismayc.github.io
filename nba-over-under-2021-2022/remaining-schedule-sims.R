# library(tidyverse)
#
# schedule <- nbastatR::current_schedule() %>% 
#   filter(
#     dateGame >= "2020-12-22", 
#     dateGame <= "2021-05-16",
#     # All-Star Game
#     dateGame != "2021-03-07"
#   ) %>% 
#   select(game_date = dateGame,
#          game_id = idGame,
#          slug_away_team = slugTeamAway,
#          away_team = nameTeamAway,
#          slug_home_team = slugTeamHome,
#          home_team = nameTeamHome,
#          start_time = hasBuzzerBeater
#   ) %>% 
#   mutate(is_complete = (game_date < Sys.Date()))
# 
# remaining_games <- schedule %>% 
#   filter(!is_complete) %>% 
#   select(-is_complete)

# Go over remaining outcomes
library(tidyverse)
library(readxl)
library(furrr)
future::plan(multisession)

picks_wide <- readr::read_rds("picks_wide.rds")
names(picks_wide) <- stringr::str_replace_all(names(picks_wide), "_points", "")
picks_wide <- picks_wide %>% 
  mutate(expected = "",
         prob = "") %>% 
  mutate(Team = stringr::str_replace_all(Team, "\U2191", ""),
         Team = stringr::str_replace_all(Team, "\U2193", ""),
         Team = stringr::str_trim(Team)) %>% 
  rename(team = Team) %>% 
  arrange(team)
#readr::write_csv(picks_wide, "picks_wide.csv")
#chester_probs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=0&single=true&output=csv"
expected <- readr::read_csv("picks_wide_input.csv")#read_csv(chester_probs)
# expected <- read_csv(phil_probs)

# Simulate
lookup_table <- expected %>% 
  select(team, expected, prob)

determined <- expected %>% 
  select(team, expected, prob) %>% 
  filter(prob == 100) %>% 
  select(-prob) %>% 
  rename(sim = expected)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")

set.seed(NULL)
#start_time <- Sys.time()
temp_list <- list()
outcome_sims <- future_map_dfr(
  1:100, 
  ~ {
    sim_prep <- picks %>% 
      inner_join(lookup_table, by = "team") %>% 
      select(team, expected, prob) %>% 
      distinct() %>% 
      mutate(not_expected = if_else(expected == "OVER", "UNDER", "OVER")) %>% 
      mutate(sim = NA_real_)
    
    for(i in seq_len(nrow(sim_prep))) {
      sim_prep$sim[i] <- sample(
        x = c(sim_prep$expected[i], sim_prep$not_expected[i]), 
        size = 1#,
        #       prob = c(sim_prep$prob[i] / 100, 1 - (sim_prep$prob[i] / 100)))
      )
    }
    
    temp_list[[1]] <- sim_prep %>% 
      bind_rows(determined) %>% 
      select(team, sim_result = sim) %>% 
      inner_join(picks, by = "team") %>% 
      mutate(sim_points = if_else(choice == sim_result, wage, -wage))
    
    sims_tibble <- tibble(num = NA_integer_) %>% 
      mutate(sim_prep = temp_list)
    
    sims_tibble
  }, 
  .progress = TRUE, 
  .options = furrr_options(seed = TRUE)
) 

sims_tbl <- outcome_sims %>% distinct() %>% 
  mutate(num = 1:2^nrow(lookup_table))#%>% 
#  mutate(join = inner_join(.$sim_prep, picks, by = "team"))

# sims <- picks %>% 
#   mutate(join = inner_join(., sims_tbl, by = "team")) %>% 
#   mutate(sim_points = if_else(choice == sim, wage, -wage))

sims <- sims_tbl %>% 
  mutate(out = map(sim_prep, 
                   ~ .x %>% 
                     group_by(player) %>% 
                     summarize(expected_total = sum(sim_points),
                               num_correct = sum(sim_points > 5),
                               num_15_correct = sum(
                                 wage == 15 & sim_points == 15, 
                                 na.rm = TRUE
                               )) %>% 
                     arrange(desc(expected_total),
                             desc(num_correct),
                             desc(num_15_correct)) %>% 
                     rownames_to_column(var = "rank") %>% 
                     mutate(rank = as.numeric(rank), .before = player) %>% 
                     mutate(playoffs = rank <= 4)
  )) %>% 
  mutate(trim_results = map(
    sim_prep,
    ~ .x %>% distinct(team, sim_result) %>% 
      inner_join(lookup_table %>% select(team), by = "team")))


#end_time <- Sys.time()
#end_time - start_time
sims_unnested <- sims %>% 
  unnest(out)

sims_final <- sims_unnested %>%
  group_by(player) %>% 
  summarize(median_expected_total = median(expected_total),
            mean_expected_total = mean(expected_total),
            sd_expected_total = sd(expected_total),
            median_rank = median(rank),
            mean_rank = mean(rank),
            prob_playoffs = mean(playoffs == TRUE) * 100,
            prob_one_rank = mean(rank == 1) * 100) %>% 
  arrange(desc(median_expected_total))

# Jake not in
jake_not_nums <- sims_unnested %>% 
  filter(player == "Jake", playoffs == FALSE) %>% 
  pull(num)

jake_not <- sims_unnested %>% 
  filter(num %in% jake_not_nums)

# Jenelle not in
jenelle_not_nums <- sims_unnested %>% 
  filter(player == "Jenelle", playoffs == FALSE) %>% 
  pull(num)

jenelle_not <- sims_unnested %>% 
  filter(num %in% jenelle_not_nums)

# Adonis in
adonis_nums <- sims_unnested %>% 
  filter(player == "Adonis", playoffs == TRUE) %>% 
  pull(num)

adonis_in <- sims_unnested %>% 
  filter(num %in% adonis_nums)

# Ryan not in
ryan_not_nums <- sims_unnested %>% 
  filter(player == "Ryan", playoffs == FALSE) %>% 
  pull(num)

ryan_not <- sims_unnested %>% 
  filter(num %in% ryan_not_nums)
