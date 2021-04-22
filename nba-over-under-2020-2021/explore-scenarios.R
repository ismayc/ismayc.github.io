library(tidyverse)
library(readxl)
library(furrr)
future::plan(multisession)

num_sims <- 200000

phil_probs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=136453584&single=true&output=csv"
chester_probs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=0&single=true&output=csv"

chester_probs_df <- picks %>% 
  distinct(team) %>% 
  arrange(team) %>% 
  mutate(expected = "OVER") %>% 
  mutate(prob = case_when(
    str_detect(team, "Knicks") ~ 100,
    str_detect(team, "Rockets") ~ 0,
    str_detect(team, "Celtics") ~ 0,
    str_detect(team, "Raptors") ~ 0,
    str_detect(team, "Hornets") ~ 100,
    str_detect(team, "Jazz") ~ 100,
    str_detect(team, "Suns") ~ 100,
    str_detect(team, "Bucks") ~ 0,
    str_detect(team, "Heat") ~ 0,
    str_detect(team, "wolves") ~ 1,
    str_detect(team, "Magic") ~ 1,
    str_detect(team, "Spurs") ~ 99,
    str_detect(team, "Pacers") ~ 1,
    str_detect(team, "76ers") ~ 90,
    str_detect(team, "Lakers") ~ 10,  
    str_detect(team, "Thunder") ~ 90,
    str_detect(team, "Pelicans") ~ 15,
    str_detect(team, "Grizzlies") ~ 90,
    str_detect(team, "Pistons") ~ 10, #20,
    
    str_detect(team, "Clippers") ~ 70,
    str_detect(team, "Blazers") ~ 50,
    str_detect(team, "Nets") ~ 80,
    str_detect(team, "Cavaliers") ~ 80,
    str_detect(team, "Hawks") ~ 75,
    str_detect(team, "Bulls") ~ 75, #60,
    str_detect(team, "Kings") ~ 75, #60,
    str_detect(team, "Nuggets") ~ 40, #45,
    str_detect(team, "Mavericks") ~ 5, #55,
    str_detect(team, "Warriors") ~ 5, #25,
    str_detect(team, "Wizards") ~ 55
  )) %>% 
  mutate(likely_result = case_when(
    prob >= 90 ~ "OVER",
    prob <= 15 ~ "UNDER",
    TRUE ~ "TBD"))

prob_list <- as.list(x = chester_probs_df$prob)
names(prob_list) <- chester_probs_df$team

by_five <- function(start, end) {
  seq(start, end, 5)
}

prob_list$`Brooklyn Nets` <- by_five(40, 85)
prob_list$`Los Angeles Clippers` <- by_five(40, 90)
prob_list$`Portland Trail Blazers` <- by_five(40, 80)
prob_list$`Cleveland Cavaliers` <- by_five(55, 90)
prob_list$`Atlanta Hawks` <- by_five(45, 90)
prob_list$`Chicago Bulls` <- by_five(30, 80)
prob_list$`Sacramento Kings` <- by_five(25, 80)
prob_list$`Denver Nuggets` <- by_five(40, 80)
prob_list$`Dallas Mavericks` <- by_five(5, 60)
prob_list$`Golden State Warriors` <- by_five(5, 80)
prob_list$`Washington Wizards` <- by_five(25, 75)

chester_probs_df <- chester_probs_df %>% 
  mutate(prob_list = prob_list)

picks_test <- picks %>% 
  inner_join(chester_probs_df %>% select(team, likely_result), by = "team") %>% 
  mutate(projected_points = case_when(
    (choice == likely_result) & (likely_result != "TBD") ~ wage,
    (choice != likely_result) & (likely_result != "TBD") ~ -wage,
    TRUE ~ NA_real_
  )) 

picks_test  %>% 
  group_by(player) %>% 
  summarize(projected_total = sum(projected_points, na.rm = TRUE)) %>% 
  arrange(desc(projected_total))


# Simulate
expected <- chester_probs_df
#expected <- read_csv(chester_probs)
# expected <- read_csv(phil_probs)

if ("prob_list" %in% names(expected)) {
  lookup_table <- expected %>% 
    select(team, expected, prob_list)
} else {
  lookup_table <- expected %>% 
    select(team, expected, prob)
}

picks <- read_excel(path = "picks.xlsx", sheet = "picks")

set.seed(2021)
start_time <- Sys.time()
outcome_sims <- future_map_dfr(
  1:num_sims, 
  ~ {
    if("prob_list" %in% names(sim_prep)) {
      sim_prep <- picks %>% 
        inner_join(lookup_table, by = "team") %>% 
        select(team, expected, prob_list) %>% 
        distinct() %>% 
        mutate(not_expected = if_else(expected == "OVER", "UNDER", "OVER")) %>% 
        mutate(sim = NA_real_) %>% 
        arrange(team)
      
      for(i in seq_len(nrow(sim_prep))) {
        
        pull_prob <- sample(sim_prep$prob_list[i][[1]],
                            size = 1)
        
        sim_prep$sim[i] <- sample(
          x = c("OVER", "UNDER"), 
          size = 1,
          prob = c(pull_prob / 100, 1 - (pull_prob / 100)))
      }
    } else {
      sim_prep <- picks %>% 
        inner_join(lookup_table, by = "team") %>% 
        select(team, expected, prob) %>% 
        distinct() %>% 
        mutate(not_expected = if_else(expected == "OVER", "UNDER", "OVER")) %>% 
        mutate(sim = NA_real_) %>% 
        arrange(team)
      
      for(i in seq_len(nrow(sim_prep))) {
        sim_prep$sim[i] <- sample(
          x = c(sim_prep$expected[i], sim_prep$not_expected[i]), 
          size = 1,
          prob = c(sim_prep$prob[i] / 100, 1 - (sim_prep$prob[i] / 100)))
      }
    }
    
    sims <- picks %>% 
      inner_join(sim_prep %>% select(team, sim), by = "team") %>% 
      mutate(sim_points = if_else(choice == sim, wage, -wage))
    
    out <- sims %>% 
      group_by(player) %>% 
      summarize(expected_total = sum(sim_points)) %>% 
      arrange(desc(expected_total))
    
    out$rank <- 1:8
    
    out %>% 
      mutate(playoffs = rank <= 4)
  }, 
  .progress = TRUE, 
  .options = furrr_options(seed = TRUE)
) 
end_time <- Sys.time()
end_time - start_time

outcome_sims %>% 
  group_by(player) %>% 
  summarize(median_expected_total = median(expected_total),
            mean_expected_total = mean(expected_total),
            sd_expected_total = sd(expected_total),
            median_rank = median(rank),
            mean_rank = mean(rank),
            prob_playoffs = mean(playoffs == TRUE) * 100,
            prob_one_rank = mean(rank == 1) * 100) %>% 
  arrange(desc(median_expected_total)) %>% 
  print()

