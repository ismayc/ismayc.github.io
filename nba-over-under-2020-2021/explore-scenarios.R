library(tidyverse)
library(readxl)
library(furrr)
future::plan(multisession)

phil_probs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=136453584&single=true&output=csv"
chester_probs <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=0&single=true&output=csv"
expected <- read_csv(chester_probs)
# expected <- read_csv(phil_probs)

# Simulate
lookup_table <- expected %>% 
  select(team, expected, prob)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")

set.seed(NULL)
start_time <- Sys.time()
outcome_sims <- future_map_dfr(
  1:100000, 
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
        size = 1,
        prob = c(sim_prep$prob[i] / 100, 1 - (sim_prep$prob[i] / 100)))
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
            prob_one_rank = mean(rank == 1) *100) %>% 
  arrange(desc(median_expected_total))

