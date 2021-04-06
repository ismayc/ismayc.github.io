library(tidyverse)
library(googlesheets)

expected <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRVvszIE_nImQEeOG8684tsMhc72OkNb7QN9FDVSsagHpG3PnPQ_e4aQkyNdwt8pF27p6EgEztDvkVr/pub?gid=0&single=true&output=csv")

# testing <- expected %>% 
#   transmute(team, prob,
#             Chester = if_else(chester_choice == expected, Chester, -Chester),
#             Jake = if_else(jake_choice == expected, Jake, -Jake),
#             Jenelle = if_else(jenelle_choice == expected, Jenelle, -Jenelle),
#             Adonis	= if_else(adonis_choice == expected, Adonis, -Adonis),
#             Mike	= if_else(mike_choice == expected, Mike, -Mike),
#             Mary	= if_else(mary_choice == expected, Mary, -Mary),
#             Ryan = if_else(ryan_choice == expected, Ryan, -Ryan),
#             Phil	= if_else(phil_choice == expected, Phil, -Phil)) %>% 
#   pivot_longer(cols = Chester:Phil,
#                names_to = "player",
#                values_to = "expected_points")

# Simulate
lookup_table <- expected %>% 
  select(team, expected, prob)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")

set.seed(NULL)
start_time <- Sys.time()
outcome_sims <- map_dfr(
  1:10000, 
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
    
    out
  }
) 
end_time <- Sys.time()
end_time - start_time

outcome_sims %>% 
  group_by(player) %>% 
  summarize(median_expected_total = median(expected_total),
            mean_expected_total = mean(expected_total),
            sd_expected_total = sd(expected_total),
            median_rank = median(rank),
            mean_rank = mean(rank)) %>% 
  arrange(desc(median_expected_total))

