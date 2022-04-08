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

not_determined <- expected %>% 
  select(team, expected, prob) %>% 
  filter(prob != 100) %>% 
  select(-prob) %>% 
  rename(sim = expected) %>% 
  arrange(team) %>% 
  select(team)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")

# Determine points where outcomes determined for each player
determined <- expected %>% 
  select(team, expected, prob) %>% 
  filter(prob == 100) %>% 
  select(-prob) %>% 
  rename(sim = expected) %>% 
  arrange(team)

determined_points <- expected %>% 
  filter(prob == 100) %>% 
  mutate(adonis_pts = if_else(adonis_choice == expected, Adonis, -Adonis),
         chester_pts = if_else(chester_choice == expected, Chester, -Chester),
         jake_pts = if_else(jake_choice == expected, Jake, -Jake),
         jenelle_pts = if_else(jenelle_choice == expected, Jenelle, -Jenelle),
         mary_pts = if_else(mary_choice == expected, Mary, -Mary),
         mike_pts = if_else(mike_choice == expected, Mike, -Mike),
         phil_pts = if_else(phil_choice == expected, Phil, -Phil),
         ryan_pts = if_else(ryan_choice == expected, Ryan, -Ryan))

determined_correct <- determined_points %>% 
  summarize(adonis_correct = sum(adonis_pts > 0),
            chester_correct = sum(chester_pts > 0),
            jake_correct = sum(jake_pts > 0),
            jenelle_correct = sum(jenelle_pts > 0),
            mary_correct = sum(mary_pts > 0),
            mike_correct = sum(mike_pts > 0),
            phil_correct = sum(phil_pts > 0),
            ryan_correct = sum(ryan_pts > 0)) %>% 
  pivot_longer(cols = everything(), names_to = "player",
               values_to = "Total Determined Correct Picks") %>% 
  mutate(player = str_to_sentence(str_replace_all(player, "_correct", "")))

correct_15s <- determined_points %>% 
  summarize(adonis_correct_15s = sum(adonis_pts == 15),
            chester_correct_15s = sum(chester_pts == 15),
            jake_correct_15s = sum(jake_pts == 15),
            jenelle_correct_15s = sum(jenelle_pts == 15),
            mary_correct_15s = sum(mary_pts == 15),
            mike_correct_15s = sum(mike_pts == 15),
            phil_correct_15s = sum(phil_pts == 15),
            ryan_correct_15s = sum(ryan_pts == 15)) %>% 
  pivot_longer(cols = everything(), names_to = "player",
               values_to = "Total Determined Correct Picks (Wage 15)") %>% 
  mutate(player = str_to_sentence(str_replace_all(player, "_correct_15s", "")))
  
player_sums <- determined_points %>% 
  summarize(adonis_total = sum(adonis_pts),
            chester_total = sum(chester_pts),
            jake_total = sum(jake_pts),
            jenelle_total = sum(jenelle_pts),
            mary_total = sum(mary_pts),
            mike_total = sum(mike_pts),
            phil_total = sum(phil_pts),
            ryan_total = sum(ryan_pts)) %>% 
  pivot_longer(cols = everything(), names_to = "player", 
               values_to = "Total Determined Points") %>% 
  mutate(player = str_to_sentence(str_replace_all(player, "_total", ""))) %>% 
  inner_join(determined_correct, by = "player") %>% 
  inner_join(correct_15s, by = "player")

# not_determined_wide <- not_determined %>% 
#   mutate(result = NA_character_) %>% 
#   pivot_wider(names_from = team, values_from = result) %>% 
#   slice(-1)
# flips <- expand_grid(df = not_determined_wide, outcome = c("OVER", "UNDER"))

flips <- expand_grid(
#  `Denver Nuggets` = c("OVER", "UNDER"),
#  `Detroit Pistons` = c("OVER", "UNDER"),
#  `Houston Rockets` = c("OVER", "UNDER"),
  `New Orleans Pelicans` = c("OVER", "UNDER"),
#  `Oklahoma City Thunder` = c("OVER", "UNDER"),
#  `Orlando Magic` = c("OVER", "UNDER"),
  `Philadelphia 76ers` = c("OVER", "UNDER"),
#  `San Antonio Spurs` = c("OVER", "UNDER"),
  `Washington Wizards` = c("OVER", "UNDER")
) %>%
  mutate(outcome = row_number())

long_flips <- flips %>%
  pivot_longer(cols = -outcome, names_to = "team", values_to = "result")

# Add on points according to each possibility in long_flips
to_link <- expected %>% 
  filter(prob != 100) %>% 
  select(-prob, -expected) %>% 
  arrange(team)

linked <- long_flips %>% 
  inner_join(to_link, by = "team") %>% 
  mutate(adonis_pts = if_else(adonis_choice == result, Adonis, -Adonis),
         chester_pts = if_else(chester_choice == result, Chester, -Chester),
         jake_pts = if_else(jake_choice == result, Jake, -Jake),
         jenelle_pts = if_else(jenelle_choice == result, Jenelle, -Jenelle),
         mary_pts = if_else(mary_choice == result, Mary, -Mary),
         mike_pts = if_else(mike_choice == result, Mike, -Mike),
         phil_pts = if_else(phil_choice == result, Phil, -Phil),
         ryan_pts = if_else(ryan_choice == result, Ryan, -Ryan))

determined_correct_sims <- linked %>% 
  group_by(outcome) %>% 
  summarize(adonis_correct = sum(adonis_pts > 0),
            chester_correct = sum(chester_pts > 0),
            jake_correct = sum(jake_pts > 0),
            jenelle_correct = sum(jenelle_pts > 0),
            mary_correct = sum(mary_pts > 0),
            mike_correct = sum(mike_pts > 0),
            phil_correct = sum(phil_pts > 0),
            ryan_correct = sum(ryan_pts > 0)) %>% 
  pivot_longer(cols = -outcome, names_to = "player",
               values_to = "Total Simulated Correct Picks") %>% 
  mutate(player = str_to_sentence(str_replace_all(player, "_correct", "")))

correct_15s_sims <- linked %>% 
  group_by(outcome) %>% 
  summarize(adonis_correct_15s = sum(adonis_pts == 15),
            chester_correct_15s = sum(chester_pts == 15),
            jake_correct_15s = sum(jake_pts == 15),
            jenelle_correct_15s = sum(jenelle_pts == 15),
            mary_correct_15s = sum(mary_pts == 15),
            mike_correct_15s = sum(mike_pts == 15),
            phil_correct_15s = sum(phil_pts == 15),
            ryan_correct_15s = sum(ryan_pts == 15)) %>% 
  pivot_longer(cols = -outcome, names_to = "player",
               values_to = "Total Simulated Correct Picks (Wage 15)") %>% 
  mutate(player = str_to_sentence(str_replace_all(player, "_correct_15s", "")))

player_sims <- linked %>% 
  group_by(outcome) %>% 
  summarize(adonis_total = sum(adonis_pts),
            chester_total = sum(chester_pts),
            jake_total = sum(jake_pts),
            jenelle_total = sum(jenelle_pts),
            mary_total = sum(mary_pts),
            mike_total = sum(mike_pts),
            phil_total = sum(phil_pts),
            ryan_total = sum(ryan_pts))  %>% 
   pivot_longer(cols = -outcome, names_to = "player", 
                values_to = "Total Simulated Points") %>% 
   mutate(player = str_to_sentence(str_replace_all(player, "_total", ""))) %>% 
   inner_join(determined_correct_sims, by = c("outcome", "player")) %>% 
   inner_join(correct_15s_sims, by = c("outcome", "player"))

remaining_outcomes <- player_sims %>% 
  inner_join(player_sums, by = "player") %>% 
  mutate("Outcome Points" = `Total Determined Points` + `Total Simulated Points`,
         "Outcome Correct Picks" = `Total Determined Correct Picks` + `Total Simulated Correct Picks`,
         "Outcome Correct Wage 15 Picks" = `Total Determined Correct Picks (Wage 15)` + `Total Simulated Correct Picks (Wage 15)`) #%>% 
#  group_by(outcome) %>% 
#  arrange(desc(`Outcome Points`)) %>% 
#  mutate(rank = rank(-`Outcome Points`)) %>% 
#  mutate(rank = data.table::frank(-`Outcome Points`, -`Outcome Correct Picks`, 
#                                  -`Outcome Correct Wage 15 Picks`,
#                                  ties.method = "dense")) %>% 
#  ungroup()

out <- remaining_outcomes %>% 
  group_by(outcome, player) %>% 
  summarize(expected_total = sum(`Outcome Points`),
            expected_correct_picks = sum(`Outcome Correct Picks`),
            expected_correct_15_picks = sum(`Outcome Correct Wage 15 Picks`)) %>% 
  arrange(outcome, desc(expected_total), desc(expected_correct_picks), 
          desc(expected_correct_15_picks)) %>% 
  mutate(rank = 1:8,
         playoffs = rank <= 4)

write_csv(out, "remaining_outcomes.csv")

View(overalls <- out %>% 
       group_by(player) %>% 
       summarize(median_expected_total = median(expected_total),
                 mean_expected_total = mean(expected_total),
                 sd_expected_total = sd(expected_total),
                 median_rank = median(rank),
                 mean_rank = mean(rank),
                 prob_playoffs = mean(playoffs == TRUE) * 100,
                 prob_one_rank = mean(rank == 1) * 100,
                 count = n()) %>% 
       arrange(desc(median_expected_total)))

View(out %>% 
       ungroup(outcome) %>% count(player, rank))

ggplot(out, aes(x = expected_total)) +
  geom_histogram(bins = 15, color = "white") +
  facet_wrap(~player, nrow = 2, ncol = 4)

ggplot(out, aes(x = rank)) +
  geom_histogram(bins = 15, color = "white") +
  scale_x_continuous(breaks = 1:8) +
  facet_wrap(~player, nrow = 2, ncol = 4)
