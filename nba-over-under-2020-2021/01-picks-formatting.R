library(tidyverse)
library(readxl)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")
projections <- read_excel(path = "picks.xlsx", sheet = "projections") %>% 
  mutate(percentage_projection = win_projection / 72 * 100)
meta <- read_excel(path = "picks.xlsx", sheet = "meta") 

picks_wide <- picks %>% 
#  unite("team_player", team:player, sep = "_") %>% 
  unite("wage_choice", wage:choice, sep = " ") %>% 
  pivot_wider(names_from = player, values_from = wage_choice) %>% 
  inner_join(projections, by = "team") %>% 
  inner_join(standings, by = c("team" = "team_name")) %>% 
  mutate(percentage_projection = round(percentage_projection / 100, 3)) %>% 
  select(-conference, -division, -`Games Back`, -differential) %>% 
  mutate(loss_projection = 72 - win_projection, .after = win_projection) %>% 
  mutate(outcome_determined = case_when(
    wins > win_projection ~ "OVER",
    losses > loss_projection ~ "UNDER",
    TRUE ~ NA_character_))

# Check picks follow rules
#picks %>% arrange(player, desc(wage), choice) %>% View()

# How many OVER and UNDER for each player?
#picks %>% count(player, choice)
