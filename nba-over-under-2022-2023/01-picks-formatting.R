library(tidyverse)
library(readxl)

num_games <- 82

picks <- read_excel(path = "picks.xlsx", sheet = "picks")
projections <- read_excel(path = "picks.xlsx", sheet = "projections") %>% 
  select(-conference) %>% 
  mutate(percentage_projection = win_projection / num_games * 100)
meta <- read_excel(path = "picks.xlsx", sheet = "meta") 

if(interactive()) {
  # Check picks follow rules
  picks %>% arrange(player, desc(wage), choice) %>% View()
  
  # How many OVER and UNDER for each player?
  picks %>% count(player, choice)
}
