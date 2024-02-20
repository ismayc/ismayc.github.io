library(tidyverse)
library(readxl)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")
projections <- read_excel(path = "picks.xlsx", sheet = "projections") %>% 
  mutate(percentage_projection = win_projection / 72 * 100)
meta <- read_excel(path = "picks.xlsx", sheet = "meta") 

# Check picks follow rules
#picks %>% arrange(player, desc(wage), choice) %>% View()

# How many OVER and UNDER for each player?
#picks %>% count(player, choice)
