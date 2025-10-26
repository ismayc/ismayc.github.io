library(tidyverse)
library(googlesheets4)
library(readxl)

num_games <- 82

#here::set_here("/Users/chester/Desktop/ismayc.github.io/nba-over-under-2022-2023")
#setwd("/Users/Chester/Desktop/ismayc.github.io/nba-over-under-2022-2023")

gs_picks_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1DJJtkDkYNe_WoYRGmc0XcrCPMun8WpTho66nZKYVr4c/edit?usp=sharing") 

picks <- gs_picks_raw |> 
  mutate(choice = str_to_upper(pick)) |> 
  mutate(player =  str_extract(str_trim(name), "^[^\\s]+")) |> 
  select(team, player, wage = wager, choice) |> 
  arrange(team, player)
#picks <- read_excel(path = "picks.xlsx", sheet = "picks")
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
