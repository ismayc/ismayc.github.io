library(tidyverse)
library(googlesheets4)

num_games <- 82

#here::set_here("/Users/chester/Desktop/ismayc.github.io/nba-over-under-2022-2023")
#setwd("/Users/Chester/Desktop/ismayc.github.io/nba-over-under-2022-2023")

gs_picks_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1DJJtkDkYNe_WoYRGmc0XcrCPMun8WpTho66nZKYVr4c/edit?usp=sharing") 

picks <- gs_picks_raw |> 
  mutate(choice = str_to_upper(pick)) |> 
  mutate(player =  str_extract(str_trim(name), "^[^\\s]+")) |> 
  select(team, player, wage = wager, choice) |> 
  arrange(team, player)