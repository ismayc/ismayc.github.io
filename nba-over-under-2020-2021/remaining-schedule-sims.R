library(tidyverse)

schedule <- nbastatR::current_schedule() %>% 
  filter(
    dateGame >= "2020-12-22", 
    dateGame <= "2021-05-16",
    # All-Star Game
    dateGame != "2021-03-07"
  ) %>% 
  select(game_date = dateGame,
         game_id = idGame,
         slug_away_team = slugTeamAway,
         away_team = nameTeamAway,
         slug_home_team = slugTeamHome,
         home_team = nameTeamHome,
         start_time = hasBuzzerBeater
  ) %>% 
  mutate(is_complete = (game_date < Sys.Date()))

remaining_games <- schedule %>% 
  filter(!is_complete) %>% 
  select(-is_complete)
