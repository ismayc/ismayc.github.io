library(tidyverse)
library(readxl)

oct <- read_excel("sportsref_download.xlsx")

excel_schedule_files <- list.files(pattern = "*.xlsx", full.names = TRUE)

schedule_raw <- purrr::map_dfr(excel_schedule_files, read_excel) |> 
  mutate(Date = sub("^[A-Za-z]{3}, ", "", Date)) |> 
  mutate(dateGame = as.Date(parse_date_time(Date, orders = "mdy")))

nba_season_start_date <- as.Date("2023-10-24")
nba_season_end_date <- as.Date("2024-04-14")
all_star_game_date <- as.Date("2024-02-18")

schedule <- schedule_raw %>% 
  filter(
    dateGame >= nba_season_start_date, 
    dateGame <= nba_season_end_date,
    # All-Star Game
    dateGame != all_star_game_date 
  ) %>% 
  select(game_date = dateGame,
#         game_id = idGame,
         away_team = `Visitor/Neutral`,
         home_team = `Home/Neutral`,
         start_time = `Start (ET)`
  ) %>% 
  mutate(is_complete = (game_date < Sys.Date()))

write_csv(schedule, "../schedule-2023-24.csv")
