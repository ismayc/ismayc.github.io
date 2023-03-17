#install.packages("devtools")
#devtools::install_github("abresler/nbastatR")

library(tidyverse)
# library(nbastatR)
library(here)
library(glue)

# https://www.nba.com/news/key-dates
nba_season_start_date <- "2022-10-18"
nba_season_end_date <- "2023-04-09"
all_star_game_date <- "2022-02-19"
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)
season <- 2023

#setwd("/Users/chester/Desktop/ismayc.github.io/nba-over-under-2022-2023")
#here::set_here("/Users/chester/Desktop/ismayc.github.io/nba-over-under-2022-2023")

if(!file.exists(here(
  "rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
) {
  season <- game_logs(seasons = season)   
  
  game_results_raw <- season %>% 
    distinct(slugSeason, nameTeam, dateGame, numberGameTeamSeason, 
             slugMatchup, slugTeam, slugOpponent, slugTeamLoser)
  
  write_rds(game_results_raw, 
            here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
} else {
  game_results_raw <- read_rds(
    here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds"))
  )
}

slug_lookup <- game_results_raw %>% 
  distinct(nameTeam, slugTeam) %>% 
  arrange(slugTeam)

.get_slug_year <- function() {
  current_date <- Sys.Date()
  current_year <- lubridate::year(current_date)
  current_month <- lubridate::month(current_date)
  slug_year <- dplyr::case_when(current_month >= 10 ~  current_year,
                                TRUE ~ current_year - 1)
  slug_year
}

#library(feather)
#current_year_results <- read_feather("2022.feather")

meta <- read_excel(path = "picks.xlsx", sheet = "meta") 

# reticulate::virtualenv_create()
# reticulate::use_virtualenv("r-reticulate")
# reticulate::py_install("nba_api", pip = TRUE)
# reticulate::py_install("pandas")
