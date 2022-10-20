library(tidyverse)
library(janitor)
library(nbastatR)
library(furrr)
library(jsonlite)
library(glue)

Sys.setenv("VROOM_CONNECTION_SIZE" = 100000000000)

seasons_rosters <-
  function(seasons = NULL,
           return_message = TRUE,
           nest_data = F) {
    if (seasons %>% is_null()) {
      stop("Enter seasons")
    }
    .get_season_roster_safe <-
      possibly(nbastatR:::.get_season_roster, tibble())
    all_data <-
      seasons %>%
      future_map_dfr(function(season) {
        .get_season_roster_safe(season = season, return_message = return_message)
      })
    
    df_dict_nba_players <- nba_players()
    
    all_data <-
      all_data %>%
      left_join(df_dict_nba_players %>% select(idPlayer, dplyr::matches("url"))) %>%
      suppressMessages()
    
    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(slugSeason), .key = dataRosters)
    }
    all_data
  }

players_2023 <- seasons_rosters(seasons = 2023)

#write_rds(players_2023, "rosters/players_2023.rds")
players_2023 <- read_rds("rosters/players_2023.rds")

players_clean <- clean_names(players_2023) %>% 
  inner_join(readxl::read_excel("picks.xlsx", sheet = "meta"),
             by = c("slug_team" = "abbreviation")) %>% 
  relocate(team, conference, division, .after = slug_team) %>% 
  select(name_player, team, conference, division, height_inches,
         age_player, number_jersey, everything())


# 2022 analysis
players_2022 <- read_rds("rosters/players_2022.rds")

mean_jersey <- mean(players_2022_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_2022_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_2022_clean$height_inches, na.rm = TRUE)
median_height <- median(players_2022_clean$height_inches, na.rm = TRUE)

mean_age <- mean(players_2022_clean$age_player, na.rm = TRUE)
median_age <- median(players_2022_clean$age_player, na.rm = TRUE)

deviation <- 1
middling <- players_2022_clean %>% 
  filter(between(number_jersey, median_jersey - deviation, median_jersey + deviation),
         between(height_inches, median_height - deviation, median_height + deviation),
         between(age_player, median_age - deviation, median_age + deviation)) %>% 
  select(slug_team, name_player, number_jersey, height_inches, group_position, 
         age_player)

percentile_stats <- function(given_quantile = 0.5, deviation = 1) {
  
  jersey_perc <- quantile(players_2022_clean$number_jersey, 
                          probs = given_quantile, na.rm = TRUE)
  height_perc <- quantile(players_2022_clean$height_inches, 
                          probs = given_quantile, na.rm = TRUE)
  age_perc <- quantile(players_2022_clean$age_player, 
                       probs = given_quantile, na.rm = TRUE)
  
  players_2022_clean %>% 
    filter(between(number_jersey, jersey_perc - deviation, jersey_perc + deviation),
           between(height_inches, height_perc - deviation, height_perc + deviation),
           between(age_player, age_perc - deviation, age_perc + deviation)) %>% 
    select(slug_team, team, conference, division, name_player, number_jersey, 
           height_inches, group_position, age_player)
}

( q1s <- percentile_stats(given_quantile = 0.25) )
( q3s <- percentile_stats(given_quantile = 0.75) )

ggplot(data = players_2022_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")

# 1998

players_1998 <- nbastatR::seasons_rosters(seasons = 1998)

players_1998_clean <- clean_names(players_1998)

mean_jersey <- mean(players_1998_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_1998_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_1998_clean$height_inches, na.rm = TRUE)
median_height <- median(players_1998_clean$height_inches, na.rm = TRUE)

ggplot(data = players_1998_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")

# 1992
players_1992 <- nbastatR::seasons_rosters(seasons = 1992)

players_1992_clean <- clean_names(players_1992)

mean_jersey_ <- mean(players_1992_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_1992_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_1992_clean$height_inches, na.rm = TRUE)
median_height <- median(players_1992_clean$height_inches, na.rm = TRUE)

ggplot(data = players_1992_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")

# 1972
players_1972 <- nbastatR::seasons_rosters(seasons = 1972)

players_1972_clean <- clean_names(players_1972)

mean_jersey <- mean(players_1972_clean$number_jersey, na.rm = TRUE)
median_jersey <- median(players_1972_clean$number_jersey, na.rm = TRUE)

mean_height <- mean(players_1972_clean$height_inches, na.rm = TRUE)
median_height <- median(players_1972_clean$height_inches, na.rm = TRUE)

ggplot(data = players_1972_clean, aes(x = number_jersey)) +
  geom_histogram(color = "white")
