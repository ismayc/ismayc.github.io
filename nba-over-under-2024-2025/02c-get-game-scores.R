# Match up with column names from nbastatR
library(reticulate)
library(tibble)
library(tidyverse)
library(here)
library(glue)

season <- "2024-25"

#scores_temp1 <- as_tibble(py$games_22) %>% 
#  mutate(GAME_DATE = as.Date(GAME_DATE)) %>% 
scores_temp1 <- read_csv("current_year.csv") %>% 
  filter(GAME_DATE >= nba_season_start_date) %>% 
  select(TEAM_NAME, TEAM_ABBREVIATION, WL, PTS, GAME_ID,
         GAME_DATE, MATCHUP) %>% 
  # Filter out Mercury and convert Suns to PHO
  filter(TEAM_ABBREVIATION != "PHO") |> 
  mutate(TEAM_ABBREVIATION = str_replace_all(TEAM_ABBREVIATION, "PHX", "PHO")) |> 
  arrange(GAME_ID) %>% 
  inner_join(meta %>% 
               select(abbreviation),
             by = c("TEAM_ABBREVIATION" = "abbreviation"))|> 
  filter(GAME_DATE < Sys.Date())

# Redo the analysis that used to be done in 02a-get-game-scores.R
if(!file.exists(here(
  "rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
) {
  # Convert the scores_temp1 dataframe
  game_results_raw <- scores_temp1 %>%
    mutate(
      # Set the slugSeason manually (you may need to adjust this based on actual data)
      slugSeason = season,
      
      # Rename columns to match the target format
      dateGame = GAME_DATE,
      nameTeam = TEAM_NAME,
      slugMatchup = MATCHUP,
      slugTeam = TEAM_ABBREVIATION,
      
      # Extract the opponent's abbreviation from the MATCHUP column
      slugOpponent = if_else(str_detect(MATCHUP, "vs."), 
                             str_extract(MATCHUP, "(?<=vs\\.\\s)\\w{3}"), 
                             str_extract(MATCHUP, "(?<=@\\s)\\w{3}")),
      
      # Determine the losing team based on the WL column
      slugTeamLoser = if_else(WL == "L", TEAM_ABBREVIATION, slugOpponent),
    ) %>%
    # Sort by date
    arrange(dateGame) %>%
    # Calculate a unique game number (within the season) for each team
    group_by(TEAM_NAME) |> 
    mutate(numberGameTeamSeason = row_number()) |> 
    ungroup() |> 
    # Select and reorder columns to match the target format
    select(
      slugSeason, dateGame, numberGameTeamSeason, nameTeam,
      slugMatchup, slugTeam, slugOpponent, slugTeamLoser
    )
  
  write_rds(game_results_raw, 
            here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds")))
} else {
  game_results_raw <- read_rds(
    here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds"))
  )
}

scores_temp_away <- scores_temp1 %>% 
  filter(str_detect(string = MATCHUP, pattern = "vs.")) %>% 
  separate(col = MATCHUP, into = c("slugTeamHome", "slugTeamAway"), 
           sep = " vs. ") %>% 
  rename(scoreHome = PTS,
         nameTeamHome = TEAM_NAME) %>% 
  select(-TEAM_ABBREVIATION, -WL)

scores_temp_home <- scores_temp1 %>% 
  filter(str_detect(string = MATCHUP, pattern = "@")) %>% 
  separate(col = MATCHUP, into = c("slugTeamAway", "slugTeamHome"), 
           sep = " @ ") %>% 
  rename(scoreAway = PTS,
         nameTeamAway = TEAM_NAME)%>% 
  select(-TEAM_ABBREVIATION, -WL)

scores_joined <- scores_temp_away %>% 
  inner_join(scores_temp_home, 
             by = c("GAME_ID", "GAME_DATE", "slugTeamAway", "slugTeamHome")) %>%
  select(idGame = GAME_ID, dateGame = GAME_DATE, slugTeamAway, slugTeamHome,
         nameTeamAway, nameTeamHome, scoreAway, scoreHome)

# 
# scores_temp2 <- scores_temp1 %>% 
#   rename(dateGame = GAME_DATE,
#          idGame = GAME_ID) %>% 
#   separate(col = MATCHUP, into = c("slugTeamAway", "slugTeamHome"), 
#            sep = " @ ") %>% 
#   mutate(slugTeamAway = ifelse(str_detect(slugTeamAway, "vs."),
#                                NA_character_,
#                                slugTeamAway))
# 
# home_away_lookup <- scores_temp2 %>% 
#   distinct(idGame, dateGame, slugTeamAway, slugTeamHome) %>% 
#   arrange(idGame) %>% 
#   na.omit()
# 
# scores_temp3 <- scores_temp2 %>% 
#   select(-slugTeamAway, -slugTeamHome) %>% 
#   inner_join(home_away_lookup, by = c("idGame", "dateGame")) %>% 
#   inner_join(meta %>% select(team, abbreviation), 
#              by = c("slugTeamAway" = "abbreviation")) %>% 
#   rename(nameTeamAway = team) %>% 
#   inner_join(meta %>% select(team, abbreviation), 
#              by = c("slugTeamHome" = "abbreviation")) %>% 
#   rename(nameTeamHome = team) %>% 
#   mutate(scoreHome = ifelse(TEAM_ABBREVIATION == slugTeamHome, PTS, NA),
#          scoreAway = ifelse(TEAM_ABBREVIATION == slugTeamAway, PTS, NA))
# 
# scores_distinct <- scores_temp3 %>% 
#   distinct(dateGame, idGame, slugTeamAway, slugTeamHome, nameTeamAway,
#            nameTeamHome, scoreHome, scoreAway) 
# 
# scores_home <- scores_distinct %>% 
#   filter(!is.na(scoreHome))
# 
# scores_away <- scores_distinct %>% 
#   filter(!is.na(scoreAway))
# 
# scores_joined <- scores_home %>% 
#   select(-scoreAway) %>% 
#   inner_join(scores_away %>% select(idGame, scoreAway), by = "idGame") %>% 
#   relocate(scoreAway, .before = scoreHome)

scores <- scores_joined %>%
  select(game_date = dateGame,
         game_id = idGame,
         slug_away_team = slugTeamAway,
         away_team = nameTeamAway,
         slug_home_team = slugTeamHome,
         home_team = nameTeamHome,
         away_score = scoreAway,
         home_score = scoreHome#,
         #         is_home_winner,
         #         is_away_winner
  ) |> 
  filter(game_date < Sys.Date())

standings_temp <- scores_temp1 %>%
  group_by(TEAM_NAME) %>%
  summarize(wins = sum(WL == "W"),
            losses = sum(WL == "L")) %>%
  mutate(`Winning Pct` = round(wins / (wins + losses), 3)) %>%
  mutate(differential = wins - losses) %>%
  mutate(TEAM_NAME = str_replace_all(
    TEAM_NAME,
    "LA Clippers",
    "Los Angeles Clippers")) %>%
  inner_join(meta, by = c("TEAM_NAME" = "team")) %>%
  rename(team_name = TEAM_NAME) %>% 
  select(team_name, conference, wins, losses) %>% 
  inner_join(meta, by = c("team_name" = "team", "conference")) %>% 
  mutate(wins = as.integer(wins), losses = as.integer(losses)) %>% 
  mutate(`Winning Pct` = round(wins / (wins + losses), 3)) %>% 
  mutate(differential = wins - losses)

top_differentials <- standings_temp %>%
  group_by(conference) %>%
  slice_max(n = 1, order_by = differential) %>%
  select(team_name, differential) %>%
  ungroup()

standings <- standings_temp %>%
  mutate(top_diff = ifelse(
    conference == "West",
    top_differentials %>% filter(conference == "West") %>% pull(differential),
    top_differentials %>% filter(conference == "East") %>% pull(differential))
  ) %>%
  group_by(conference) %>%
  mutate(`Games Back` = (top_diff - differential) / 2) %>%
  select(-top_diff) %>%
  ungroup() %>%
  mutate(`Games Back` = if_else(
    `Games Back` == 0,
    "-",
    sprintf("%.1f", round(`Games Back`, 1)))
  )

# Manual fix to reduce Lakers total wins by 1 and Pacers loss total by 1. 
# The IST championship does not count towards regular season outcomes.
# standings[standings$abbreviation == "LAL", "wins"] <- standings[standings$abbreviation == "LAL", "wins"] - 1
# standings[standings$abbreviation == "IND", "losses"] <- standings[standings$abbreviation == "IND", "losses"] - 1

write_rds(standings,
          here::here("rds", glue::glue("standings_through_{Sys.Date() - 1}.rds")))
