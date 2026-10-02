# Match up with column names from nbastatR
library(reticulate)
library(tibble)
library(tidyverse)
library(here)
library(glue)

if (!exists("season_path")) source("00-season-config.R")
season <- season_label

# Once the NBA Cup championship has been played, rerun the schedule finder to
# pick up the two games per team that depend on Cup Group Play. This runs on
# each build until the complete 82-game schedule is saved, then never again.
# A failed scrape only logs a message, so the daily build still finishes.
if (Sys.Date() > as.Date(nba_cup_champ_date) &&
    !file.exists(season_path(schedule_after_cup_file))) {
  message("NBA Cup final (", nba_cup_champ_date, ") has been played and ",
          schedule_after_cup_file, " does not exist yet. Rerunning the ",
          "schedule finder.")
  load_schedule_function_only <- TRUE
  source(season_path("00-get_schedule.R"))
  rm(load_schedule_function_only)
  tryCatch(
    get_nba_schedule(),
    error = function(e) {
      message("Schedule refresh failed, will retry next run: ",
              conditionMessage(e))
    }
  )
}

#scores_temp1 <- as_tibble(py$games_22) %>% 
#  mutate(GAME_DATE = as.Date(GAME_DATE)) %>% 
scores_temp1 <- read_csv("current_year.csv") %>% 
  filter(GAME_DATE >= nba_season_start_date) %>% 
  select(TEAM_NAME, TEAM_ABBREVIATION, WL, PTS, GAME_ID,
         GAME_DATE, MATCHUP) %>% 
  # Filter out Mercury and convert Suns to PHO
  filter(TEAM_ABBREVIATION != "PHO") |> 
  mutate(TEAM_ABBREVIATION = str_replace_all(TEAM_ABBREVIATION, "PHX", "PHO")) |> 
  arrange(GAME_ID) |> 
  # Dedup: a team plays at most one game per day. Different sources

  # (NBA API vs ESPN) use different GAME_IDs for the same game.
  distinct(GAME_DATE, TEAM_ABBREVIATION, .keep_all = TRUE) 

# Check for abbreviation mismatches before joining (catches ESPN fallback issues)
unmatched_abbrevs <- setdiff(
  unique(scores_temp1$TEAM_ABBREVIATION), 
  meta$abbreviation
)
if (length(unmatched_abbrevs) > 0) {
  warning(
    "Unmatched team abbreviations found in game data (likely ESPN fallback): ",
    paste(unmatched_abbrevs, collapse = ", "),
    "\nThese teams will be DROPPED from the analysis!"
  )
}

scores_temp1 <- scores_temp1 %>%
  inner_join(meta %>% 
               select(abbreviation),
             by = c("TEAM_ABBREVIATION" = "abbreviation"))

# Drop the NBA Cup championship, which does not count toward regular-season
# records. NBA API game IDs mark it with a leading 6 (0062600001, read in as
# 62600001); ESPN and basketball-reference IDs do not, so fall back to the
# date for rows from those sources.
cup_final_rows <- scores_temp1 |>
  filter(GAME_DATE == as.Date(nba_cup_champ_date))
cup_final_has_nba_id <- any(str_detect(as.character(cup_final_rows$GAME_ID),
                                       "^0*6\\d{7}$"))
if (cup_final_has_nba_id) {
  scores_temp1 <- scores_temp1 |>
    filter(!str_detect(as.character(GAME_ID), "^0*6\\d{7}$"))
} else {
  if (nrow(cup_final_rows) > 2) {
    warning("Dropping ", nrow(cup_final_rows), " team-game rows on the NBA ",
            "Cup final date (", nba_cup_champ_date, "); expected 2. Check ",
            "whether regular-season games were also played that day.")
  }
  scores_temp1 <- scores_temp1 |>
    filter(GAME_DATE != as.Date(nba_cup_champ_date))
}

scores_temp1 <- scores_temp1 |>
  filter(GAME_DATE < Sys.Date()) # |>
  # mutate(MATCHUP = case_when(
  #   str_detect(GAME_ID, "22500147") & TEAM_ABBREVIATION == "DET" ~ "DET vs. DAL",
  #   str_detect(GAME_ID, "22500578") & TEAM_ABBREVIATION == "ORL" ~ "ORL vs. MEM",
  #   str_detect(GAME_ID, "22500602") & TEAM_ABBREVIATION == "MEM" ~ "MEM vs. ORL",
  #   str_detect(GAME_ID, "22501229") & TEAM_ABBREVIATION == "ORL" ~ "ORL vs. NYK",
  #   str_detect(GAME_ID, "22501230") & TEAM_ABBREVIATION == "OKC" ~ "OKC vs. SAS",
  #   TRUE ~ MATCHUP
  # ))

scores_temp1 <- scores_temp1 %>%
  group_by(GAME_ID) %>%
  mutate(
    has_vs = any(str_detect(MATCHUP, "vs\\.")),
    MATCHUP = if_else(
      !has_vs & str_detect(MATCHUP, paste0("@ ", TEAM_ABBREVIATION, "$")),
      paste0(TEAM_ABBREVIATION, " vs. ", str_extract(MATCHUP, "^\\w+")),
      MATCHUP
    )
  ) %>%
  select(-has_vs) %>%
  ungroup()

# Redo the analysis that used to be done in 02a-get-game-scores.R
#if(!file.exists(here(
#   "rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds"))) &&
#   sum(is.na(scores_temp1)) != 0
#) {
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
#} else {
  game_results_raw <- read_rds(
    here("rds", glue("game_results_raw_through_{Sys.Date() - 1}.rds"))
  )
#}

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

# The NBA Cup championship does not count towards regular season outcomes.
# It is dropped from scores_temp1 near the top of this file, and the
# post-Cup schedule refresh also happens there.

write_rds(standings,
          here::here("rds", glue::glue("standings_through_{Sys.Date() - 1}.rds")))
