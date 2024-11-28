library(readr)

team_season_roster <- function(team = "Denver Nuggets", season = season, 
                                return_message = TRUE) {
  if (!require("remotes")) { install.packages("remotes") }
  if (!require("nbastatR")) { remotes::install_github("abresler/nbastatR") }
  if (!"team" %>% exists()) {
    stop("Please enter a team")
  }
  season_start <- season - 1
  slugSeason <- season_start %>% paste(season %>% substr(start = 3,
                                                         stop = 4), sep = "-")
  if (!"df_dict_team_history" %>% exists()) {
    df_dict_team_history <- nba_franchise_history(only_active = T)
    assign("df_dict_team_history", df_dict_team_history,
           envir = .GlobalEnv)
  }
  team_id <- df_dict_team_history %>% filter(nameTeam %>% str_detect(team)) %>%
    pull(idTeam) %>% unique()
  json_url <- glue::glue("https://stats.nba.com/stats/commonteamroster?LeagueID=00&Season={slugSeason}&TeamID={team_id}") %>%
    as.character()
  json_data <- json_url %>% nbastatR:::.curl_chinazi()
  Sys.sleep(3)
  names_roster <- json_data$resultSets$headers[1] %>% unlist() %>%
    str_to_lower()
  data_roster <- json_data$resultSets$rowSet[1] %>% data.frame(stringsAsFactors = F) %>%
    tibble::as_tibble()
  actual_names <- c("idTeam", "yearSeason", "idLeague", "namePlayer",
                    "firstNamePlayer","playerHandle",
                    "numberJerseySeason", "groupPosition", "height",
                    "weightLBS", "dateBirth", "agePlayerSeason", "countSeasons",
                    "nameSchool", "idPlayer", "transaction")[seq_along(names(data_roster))]
  data_roster <- data_roster %>% purrr::set_names(actual_names) %>%
    mutate(slugSeason, nameTeam = team) %>% dplyr::select(slugSeason,
                                                          yearSeason, nameTeam, everything())
  data_roster <- data_roster %>% mutate_at(c("idTeam", "yearSeason",
                                             "idLeague", "weightLBS", "agePlayerSeason", "countSeasons",
                                             "idPlayer"), funs(. %>% as.character() %>% readr::parse_number())) %>%
    mutate(dateBirth = lubridate::mdy(dateBirth),
           #         heightInches = heightInches %>% map_dbl(height_in_inches),
           countSeasons = ifelse(countSeasons %>% is.na(), 0, countSeasons),
           isRookie = ifelse(countSeasons ==  0, TRUE, FALSE)) %>% dplyr::select(-one_of(c("idLeague"))) %>%
    suppressMessages() %>% suppressWarnings()
  if (return_message) {
    glue::glue("You got the {team}'s roster for the {slugSeason}") %>%
      cat(fill = TRUE)
  }
  data_roster
}

#write_rds(teams, "teams.rds")
teams <- read_rds("teams.rds")
players_season_pulled <- purrr::map_dfr(
  teams,
  team_season_roster,
  season = season, return_message = TRUE)

write_rds(players_season_pulled, "players_season_pulled.rds")
