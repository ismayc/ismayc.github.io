#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(remotes)
#install_github("abresler/nbastatR")

library(shiny)
library(janitor)
library(tidyverse)
library(DT)
library(tictoc)
library(nbastatR)
library(httr)
library(jsonlite)

update <- FALSE
date_added <- "2023-10-24"
season <- 2024
#date_added <- Sys.Date()

if (update){
  team_season_roster <- function (team = "Denver Nuggets", season = season, return_message = T)
  {
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
}

players_season <- read_rds("players_season_pulled.rds")

players_clean <- clean_names(players_season) %>% 
  rename(team = name_team) %>% 
  inner_join(readxl::read_excel("picks.xlsx", sheet = "meta"),
             by = "team") %>% 
  relocate(team, conference, division, .after = team) %>% 
  select(name_player, team, conference, division, height,
         age = age_player_season, 
         number_jersey = number_jersey_season, 
         everything())

players_data <- players_clean %>% 
  select(player = name_player, team, conference, division, 
         height, age, number_jersey) %>% 
  separate(col = height, 
           into = c("height_feet", "height_inches"),
           sep = "-") %>% 
  mutate(
    height_feet = as.integer(height_feet),
    height_inches = as.integer(height_inches),
    height_total_inches = height_feet * 12 + height_inches)

sw_teams <- players_data %>% 
  distinct(division, team) %>% 
  filter(division == "Southwest") %>% 
  pull(team)

nw_teams <- players_data %>% 
  distinct(division, team) %>% 
  filter(division == "Northwest") %>% 
  pull(team)

pac_teams <- players_data %>% 
  distinct(division, team) %>% 
  filter(division == "Pacific") %>% 
  pull(team)

atlantic_teams <- players_data %>% 
  distinct(division, team) %>% 
  filter(division == "Atlantic") %>% 
  pull(team)

se_teams <- players_data %>% 
  distinct(division, team) %>% 
  filter(division == "Southeast") %>% 
  pull(team)

cen_teams <- players_data %>% 
  distinct(division, team) %>% 
  filter(division == "Central") %>% 
  pull(team)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("NBA Player Finder"),
  p(paste("Player data was last pulled on", date_added)),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("conference",
                  "Conference:",
                  choices = unique(players_data$conference),
                  selected = "West"),
      conditionalPanel(
        condition = "input.conference == 'West'",
        checkboxGroupInput("division_west",
                           "Division:",
                           choices = c("Southwest", "Northwest", "Pacific"),
                           selected = c("Southwest", "Northwest", "Pacific"))#NA_character_)
      ),
      conditionalPanel(
        condition = "input.conference == 'East'",
        checkboxGroupInput("division_east",
                           "Division:",
                           choices = c("Southeast", "Atlantic", "Central"),
                           selected = c("Southeast", "Atlantic", "Central"))#NA_character_)
      ),
      conditionalPanel(
        condition = "input.division_west.indexOf('Southwest') != -1 && input.conference == 'West'",
        checkboxGroupInput("sw_team",
                           "Southwest teams",
                           choices = sw_teams,
                           selected = sw_teams)
      ),
      conditionalPanel(
        condition = "input.division_west.indexOf('Pacific') != -1 && input.conference == 'West'",
        checkboxGroupInput("pac_team",
                           "Pacific teams",
                           choices = pac_teams,
                           selected = pac_teams)
      ),
      conditionalPanel(
        condition = "input.division_west.indexOf('Northwest') != -1 && input.conference == 'West'",
        checkboxGroupInput("nw_team",
                           "Northwest teams",
                           choices = nw_teams,
                           selected = nw_teams)
      ),
      conditionalPanel(
        condition = "input.division_east.indexOf('Southeast') != -1 && input.conference == 'East'",
        checkboxGroupInput("se_team",
                           "Southeast teams",
                           choices = se_teams,
                           selected = se_teams)
      ),
      conditionalPanel(
        condition = "input.division_east.indexOf('Atlantic') != -1 && input.conference == 'East'",
        checkboxGroupInput("atlantic_team",
                           "Atlantic teams",
                           choices = atlantic_teams,
                           selected = atlantic_teams)
      ),
      conditionalPanel(
        condition = "input.division_east.indexOf('Central') != -1 && input.conference == 'East'",
        checkboxGroupInput("cen_team",
                           "Central teams",
                           choices = cen_teams,
                           selected = cen_teams)
      ),
      hr(),
      checkboxInput("known_height",
                    "Known Height",
                    value = FALSE),
      conditionalPanel(
        condition = "input.known_height == true",
        fluidRow(
          column(4, selectInput(
            "chosen_feet",
            "Chosen feet",
            choices = sort(unique(players_data$height_feet)),
            selected = 6)),
          column(4, selectInput(
            "chosen_inches",
            "Chosen inches",
            choices = sort(unique(players_data$height_inches)),
            selected = 6))
        )),
      conditionalPanel(
        condition = "input.known_height != true",    
        fluidRow(
          column(4, selectInput(
            "min_feet",
            "Minimum feet",
            choices = sort(unique(players_data$height_feet)),
            selected = 5)),
          column(4, selectInput(
            "min_inches",
            "Minimum inches",
            choices = sort(unique(players_data$height_inches)),
            selected = min(players_data$height_total_inches, na.rm = TRUE) %% 12))
        )),
      conditionalPanel(
        condition = "input.known_height != true",  
        fluidRow(
          column(4, selectInput(
            "max_feet",
            "Maximum feet",
            choices = sort(unique(players_data$height_feet)),
            selected = 7)),
          column(4, selectInput(
            "max_inches",
            "Maximum inches",
            choices = sort(unique(players_data$height_inches)),
            selected = max(players_data$height_total_inches, na.rm = TRUE) %% 12))
        )
      ),
      checkboxInput("known_jersey",
                    "Known Jersey",
                    value = FALSE),
      conditionalPanel("input.known_jersey == true",
                       numericInput("input_jersey", 
                                    label = "Jersey Number",
                                    value = 12,
                                    min = 0,
                                    max = 99
                       )),
      conditionalPanel("input.known_jersey != true",      
                       sliderInput("slider_jersey", "Jersey", min = 0, max = 99, value = c(0, 99))
      ),
      checkboxInput("known_age",
                    "Known Age",
                    value = FALSE),      
      conditionalPanel("input.known_age == true",
                       numericInput("input_age", 
                                    label = "Age",
                                    value = 25,
                                    min = min(players_data$age),
                                    max = max(players_data$age)
                       )),
      conditionalPanel("input.known_age != true",
                       sliderInput("slider_age", "Age", min = min(players_data$age),
                                   max(players_data$age),
                                   value = c(min(players_data$age), max(players_data$age)))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      DTOutput("filteredTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$filteredTable <- renderDT({
    players_data %>% 
      dplyr::filter(
        conference == input$conference,
        division %in% c(input$division_west, input$division_east),
        team %in% c(input$sw_team, input$nw_team, input$pac_team,
                    input$se_team, input$atlantic_team, input$cen_team),
        {
          if (input$known_height){
            height_total_inches == as.integer(input$chosen_feet) * 12 + as.integer(input$chosen_inches)
          } else {
            between(
              height_total_inches,
              as.integer(input$min_feet) * 12 + as.integer(input$min_inches),
              as.integer(input$max_feet) * 12 + as.integer(input$max_inches))}
        },
        {
          if (input$known_age) {
            age == input$input_age
          } else {
            between(age, input$slider_age[1], input$slider_age[2])
          }
        },
        {
          if (input$known_jersey) {
            as.integer(number_jersey) == input$input_jersey
          } else { 
            between(as.integer(number_jersey), 
                    input$slider_jersey[1],
                    input$slider_jersey[2])
          }
        }
      ) %>% 
      mutate(height = paste0(height_feet, "-", height_inches)) %>% 
      select(-height_total_inches, -height_feet, -height_inches)
    #   }
  },
  options = list(pageLength = 100) #nrow(players_data))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
