#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(janitor)
library(tidyverse)
library(DT)

players_2023 <- read_rds("players_2023.rds")

players_clean <- clean_names(players_2023) %>% 
  inner_join(readxl::read_excel("picks.xlsx", sheet = "meta"),
             by = c("slug_team" = "abbreviation")) %>% 
  relocate(team, conference, division, .after = slug_team) %>% 
  select(name_player, team, conference, division, height_inches,
         age = age_player, number_jersey, everything())

players_data <- players_clean %>% 
  select(player = name_player, team, conference, division, 
         height_total_inches = height_inches, age, number_jersey) %>% 
  mutate(height_feet = height_total_inches %/% 12,
         height_inches = height_total_inches %% 12)

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
  p("Player data was last pulled on 2022-07-28"),
  
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
      ),
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
      ),
      sliderInput("age", "Age", min = min(players_data$age),
                  max = 45, #max(players_data$age),
                  value = c(min(players_data$age), 45)), #max(players_data$age))),
      #       selectInput("min_age",
      #             "Minimum age",
      #             choices = sort(unique(players_data$age)),
      #             selected = min(players_data$age)),
      # selectInput("max_age",
      #             "Maximum age",
      #             choices = sort(unique(players_data$age)),
      #             selected = max(players_data$age)),
      sliderInput("jersey", "Jersey", min = 0, max = 99, value = c(0, 99)),
      # selectInput("min_jersey",
      #             "Minimum jersey",
      #             choices = sort(unique(players_data$number_jersey)),
      #             selected = "00"),
      # selectInput("max_jersey",
      #             "Maximum jersey",
      #             choices = sort(unique(players_data$number_jersey)),
      #             selected = "99"),
      # selectInput("team",
      #             "Team:",
      #             choices = unique(players_data$team),
      #             selected = "Dallas Mavericks"),
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
      dplyr::filter(conference == input$conference,
                    division %in% c(input$division_west, input$division_east),
                    team %in% c(input$sw_team, input$nw_team, input$pac_team,
                                input$se_team, input$atlantic_team, input$cen_team),
                    between(
                      height_total_inches,
                      as.integer(input$min_feet) * 12 + as.integer(input$min_inches),
                      as.integer(input$max_feet) * 12 + as.integer(input$max_inches)),
                    between(age, input$age[1], input$age[2]),
                    between(as.integer(number_jersey), 
                            input$jersey[1],
                            input$jersey[2])) %>% 
      mutate(height = paste0(height_feet, "-", height_inches)) %>% 
      select(-height_total_inches, -height_feet, -height_inches)},
    options = list(pageLength = 100) #nrow(players_data))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
