# https://chesterismay.shinyapps.io/over-under-points-calculator/

required_packages <- c("shiny", "readxl", "tidyverse", "kableExtra", "DT")

install_if_missing <- function(packages) {
  installed <- rownames(installed.packages())
  to_install <- packages[!packages %in% installed]
  if (length(to_install) > 0) {
    install.packages(to_install)
  }
}

install_if_missing(required_packages)

library(shiny)
library(readxl)
library(tidyverse)
library(kableExtra)
library(DT)

# Get today's date
today <- Sys.Date()

# Extract the current year and month
current_year <- as.integer(format(today, "%Y"))
current_month <- as.integer(format(today, "%m"))

# Check if the month is between October (10) and April (4)
if (current_month >= 10 || current_month <= 4) {
  # If today is between October and December, it's the current year-season
  # If today is between January and April, it's the previous year-season
  start_year <- ifelse(current_month >= 10, current_year, current_year - 1)
  end_year <- start_year + 1
  season_year <- paste(start_year, end_year, sep = "-")
}

# file.copy(from = "../picks.xlsx", to = "picks.xlsx", overwrite = TRUE)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")
num_players <- 8

file_names <- list.files(pattern = "determined_outcomes")

# Extract dates using regular expressions
dates <- gsub("determined_outcomes_(.*)\\.rds", "\\1", file_names)

# Convert the date strings to Date objects
dates <- as.Date(dates)

# Find the maximum date
max_date <- max(dates)

if (file.exists(paste0("determined_outcomes_", 
                       as.Date(Sys.time()), 
                       ".rds"))) {
  temp <- read_rds(paste0("determined_outcomes_", 
                          as.Date(Sys.time()), 
                          ".rds"))
} else {
  temp <- read_rds(paste0("determined_outcomes_", 
                          max_date, 
                          ".rds"))
}

determined <- temp |> 
  select(Team, `Outcome Determined`) |> 
  mutate(Team = str_extract(Team, "\\w+$"))

# determined[determined$Team == "Magic", ]$`Outcome Determined` <- "OVER"

chester_picks <- picks |> 
  filter(player == "Chester") |> 
  mutate(Team = str_extract(team, "\\w+$")) |> 
  select(Team, choice)

over_under_choice <- function(
    input_id, choices = c("OVER", "UNDER"), selected = choices[1]
) {
  fluidRow(
    tags$head(
      tags$style(
        type="text/css", 
        "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
    ),
    column(2),
    div(style="display: inline-block;vertical-align:top; width: 300px;",
        selectInput(
          inputId = input_id,
          #        label = str_c(str_to_sentence(input_id), ":"),
          label = str_c(
            picks %>% 
              distinct(team) %>% 
              filter(str_detect(team, input_id)) %>% 
              pull(team),
            ":"),
          choices = choices,
          selected = selected,
          width = '35%',
          selectize = FALSE,
          size = 1)
    )
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(paste("NBA Over/Under", season_year, "Point Calculations")),
  h3("Created by Chester Ismay"),
  p(str_c("Last updated at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " US Arizona time")),
  
  tags$head(
    tags$style(HTML("
      /* Set the table headers to not wrap */
      table.dataTable th {
        white-space: nowrap;
        overflow: hidden;
      }
      /* Increase minimum width for headers */
      table.dataTable th {
        min-width: 50px;
      }
    "))
  ),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h5("Outcome still to be determined"),
      if (determined |> filter(Team == "Hawks") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Hawks", selected = chester_picks |> filter(Team == "Hawks") |> pull(choice))
      },
      if (determined |> filter(Team == "Celtics") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Celtics", selected = chester_picks |> filter(Team == "Celtics") |> pull(choice))
      },
      if (determined |> filter(Team == "Nets") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Nets", selected = chester_picks |> filter(Team == "Nets") |> pull(choice))
      },
      if (determined |> filter(Team == "Hornets") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Hornets", selected = chester_picks |> filter(Team == "Hornets") |> pull(choice))
      },
      if (determined |> filter(Team == "Bulls") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Bulls", selected = chester_picks |> filter(Team == "Bulls") |> pull(choice))
      },
      if (determined |> filter(Team == "Cavaliers") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Cavaliers", selected = chester_picks |> filter(Team == "Cavaliers") |> pull(choice))
      },
      if (determined |> filter(Team == "Mavericks") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Mavericks", selected = chester_picks |> filter(Team == "Mavericks") |> pull(choice))
      },
      if (determined |> filter(Team == "Nuggets") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Nuggets", selected = chester_picks |> filter(Team == "Nuggets") |> pull(choice))
      },
      if (determined |> filter(Team == "Pistons") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Pistons", selected = chester_picks |> filter(Team == "Pistons") |> pull(choice))
      },
      if (determined |> filter(Team == "Warriors") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Warriors", selected = chester_picks |> filter(Team == "Warriors") |> pull(choice))
      },
      if (determined |> filter(Team == "Rockets") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Rockets", selected = chester_picks |> filter(Team == "Rockets") |> pull(choice))
      },
      if (determined |> filter(Team == "Pacers") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Pacers", selected = chester_picks |> filter(Team == "Pacers") |> pull(choice))
      },
      if (determined |> filter(Team == "Clippers") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Clippers", selected = chester_picks |> filter(Team == "Clippers") |> pull(choice))
      },
      if (determined |> filter(Team == "Lakers") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Lakers", selected = chester_picks |> filter(Team == "Lakers") |> pull(choice))
      },
      if (determined |> filter(Team == "Grizzlies") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Grizzlies", selected = chester_picks |> filter(Team == "Grizzlies") |> pull(choice))
      },
      if (determined |> filter(Team == "Heat") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Heat", selected = chester_picks |> filter(Team == "Heat") |> pull(choice))
      },
      if (determined |> filter(Team == "Bucks") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Bucks", selected = chester_picks |> filter(Team == "Bucks") |> pull(choice))
      },
      if (determined |> filter(Team == "Timberwolves") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Timberwolves", selected = chester_picks |> filter(Team == "Timberwolves") |> pull(choice))
      },
      if (determined |> filter(Team == "Pelicans") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Pelicans", selected = chester_picks |> filter(Team == "Pelicans") |> pull(choice))
      },
      if (determined |> filter(Team == "Knicks") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Knicks", selected = chester_picks |> filter(Team == "Knicks") |> pull(choice))
      },
      if (determined |> filter(Team == "Thunder") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Thunder", selected = chester_picks |> filter(Team == "Thunder") |> pull(choice))
      },
      if (determined |> filter(Team == "Magic") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Magic", selected = chester_picks |> filter(Team == "Magic") |> pull(choice))
      },
      if (determined |> filter(Team == "76ers") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("76ers", selected = chester_picks |> filter(Team == "76ers") |> pull(choice))
      },
      if (determined |> filter(Team == "Suns") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Suns", selected = chester_picks |> filter(Team == "Suns") |> pull(choice))
      },
      if (determined |> filter(Team == "Blazers") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Blazers", selected = chester_picks |> filter(Team == "Blazers") |> pull(choice))
      },
      if (determined |> filter(Team == "Kings") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Kings", selected = chester_picks |> filter(Team == "Kings") |> pull(choice))
      },
      if (determined |> filter(Team == "Spurs") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Spurs", selected = chester_picks |> filter(Team == "Spurs") |> pull(choice))
      },
      if (determined |> filter(Team == "Raptors") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Raptors", selected = chester_picks |> filter(Team == "Raptors") |> pull(choice))
      },
      if (determined |> filter(Team == "Jazz") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Jazz", selected = chester_picks |> filter(Team == "Jazz") |> pull(choice))
      },
      if (determined |> filter(Team == "Wizards") |> pull(`Outcome Determined`) == "not yet") {
        over_under_choice("Wizards", selected = chester_picks |> filter(Team == "Wizards") |> pull(choice))
      },
      # over_under_choice("Hawks", selected = "OVER"),
      # over_under_choice("Celtics", selected = "OVER"),
      # over_under_choice("Nets", selected = "OVER"),
      # over_under_choice("Hornets", selected = "UNDER"),
      # over_under_choice("Bulls", selected = "UNDER"),
      # over_under_choice("Cavaliers", selected = "OVER"),
      # over_under_choice("Mavericks", selected = "OVER"),
      # over_under_choice("Nuggets", selected = "OVER"),
      # over_under_choice("Pistons", selected = "OVER"),
      # over_under_choice("Warriors", selected = "UNDER"),
      #over_under_choice("Rockets", selected = "OVER"),
      # over_under_choice("Pacers", selected = "OVER"),
      # over_under_choice("Clippers", selected = "UNDER"),
      # over_under_choice("Lakers", selected = "OVER"),
      #over_under_choice("Grizzlies", choices = "OVER"),
      # over_under_choice("Heat", selected = "UNDER"),
      # over_under_choice("Bucks", selected = "OVER"),
      #over_under_choice("Timberwolves", selected = "OVER"),
      # over_under_choice("Pelicans", selected = "UNDER"),
      # over_under_choice("Knicks", selected = "OVER"),            
      #over_under_choice("Thunder", selected = "OVER"),
      # over_under_choice("Magic", selected = "OVER"),
      # over_under_choice("76ers", selected = "UNDER"),
      # over_under_choice("Suns", selected = "UNDER"),
      # over_under_choice("Blazers", selected = "UNDER"), 
      # over_under_choice("Kings", selected = "UNDER"),
      # over_under_choice("Spurs", selected = "OVER"),
      # over_under_choice("Raptors", selected = "UNDER"),
      # over_under_choice("Jazz", selected = "OVER"),
      # over_under_choice("Wizards", selected = "UNDER"),
      br(),
      h5("Outcome determined"),
      if (determined |> filter(Team == "Hawks") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Hawks", choices = determined |> filter(Team == "Hawks") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Celtics") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Celtics", choices = determined |> filter(Team == "Celtics") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Nets") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Nets", choices = determined |> filter(Team == "Nets") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Hornets") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Hornets", choices = determined |> filter(Team == "Hornets") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Bulls") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Bulls", choices = determined |> filter(Team == "Bulls") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Cavaliers") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Cavaliers", choices = determined |> filter(Team == "Cavaliers") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Mavericks") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Mavericks", choices = determined |> filter(Team == "Mavericks") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Nuggets") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Nuggets", choices = determined |> filter(Team == "Nuggets") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Pistons") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Pistons", choices = determined |> filter(Team == "Pistons") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Warriors") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Warriors", choices = determined |> filter(Team == "Warriors") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Rockets") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Rockets", choices = determined |> filter(Team == "Rockets") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Pacers") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Pacers", choices = determined |> filter(Team == "Pacers") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Clippers") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Clippers", choices = determined |> filter(Team == "Clippers") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Lakers") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Lakers", choices = determined |> filter(Team == "Lakers") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Grizzlies") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Grizzlies", choices = determined |> filter(Team == "Grizzlies") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Heat") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Heat", choices = determined |> filter(Team == "Heat") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Bucks") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Bucks", choices = determined |> filter(Team == "Bucks") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Timberwolves") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Timberwolves", choices = determined |> filter(Team == "Timberwolves") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Pelicans") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Pelicans", choices = determined |> filter(Team == "Pelicans") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Knicks") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Knicks", choices = determined |> filter(Team == "Knicks") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Thunder") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Thunder", choices = determined |> filter(Team == "Thunder") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Magic") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Magic", choices = determined |> filter(Team == "Magic") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "76ers") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("76ers", choices = determined |> filter(Team == "76ers") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Suns") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Suns", choices = determined |> filter(Team == "Suns") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Blazers") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Trail Blazers", choices = determined |> filter(Team == "Blazers") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Kings") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Kings", choices = determined |> filter(Team == "Kings") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Spurs") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Spurs", choices = determined |> filter(Team == "Spurs") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Raptors") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Raptors", choices = determined |> filter(Team == "Raptors") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Jazz") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Jazz", choices = determined |> filter(Team == "Jazz") |> pull(`Outcome Determined`))
      },
      if (determined |> filter(Team == "Wizards") |> pull(`Outcome Determined`) != "not yet") {
        over_under_choice("Wizards", choices = determined |> filter(Team == "Wizards") |> pull(`Outcome Determined`))
      }
      # over_under_choice("Celtics", choices = "OVER"),
      # over_under_choice("Nets", choices = "UNDER"),
      # over_under_choice("Hornets", choices = "UNDER"),
      # over_under_choice("Bulls", choices = "UNDER"),
      # over_under_choice("Cavaliers", choices = "OVER"),
      # over_under_choice("Mavericks", choices = "UNDER"),
      # over_under_choice("Nuggets", choices = "OVER"),
      # over_under_choice("Pistons", choices = "UNDER"),
      # over_under_choice("Warriors", choices = "UNDER"),
      #over_under_choice("Rockets", choices = "OVER"),
      # over_under_choice("Pacers", choices = "OVER"),
      # over_under_choice("Clippers", choices = "UNDER"),
      # over_under_choice("Lakers", choices = "UNDER"),
      #over_under_choice("Grizzlies", choices = "UNDER"),
      # over_under_choice("Heat", choices = "UNDER"),
      # over_under_choice("Bucks", choices = "OVER"),
      #over_under_choice("Timberwolves", choices = "OVER"),
      # over_under_choice("Pelicans", choices = "UNDER"),
      # over_under_choice("Knicks", choices = "OVER"),            
      #over_under_choice("Thunder", choices = "OVER"),
      # over_under_choice("Magic", choices = "OVER"),
      # over_under_choice("76ers", choices = "OVER"),
      # over_under_choice("Suns", choices = "UNDER"),
      # over_under_choice("Blazers", choices = "UNDER"), 
      # over_under_choice("Kings", choices = "OVER"),
      # over_under_choice("Spurs", choices = "UNDER"),
      # over_under_choice("Raptors", choices = "UNDER"),
      # over_under_choice("Jazz", choices = "OVER"),
      # over_under_choice("Wizards", choices = "UNDER")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      width = 2,
      DTOutput("point_table")#,
      #     h4("Tie-breakers"),
      #      tableOutput("correct_table"),
      #      tableOutput("correct_15_table"),
      #      tableOutput("correct_14_table"),
      #      tableOutput("correct_13_table"),
      #         fluidRow(
      #             column(width = 7, tableOutput("point_table")),
      #             column(width = 5, tableOutput("correct_table"))
      #        )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$point_table <- # function() {
    renderDT({
      
      chester_probs_df <- picks %>% 
        distinct(team) %>% 
        arrange(team) %>% 
        mutate(likely_result = case_when(
          str_detect(team, "Pistons") ~ input$Pistons,  
          str_detect(team, "Blazers") ~ input$Blazers,
          str_detect(team, "Bulls") ~ input$Bulls,
          str_detect(team, "Kings") ~ input$Kings,
          str_detect(team, "Nuggets") ~ input$Nuggets,
          str_detect(team, "Mavericks") ~ input$Mavericks,
          str_detect(team, "Warriors") ~ input$Warriors,
          str_detect(team, "Wizards") ~ input$Wizards,
          str_detect(team, "Hawks") ~ input$Hawks,
          str_detect(team, "Celtics") ~ input$Celtics,
          str_detect(team, "Nets") ~ input$Nets,
          str_detect(team, "Hornets") ~ input$Hornets,
          str_detect(team, "Bulls") ~ input$Bulls,
          str_detect(team, "Cavaliers") ~ input$Cavaliers,
          str_detect(team, "Mavericks") ~ input$Mavericks,
          str_detect(team, "Nuggets") ~ input$Nuggets,
          str_detect(team, "Pistons") ~ input$Pistons,
          str_detect(team, "Warriors") ~ input$Warriors,
          str_detect(team, "Rockets") ~ input$Rockets,
          str_detect(team, "Pacers") ~ input$Pacers,
          str_detect(team, "Clippers") ~ input$Clippers,
          str_detect(team, "Lakers") ~ input$Lakers,
          str_detect(team, "Grizzlies") ~ input$Grizzlies,
          str_detect(team, "Heat") ~ input$Heat,
          str_detect(team, "Bucks") ~ input$Bucks,
          str_detect(team, "Timberwolves") ~ input$Timberwolves,
          str_detect(team, "Pelicans") ~ input$Pelicans,
          str_detect(team, "Knicks") ~ input$Knicks,
          str_detect(team, "Thunder") ~ input$Thunder,
          str_detect(team, "Magic") ~ input$Magic,
          str_detect(team, "76ers") ~ input$`76ers`,
          str_detect(team, "Suns") ~ input$Suns,
          str_detect(team, "Blazers") ~ input$Blazers,
          str_detect(team, "Kings") ~ input$Kings,
          str_detect(team, "Spurs") ~ input$Spurs,
          str_detect(team, "Raptors") ~ input$Raptors,
          str_detect(team, "Jazz") ~ input$Jazz,
          str_detect(team, "Wizards") ~ input$Wizards
        ))
      
      picks_test <- picks %>% 
        inner_join(
          chester_probs_df %>% select(team, likely_result), 
          by = "team"
        ) %>% 
        mutate(projected_points = case_when(
          (choice == likely_result) & (likely_result != "TBD") ~ wage,
          (choice != likely_result) & (likely_result != "TBD") ~ -wage
        ))
      
      num_correct <- picks_test %>% 
        mutate(correct = projected_points > 0) %>% 
        group_by(Player = player) %>% 
        summarize(
          `Number of Correct Picks` = as.integer(
            sum(correct, na.rm = TRUE))
        )
      
      num_15_correct <- picks_test %>% 
        filter(wage == 15) %>% 
        mutate(correct = projected_points > 0) %>% 
        group_by(Player = player) %>% 
        summarize(
          `Correct Picks (Wage 15)` = as.integer(
            sum(correct, na.rm = TRUE))
        ) 
      
      num_14_correct <- picks_test %>% 
        filter(wage == 14) %>% 
        mutate(correct = projected_points > 0) %>% 
        group_by(Player = player) %>% 
        summarize(
          `Correct Picks (Wage 14)` = as.integer(
            sum(correct, na.rm = TRUE))
        ) 
      
      num_13_correct <- picks_test %>% 
        filter(wage == 13) %>% 
        mutate(correct = projected_points > 0) %>% 
        group_by(Player = player) %>% 
        summarize(
          `Correct Picks (Wage 13)` = as.integer(
            sum(correct, na.rm = TRUE))
        ) 
      
      final_table <- picks_test %>% 
        group_by(Player = player) %>% 
        summarize(
          `Points Total` = as.integer(sum(projected_points, na.rm = TRUE))
        ) %>% 
        inner_join(num_correct, by = "Player") %>% 
        inner_join(num_15_correct, by = "Player") %>% 
        inner_join(num_14_correct, by = "Player") %>% 
        inner_join(num_13_correct, by = "Player") %>% 
        arrange(desc(`Points Total`), 
                desc(`Number of Correct Picks`),
                desc(`Correct Picks (Wage 15)`),
                desc(`Correct Picks (Wage 14)`)) #%>% 
      #     select(-`Number of Correct Picks`, -`Correct Picks (Wage 15)`,
      #             -`Correct Picks (Wage 14)`) %>% 
      #      mutate(Rank = 1:num_players, .before = Player)  %>% 
      # knitr::kable() %>% 
      # kableExtra::kable_styling("striped", full_width = FALSE) |> 
      # kableExtra::column_spec(4, width = "300px", extra_css = "width: 300px !important;") %>% 
      # kableExtra::column_spec(5, width = "300px", extra_css = "width: 300px !important;") |> 
      # kableExtra::row_spec(4, extra_css = "border-bottom: 1px solid")  
      
      # Modify final_table to add a separator row
      separator_row <- data.frame(
        Player = "",  # Or any placeholder text for separation
        `Points Total` = NA,
        `Number of Correct Picks` = NA,
        `Correct Picks (Wage 15)` = NA,
        `Correct Picks (Wage 14)` = NA,
        `Correct Picks (Wage 13)` = NA
      )
      
      # Add row numbers, skipping the separator row
      row_numbers <- c("1", "2", "3", "4", NA, "5", "6", "7", "8")
      
      # Insert the separator row after the fourth row
      final_table_with_separator <- bind_rows(
        final_table[1:4, ],
        separator_row,
        final_table[5:nrow(final_table), ]
      )  |> 
        mutate(`Rank` = row_numbers) |> 
        rename(
          `Number of<br>Correct Picks` = `Number of Correct Picks`,
          `Correct Picks<br>(Wage 15)` = `Correct Picks (Wage 15)`,
          `Correct Picks<br>(Wage 14)` = `Correct Picks (Wage 14)`,
          `Correct Picks<br>(Wage 13)` = `Correct Picks (Wage 13)`
        ) |> 
        select(`Rank`, Player, `Points Total`:`Correct Picks<br>(Wage 13)`)
      
      # Render with DT and minimal styling
      datatable(
        final_table_with_separator, 
        escape = FALSE,
        options = list(
          dom = 't',                  # Only show the table (no search, pagination, etc.)
          paging = FALSE,             # Disable pagination
          searching = FALSE,          # Disable search bar
          info = FALSE,               # Remove table info
          #     ordering = FALSE,           # Disable column sorting
          autoWidth = FALSE#,           # Automatically adjust width for table
          # columnDefs = list(
          #   list(width = '1000px', targets = 0),   # Width for 'Rank'
          #   list(width = '200px', targets = 1),   # Width for 'Player'
          #   list(width = '120px', targets = 2),   # Width for 'Points Total'
          #   list(width = '1500px', targets = 3),   # Width for 'Number of Correct Picks'
          #   list(width = '150px', targets = 4),   # Width for 'Correct Picks (Wage 15)'
          #   list(width = '150px', targets = 5),   # Width for 'Correct Picks (Wage 14)'
          #   list(width = '150px', targets = 6)    # Width for 'Correct Picks (Wage 13)'
          # )
        ),         
        rownames = FALSE,          # Remove row numbers displaying
        style = "bootstrap")  %>%
        formatStyle(
          'Player',
          target = 'row',
          backgroundColor = styleEqual("", "black"), # Apply black background to the separator row
          color = styleEqual("", "white")            # Make text white to blend with black background
        )
    }) # end of renderDT
  
} # end of server

# Run the application 
shinyApp(ui = ui, server = server)  
