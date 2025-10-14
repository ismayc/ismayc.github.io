## Cleaned Shiny App Code Using purrr

# --- Package Setup ----------------------------------------------------------
required_packages <- c("shiny", "readxl", "tidyverse", "kableExtra", "DT", "purrr")
for(pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(shiny)
library(readxl)
library(tidyverse)
library(kableExtra)
library(DT)
library(purrr)

# --- Season & Data Loading --------------------------------------------------
today <- Sys.Date()
current_year <- as.integer(format(today, "%Y"))
current_month <- as.integer(format(today, "%m"))
# For October–December use the current year; for January–April use previous year:
start_year <- ifelse(current_month >= 10, current_year, current_year - 1)
end_year <- start_year + 1
season_year <- paste(start_year, end_year, sep = "-")

# Read picks data from Excel
picks <- read_excel("picks.xlsx", sheet = "picks")
num_players <- 8

# Load determined outcomes from the latest .rds file
# Always try yesterday's file first, fallback to latest available if missing
file_names <- list.files(pattern = "determined_outcomes_.*\\.rds")
dates <- as.Date(gsub("determined_outcomes_(.*)\\.rds", "\\1", file_names))
tz_today <- as.Date(format(Sys.time(), tz = "America/Phoenix", usetz = TRUE))
preferred_file <- paste0("determined_outcomes_", tz_today, ".rds")
backup_file <- paste0("determined_outcomes_", max(dates, na.rm = TRUE) - 1, ".rds")

current_file <- if (file.exists(preferred_file)) preferred_file else backup_file
temp <- read_rds(current_file)

determined <- temp %>% 
  select(Team, `Outcome Determined`) %>% 
  # Use the last word as the team “short” name:
  mutate(Team = str_extract(Team, "\\w+$"))

# Create a version of picks for Chester (extracting the last word of the team)
chester_picks <- picks %>% 
  filter(player == "Chester") %>% 
  mutate(Team = str_extract(team, "\\w+$")) %>% 
  select(Team, choice)

# --- Helper UI Function -----------------------------------------------------
over_under_choice <- function(input_id, choices = c("OVER", "UNDER"), selected = choices[1]) {
  fluidRow(
    tags$head(
      tags$style(HTML("
        label.control-label, .selectize-control.single {
          display: table-cell; 
          text-align: center; 
          vertical-align: middle;
        }
        .form-group { 
          display: table-row;
        }
      "))
    ),
    column(2),
    div(style = "display: inline-block; vertical-align: top; width: 300px;",
        selectInput(
          inputId = input_id,
          label = paste0(
            picks %>% 
              distinct(team) %>% 
              filter(str_detect(team, input_id)) %>% 
              pull(team),
            ":"
          ),
          choices = choices,
          selected = selected,
          width = '35%',
          selectize = FALSE,
          size = 1
        )
    )
  )
}

# --- Team UI Helpers --------------------------------------------------------
# List of all teams
all_teams <- c("Hawks", "Celtics", "Nets", "Hornets", "Bulls", "Cavaliers",
               "Mavericks", "Nuggets", "Pistons", "Warriors", "Rockets", "Pacers",
               "Clippers", "Lakers", "Grizzlies", "Heat", "Bucks", "Timberwolves",
               "Pelicans", "Knicks", "Thunder", "Magic", "76ers", "Suns", "Blazers",
               "Kings", "Spurs", "Raptors", "Jazz", "Wizards")

# A helper function to extract the outcome for a given team
get_outcome <- function(team) {
  determined %>% filter(Team == team) %>% pull(`Outcome Determined`)
}

# Using purrr::keep() to filter teams based on their outcome.
teams_not_determined <- keep(all_teams, ~ get_outcome(.x) == "not yet")
teams_determined     <- keep(all_teams, ~ get_outcome(.x) != "not yet")

# When a team is determined, you might wish to display a different label.
# For example, if you want to show "Trail Blazers" instead of "Blazers":
## (Doesn't work currently to change to "Trail Blazers")
display_name <- function(team, outcome_determined = FALSE) {
  if (team == "Blazers" && outcome_determined) "Blazers" else team
}

# --- UI ---------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(paste("Point Calculations for NBA Over/Under", season_year)),
  h3("Created by Chester Ismay"),
  p(paste("Last updated on", Sys.Date())),
  
  tags$head(
    tags$style(HTML("
      /* Ensure table headers don’t wrap */
      table.dataTable th {
        white-space: nowrap;
        overflow: hidden;
        min-width: 50px;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h5(paste0("Outcome still to be determined (", 
                30 - length(teams_determined), ")")),
      # Build UI elements for teams not determined using purrr::map()
      tagList(map(teams_not_determined, ~ over_under_choice(
        .x,
        selected = chester_picks %>% filter(Team == .x) %>% pull(choice)))),
      br(),
      h5(paste0("Outcome determined (", length(teams_determined), ")")),
      # Build UI elements for determined teams using purrr::map()
      tagList(map(teams_determined, ~ over_under_choice(
        display_name(.x, outcome_determined = TRUE), 
        choices = get_outcome(.x))))
    ),
    
    mainPanel(
      width = 2,
      DTOutput("point_table")
    )
  )
)

# --- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  
  output$point_table <- renderDT({
    
    # Use purrr::map_chr() to loop over team_short values and extract input values.
    chester_probs_df <- picks %>% 
      distinct(team) %>% 
      arrange(team) %>% 
      mutate(team_short = str_extract(team, "\\w+$"),  # Extracts the last word now
             likely_result = map_chr(team_short, ~ {
               val <- input[[.x]]
               if (is.null(val)) "TBD" else val
             }))
    
    
    picks_test <- picks %>% 
      inner_join(chester_probs_df %>% select(team, likely_result), by = "team") %>% 
      mutate(projected_points = case_when(
        (choice == likely_result) & (likely_result != "TBD") ~ wage,
        (choice != likely_result) & (likely_result != "TBD") ~ -wage,
        TRUE ~ 0
      ))
    
    # Helper function to calculate the number of correct picks for a given wager
    compute_correct <- function(df, wage_val, label) {
      df %>% 
        filter(wage == wage_val) %>% 
        mutate(correct = projected_points > 0) %>% 
        group_by(Player = player) %>% 
        summarize(!!label := as.integer(sum(correct, na.rm = TRUE)))
    }
    
    num_correct <- picks_test %>% 
      mutate(correct = projected_points > 0) %>% 
      group_by(Player = player) %>% 
      summarize(`Number of Correct Picks` = as.integer(sum(correct, na.rm = TRUE)))
    
    num_15_correct <- compute_correct(picks_test, 15, "Correct Picks (Wage 15)")
    num_14_correct <- compute_correct(picks_test, 14, "Correct Picks (Wage 14)")
    num_13_correct <- compute_correct(picks_test, 13, "Correct Picks (Wage 13)")
    num_12_correct <- compute_correct(picks_test, 12, "Correct Picks (Wage 12)")
    
    
    final_table <- picks_test %>% 
      group_by(Player = player) %>% 
      summarize(`Points Total` = as.integer(sum(projected_points, na.rm = TRUE))) %>% 
      inner_join(num_correct, by = "Player") %>% 
      inner_join(num_15_correct, by = "Player") %>% 
      inner_join(num_14_correct, by = "Player") %>% 
      inner_join(num_13_correct, by = "Player") %>% 
      inner_join(num_12_correct, by = "Player") %>% 
      arrange(desc(`Points Total`), 
              desc(`Number of Correct Picks`),
              desc(`Correct Picks (Wage 15)`),
              desc(`Correct Picks (Wage 14)`),
              desc(`Correct Picks (Wage 13)`),
              desc(`Correct Picks (Wage 12)`))
    
    # Insert a separator row after the fourth row (for visual grouping)
    separator_row <- data.frame(
      Player = "",  
      `Points Total` = NA,
      `Number of Correct Picks` = NA,
      `Correct Picks (Wage 15)` = NA,
      `Correct Picks (Wage 14)` = NA,
      `Correct Picks (Wage 13)` = NA,
      `Correct Picks (Wage 12)` = NA
    )
    row_numbers <- c("1", "2", "3", "4", NA, as.character(5:nrow(final_table)))
    final_table_with_separator <- bind_rows(
      final_table[1:4, ],
      separator_row,
      final_table[5:nrow(final_table), ]
    ) %>% 
      mutate(`Rank` = row_numbers) %>% 
      rename(
        `Number of<br>Correct Picks` = `Number of Correct Picks`,
        `Correct Picks<br>(Wage 15)` = `Correct Picks (Wage 15)`,
        `Correct Picks<br>(Wage 14)` = `Correct Picks (Wage 14)`,
        `Correct Picks<br>(Wage 13)` = `Correct Picks (Wage 13)`,
        `Correct Picks<br>(Wage 12)` = `Correct Picks (Wage 12)`
      ) %>% 
      select(`Rank`, Player, `Points Total`, `Number of<br>Correct Picks`,
             `Correct Picks<br>(Wage 15)`, `Correct Picks<br>(Wage 14)`, 
             `Correct Picks<br>(Wage 13)`, `Correct Picks<br>(Wage 12)`)
    
    # Render the table using DT with minimal styling
    datatable(
      final_table_with_separator, 
      escape = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = FALSE
      ),         
      rownames = FALSE,
      style = "bootstrap"
    ) %>% formatStyle(
      'Player',
      target = 'row',
      backgroundColor = styleEqual("", "black"),
      color = styleEqual("", "white")
    )
    
  })
  
}

# --- Run the App ------------------------------------------------------------
shinyApp(ui = ui, server = server)
