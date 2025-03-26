library(shiny)
library(tidyverse)

# Read in undecided teams from the latest determined outcomes file
teams <- read_rds(
  paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
) |>
  filter(`Outcome Determined` == "not yet") |>
  pull(Team)

# Define UI
ui <- fluidPage(
  titlePanel("NBA Playoff Probability Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Team Outcomes"),
      uiOutput("team_selectors")
    ),
    mainPanel(
      h4("Playoff Probabilities by Player"),
      tableOutput("results_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Generate dropdowns for each team
  output$team_selectors <- renderUI({
    lapply(teams, function(team) {
      selectInput(inputId = paste0("team_", gsub(" ", "_", team)),
                  label = team,
                  choices = c("not yet", "OVER", "UNDER"),
                  selected = "not yet")
    })
  })
  
  # Reactive to build scenario filename from selected values
  scenario_file <- reactive({
    selected_outcomes <- map_chr(teams, function(team) {
      input[[paste0("team_", gsub(" ", "_", team))]]
    })
    names(selected_outcomes) <- teams
    
    # Filter down to only selected outcomes (ignore not yet)
    filtered <- selected_outcomes[selected_outcomes != "not yet"]
    
    if (length(filtered) == 0) return(NULL)  # no scenario
    
    # Extract last word from team name (e.g., Celtics, Nuggets)
    team_labels <- map_chr(names(filtered), ~ str_extract(.x, "\\w+$"))
    
    # Format like Celtics_OVER, Nuggets_UNDER, etc.
    label_outcomes <- paste0(team_labels, "_", filtered)
    
    # Combine with dashes
    filename <- paste0("whatif_outputs/scenarios_final-", paste(label_outcomes, collapse = "-"), ".rds")
    
    if (file.exists(filename)) return(filename)
    else return(NULL)
  })
  
  # Render table
  output$results_table <- renderTable({
    file <- scenario_file()
    if (is.null(file)) {
      return(data.frame(Message = "No scenario selected or file not found."))
    }
    read_rds(file)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
