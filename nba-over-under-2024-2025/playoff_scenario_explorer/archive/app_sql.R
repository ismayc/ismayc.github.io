library(shiny)
library(tidyverse)
library(DBI)
library(RSQLite)

# Read undecided teams from latest file
teams <- read_rds(
  paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
) |>
  filter(`Outcome Determined` == "not yet") |>
  pull(Team)

# Connect to SQLite DB once when app starts
con <- dbConnect(SQLite(), "scenarios.sqlite")

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
  
  # Dynamic team dropdowns
  output$team_selectors <- renderUI({
    lapply(teams, function(team) {
      selectInput(
        inputId = paste0("team_", gsub(" ", "_", team)),
        label = team,
        choices = c("not yet", "OVER", "UNDER"),
        selected = "not yet"
      )
    })
  })
  
  # Reactive scenario ID builder
  scenario_id <- reactive({
    selected_outcomes <- map_chr(teams, function(team) {
      input[[paste0("team_", gsub(" ", "_", team))]]
    })
    names(selected_outcomes) <- teams
    
    filtered <- selected_outcomes[selected_outcomes != "not yet"]
    if (length(filtered) == 0) return(NULL)
    
    team_labels <- map_chr(names(filtered), ~ str_extract(.x, "\\w+$"))
    label_outcomes <- paste0(team_labels, "_", filtered)
    
    paste(label_outcomes, collapse = "-")
  })
  
  # Query and display results
  output$results_table <- renderTable({
    sid <- scenario_id()
    if (is.null(sid)) {
      return(data.frame(Message = "No scenario selected."))
    }
    
    results <- dbReadTable(con, "scenarios") %>%
      as_tibble() %>%
      filter(scenario == sid) |> 
      select(-scenario)
    
    if (nrow(results) == 0) {
      return(data.frame(Message = "Scenario not found in database."))
    } else {
      return(results)
    }
  })
  
  # Disconnect on session end
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
