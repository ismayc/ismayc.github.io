library(shiny)
library(tidyverse)

# 1) Load picks_matrix built offline
picks_matrix <- readRDS("picks_matrix.rds")

# 2) Identify the unique teams from rownames
#    rownames look like "Boston Celtics_OVER", "Boston Celtics_UNDER", ...
#    We'll parse them to get a unique set of "Boston Celtics" for the dropdown
all_row_names <- rownames(picks_matrix)
all_teams <- unique(sub("^(.+)_(OVER|UNDER)$", "\\1", all_row_names))

# If we also had 'determined_outcomes' logic, we'd do that here:
teams <- read_rds(
  paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
) |> 
  filter(`Outcome Determined` == "not yet") |> 
  pull(Team)

player_names <- colnames(picks_matrix)

# Define UI
ui <- fluidPage(
  titlePanel("NBA Playoff Probability Explorer (Matrix Summation)"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Team Outcomes"),
      uiOutput("team_selectors"),
      actionButton("computeBtn", "Compute Scenario")
    ),
    mainPanel(
      h4("Playoff Probabilities by Player"),
      tableOutput("results_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # 3) Create dropdowns for each not-yet-determined team
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
  
  # 4) Summation on demand
  scenario_df <- eventReactive(input$computeBtn, {
    # Build a vector: for each team, user picks "OVER", "UNDER", or "not yet"
    selected_outcomes <- map_chr(teams, function(team) {
      input[[paste0("team_", gsub(" ", "_", team))]]
    })
    names(selected_outcomes) <- teams
    
    # Filter out "not yet"
    locked <- selected_outcomes[selected_outcomes != "not yet"]
    if (length(locked) == 0) {
      return(NULL)
    }
    
    # For each locked team, we find the row in picks_matrix
    # e.g. rowname = "Boston Celtics_OVER"
    row_indices <- c()
    for (nm in names(locked)) {
      row_name <- paste0(nm, "_", locked[[nm]])
      if (!row_name %in% rownames(picks_matrix)) {
        # handle error or skip
        next
      }
      row_indices <- c(row_indices, row_name)
    }
    
    if (length(row_indices) == 0) {
      return(NULL)
    }
    
    # colSums across these rows
    # This is the total points for each player under the scenario
    scenario_points <- colSums(picks_matrix[row_indices, , drop = FALSE])
    # scenario_points is a numeric named vector, names = c("Adonis", "Andy", "Chester", ...)
    
    # 5) Compute rank or playoff qualification
    # Example: order from highest to lowest points
    # Ties: we do a simple rank logic or dense_rank
    df <- tibble(
      player = names(scenario_points),
      total_points = as.numeric(scenario_points)
    )
    
    # Sort descending by total_points
    df <- df %>% arrange(desc(total_points))
    
    # Compute rank. If you want 1-based rank with 1= best, do:
    df <- df %>%
      mutate(rank = row_number()) %>%
      mutate(playoffs = if_else(rank <= 4, TRUE, FALSE))
    
    df
  })
  
  # 6) Show results in a table
  output$results_table <- renderTable({
    df <- scenario_df()
    if (is.null(df)) {
      return(data.frame(Message = "No scenario selected"))
    }
    df
  })
}

# Run the app
shinyApp(ui = ui, server = server)
