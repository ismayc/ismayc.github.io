# app.R

library(shiny)
library(tidyverse)
library(readr)
#library(DT)

ui <- fluidPage(
  
  titlePanel("NBA Over/Under 2024-2025 Playoff Scenarios Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("teamToggles")  # We'll dynamically generate toggles here
    ),
    
    mainPanel(
      h3("Playoff Probabilities Based on Remaining Scenarios"),
 #     tableOutput("summaryTable")
      uiOutput("summaryTable")
 
 
    )
  )
)

server <- function(input, output, session) {
  
  #------------------
  # 1) Load your data
  #------------------
  players <- c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")
  
  choice_col_for <- function(player) paste0(tolower(player), "_choice")
  points_col_for <- function(player) paste0(player, "_points")
  
  # Load picks
  picks_wide_new <- read_rds("picks_wide_new.rds") %>%
    rename(Team = team)
  
  # Load outcomes that have been determined so far
  # determined_so_far <- read_rds(
  #   paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
  # )
  # write_rds(determined_so_far, "determined_so_far_2025_03_31.rds")
  
  determined_so_far <- read_rds("determined_so_far_2025_03_31.rds")
  
  # Join to get outcomes with picks
  picks_joined <- picks_wide_new %>%
    left_join(determined_so_far %>% select(Team, `Outcome Determined`),
              by = "Team")
  
  #-------------------------------------------------------------
  # 2) Keep only the teams that haven't been determined yet
  #    i.e. Outcome Determined is NOT "OVER" or "UNDER"
  #-------------------------------------------------------------
  team_list <- picks_joined %>%
    filter(!(`Outcome Determined` %in% c("OVER", "UNDER"))) %>%
    pull(Team)
  
  #---------------------------------------------------
  # 3) Dynamically create radio buttons for each team
  #---------------------------------------------------
  output$teamToggles <- renderUI({
    # For each *undecided* team, create a set of radio buttons:
    lapply(sort(team_list), 
    function(tm) {
      radioButtons(
        inputId = paste0("override_", tm), 
        label   = tm,
        choices = c("Not Yet", "OVER", "UNDER"),
        inline = TRUE,
        selected = "Not Yet"  # Default to "Not Yet"
      )
    })
  })
  
  #---------------------------------------------------
  # 4) Reactive expression that computes the summary
  #    based on manual overrides from the radio buttons
  #---------------------------------------------------
  summaryData <- reactive({

    
    # 1) If there are no teams or if overrides is NULL, skip
    if (length(team_list) == 0) {
      # Return either an empty data frame or some placeholder
      return(tibble(Message = "No Undecided Teams."))
    }
    
    # Gather the user’s overrides and ensure they are a character vector
    overrides <- sapply(team_list, function(tm) {
      input[[paste0("override_", tm)]]
    }, USE.NAMES = TRUE)
    overrides <- unlist(overrides)  
    overrides <- ifelse(overrides == "Not Yet", NA, overrides)
    
    # Ensure picks_joined columns are character and apply overrides
    picks_local <- picks_joined %>%
      mutate(
        Team = as.character(Team),
        `Outcome Determined` = as.character(`Outcome Determined`),
        override_value = as.character(overrides[Team])
      ) %>%
      mutate(
        `Outcome Determined` = ifelse(
          !is.na(override_value),
          override_value,
          `Outcome Determined`
        )
      ) %>%
      select(-override_value)
    
    
    
    #-----------------------------------------------------------------
    # 5) Re-run your scenario logic with the new `Outcome Determined`
    #-----------------------------------------------------------------
    
    team_names <- picks_local$Team
    num_teams <- length(team_names)
    num_players <- length(players)
    
    # Build pick and points matrices
    pick_matrix <- matrix(NA, nrow = num_teams, ncol = num_players, 
                          dimnames = list(team_names, players))
    points_matrix <- matrix(NA, nrow = num_teams, ncol = num_players,
                            dimnames = list(team_names, players))
    
    for (j in seq_along(players)) {
      pick_matrix[, j] <- ifelse(picks_local[[choice_col_for(players[j])]] == "OVER", 1, -1)
      points_matrix[, j] <- picks_local[[points_col_for(players[j])]]
    }
    
    # Create outcome vector (+1, -1, or 0)
    outcome_vector <- ifelse(
      picks_local$`Outcome Determined` == "OVER", 1,
      ifelse(picks_local$`Outcome Determined` == "UNDER", -1, 0)
    )
    
    # Identify undecided teams
    not_determined_idx <- which(outcome_vector == 0)
    num_not_determined <- length(not_determined_idx)
    
    if (num_not_determined == 0) {
      # If there are no undecided teams, there's only 1 scenario
      all_combos <- matrix(nrow = 1, ncol = 0)
    } else {
      # Generate all combinations for the not-yet-determined teams
      all_combos <- expand.grid(rep(list(c(-1, 1)), num_not_determined))
    }
    
    n_scen <- nrow(all_combos)
    
    scenario_outcomes <- matrix(rep(outcome_vector, times = n_scen), 
                                nrow = n_scen, byrow = TRUE)
    if (num_not_determined > 0) {
      scenario_outcomes[, not_determined_idx] <- as.matrix(all_combos)
    }
    
    # Broadcasting arrays
    n_teams <- ncol(scenario_outcomes)
    arr_outcome <- array(rep(scenario_outcomes, times = num_players),
                         dim = c(n_scen, n_teams, num_players))
    
    arr_picks <- array(rep(pick_matrix, each = n_scen),
                       dim = c(n_scen, n_teams, num_players))
    
    arr_points <- array(rep(points_matrix, each = n_scen),
                        dim = c(n_scen, n_teams, num_players))
    
    # Multiply elementwise
    arr_scores <- arr_outcome * arr_picks * arr_points
    
    # Compute scenario scores
    scores <- apply(arr_scores, c(1, 3), sum)
    colnames(scores) <- players
    
    # Compute rankings
    scenario_ranks <- t(apply(scores, 1, function(x) rank(-x, ties.method = "min")))
    made_playoffs <- scenario_ranks <= 4
    
    # Summarize
    summary_df <- data.frame(
      player        = players,
      median_rank   = apply(scenario_ranks, 2, median),
      mean_rank     = round(colMeans(scenario_ranks), 2),
      highest_rank  = apply(scenario_ranks, 2, min),
      lowest_rank   = apply(scenario_ranks, 2, max),
      num_scenarios = n_scen,
      prob_playoffs = round(100 * colMeans(made_playoffs), 2),
      prob_first    = round(100 * colMeans(scenario_ranks == 1), 2)
    ) %>% 
      arrange(mean_rank)
    
    rownames(summary_df) <- NULL
    summary_df
  })
  
  #------------------------------------------------------
  # 6) Render the summary table in the main panel
  #------------------------------------------------------
  # output$summaryTable <- renderTable({
  #   summaryData()
  # })
  output$summaryTable <- renderUI({
    df <- summaryData()
    
    # If it's just a message, return it as a simple paragraph
    if ("Message" %in% names(df)) {
      return(HTML(paste0("<p><strong>", df$Message[1], "</strong></p>")))
    }
    
    # Create the header row
    headers <- paste0("<tr>", paste(paste0("<th>", names(df), "</th>"), collapse = ""), "</tr>")
    
    # Create the rows with optional red text and horizontal line
    rows <- sapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      is_red <- as.numeric(row$highest_rank) %in% c(5, 6, 7, 8)
      style <- if (is_red) ' style="color:red;"' else ""
      
      # Add a top border to the 5th row (i == 5)
      if (i == 5) {
        style <- paste0(style, ' style="border-top:2px solid black;"')
      }
      
      row_html <- paste(paste0("<td>", as.character(row), "</td>"), collapse = "")
      paste0("<tr", style, ">", row_html, "</tr>")
    })
    
    # Combine all rows into an HTML table
    table_html <- paste0("<table class='table table-bordered table-condensed'>", headers, paste(rows, collapse = ""), "</table>")
    
    HTML(table_html)
  })
  
  
  
}

shinyApp(ui, server)
