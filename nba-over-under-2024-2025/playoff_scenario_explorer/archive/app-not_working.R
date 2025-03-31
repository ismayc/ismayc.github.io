library(shiny)
library(tidyverse)
library(readr)

ui <- fluidPage(
  titlePanel("NBA Over/Under 2024-2025 Playoff Scenarios Explorer"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("teamToggles")  # We'll dynamically generate toggles here
    ),
    mainPanel(
      h3("Playoff Probabilities Based on Remaining Scenarios"),
      uiOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  
  #------------------
  # 1) Load your data
  #------------------
  players <- c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")
  
  # Helper: pick and points column names for a given player
  choice_col_for <- function(player) paste0(tolower(player), "_choice")
  points_col_for <- function(player) paste0(player, "_points")
  
  # Read picks
  picks_wide_new <- read_rds("picks_wide_new.rds") %>%
    rename(Team = team)
  
  # Read determined outcomes
  determined_so_far <- read_rds("determined_so_far_2025_03_31.rds")
  
  # Join picks with partial outcomes
  picks_joined <- picks_wide_new %>%
    left_join(determined_so_far %>% select(Team, `Outcome Determined`), by = "Team")
  
  #-------------------------------------------------------------
  # 2) Keep only the teams that haven't been determined yet
  #    i.e. 'Outcome Determined' != 'OVER' or 'UNDER'
  #-------------------------------------------------------------
  team_list <- picks_joined %>%
    filter(!(`Outcome Determined` %in% c("OVER", "UNDER"))) %>%
    pull(Team)
  
  #---------------------------------------------------
  # 3) Dynamically create radio buttons for each team
  #---------------------------------------------------
  output$teamToggles <- renderUI({
    # For each *undecided* team, create a set of radio buttons:
    lapply(sort(team_list), function(tm) {
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
  #    based on manual overrides
  #---------------------------------------------------
  summaryData <- reactive({
    # If no undecided teams, short-circuit
    if (length(team_list) == 0) {
      return(tibble(Message = "No Undecided Teams."))
    }
    
    # Gather overrides (our radio button states)
    overrides <- sapply(team_list, function(tm) {
      input[[paste0("override_", tm)]]
    }, USE.NAMES = TRUE)
    overrides <- unlist(overrides)
    overrides <- ifelse(overrides == "Not Yet", NA, overrides)
    
    # Apply overrides to local copy
    picks_local <- picks_joined %>%
      mutate(
        Team                 = as.character(Team),
        `Outcome Determined` = as.character(`Outcome Determined`),
        override_value       = as.character(overrides[Team])
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
    # 5) Re-run scenario logic with the updated 'Outcome Determined'
    #-----------------------------------------------------------------
    team_names  <- picks_local$Team
    num_teams   <- length(team_names)
    num_players <- length(players)
    
    # Build pick and points matrices
    pick_matrix   <- matrix(NA, nrow = num_teams, ncol = num_players, 
                            dimnames = list(team_names, players))
    points_matrix <- matrix(NA, nrow = num_teams, ncol = num_players,
                            dimnames = list(team_names, players))
    
    for (j in seq_along(players)) {
      pick_matrix[, j]   <- ifelse(picks_local[[choice_col_for(players[j])]] == "OVER", 1, -1)
      points_matrix[, j] <- picks_local[[points_col_for(players[j])]]
    }
    
    # Determine which outcomes are known (+1 or -1) vs. undetermined (0)
    outcome_vector <- ifelse(
      picks_local$`Outcome Determined` == "OVER", 1,
      ifelse(picks_local$`Outcome Determined` == "UNDER", -1, 0)
    )
    
    not_determined_idx <- which(outcome_vector == 0)
    num_not_determined <- length(not_determined_idx)
    
    # Generate all combos for the not-yet-determined teams
    if (num_not_determined == 0) {
      # If no teams are undetermined, there's exactly 1 scenario
      all_combos <- matrix(nrow = 1, ncol = 0)
    } else {
      all_combos <- expand.grid(rep(list(c(-1, 1)), num_not_determined))
    }
    
    n_scen <- nrow(all_combos)
    
    # Fill scenario_outcomes
    scenario_outcomes <- matrix(rep(outcome_vector, times = n_scen),
                                nrow = n_scen, byrow = TRUE)
    if (num_not_determined > 0) {
      scenario_outcomes[, not_determined_idx] <- as.matrix(all_combos)
    }
    
    # Convert scenario outcomes into 3D array for broadcasting
    arr_outcome <- array(
      rep(scenario_outcomes, times = num_players),
      dim = c(n_scen, num_teams, num_players)
    )
    arr_picks <- array(
      rep(pick_matrix, each = n_scen),
      dim = c(n_scen, num_teams, num_players)
    )
    arr_points <- array(
      rep(points_matrix, each = n_scen),
      dim = c(n_scen, num_teams, num_players)
    )
    
    # scenario scores: outcome * pick * points
    arr_scores <- arr_outcome * arr_picks * arr_points
    scores     <- apply(arr_scores, c(1, 3), sum)   # shape: [n_scen, num_players]
    colnames(scores) <- players
    
    # Determine correctness: 1 if arr_outcome == arr_picks
    arr_correct    <- ifelse(arr_outcome == arr_picks, 1, 0)
    arr_correct_15 <- arr_correct * ifelse(arr_points == 15, 1, 0)
    
    # Totals per scenario & player
    total_correct <- apply(arr_correct, c(1, 3), sum)    # [n_scen, num_players]
    correct_15pt  <- apply(arr_correct_15, c(1, 3), sum)
    
    #-----------------------------------------
    # Compute tie-break-based ranks per scenario
    #-----------------------------------------
    scenario_ranks <- matrix(NA, nrow = n_scen, ncol = num_players,
                             dimnames = list(NULL, players))
    
    for (i in seq_len(n_scen)) {
      df_tie <- tibble(
        player    = players,
        score     = scores[i, ],
        correct   = total_correct[i, ],
        correct15 = correct_15pt[i, ]
      ) %>%
        arrange(desc(score), desc(correct), desc(correct15)) %>%
        mutate(rank = row_number())
      
      # Map each player's rank back to the scenario row
      for (p in df_tie$player) {
        scenario_ranks[i, p] <- df_tie$rank[df_tie$player == p]
      }
    }
    
    # Identify who made the playoffs in each scenario
    made_playoffs <- scenario_ranks <= 4
    
    #-----------------------------------------
    # Summarize across all scenarios
    #-----------------------------------------
    summary_df <- tibble(
      player        = players,
      median_rank   = apply(scenario_ranks, 2, median),
      mean_rank     = round(colMeans(scenario_ranks), 2),
      highest_rank  = apply(scenario_ranks, 2, min),
      lowest_rank   = apply(scenario_ranks, 2, max),
      num_scenarios = n_scen,
      prob_playoffs = round(100 * colMeans(made_playoffs), 2),
      prob_first    = round(100 * colMeans(scenario_ranks == 1), 2),
      total_correct = round(colMeans(total_correct)),
      correct_15pt  = round(colMeans(correct_15pt))
    ) %>% 
      arrange(mean_rank)
    
    summary_df
  })
  
  #------------------------------------------------------
  # 6) Render the summary table in the main panel
  #------------------------------------------------------
  output$summaryTable <- renderUI({
    df <- summaryData()
    
    # If there's just a message, display it
    if ("Message" %in% names(df)) {
      return(HTML(paste0("<p><strong>", df$Message[1], "</strong></p>")))
    }
    
    # Build the table header
    headers <- paste0(
      "<tr>",
      paste(paste0("<th>", names(df), "</th>"), collapse = ""),
      "</tr>"
    )
    
    # Build each row
    rows <- sapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      # Mark in red if highest_rank >= 5
      is_red <- as.numeric(row$highest_rank) %in% c(5, 6, 7, 8)
      style  <- if (is_red) ' style="color:red;"' else ""
      
      # Insert a top border for row 5
      if (i == 5) {
        style <- paste0(style, ' style="border-top:2px solid black;"')
      }
      
      row_html <- paste(
        paste0("<td>", as.character(row), "</td>"),
        collapse = ""
      )
      paste0("<tr", style, ">", row_html, "</tr>")
    })
    
    table_html <- paste0(
      "<table class='table table-bordered table-condensed'>",
      headers,
      paste(rows, collapse = ""),
      "</table>"
    )
    
    HTML(table_html)
  })
}

shinyApp(ui, server)
