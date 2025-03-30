# app.R

library(shiny)
library(tidyverse)
library(readr)

ui <- fluidPage(
  
  titlePanel("Over/Under Playoff Scenarios Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("teamToggles")  # We'll dynamically generate toggles here
    ),
    
    mainPanel(
      h3("Summary Table"),
      tableOutput("summaryTable")
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
  
  # Load outcomes
  determined_so_far <- read_rds(
    paste0("../over-under-points-calculator/determined_outcomes_", Sys.Date(), ".rds")
  )
  
  # Join to get outcomes with picks
  picks_joined <- picks_wide_new %>%
    left_join(determined_so_far %>% select(Team, `Outcome Determined`),
              by = "Team")
  
  #-------------------------------------------------------------
  # 2) Keep only the 13 teams that haven't been determined yet
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
    lapply(team_list, function(tm) {
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
    
    # Gather the user’s overrides
    overrides <- sapply(team_list, function(tm) {
      input[[paste0("override_", tm)]]
    }, USE.NAMES = TRUE)
    
    # Convert "Not Yet" to NA (meaning no override)
    overrides <- ifelse(overrides == "Not Yet", NA, overrides)
    
    picks_joined <- picks_joined %>%
      # Convert from factor to character so they're compatible
      mutate(
        Team              = as.character(Team),
        `Outcome Determined` = as.character(`Outcome Determined`)
      )
    
    
    # Apply overrides only to the teams in team_list
    picks_local <- picks_joined %>%
      mutate(
        # 1) Pull out the override value for each row's Team into a new column
        override_value = overrides[Team]
      ) %>%
      mutate(
        # 2) Use if_else() on that new column
        `Outcome Determined` = if_else(
          !is.na(override_value),
          override_value,
          `Outcome Determined`
        )
      ) %>%
      select(-override_value)  # optional: remove temp column
    
    
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
      mean_rank     = colMeans(scenario_ranks),
      highest_rank  = apply(scenario_ranks, 2, min),
      lowest_rank   = apply(scenario_ranks, 2, max),
      num_sims      = n_scen,
      prob_playoffs = 100 * colMeans(made_playoffs),
      prob_first    = 100 * colMeans(scenario_ranks == 1)
    ) %>% 
      arrange(median_rank)
    
    rownames(summary_df) <- NULL
    summary_df
  })
  
  #------------------------------------------------------
  # 6) Render the summary table in the main panel
  #------------------------------------------------------
  output$summaryTable <- renderTable({
    summaryData()
  })
}

shinyApp(ui, server)
