library(shiny)
library(tidyverse)
library(readr)

ui <- fluidPage(
  titlePanel("NBA Over/Under 2024-2025 Playoff Scenarios Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("teamToggles")
    ),
    mainPanel(
      h3("Playoff Probabilities Based on Remaining Scenarios"),
      uiOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  
  players <- c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")
  
  choice_col_for <- function(player) paste0(tolower(player), "_choice")
  points_col_for <- function(player) paste0(player, "_points")
  
  picks_wide_new <- read_rds("picks_wide_new.rds") %>% rename(Team = team)
  determined_so_far <- read_rds("determined_so_far_2025_03_31.rds") %>%
    mutate(`Outcome Determined` = as.character(`Outcome Determined`)) %>%
    mutate(`Outcome Determined` = ifelse(`Outcome Determined` == "not yet", "Not Yet", `Outcome Determined`))
  
  picks_joined <- picks_wide_new %>%
    left_join(determined_so_far %>% select(Team, `Outcome Determined`), by = "Team")
  
  team_list <- picks_joined %>%
    filter(!(`Outcome Determined` %in% c("OVER", "UNDER"))) %>%
    pull(Team)
  
  output$teamToggles <- renderUI({
    lapply(sort(team_list), function(tm) {
      radioButtons(
        inputId = paste0("override_", tm),
        label   = tm,
        choices = c("Not Yet", "OVER", "UNDER"),
        inline  = TRUE,
        selected = "Not Yet"
      )
    })
  })
  
  summaryData <- reactive({
    if (length(team_list) == 0) return(tibble(Message = "No Undecided Teams."))
    
    overrides <- vapply(team_list, function(tm) input[[paste0("override_", tm)]], character(1))
    overrides <- ifelse(overrides == "Not Yet", NA, overrides)
    
    picks_local <- picks_joined %>%
      mutate(
        Team = as.character(Team),
        `Outcome Determined` = as.character(`Outcome Determined`),
        override_value = overrides[Team]
      ) %>%
      mutate(`Outcome Determined` = ifelse(!is.na(override_value), override_value, `Outcome Determined`)) %>%
      select(-override_value)
    
    team_names <- picks_local$Team
    num_teams <- length(team_names)
    num_players <- length(players)
    
    pick_matrix <- matrix(NA, nrow = num_teams, ncol = num_players, dimnames = list(team_names, players))
    points_matrix <- matrix(NA, nrow = num_teams, ncol = num_players, dimnames = list(team_names, players))
    
    for (j in seq_along(players)) {
      pick_matrix[, j] <- ifelse(picks_local[[choice_col_for(players[j])]] == "OVER", 1, -1)
      points_matrix[, j] <- picks_local[[points_col_for(players[j])]]
    }
    
    outcome_vector <- ifelse(picks_local$`Outcome Determined` == "OVER", 1,
                             ifelse(picks_local$`Outcome Determined` == "UNDER", -1, 0))
    not_determined_idx <- which(outcome_vector == 0)
    num_not_determined <- length(not_determined_idx)
    
    all_combos <- if (num_not_determined == 0) {
      matrix(nrow = 1, ncol = 0)
    } else {
      expand.grid(rep(list(c(-1, 1)), num_not_determined))
    }
    n_scen <- nrow(all_combos)
    
    scenario_outcomes <- matrix(rep(outcome_vector, times = n_scen), nrow = n_scen, byrow = TRUE)
    if (num_not_determined > 0) {
      scenario_outcomes[, not_determined_idx] <- as.matrix(all_combos)
    }
    
    scenario_ranks <- matrix(NA, nrow = n_scen, ncol = num_players, dimnames = list(NULL, players))
    
    for (i in seq_len(n_scen)) {
      outcome_i <- scenario_outcomes[i, ]
      outcome_mat <- matrix(outcome_i, nrow = num_teams, ncol = num_players)
      
      correct_matrix <- (outcome_mat == pick_matrix)
      correct_15pt_matrix <- correct_matrix & (points_matrix == 15)
      
      score_i <- colSums((outcome_mat * pick_matrix) * points_matrix)
      total_correct_i <- colSums(correct_matrix)
      correct_15pt_i <- colSums(correct_15pt_matrix)
      
      df_rank <- tibble(
        player = players,
        score = score_i,
        correct = total_correct_i,
        correct15 = correct_15pt_i
      ) %>%
        arrange(desc(score), desc(correct), desc(correct15)) %>%
        mutate(rank = row_number())
      
      scenario_ranks[i, ] <- df_rank$rank[match(players, df_rank$player)]
    }
    
    made_playoffs <- scenario_ranks <= 4
    
    summary_df <- tibble(
      player = players,
      median_rank = as.numeric(apply(scenario_ranks, 2, median)),
      mean_rank = as.numeric(round(colMeans(scenario_ranks), 2)),
      highest_rank = as.numeric(apply(scenario_ranks, 2, min)),
      lowest_rank = as.numeric(apply(scenario_ranks, 2, max)),
      num_scenarios = as.integer(n_scen),
      prob_playoffs = as.numeric(round(100 * colMeans(made_playoffs), 2)),
      prob_first = as.numeric(round(100 * colMeans(scenario_ranks == 1), 2))
    ) %>% arrange(mean_rank)
    
    summary_df
  })
  
  output$summaryTable <- renderUI({
    df <- summaryData()
    if ("Message" %in% names(df)) return(HTML(paste0("<p><strong>", df$Message[1], "</strong></p>")))
    headers <- paste0("<tr>", paste(paste0("<th>", names(df), "</th>"), collapse = ""), "</tr>")
    rows <- sapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      is_red <- as.numeric(row$highest_rank) %in% 5:8
      style <- if (is_red) ' style="color:red;"' else ""
      if (i == 5) style <- paste0(style, ' style="border-top:2px solid black;"')
      row_html <- paste(paste0("<td>", as.character(row), "</td>"), collapse = "")
      paste0("<tr", style, ">", row_html, "</tr>")
    })
    table_html <- paste0("<table class='table table-bordered table-condensed'>", headers, paste(rows, collapse = ""), "</table>")
    HTML(table_html)
  })
}

shinyApp(ui, server)
