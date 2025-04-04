library(shiny)
library(tidyverse)
library(readr)

players <- c("Adonis", "Andy", "Chester", "Jake", "Mary", "Mike", "Phil", "Ryan")

choice_col_for <- function(player) paste0(tolower(player), "_choice")
points_col_for <- function(player) paste0(player, "_points")

picks_wide_new <- read_rds("picks_wide_new.rds") %>% rename(Team = team)

todays_determined <- paste0("determined_outcomes_", Sys.Date(), ".rds")

if (!file.exists(todays_determined)) {
  if (file.exists(paste0("../over-under-points-calculator/", todays_determined))) {
    file.copy(paste0("../over-under-points-calculator/", todays_determined),
              todays_determined)
  } else {
    todays_determined <- paste0("determined_outcomes_", Sys.Date() - 1, ".rds")
  }
}

determined_so_far <- read_rds(todays_determined) %>%
  mutate(`Outcome Determined` = as.character(`Outcome Determined`)) %>%
  mutate(`Outcome Determined` = ifelse(
    `Outcome Determined` == "not yet", 
    "Not Yet", 
    `Outcome Determined`))

picks_joined <- picks_wide_new %>%
  left_join(determined_so_far %>% select(Team, `Outcome Determined`), 
            by = "Team")  |> 
  mutate(`Outcome Determined` = if_else(str_detect(Team, "Wizards"), "UNDER", 
                                        `Outcome Determined`))

team_list <- picks_joined %>%
  filter(!(`Outcome Determined` %in% c("OVER", "UNDER"))) %>%
  pull(Team)

ui <- fluidPage(
  titlePanel("NBA Over/Under 2024-2025 Playoff Scenarios Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("reset_btn", "Reset All to Not Yet"),
      br(), br(),
      uiOutput("teamToggles")
    ),
    
    mainPanel(
      h3("Playoff Probabilities Based on Remaining Scenarios"),
      h5("Last updated on", Sys.Date()),
      br(),
      textOutput("numScenariosText"),
      br(),
      uiOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  
  refresh_ui <- reactiveVal(0)
  
  
  # Store override states for each team
  overrides <- reactiveValues()
  
  # Dynamically render radio buttons using override state
  output$teamToggles <- renderUI({
    refresh_ui()  # depend on this trigger to force re-render
    lapply(sort(team_list), function(tm) {
      selected_val <- isolate(overrides[[tm]])
      if (is.null(selected_val)) selected_val <- "Not Yet"
      radioButtons(
        inputId = paste0("override_", tm),
        label = tm,
        choices = c("Not Yet", "OVER", "UNDER"),
        inline = TRUE,
        selected = selected_val
      )
    })
  })
  
  
  # Observe individual radio button inputs and store in reactiveValues
  observe({
    lapply(team_list, function(tm) {
      observeEvent(input[[paste0("override_", tm)]], {
        overrides[[tm]] <- input[[paste0("override_", tm)]]
      }, ignoreNULL = TRUE)
    })
  })
  
  # Reset all team overrides to "Not Yet"
  observeEvent(input$reset_btn, {
    lapply(team_list, function(tm) {
      overrides[[tm]] <- "Not Yet"
    })
    refresh_ui(refresh_ui() + 1)  # trigger UI re-render
  })
  
  
  # Summary reactive based on current overrides
  summaryData <- reactive({
    if (length(team_list) == 0) return(tibble(Message = "No Undecided Teams."))
    
    overrides_list <- sapply(team_list, function(tm) {
      val <- overrides[[tm]]
      if (is.null(val)) "Not Yet" else val
    }, USE.NAMES = TRUE)
    overrides_clean <- ifelse(overrides_list == "Not Yet", NA, overrides_list)
    
    picks_local <- picks_joined %>%
      mutate(
        Team = as.character(Team),
        `Outcome Determined` = as.character(`Outcome Determined`),
        override_value = overrides_clean[Team]
      ) %>%
      mutate(`Outcome Determined` = ifelse(!is.na(override_value), override_value, `Outcome Determined`)) %>%
      select(-override_value)
    
    team_names <- picks_local$Team
    num_teams <- length(team_names)
    num_players <- length(players)
    
    pick_matrix <- sapply(players, function(player) {
      ifelse(picks_local[[choice_col_for(player)]] == "OVER", 1, -1)
    })
    points_matrix <- sapply(players, function(player) {
      picks_local[[points_col_for(player)]]
    })
    
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
    
    scenario_ranks <- matrix(NA_integer_, nrow = n_scen, ncol = num_players, dimnames = list(NULL, players))
    
    for (i in seq_len(n_scen)) {
      outcome_i <- outcome_vector
      if (num_not_determined > 0) {
        outcome_i[not_determined_idx] <- as.integer(all_combos[i, ])
      }
      
      correct_matrix <- sweep(pick_matrix, 1, outcome_i, `==`)
      correct_15pt_matrix <- correct_matrix & (points_matrix == 15)
      
      score_i <- colSums((pick_matrix * outcome_i) * points_matrix)
      total_correct_i <- colSums(correct_matrix)
      correct_15pt_i <- colSums(correct_15pt_matrix)
      
      ord <- order(-score_i, -total_correct_i, -correct_15pt_i)
      scenario_ranks[i, ord] <- seq_along(ord)
    }
    
    made_playoffs <- scenario_ranks <= 4
    
    summary_df <- tibble(
      player = players,
      median_rank = as.numeric(apply(scenario_ranks, 2, median)),
      mean_rank = as.numeric(round(colMeans(scenario_ranks), 2)),
      highest_rank = as.numeric(apply(scenario_ranks, 2, min)),
      lowest_rank = as.numeric(apply(scenario_ranks, 2, max)),
      prob_playoffs = as.numeric(round(100 * colMeans(made_playoffs), 2)),
      prob_first = as.numeric(round(100 * colMeans(scenario_ranks == 1), 2))
    ) %>% arrange(mean_rank)
    
    attr(summary_df, "num_scenarios") <- n_scen
    summary_df
  })
  
  output$numScenariosText <- renderText({
    df <- summaryData()
    if ("Message" %in% names(df)) return("")
    num_scenarios <- attr(df, "num_scenarios")
    if (!is.null(num_scenarios)) {
      paste0("Number of scenarios simulated (2^", log(num_scenarios, base = 2), "): ", num_scenarios)
    } else {
      ""
    }
  })
  
  output$summaryTable <- renderUI({
    df <- summaryData()
    if ("Message" %in% names(df)) return(HTML(paste0("<p><strong>", df$Message[1], "</strong></p>")))
    headers <- paste0("<tr>", paste(paste0("<th>", names(df), "</th>"), collapse = ""), "</tr>")
    rows <- sapply(seq_len(nrow(df)), function(i) {
      row <- df[i, ]
      is_red <- as.numeric(row$highest_rank) %in% 5:8
      is_green <- as.numeric(row$prob_playoffs) == 100
      is_gold <- as.numeric(row$prob_first) == 100
      style <- if (is_gold) ' style="background-color:#fff3cd;"' else if (is_green) ' style="background-color:#d4edda;"' else if (is_red) ' style="color:red;"' else ""
      if (i == 5) style <- paste0(style, ' style="border-top:2px solid black;"')
      row_html <- paste(paste0("<td>", as.character(row), "</td>"), collapse = "")
      paste0("<tr", style, ">", row_html, "</tr>")
    })
    table_html <- paste0("<table class='table table-bordered table-condensed'>", headers, paste(rows, collapse = ""), "</table>")
    HTML(table_html)
  })
}

shinyApp(ui, server)
