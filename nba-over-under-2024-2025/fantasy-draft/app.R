source("00-prior.R")

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("2024-2025 Fantasy Draft Board"),
  
  tabsetPanel(
    tabPanel("Draft Board",
             # Display current pick at the top
             fluidRow(
               column(12, h4(htmlOutput("current_pick_label")))
             ),
             
             # Text Input, Assign, and Undo Buttons
             fluidRow(
               column(3, 
                      selectizeInput("player_select", "Type a Player's Name:", choices = NULL, selected = NULL, multiple = FALSE)
               ),
               column(9, 
                      br(),
                      div(
                        style = "display: flex; align-items: center; gap: 10px;",
                        actionButton("assign_button", "Assign Player"),
                        actionButton("undo_button", "Undo Last Pick")
                      )
               )
             ),
             
             # Add export and download button
             fluidRow(
               column(3,
                      strong("Export Draft:"),
                      br(),
                      actionButton("export_button", "Export Draft Results"),
                      downloadButton("download_csv", "Download CSV of Results")
               ),
               # Add file input for importing draft and an import button
               column(9,
                      column(4,
                      fileInput("import_draft", "Import Previous Draft (CSV)", accept = c(".csv"))
                      ),
                      br(),
                      column(5, 
                      actionButton("import_button", "Import Draft")
                      )
               )
             ),
             
             # Main Layout: Three Columns for Available Players and Draft Board
             fluidRow(
               column(4, h3("Guards"), div(id = "available_guards", uiOutput("guards_ui"))),
               column(4, h3("Wings"), div(id = "available_wings", uiOutput("wings_ui"))),
               column(4, h3("Posts"), div(id = "available_posts", uiOutput("posts_ui")))
             ),
             
             # Draft Board
             fluidRow(
               column(12, 
                      lapply(1:10, function(round) {
                        div(
                          style = "margin-bottom: 20px;",
                          column(12,
                                 div(
                                   style = "display: flex; justify-content: space-between; align-items: center;",
                                   h4(paste("Round", round)),
                                   actionButton(inputId = paste0("toggle_round_", round), label = "Hide/Show Picks", style = "margin-left: 10px;")
                                 ),
                                 div(
                                   id = paste0("round_content_", round),
                                   style = "margin-top: 10px;",
                                   div(
                                     style = "display: flex; flex-direction: row; justify-content: space-between;",
                                     lapply(if (round %% 2 == 0) rev(1:6) else 1:6, function(pick_in_round) {
                                       pick_number <- (round - 1) * 6 + pick_in_round
                                       fantasy_player <- snake_order$player[pick_number]
                                       div(
                                         id = paste0("pick_", pick_number),
                                         style = "min-height: 50px; border: 1px solid black; margin: 5px; padding: 5px; width: 20%; font-size: 12px; text-align: center;",
                                         div(style = "font-weight: bold;", paste("Pick", pick_number)),
                                         div(style = "font-size: 10px; color: grey;", paste("Player:", fantasy_player)),
                                         div(id = paste0("assigned_player_", pick_number), style = "font-size: 12px; color: black;")
                                       )
                                     })
                                   )
                                 )
                          )
                        )
                      })
               )
             )
    ),
    tabPanel("Selections",
             fluidRow(
               column(12, h3("Selections by Fantasy Player")),
               uiOutput("selections_ui")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value for available players
  available_players <- reactiveVal(players)
  
  # Reactive value to track the current pick
  current_pick <- reactiveVal(1)
  
  # Reactive value to store assigned players
  assigned_players <- reactiveVal(data.frame(
    pick = integer(0), 
    round = integer(0), 
    name = character(0), 
    position = character(0), 
    fantasy_player = character(0), 
    stringsAsFactors = FALSE
  ))
  
  # Reactive value to track the visibility of rounds
  round_visibility <- reactiveVal(rep(TRUE, 10))
  
  # Toggle visibility for each round
  lapply(1:10, function(round) {
    observeEvent(input[[paste0("toggle_round_", round)]], {
      visibility <- round_visibility()
      if (visibility[round]) {
        shinyjs::hide(id = paste0("round_content_", round))
      } else {
        shinyjs::show(id = paste0("round_content_", round))
      }
      visibility[round] <- !visibility[round]
      round_visibility(visibility)
    })
  })
  
  
  # Reactive value for category counts
  category_counts <- reactiveVal(
    data.frame(
      fantasy_player = unique(snake_order$player),
      Guard = 0,
      Wing = 0,
      Post = 0,
      stringsAsFactors = FALSE
    )
  )
  
  # Update current pick label
  output$current_pick_label <- renderText({
    pick <- current_pick()
    if (pick <= 60) {
      HTML(paste("<i>Current Pick</i>:", "Pick", pick, "-", "Player:", snake_order$player[pick]))
    } else {
      "Draft Complete. Best of luck to you all on a great season!"
    }
  })
  
  # Update Selectize Input
  observe({
    available <- available_players()
    if (nrow(available) > 0) {
      # Sort available players alphabetically by name
      sorted_names <- available$name |> sort()
      updateSelectizeInput(session, "player_select", 
                           choices = sorted_names, 
                           selected = "",  # Ensure no default selection
                           server = TRUE)
    } else {
      updateSelectizeInput(session, "player_select", 
                           choices = NULL, 
                           selected = "",  # Ensure it resets to blank
                           server = TRUE)
    }
  })
  
  
  # Update Available Players UI
  output$guards_ui <- renderUI({
    available <- available_players()
    guard_players <- available[available$position == "Guard", ]
    fantasy_player <- snake_order$player[current_pick()]
    counts <- category_counts()
    
    max_reached <- if (!is.null(counts) && nrow(counts) > 0 && fantasy_player %in% counts$fantasy_player) {
      counts[counts$fantasy_player == fantasy_player, "Guard"] >= 4
    } else {
      FALSE
    }
    
    div(
      style = "display: flex; flex-wrap: wrap; gap: 2px;",
      lapply(1:nrow(guard_players), function(i) {
        div(
          style = paste0("margin: 0px; padding: 0px; background-color: ", 
                         if (isTRUE(max_reached)) "lightgray" else guard_players$color[i], 
                         "; color: ", 
                         if (isTRUE(max_reached)) "darkgray" else guard_players$text_color[i], 
                         "; border-radius: 5px; font-size: 12px; text-align: center; width: 175px; height: 25px;"),
          guard_players$name[i]
        )
      })
    )
  })
  
  
  output$wings_ui <- renderUI({
    available <- available_players()
    wing_players <- available[available$position == "Wing", ]
    fantasy_player <- snake_order$player[current_pick()]
    counts <- category_counts()
    
    max_reached <- if (!is.null(counts) && nrow(counts) > 0 && fantasy_player %in% counts$fantasy_player) {
      counts[counts$fantasy_player == fantasy_player, "Wing"] >= 4
    } else {
      FALSE
    }
    
    div(
      style = "display: flex; flex-wrap: wrap; gap: 2px;",
      lapply(1:nrow(wing_players), function(i) {
        div(
          style = paste0("margin: 0px; padding: 0px; background-color: ", 
                         if (isTRUE(max_reached)) "lightgray" else wing_players$color[i], 
                         "; color: ", 
                         if (isTRUE(max_reached)) "darkgray" else wing_players$text_color[i], 
                         "; border-radius: 5px; font-size: 12px; text-align: center; width: 175px; height: 25px;"),
          wing_players$name[i]
        )
      })
    )
  })
  
  output$posts_ui <- renderUI({
    available <- available_players()
    post_players <- available[available$position == "Post", ]
    fantasy_player <- snake_order$player[current_pick()]
    counts <- category_counts()
    
    max_reached <- if (!is.null(counts) && nrow(counts) > 0 && fantasy_player %in% counts$fantasy_player) {
      counts[counts$fantasy_player == fantasy_player, "Post"] >= 4
    } else {
      FALSE
    }
    
    div(
      style = "display: flex; flex-wrap: wrap; gap: 2px;",
      lapply(1:nrow(post_players), function(i) {
        div(
          style = paste0("margin: 0px; padding: 0px; background-color: ", 
                         if (isTRUE(max_reached)) "lightgray" else post_players$color[i], 
                         "; color: ", 
                         if (isTRUE(max_reached)) "darkgray" else post_players$text_color[i], 
                         "; border-radius: 5px; font-size: 12px; text-align: center; width: 175px; height: 25px;"),
          post_players$name[i]
        )
      })
    )
  })
  
  # Assign Player Button Logic
  observeEvent(input$assign_button, {
    selected_player <- input$player_select
    all_players <- available_players()
    pick <- current_pick()
    fantasy_player <- snake_order$player[pick]
    
    if (selected_player %in% all_players$name && pick <= 60) {
      player_info <- all_players[all_players$name == selected_player, ]
      position <- player_info$position
      
      # Check category limit
      counts <- category_counts()
      if (counts[counts$fantasy_player == fantasy_player, position] >= 4) {
        showModal(modalDialog(
          title = "Category Limit Reached",
          paste("You cannot select more than 4 players from", position, "category.")
        ))
        return()
      }
      
      # Update category count
      counts[counts$fantasy_player == fantasy_player, position] <- 
        counts[counts$fantasy_player == fantasy_player, position] + 1
      category_counts(counts)
      
      # Assign player logic (existing)
      shinyjs::html(
        id = paste0("assigned_player_", pick),
        html = paste0("<div style='background-color:", player_info$color, 
                      "; color:", player_info$text_color, "; padding: 5px; border-radius: 5px;'>",
                      selected_player, "</div>")
      )
      
      # Add player to assigned list
      assigned_list <- assigned_players()
      assigned_players(rbind(assigned_list, data.frame(
        pick = pick,
        round = snake_order$round[pick],
        name = selected_player,
        position = position,
        fantasy_player = fantasy_player
      )))
      
      # Remove player from available list
      updated_players <- all_players[all_players$name != selected_player, ]
      available_players(updated_players)
      
      # Move to the next pick
      current_pick(pick + 1)
    } else {
      showModal(modalDialog(
        title = "Invalid Action",
        "Please select a valid player or ensure the draft isn't complete."
      ))
    }
  })
  
  # Undo Button Logic
  observeEvent(input$undo_button, {
    assigned_list <- assigned_players()
    if (nrow(assigned_list) > 0) {
      # Get the last assigned player
      last_assigned <- tail(assigned_list, 1)
      last_pick <- last_assigned$pick
      last_player <- last_assigned$name
      last_position <- last_assigned$position
      last_fantasy_player <- last_assigned$fantasy_player
      
      # Update category count
      counts <- category_counts()
      counts[counts$fantasy_player == last_fantasy_player, last_position] <- 
        counts[counts$fantasy_player == last_fantasy_player, last_position] - 1
      category_counts(counts)
      
      # Undo logic (existing)
      assigned_players(assigned_list[-nrow(assigned_list), ])
      all_players <- available_players()
      player_info <- players[players$name == last_player, ]
      available_players(rbind(all_players, player_info))
      shinyjs::html(id = paste0("assigned_player_", last_pick), html = "")
      current_pick(last_pick)
    } else {
      showModal(modalDialog(
        title = "No Action",
        "There are no picks to undo."
      ))
    }
  })
  
  # Three columns for selections table
  output$selections_ui <- renderUI({
    selections <- assigned_players()
    
    if (nrow(selections) == 0) {
      return(div(h4("No players selected yet."), style = "margin-left: 20px;"))
    }
    
    # Get the unique fantasy players
    fantasy_players <- unique(selections$fantasy_player)
    
    # Split fantasy players into three groups for columns
    column_groups <- split(fantasy_players, cut(seq_along(fantasy_players), breaks = 3, labels = FALSE))
    
    # Create the layout for three columns
    fluidRow(
      lapply(column_groups, function(group) {
        column(
          width = 4,
          lapply(group, function(player) {
            player_selections <- selections[selections$fantasy_player == player, ] |> 
              mutate(pick = as.integer(pick))
            div(
              style = "margin-bottom: 20px;",
              h4(player),
              tableOutput(outputId = paste0("table_", player))
            )
          })
        )
      })
    )
  })
  
  # Ensure table outputs for each player are dynamically generated
  observe({
    selections <- assigned_players()
    
    if (nrow(selections) > 0) {
      lapply(unique(selections$fantasy_player), function(fantasy_player) {
        player_selections <- selections[selections$fantasy_player == fantasy_player, ] |> 
          mutate(pick = as.integer(pick))
        output[[paste0("table_", fantasy_player)]] <- renderTable({
          player_selections[, c("round", "pick", "name", "position")] %>%
            rename(`Overall Pick` = pick, `NBA Player` = name, Position = position)
        })
      })
    }
  })
  
  
  
  
  
  # Reactive value to store the export data
  export_data <- reactiveVal(NULL)
  
  # Observe Export Button
  observeEvent(input$export_button, {
    selections <- assigned_players()
    
    if (nrow(selections) == 0) {
      showModal(modalDialog(
        title = "No Data to Export",
        "There are no draft results to export."
      ))
      return()
    }
    
    # Format the selections for export
    formatted_data <- selections %>%
      arrange(pick) %>%
      rename(
        draft_order = pick,
        player = fantasy_player,
        round = round,
        nba_player_taken = name,
        position = position
      )
    
    # Save the formatted data to the reactive value
    export_data(formatted_data)
    
    # Save as RDS
    #    saveRDS(formatted_data, "draft_selections.rds")
    
    showModal(modalDialog(
      title = "Export Successful",
      "Draft results are ready for download as CSV file."
    ))
  })
  
  # Download Handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("draft_selections-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- export_data()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      } else {
        showModal(modalDialog(
          title = "No Data to Download",
          "There are no draft results to download."
        ))
      }
    }
  )
  
  # Handle import of a previous draft
  observeEvent(input$import_button, {
    req(input$import_draft) # Ensure a file is uploaded
    
    file <- input$import_draft
    draft_data <- read.csv(file$datapath, stringsAsFactors = FALSE)
    
    # Validate the format of the uploaded data
    required_columns <- c("draft_order", "round", "nba_player_taken", "position", "player")
    if (!all(required_columns %in% colnames(draft_data))) {
      showModal(modalDialog(
        title = "Invalid File",
        "The uploaded file must include the following columns: draft_order, round, nba_player_taken, position, player.",
        easyClose = TRUE
      ))
      return()
    }
    
    # Ensure data is properly sorted by draft_order
    draft_data <- draft_data %>%
      arrange(draft_order) %>%
      mutate(pick = draft_order)
    
    # Update reactive values
    assigned_players(draft_data)
    
    # Update current pick
    next_pick <- ifelse(nrow(draft_data) > 0, max(draft_data$draft_order) + 1, 1)
    current_pick(next_pick)
    
    # Update available players
    imported_names <- draft_data$nba_player_taken
    updated_players <- players[!players$name %in% imported_names, ]
    available_players(updated_players)
    
    # Update the draft board
    lapply(1:nrow(draft_data), function(i) {
      pick <- draft_data$draft_order[i]
      player_info <- draft_data[i, ]
      shinyjs::html(
        id = paste0("assigned_player_", pick),
        html = paste0("<div style='background-color: ", 
                      ifelse(player_info$position == "Guard", "orange", 
                             ifelse(player_info$position == "Wing", "lightblue", "purple")), 
                      "; color: ", 
                      ifelse(player_info$position %in% c("Post"), "white", "black"), 
                      "; padding: 5px; border-radius: 5px;'>",
                      player_info$nba_player_taken, 
                      "</div>")
      )
    })
    
    showModal(modalDialog(
      title = "Draft Imported",
      "The previous draft has been successfully imported.",
      easyClose = TRUE
    ))
  })
  
  
  
}

# App
shinyApp(ui = ui, server = server)
