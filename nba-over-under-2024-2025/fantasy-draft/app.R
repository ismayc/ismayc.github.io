library(shiny)
library(shinyjs)
library(tibble)
library(readr)

# Load player data by position
players <- read_rds("players_ballot.rds")

# Assign colors based on position
players$color <- ifelse(players$position == "Guard", "orange", 
                        ifelse(players$position == "Wing", "blue", "grey"))

players$text_color <- ifelse(players$color %in% c("grey","blue"), "white", "black")

# Snake draft order
snake_order <- tibble(
  pick_number = 1:60,
  round = rep(1:10, each = 6),
  player = unlist(lapply(1:10, function(r) {
    if (r %% 2 == 1) {
      c("Mary", "Jake", "Steve", "Chester", "Ryan", "Phil")
    } else {
      c("Phil", "Ryan", "Chester", "Steve", "Jake", "Mary")
    }
  }))
)

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("2024-2025 Fantasy Draft Board"),
  
  # Display current pick at the top
  fluidRow(
    column(12, h4(htmlOutput("current_pick_label")))
  ),
  
  # Text Input, Assign, and Undo Buttons
  fluidRow(
    column(12,
           selectizeInput("player_select", "Type a Player's Name:", choices = NULL, multiple = FALSE),
           actionButton("assign_button", "Assign Player"),
           actionButton("undo_button", "Undo Last Pick")
    )
  ),
  
  # Main Layout: Three Columns for Available Players and Draft Board
  fluidRow(
    column(4,  # Left Column: Guards
           h3("Guards"),
           div(
             id = "available_guards",
             style = "display: flex; flex-wrap: wrap; height: 300px; overflow-y: auto; border: 1px solid #ccc; padding: 0px;",
             uiOutput("guards_ui")
           )
    ),
    column(4,  # Middle Column: Wings
           h3("Wings"),
           div(
             id = "available_wings",
             style = "display: flex; flex-wrap: wrap; height: 300px; overflow-y: auto; border: 1px solid #ccc; padding: 0px;",
             uiOutput("wings_ui")
           )
    ),
    column(4,  # Right Column: Posts
           h3("Posts"),
           div(
             id = "available_posts",
             style = "display: flex; flex-wrap: wrap; height: 300px; overflow-y: auto; border: 1px solid #ccc; padding: 0px;",
             uiOutput("posts_ui")
           )
    )
  ),
  
  # Draft Board
  fluidRow(
    column(12, 
           lapply(1:10, function(round) {
             column(12, h4(paste("Round", round)),
                    div(
                      style = "display: flex; flex-direction: row; justify-content: space-between;",
                      lapply(1:6, function(pick_in_round) {
                        pick_number <- (round - 1) * 6 + pick_in_round
                        fantasy_player <- snake_order$player[pick_number]
                        div(
                          id = paste0("pick_", pick_number),
                          style = "min-height: 50px; border: 1px solid black; margin: 5px; padding: 5px; width: 20%; font-size: 12px; text-align: center;",
                          div(
                            style = "font-weight: bold;",
                            paste("Pick", pick_number)
                          ),
                          div(
                            style = "font-size: 10px; color: grey;",
                            paste("Player:", fantasy_player)
                          ),
                          div(
                            id = paste0("assigned_player_", pick_number),
                            style = "font-size: 12px; color: black;"
                          )
                        )
                      })
                    ))
           })
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
  assigned_players <- reactiveVal(data.frame(pick = integer(0), name = character(0), stringsAsFactors = FALSE))
  
  # Update current pick label
  output$current_pick_label <- renderText({
    pick <- current_pick()
    if (pick <= 60) {
      HTML(paste("<i>Current Pick</i>:", "Pick", pick, "-", "Player:", snake_order$player[pick]))
    } else {
      "Draft Complete"
    }
  })
  
  # Update Available Players UI with Multi-Column Layout
  output$guards_ui <- renderUI({
    available <- available_players()
    guard_players <- available[available$position == "Guard", ]
    div(
      style = "display: flex; flex-wrap: wrap; gap: 10px;",
      lapply(1:nrow(guard_players), function(i) {
        div(
          style = paste0("flex: 1 1 calc(33% - 10px); margin: 0px; padding: 0px; background-color: ", guard_players$color[i], 
                         "; color: ", guard_players$text_color[i], "; border-radius: 5px; font-size: 12px; text-align: center;"),
          guard_players$name[i]
        )
      })
    )
  })
  
  output$wings_ui <- renderUI({
    available <- available_players()
    wing_players <- available[available$position == "Wing", ]
    div(
      style = "display: flex; flex-wrap: wrap; gap: 10px;",
      lapply(1:nrow(wing_players), function(i) {
        div(
          style = paste0("flex: 1 1 calc(33% - 10px); margin: 0px; padding: 0px; background-color: ", wing_players$color[i], 
                         "; color: ", wing_players$text_color[i], "; border-radius: 5px; font-size: 12px; text-align: center;"),
          wing_players$name[i]
        )
      })
    )
  })
  
  output$posts_ui <- renderUI({
    available <- available_players()
    post_players <- available[available$position == "Post", ]
    div(
      style = "display: flex; flex-wrap: wrap; gap: 10px;",
      lapply(1:nrow(post_players), function(i) {
        div(
          style = paste0("flex: 1 1 calc(33% - 10px); margin: 0px; padding: 0px; background-color: ", post_players$color[i], 
                         "; color: ", post_players$text_color[i], "; border-radius: 5px; font-size: 12px; text-align: center;"),
          post_players$name[i]
        )
      })
    )
  })
  
  
  # Update Selectize Input
  observe({
    updateSelectizeInput(session, "player_select", choices = available_players()$name, server = TRUE)
  })
  
  # Assign Player Button Logic
  observeEvent(input$assign_button, {
    selected_player <- input$player_select
    all_players <- available_players()
    pick <- current_pick()
    
    if (selected_player %in% all_players$name && pick <= 60) {
      # Assign the player to the draft spot
      player_info <- all_players[all_players$name == selected_player, ]
      shinyjs::html(
        id = paste0("assigned_player_", pick),
        html = paste0("<div style='background-color:", player_info$color, 
                      "; color:", player_info$text_color, "; padding: 5px; border-radius: 5px;'>",
                      selected_player, "</div>")
      )
      
      # Add player to assigned list
      assigned_list <- assigned_players()
      assigned_players(rbind(assigned_list, data.frame(pick = pick, name = selected_player)))
      
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
      
      # Remove player from assigned list
      assigned_players(assigned_list[-nrow(assigned_list), ])
      
      # Add player back to available list
      all_players <- available_players()
      player_info <- players[players$name == last_player, ]
      available_players(rbind(all_players, player_info))
      
      # Clear the draft spot
      shinyjs::html(id = paste0("assigned_player_", last_pick), html = "")
      
      # Move back to the previous pick
      current_pick(last_pick)
    } else {
      showModal(modalDialog(
        title = "No Action",
        "There are no picks to undo."
      ))
    }
  })
}

# App
shinyApp(ui = ui, server = server)
