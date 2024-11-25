library(shiny)
library(shinyjqui)
library(shinyjs)
library(tibble)

# Load player data by position
players <- tibble(
  name = c(
    # Guards
    "Luka Doncic", "Shai Gilgeous-Alexander", "Jalen Brunson", "Tyrese Haliburton", 
    "Stephen Curry", "Tyrese Maxey", "Devin Booker", "Donovan Mitchell", 
    "Ja Morant", "Damian Lillard", "De’Aaron Fox", "Trae Young", "Kyrie Irving",
    "Jamal Murray", "Cade Cunningham", "LaMelo Ball", "James Harden", "Derrick White", 
    "Darius Garland", "Jrue Holiday", "Dejounte Murray", "Jalen Green", "Fred VanVleet",
    "Coby White", "CJ McCollum", "D’Angelo Russell", "Immanuel Quickley", "Malik Monk", 
    "Cam Thomas", "Donte DiVincenzo",
    # Wings
    "Jayson Tatum", "Anthony Edwards", "Kevin Durant", "LeBron James", "Jaylen Brown",
    "Kawhi Leonard", "Paul George", "Jimmy Butler", "Scottie Barnes", "Jalen Williams", 
    "DeMar DeRozan", "Brandon Ingram", "Franz Wagner", "Desmond Bane", "Mikal Bridges",
    "OG Anunoby", "Brandon Miller", "Zach LaVine", "Austin Reaves", "Michael Porter Jr.",
    "Miles Bridges", "Jalen Johnson", "Tyler Herro", "Josh Giddey", "Bradley Beal", 
    "Trey Murphy III", "Devin Vassell", "Herbert Jones", "Jaden McDaniels", "Josh Hart",
    # Posts
    "Nikola Jokic", "Giannis Antetokounmpo", "Joel Embiid", "Anthony Davis", 
    "Victor Wembanyama", "Domantas Sabonis", "Zion Williamson", "Paolo Banchero", 
    "Pascal Siakam", "Bam Adebayo", "Karl-Anthony Towns", "Rudy Gobert", "Lauri Markkanen",
    "Julius Randle", "Alperen Sengun", "Chet Holmgren", "Jaren Jackson Jr.", 
    "Kristaps Porzingis", "Evan Mobley", "Myles Turner", "Aaron Gordon", "Jarrett Allen", 
    "Kyle Kuzma", "Jerami Grant", "Keegan Murray", "Jonathan Kuminga", "Jabari Smith Jr.",
    "Nikola Vucevic", "Tobias Harris", "Naz Reid"
  ),
  position = c(
    rep("Guard", 30), 
    rep("Wing", 30), 
    rep("Post", 30)
  )
)

# Assign colors based on position
players$color <- ifelse(players$position == "Guard", "orange", 
                        ifelse(players$position == "Wing", "green", "brown"))

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Fantasy Draft Board"),
  
  # Main Layout: Three Columns for Available Players and Draft Board
  fluidRow(
    column(4,  # Left Column: Guards
           h3("Guards"),
           div(
             id = "available_guards",
             style = "display: flex; flex-wrap: wrap; height: 700px; overflow-y: auto; border: 1px solid #ccc; padding: 5px;",
             lapply(which(players$position == "Guard"), function(i) {
               div(
                 id = paste0("player_", i),
                 class = "draggable",
                 style = paste0("margin: 3px; padding: 2px 5px; background-color: ", players$color[i], 
                                "; color: ", ifelse(players$color[i] == "brown", "white", "black"), 
                                "; border-radius: 3px; font-size: 10px; cursor: pointer; width: 80px; height: 20px; text-align: center;"),
                 players$name[i]
               )
             })
           )
    ),
    column(4,  # Middle Column: Wings
           h3("Wings"),
           div(
             id = "available_wings",
             style = "display: flex; flex-wrap: wrap; height: 700px; overflow-y: auto; border: 1px solid #ccc; padding: 5px;",
             lapply(which(players$position == "Wing"), function(i) {
               div(
                 id = paste0("player_", i),
                 class = "draggable",
                 style = paste0("margin: 3px; padding: 2px 5px; background-color: ", players$color[i], 
                                "; color: ", ifelse(players$color[i] == "brown", "white", "black"), 
                                "; border-radius: 3px; font-size: 10px; cursor: pointer; width: 80px; height: 20px; text-align: center;"),
                 players$name[i]
               )
             })
           )
    ),
    column(4,  # Right Column: Posts
           h3("Posts"),
           div(
             id = "available_posts",
             style = "display: flex; flex-wrap: wrap; height: 700px; overflow-y: auto; border: 1px solid #ccc; padding: 5px;",
             lapply(which(players$position == "Post"), function(i) {
               div(
                 id = paste0("player_", i),
                 class = "draggable",
                 style = paste0("margin: 3px; padding: 2px 5px; background-color: ", players$color[i], 
                                "; color: ", ifelse(players$color[i] == "brown", "white", "black"), 
                                "; border-radius: 3px; font-size: 10px; cursor: pointer; width: 80px; height: 20px; text-align: center;"),
                 players$name[i]
               )
             })
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
                        div(
                          id = paste0("pick_", pick_number),
                          class = "dropzone",
                          style = "min-height: 50px; border: 1px solid black; margin: 5px; padding: 5px; width: 12%; font-size: 12px; text-align: center;",
                          div(
                            style = "font-weight: bold;",
                            paste("Pick", pick_number)
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
  # Initialize draggable and droppable elements
  observe({
    jqui_draggable(".draggable", options = list(revert = "invalid"))
    jqui_droppable(".dropzone", options = list(
      accept = ".draggable",
      drop = JS("
        function(event, ui) {
          var draggableId = ui.draggable.attr('id');  // Get the ID of the draggable item
          var droppableId = $(this).attr('id');      // Get the ID of the dropzone
          
          // Move the draggable element to the dropzone
          ui.draggable.detach().css({
            top: '0px',
            left: '0px'
          }).appendTo($(this));
        }
      ")
    ))
  })
}

# App
shinyApp(ui = ui, server = server)
