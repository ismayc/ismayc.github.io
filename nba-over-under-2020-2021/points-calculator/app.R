#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

picks <- read_excel(path = "../picks.xlsx", sheet = "picks")

over_under_choice <- function(input_id, choices = c("OVER", "UNDER"), 
                              selected = choices[1]) {
    selectInput(
        inputId = input_id,
        #        label = str_c(str_to_sentence(input_id), ":"),
        label = str_c(
            chester_probs_df %>% 
                filter(str_detect(team, input_id)) %>% 
                pull(team),
            ":"),
        choices = choices,
        selected = selected,
        width = '35%',
        selectize = FALSE,
        size = 2)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("NBA Over/Under 2021 Point Calculations"),
    h3("Created by Chester Ismay"),
    p(str_c("Last updated at ", Sys.time(), " US Pacific time")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 4,
            over_under_choice("Hawks"),
            over_under_choice("Celtics", choices = "UNDER"),
            over_under_choice("Nets"),
            over_under_choice("Hornets", choices = "OVER"),
            over_under_choice("Bulls"),
            over_under_choice("Cavaliers"),
            over_under_choice("Mavericks", selected = "UNDER"),
            over_under_choice("Nuggets"),
            over_under_choice("Pistons", selected = "UNDER"),
            over_under_choice("Warriors"),
            over_under_choice("Rockets", choices = "UNDER"),
            over_under_choice("Pacers", selected = "UNDER"),
            over_under_choice("Clippers"),
            over_under_choice("Lakers", selected = "UNDER"),
            over_under_choice("Grizzlies"),
            over_under_choice("Heat", choices = "UNDER"),
            over_under_choice("Bucks", choices = "UNDER"),
            over_under_choice("Timberwolves", selected = "UNDER"),
            over_under_choice("Pelicans", selected = "UNDER"),
            over_under_choice("Knicks", choices = "OVER"),
            over_under_choice("Thunder"),
            over_under_choice("Magic", choices = "UNDER"),
            over_under_choice("76ers"),
            over_under_choice("Suns", choices = "OVER"),
            over_under_choice("Blazers", choices = "UNDER"),
            over_under_choice("Kings"),
            over_under_choice("Spurs", choices = "OVER"),
            over_under_choice("Raptors", choices = "UNDER"),
            over_under_choice("Jazz", choices = "OVER"),
            over_under_choice("Wizards")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("point_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$point_table <- renderTable({
        
        chester_probs_df <- picks %>% 
            distinct(team) %>% 
            arrange(team) %>% 
            mutate(likely_result = case_when(
                str_detect(team, "Pistons") ~ input$Pistons,  
                str_detect(team, "Blazers") ~ input$Blazers,
                str_detect(team, "Bulls") ~ input$Bulls,
                str_detect(team, "Kings") ~ input$Kings,
                str_detect(team, "Nuggets") ~ input$Nuggets,
                str_detect(team, "Mavericks") ~ input$Mavericks,
                str_detect(team, "Warriors") ~ input$Warriors,
                str_detect(team, "Wizards") ~ input$Wizards,
                str_detect(team, "Hawks") ~ input$Hawks,
                str_detect(team, "Celtics") ~ input$Celtics,
                str_detect(team, "Nets") ~ input$Nets,
                str_detect(team, "Hornets") ~ input$Hornets,
                str_detect(team, "Bulls") ~ input$Bulls,
                str_detect(team, "Cavaliers") ~ input$Cavaliers,
                str_detect(team, "Mavericks") ~ input$Mavericks,
                str_detect(team, "Nuggets") ~ input$Nuggets,
                str_detect(team, "Pistons") ~ input$Pistons,
                str_detect(team, "Warriors") ~ input$Warriors,
                str_detect(team, "Rockets") ~ input$Rockets,
                str_detect(team, "Pacers") ~ input$Pacers,
                str_detect(team, "Clippers") ~ input$Clippers,
                str_detect(team, "Lakers") ~ input$Lakers,
                str_detect(team, "Grizzlies") ~ input$Grizzlies,
                str_detect(team, "Heat") ~ input$Heat,
                str_detect(team, "Bucks") ~ input$Bucks,
                str_detect(team, "Timberwolves") ~ input$Timberwolves,
                str_detect(team, "Pelicans") ~ input$Pelicans,
                str_detect(team, "Knicks") ~ input$Knicks,
                str_detect(team, "Thunder") ~ input$Thunder,
                str_detect(team, "Magic") ~ input$Magic,
                str_detect(team, "76ers") ~ input$`76ers`,
                str_detect(team, "Suns") ~ input$Suns,
                str_detect(team, "Blazers") ~ input$Blazers,
                str_detect(team, "Kings") ~ input$Kings,
                str_detect(team, "Spurs") ~ input$Spurs,
                str_detect(team, "Raptors") ~ input$Raptors,
                str_detect(team, "Jazz") ~ input$Jazz,
                str_detect(team, "Wizards") ~ input$Wizards
            ))
        
        picks_test <- picks %>% 
            inner_join(
                chester_probs_df %>% select(team, likely_result), 
                by = "team"
            ) %>% 
            mutate(projected_points = case_when(
                (choice == likely_result) & (likely_result != "TBD") ~ wage,
                (choice != likely_result) & (likely_result != "TBD") ~ -wage
            ))
        
        picks_test %>% 
            group_by(Player = player) %>% 
            summarize(
                `Points Total` = as.integer(sum(projected_points, na.rm = TRUE))
            ) %>% 
            arrange(desc(`Points Total`)) %>% 
            mutate(Rank = 1:8, .before = Player)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
