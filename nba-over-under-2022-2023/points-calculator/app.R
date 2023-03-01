# https://chesterismay.shinyapps.io/over-under-points-calculator/

library(shiny)
library(readxl)
library(tidyverse)
library(kableExtra)

picks <- read_excel(path = "picks.xlsx", sheet = "picks")
num_players <- 9

over_under_choice <- function(
    input_id, choices = c("OVER", "UNDER"), selected = choices[1]
) {
    fluidRow(
        tags$head(
            tags$style(
                type="text/css", 
                "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
        ),
        column(2),
        div(style="display: inline-block;vertical-align:top; width: 300px;",
            selectInput(
                inputId = input_id,
                #        label = str_c(str_to_sentence(input_id), ":"),
                label = str_c(
                    picks %>% 
                        distinct(team) %>% 
                        filter(str_detect(team, input_id)) %>% 
                        pull(team),
                    ":"),
                choices = choices,
                selected = selected,
                width = '35%',
                selectize = FALSE,
                size = 1)
        )
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("NBA Over/Under 2023 Point Calculations"),
    h3("Created by Chester Ismay"),
    p(str_c("Last updated at ", Sys.time(), " PST")),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            width = 4,
            h5("Outcome still to be determined"),
            over_under_choice("Hawks", selected = "OVER"),
            over_under_choice("Celtics", selected = "UNDER"),
            over_under_choice("Nets", selected = "UNDER"),
            over_under_choice("Hornets", selected = "UNDER"),
            over_under_choice("Bulls", selected = "OVER"),
            over_under_choice("Cavaliers", selected = "OVER"),
            over_under_choice("Mavericks", selected = "OVER"),
            over_under_choice("Nuggets", selected = "OVER"),
            over_under_choice("Pistons", selected = "OVER"),
            over_under_choice("Warriors", selected = "OVER"),
            over_under_choice("Rockets", selected = "OVER"),
            over_under_choice("Lakers", selected = "UNDER"),
            over_under_choice("Grizzlies", selected = "OVER"),
            over_under_choice("Heat", selected = "UNDER"),
            over_under_choice("Bucks", selected = "OVER"),
            over_under_choice("Timberwolves", selected = "OVER"),
            over_under_choice("Pelicans", selected = "OVER"),
            over_under_choice("Knicks", selected = "UNDER"),
            over_under_choice("Magic", selected = "OVER"),
            over_under_choice("76ers", selected = "UNDER"),
            over_under_choice("Suns", selected = "UNDER"),
            over_under_choice("Blazers", selected = "OVER"), 
            over_under_choice("Spurs", selected = "OVER"),
            over_under_choice("Raptors", selected = "OVER"),
            over_under_choice("Wizards", selected = "OVER"),
            br(),
            h5("Outcome determined"),
            over_under_choice("Pacers", choices = "OVER"),
            over_under_choice("Clippers", choices = "UNDER"),
            over_under_choice("Thunder", choices = "OVER"),
            over_under_choice("Kings", choices = "OVER"),
            over_under_choice("Jazz", choices = "OVER")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            width = 2,
            tableOutput("point_table"),
            h4("Tie-breakers"),
            tableOutput("correct_table"),
            tableOutput("correct_15_table"),
            tableOutput("correct_14_table")
   #         fluidRow(
   #             column(width = 7, tableOutput("point_table")),
   #             column(width = 5, tableOutput("correct_table"))
   #        )
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$point_table <- function() {
        #        renderTable({
        
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
        
        num_correct <- picks_test %>% 
            mutate(correct = projected_points > 0) %>% 
            group_by(Player = player) %>% 
            summarize(
                `Number of Correct Picks` = as.integer(
                    sum(correct, na.rm = TRUE))
            )
 
        num_15_correct <- picks_test %>% 
            filter(wage == 15) %>% 
            mutate(correct = projected_points > 0) %>% 
            group_by(Player = player) %>% 
            summarize(
                `Correct Picks (Wage 15)` = as.integer(
                    sum(correct, na.rm = TRUE))
            ) 
         
        num_14_correct <- picks_test %>% 
            filter(wage == 14) %>% 
            mutate(correct = projected_points > 0) %>% 
            group_by(Player = player) %>% 
            summarize(
                `Correct Picks (Wage 14)` = as.integer(
                    sum(correct, na.rm = TRUE))
            ) 
        
        picks_test %>% 
            group_by(Player = player) %>% 
            summarize(
                `Points Total` = as.integer(sum(projected_points, na.rm = TRUE))
            ) %>% 
            inner_join(num_correct, by = "Player") %>% 
            inner_join(num_15_correct, by = "Player") %>% 
            inner_join(num_14_correct, by = "Player") %>% 
            arrange(desc(`Points Total`), 
                    desc(`Number of Correct Picks`),
                    desc(`Correct Picks (Wage 15)`),
                    desc(`Correct Picks (Wage 14)`)) %>% 
            select(-`Number of Correct Picks`, -`Correct Picks (Wage 15)`,
                   -`Correct Picks (Wage 14)`) %>% 
            mutate(Rank = 1:num_players, .before = Player) %>% 
            knitr::kable() %>% 
            kableExtra::kable_styling("striped", full_width = F) %>%
            kableExtra::row_spec(4, extra_css = "border-bottom: 1px solid")
    }#)
    
    output$correct_table <- function() {
        
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
            mutate(correct = projected_points > 0) %>% 
            group_by(Player = player) %>% 
            summarize(
                `Correct Picks` = as.integer(
                    sum(correct, na.rm = TRUE))
            ) %>% 
            arrange(desc(`Correct Picks`)) %>% 
            knitr::kable() %>% 
            kableExtra::kable_styling("striped", full_width = T)
    }
    
    output$correct_15_table <- function() {
        
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
            filter(wage == 15) %>% 
            mutate(correct = projected_points > 0) %>% 
            group_by(Player = player) %>% 
            summarize(
                `Correct Picks (Wage 15)` = as.integer(
                    sum(correct, na.rm = TRUE))
            ) %>% 
            arrange(desc(`Correct Picks (Wage 15)`)) %>% 
            knitr::kable() %>% 
            kableExtra::kable_styling("striped", full_width = T)
    }
    
    output$correct_14_table <- function() {
        
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
            filter(wage == 14) %>% 
            mutate(correct = projected_points > 0) %>% 
            group_by(Player = player) %>% 
            summarize(
                `Correct Picks (Wage 14)` = as.integer(
                    sum(correct, na.rm = TRUE))
            ) %>% 
            arrange(desc(`Correct Picks (Wage 14)`)) %>% 
            knitr::kable() %>% 
            kableExtra::kable_styling("striped", full_width = T)
    }
    
}

# Run the application 
shinyApp(ui = ui, server = server)
