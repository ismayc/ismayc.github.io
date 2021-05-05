library(shiny)

server <- shinyServer(function(input, output) { NULL })
ui <- shinyUI(
    pageWithSidebar( 
        
        headerPanel("side-by-side"), 
        
        sidebarPanel(
            fluidRow(
                tags$head(
                    tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
                ),
                column(2),
                column(4,
                       selectInput(inputId = "options", label = "some text", 
                                   choices = list(a = 0, b = 1))
                )
            )),
        mainPanel(
            fluidRow( 
                h3("bla bla")
            ))
    )
)

shinyApp(ui=ui,server=server)