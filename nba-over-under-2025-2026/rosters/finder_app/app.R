# NBA Player Finder — runs entirely in the browser via shinylive (WebAssembly).
# Data prep (NBA API pull, picks-meta join, height split) happens server-side in the
# weekly GitHub Action that regenerates players_finder.csv; this app only filters.

library(shiny)
library(bslib)
library(DT)
library(dplyr)

source("filter_players.R")  # provides filter_players(); bundled by shinylive

players_data <- read.csv("players_finder.csv", stringsAsFactors = FALSE)

date_added <- if ("date_pulled" %in% names(players_data)) {
  players_data$date_pulled[1]
} else {
  NA_character_
}

teams_in <- function(div) {
  players_data %>%
    distinct(division, team) %>%
    filter(division == div) %>%
    pull(team)
}

sw_teams       <- teams_in("Southwest")
nw_teams       <- teams_in("Northwest")
pac_teams      <- teams_in("Pacific")
atlantic_teams <- teams_in("Atlantic")
se_teams       <- teams_in("Southeast")
cen_teams      <- teams_in("Central")

all_teams  <- sort(unique(players_data$team))

# Map a division to the id of its team checkbox group, and group the ids by
# conference (used by the "Focus on one team" quick selector below).
div_group   <- c(Southwest = "sw_team", Northwest = "nw_team", Pacific = "pac_team",
                 Southeast = "se_team", Atlantic = "atlantic_team", Central = "cen_team")
west_groups <- c("sw_team", "nw_team", "pac_team")
east_groups <- c("se_team", "atlantic_team", "cen_team")
team_meta   <- distinct(players_data, team, conference, division)
team_choices_by_group <- list(
  sw_team = sw_teams, nw_team = nw_teams, pac_team = pac_teams,
  se_team = se_teams, atlantic_team = atlantic_teams, cen_team = cen_teams
)

section <- function(label) {
  tags$div(class = "filter-section-label", label)
}

ui <- page_sidebar(
  title = "🏀 NBA Player Finder",
  theme = bs_theme(
    version = 5,
    primary = "#1d428a",     # NBA blue
    secondary = "#c8102e",   # NBA red
    "border-radius" = "0.5rem"
  ),
  tags$head(tags$style(HTML("
    .filter-section-label {
      font-weight: 600; font-size: .78rem; letter-spacing: .04em;
      text-transform: uppercase; color: #1d428a;
      margin: .9rem 0 .35rem; padding-bottom: .2rem;
      border-bottom: 1px solid #e3e6ec;
    }
    .sidebar .form-group { margin-bottom: .6rem; }
    table.dataTable tbody tr:hover { background-color: #eef2fb !important; }
    .result-count { font-weight: 600; color: #1d428a; }
  "))),

  sidebar = sidebar(
    width = 340,
    title = "Filters",

    selectInput("focus_team", "Focus on one team:",
                choices = c("All teams" = "", all_teams), selected = ""),
    helpText("Pick a team to show only its roster; choose \"All teams\" to reset."),

    section("Location"),
    selectInput("conference", "Conference:",
                choices = unique(players_data$conference), selected = "West"),
    conditionalPanel(
      condition = "input.conference == 'West'",
      checkboxGroupInput("division_west", "Division:",
                         choices = c("Southwest", "Northwest", "Pacific"),
                         selected = c("Southwest", "Northwest", "Pacific"))
    ),
    conditionalPanel(
      condition = "input.conference == 'East'",
      checkboxGroupInput("division_east", "Division:",
                         choices = c("Southeast", "Atlantic", "Central"),
                         selected = c("Southeast", "Atlantic", "Central"))
    ),
    conditionalPanel(
      condition = "input.division_west.indexOf('Southwest') != -1 && input.conference == 'West'",
      checkboxGroupInput("sw_team", "Southwest teams", choices = sw_teams, selected = sw_teams)
    ),
    conditionalPanel(
      condition = "input.division_west.indexOf('Pacific') != -1 && input.conference == 'West'",
      checkboxGroupInput("pac_team", "Pacific teams", choices = pac_teams, selected = pac_teams)
    ),
    conditionalPanel(
      condition = "input.division_west.indexOf('Northwest') != -1 && input.conference == 'West'",
      checkboxGroupInput("nw_team", "Northwest teams", choices = nw_teams, selected = nw_teams)
    ),
    conditionalPanel(
      condition = "input.division_east.indexOf('Southeast') != -1 && input.conference == 'East'",
      checkboxGroupInput("se_team", "Southeast teams", choices = se_teams, selected = se_teams)
    ),
    conditionalPanel(
      condition = "input.division_east.indexOf('Atlantic') != -1 && input.conference == 'East'",
      checkboxGroupInput("atlantic_team", "Atlantic teams", choices = atlantic_teams, selected = atlantic_teams)
    ),
    conditionalPanel(
      condition = "input.division_east.indexOf('Central') != -1 && input.conference == 'East'",
      checkboxGroupInput("cen_team", "Central teams", choices = cen_teams, selected = cen_teams)
    ),

    section("Height"),
    checkboxInput("known_height", "Exact height", value = FALSE),
    conditionalPanel(
      condition = "input.known_height == true",
      layout_columns(
        col_widths = c(6, 6),
        selectInput("chosen_feet", "Feet",
                    choices = sort(unique(players_data$height_feet)), selected = 6),
        selectInput("chosen_inches", "Inches",
                    choices = sort(unique(players_data$height_inches)), selected = 6)
      )
    ),
    conditionalPanel(
      condition = "input.known_height != true",
      layout_columns(
        col_widths = c(6, 6),
        selectInput("min_feet", "Min feet",
                    choices = sort(unique(players_data$height_feet)), selected = 5),
        selectInput("min_inches", "Min inches",
                    choices = sort(unique(players_data$height_inches)),
                    selected = min(players_data$height_total_inches, na.rm = TRUE) %% 12)
      ),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("max_feet", "Max feet",
                    choices = sort(unique(players_data$height_feet)), selected = 7),
        selectInput("max_inches", "Max inches",
                    choices = sort(unique(players_data$height_inches)),
                    selected = max(players_data$height_total_inches, na.rm = TRUE) %% 12)
      )
    ),

    section("Jersey"),
    checkboxInput("known_jersey", "Exact number", value = FALSE),
    conditionalPanel("input.known_jersey == true",
      numericInput("input_jersey", "Jersey number", value = 12, min = 0, max = 99)),
    conditionalPanel("input.known_jersey != true",
      sliderInput("slider_jersey", "Jersey range", min = 0, max = 99, value = c(0, 99))),

    section("Age"),
    checkboxInput("known_age", "Exact age", value = FALSE),
    conditionalPanel("input.known_age == true",
      numericInput("input_age", "Age", value = 25,
                   min = min(players_data$age), max = max(players_data$age))),
    conditionalPanel("input.known_age != true",
      sliderInput("slider_age", "Age range",
                  min = min(players_data$age), max = max(players_data$age),
                  value = c(min(players_data$age), max(players_data$age)))),

    tags$div(class = "text-muted small mt-3",
             paste("Rosters last pulled", date_added))
  ),

  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      tags$span("Players"),
      tags$span(class = "result-count", textOutput("result_count", inline = TRUE))
    ),
    DTOutput("filteredTable")
  )
)

server <- function(input, output, session) {

  filtered <- reactive({
    filter_players(
      players_data,
      conf = input$conference,
      divisions = c(input$division_west, input$division_east),
      teams = c(input$sw_team, input$nw_team, input$pac_team,
                input$se_team, input$atlantic_team, input$cen_team),
      known_height = input$known_height,
      chosen_feet = input$chosen_feet, chosen_inches = input$chosen_inches,
      min_feet = input$min_feet, min_inches = input$min_inches,
      max_feet = input$max_feet, max_inches = input$max_inches,
      known_age = input$known_age,
      age_value = input$input_age, age_range = input$slider_age,
      known_jersey = input$known_jersey,
      jersey_value = input$input_jersey, jersey_range = input$slider_jersey
    )
  })

  # "Focus on one team": select a single team (clearing the others), or reset to
  # all teams in the current conference when "All teams" is chosen.
  observeEvent(input$focus_team, {
    ft <- input$focus_team
    if (is.null(ft) || ft == "") {
      if (identical(input$conference, "West")) {
        updateCheckboxGroupInput(session, "division_west",
                                 selected = c("Southwest", "Northwest", "Pacific"))
        for (g in west_groups) {
          updateCheckboxGroupInput(session, g, selected = team_choices_by_group[[g]])
        }
      } else {
        updateCheckboxGroupInput(session, "division_east",
                                 selected = c("Southeast", "Atlantic", "Central"))
        for (g in east_groups) {
          updateCheckboxGroupInput(session, g, selected = team_choices_by_group[[g]])
        }
      }
      return()
    }
    plan <- focus_team_plan(ft, team_meta, div_group,
                            list(West = west_groups, East = east_groups))
    if (is.null(plan)) return()
    conf <- plan$conference
    updateSelectInput(session, "conference", selected = conf)
    div_input <- if (conf == "West") "division_west" else "division_east"
    all_divs <- if (conf == "West") c("Southwest", "Northwest", "Pacific")
                else c("Southeast", "Atlantic", "Central")
    updateCheckboxGroupInput(session, div_input, selected = all_divs)
    for (g in names(plan$selected)) {
      updateCheckboxGroupInput(session, g, selected = plan$selected[[g]])
    }
  }, ignoreInit = TRUE)

  output$result_count <- renderText({
    n <- nrow(filtered())
    paste0(n, " player", if (n == 1) "" else "s")
  })

  output$filteredTable <- renderDT({
    datatable(
      filtered(),
      rownames = FALSE,
      colnames = c("Player", "Team", "Conference", "Division", "Height", "Age", "Jersey"),
      class = "compact stripe hover nowrap",
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        dom = "ftip",
        columnDefs = list(list(className = "dt-center", targets = c(2, 3, 4, 5, 6)))
      )
    )
  })
}

shinyApp(ui = ui, server = server)
