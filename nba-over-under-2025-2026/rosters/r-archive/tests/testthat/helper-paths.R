# Locate the rosters/ directory (the one containing finder_app/) by walking up
# from the current working directory, so tests run from any wd.
rosters_dir <- function() {
  d <- normalizePath(getwd(), mustWork = FALSE)
  for (i in 1:6) {
    if (dir.exists(file.path(d, "finder_app"))) return(d)
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  stop("Could not locate the rosters/ directory containing finder_app/.")
}

finder_app_dir <- function() file.path(rosters_dir(), "finder_app")
players_csv_path <- function() file.path(finder_app_dir(), "players_finder.csv")

# Make filter_players() available to every test file.
source(file.path(finder_app_dir(), "filter_players.R"))

# Small synthetic roster used by the filtering unit tests, so they don't depend
# on whatever the latest live pull happens to contain. (NA-jersey handling gets
# its own dedicated test that sets a jersey to NA.)
sample_players <- function() {
  data.frame(
    player              = c("Player A", "Player B", "Player C", "Player D", "Player E"),
    team                = c("Los Angeles Lakers", "Los Angeles Lakers",
                            "Boston Celtics", "Denver Nuggets", "Miami Heat"),
    conference          = c("West", "West", "East", "West", "East"),
    division            = c("Pacific", "Pacific", "Atlantic", "Northwest", "Southeast"),
    height_feet         = c(6L, 7L, 6L, 6L, 5L),
    height_inches       = c(6L, 0L, 9L, 3L, 11L),
    height_total_inches = c(78L, 84L, 81L, 75L, 71L),
    age                 = c(25L, 30L, 22L, 28L, 35L),
    number_jersey       = c("23", "6", "0", "12", "13"),
    stringsAsFactors    = FALSE
  )
}

# Defaults that select "everything" for the West conference sample, so each test
# can override just the dimension it exercises.
west_all <- function(players = sample_players(), ...) {
  args <- list(
    players_data = players,
    conf         = "West",
    divisions    = c("Pacific", "Northwest", "Southwest"),
    teams        = unique(players$team),
    known_height = FALSE, min_feet = 5, min_inches = 0, max_feet = 7, max_inches = 11,
    known_age    = FALSE, age_range = c(0, 99),
    known_jersey = FALSE, jersey_range = c(0, 99)
  )
  modifyList(args, list(...))
}
