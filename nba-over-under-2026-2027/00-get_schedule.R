library(rvest)
library(tidyverse)

# Season constants (season_label, ending_season_year, nba_cup_champ_date, ...)
if (!exists("season_path")) {
  source(if (file.exists("00-season-config.R")) "00-season-config.R" else
    file.path("nba-over-under-2026-2027", "00-season-config.R"))
}

# https://www.basketball-reference.com/leagues/NBA_2027_games-october.html
base_url <- paste0("https://www.basketball-reference.com/leagues/NBA_",
                   ending_season_year, "_games-")
months <- c("october", "november", "december", "january", "february", "march",
            "april")

scrape_month <- function(month) {
  url <- paste0(base_url, month, ".html")
  page <- read_html(url)

  table_raw <- page %>%
    html_table(header = TRUE) %>%
    .[[1]]  # The schedule is the first table on the page

  # Rename every column so the duplicate "PTS" and blank headers become unique
  table <- table_raw %>%
    rename(
      game_date = Date,
      start_time = `Start (ET)`,
      away_team = `Visitor/Neutral`,
      visitor_pts = 4,
      home_team = `Home/Neutral`,
      home_pts = 6,
      box_score = 7,
      overtime = 8,
      attendance = Attend.,
      game_duration = LOG,
      arena = Arena,
      notes = 12
    )

  message("Scraping schedule for month of ", str_to_sentence(month))
  Sys.sleep(3)

  table |> select(game_date, start_time, away_team, home_team)
}

# Scrape basketball-reference and write the schedule CSV.
#
# Before the NBA Cup championship, basketball-reference lists 80 games per
# team (the last two per team depend on Group Play), so this writes the
# initial schedule. After the championship it writes the after-Cup schedule,
# but only once every team has 82 games; otherwise it keeps the current file
# and returns FALSE so the next run tries again. Home/away splits are not
# checked: Cup knockout hosts get an extra home game (2025-26 had OKC and ORL
# at 42 home, NYK and SAS at 40).
#
# Returns TRUE when the after-Cup schedule was written.
get_nba_schedule <- function() {
  nba_schedule_raw <- map_dfr(months, scrape_month)

  nba_schedule <- nba_schedule_raw %>%
    filter(game_date != "Date") %>%  # repeated header rows
    mutate(
      game_date = as.Date(game_date, format = "%a, %b %d, %Y"),
      start_time = as.character(start_time)
    ) |>
    # The April page also lists play-in and playoff games once they are set
    filter(!is.na(game_date),
           game_date <= as.Date(nba_season_end_date)) |>
    # The NBA Cup championship does not count toward the regular season
    filter(game_date != as.Date(nba_cup_champ_date))

  games_per_team <- tibble(team = c(nba_schedule$home_team,
                                    nba_schedule$away_team)) |>
    count(team)
  incomplete <- games_per_team |> filter(n != 82)
  complete <- nrow(incomplete) == 0 && nrow(games_per_team) == 30

  message("Scraped ", nrow(nba_schedule), " regular-season games ",
          "(a complete season is 1230).")

  cup_final_played <- Sys.Date() > as.Date(nba_cup_champ_date)

  if (cup_final_played && complete) {
    write_csv(nba_schedule, season_path(schedule_after_cup_file))
    message("Wrote ", schedule_after_cup_file)
    return(invisible(TRUE))
  }

  if (cup_final_played) {
    message("The NBA Cup final has been played but the schedule is not ",
            "complete yet (", nrow(incomplete), " teams do not have 82 ",
            "games). Keeping the existing schedule; rerun later.")
  } else {
    write_csv(nba_schedule, season_path(schedule_initial_file))
    message("Wrote ", schedule_initial_file, " (before the NBA Cup final on ",
            nba_cup_champ_date, ")")
  }
  invisible(FALSE)
}

# Run when called as a script (Rscript 00-get_schedule.R or source()),
# but not when 02c-get-game-scores.R loads it just for the function.
if (!exists("load_schedule_function_only") || !load_schedule_function_only) {
  get_nba_schedule()
}
