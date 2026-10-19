# Build each past season's rooting guide the way the 2025-26 one is built:
# the same page, app, and Progression window (the first date with 20 or fewer
# teams open to the last game), opening on the final standings. Run from the
# repo root:
#
#   Rscript --vanilla nba-over-under-META/site-style/past_rooting_guides.R
#   python3 nba-over-under-META/site-style/restyle_season_pages.py guide 2021  (and 2022-2025)
#   python3 nba-over-under-META/site-style/restyle_season_pages.py past
#
# Writes <year>-nba-rooting-guide.html to the repo root and docs/. Those
# seasons have no saved daily determined_outcomes files, so each date's table
# is rebuilt from the game results with the 2025-26 pipeline's rules
# (03-data-wrangling.R): wins to go is ceiling of the line's win % (rounded to
# two decimals) times the games, Over once that is reached, Under once it
# needs more than the games left. The remaining schedule is the games as
# played, so postponed games sit on the dates they were made up.

source("nba-over-under-META/site-style/past_seasons.R")
suppressMessages({ library(stringr); library(jsonlite) })
source("nba-over-under-2026-2027/11-rooting-guide-history.R")

js_code <- paste(readLines("nba-over-under-2025-2026/rooting-guide-app.js", warn = FALSE),
                 collapse = "\n")
conferences <- readRDS("nba-over-under-2025-2026/rds/standings_through_2026-04-12.rds") |>
  select(team_name, conference)

# The determined_outcomes columns for one date's records
outcomes_table <- function(records, lines, n_games) {
  records |>
    left_join(lines, by = "team") |>
    mutate(
      rem = n_games - w - l,
      wtg = ceiling(round(win_projection / n_games * 100, 2) / 100 * n_games) - w,
      needed = round(wtg / rem * 100, 2),
      ltg = n_games - ceiling(round(win_projection / n_games * 100, 2) / 100 * n_games) - l,
      det = case_when(wtg <= 0 ~ "OVER", needed > 100 ~ "UNDER", TRUE ~ "not yet"),
      wtg = if_else(ltg < 0 | needed > 100 | wtg <= 0, NA_real_, wtg)
    ) |>
    transmute(Team = team, `Outcome Determined` = det,
              `Current Record` = paste0(w, "-", l), `Win Projection` = win_projection,
              `Remaining Games` = as.integer(rem), `Wins To Go Over Vegas` = wtg)
}

for (i in seq_len(nrow(past_seasons))) {
  s <- past_seasons[i, ]
  if (s$year >= 2026) next  # 2025-26 has its own generator
  d <- load_past_season(s)
  n <- d$n_games
  cat(d$label, "\n")

  # Home and away for each game from the matchup text ("NYK @ BOS" and
  # "BOS vs. NYK" both make Boston home), one row per game
  games <- readRDS(file.path(paste0("nba-over-under-", s$folder), "rds", s$games_file)) |>
    filter(slugSeason == d$label) |>
    mutate(team = if_else(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam),
           date = as.Date(dateGame))
  slug_name <- distinct(games, slugTeam, team)
  schedule <- games |>
    mutate(at = str_detect(slugMatchup, " @ "),
           a = str_extract(slugMatchup, "^[A-Z]+"), b = str_extract(slugMatchup, "[A-Z]+$"),
           home = if_else(at, b, a), away = if_else(at, a, b)) |>
    distinct(date, home, away) |>
    left_join(rename(slug_name, home = slugTeam, home_team = team), by = "home") |>
    left_join(rename(slug_name, away = slugTeam, away_team = team), by = "away") |>
    transmute(game_date = date, away_team, home_team) |>
    arrange(game_date)
  stopifnot(nrow(schedule) == 15 * n, !anyNA(schedule$away_team))

  team_order <- sort(unique(d$standings_grid$team))
  stopifnot(setequal(team_order, conferences$team_name))
  days <- map(d$dates, function(dt) {
    rec <- d$standings_grid |>
      filter(date == dt) |>
      transmute(team, w = as.integer(current_wins), l = as.integer(current_losses))
    st <- rec |>
      left_join(conferences, by = c("team" = "team_name")) |>
      transmute(team_name = team, conference, wins = w, losses = l)
    list(through = dt, out_table = outcomes_table(rec, d$projections, n),
         standings = function() st)
  })
  history <- guide_history(days, team_order, 20, schedule, n)

  # The final standings, the day after the last game
  final <- days[[length(days)]]
  guide_date <- final$through + 1
  out_table <- final$out_table[match(team_order, final$out_table$Team), ]
  teams_list <- guide_teams(out_table, guide_rest_candidates(final$standings(), n),
                            schedule, guide_date, n)
  players <- sort(unique(d$picks$player))
  picks_list <- guide_picks(d$picks, players)
  stopifnot(setequal(names(picks_list), team_order))

  data <- list(teams = teams_list, picks = picks_list, todaysGames = list())
  if (n != 82) data$nGames <- n  # the app assumes 82 without it
  data_json <- toJSON(data, auto_unbox = TRUE, null = "null", pretty = FALSE)
  history_json <- toJSON(history, auto_unbox = TRUE, na = "null", pretty = FALSE)
  html_output <- guide_page(data_json, toJSON(players, auto_unbox = TRUE), history_json,
                            js_code, d$label, paste0(s$year, "-nba-over-under.html"),
                            format(final$through, "%B %d, %Y"),
                            min(out_table$`Remaining Games`), max(out_table$`Remaining Games`))
  name <- paste0(s$year, "-nba-rooting-guide.html")
  writeLines(html_output, name)
  file.copy(name, file.path("docs", name), overwrite = TRUE)
  cat(sprintf("  wrote %s: %d players, %d dates from %s (%d open), final %d-%d decided O/U\n",
              name, length(players), length(history$snaps), history$snaps[[1]]$d,
              history$snaps[[1]]$n, sum(out_table$`Outcome Determined` == "OVER"),
              sum(out_table$`Outcome Determined` == "UNDER")))
}
