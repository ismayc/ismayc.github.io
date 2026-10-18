# Each past season's games, lines, and picks, read the same way for
# past_chances.R and past_explorers.R. Run those from the repo root.

suppressMessages({
  library(dplyr); library(tidyr); library(purrr); library(readxl); library(readr)
})

past_seasons <- tribble(
  ~year, ~folder,      ~games_file,                                ~n_games,
  2021,  "2020-2021",  "game_results_raw_through_2021-05-16.rds",  72,
  2022,  "2021-2022",  "game_results_raw_through_2022-04-10.rds",  82,
  2023,  "2022-2023",  "game_results_raw_through_2023-04-09.rds",  82,
  2024,  "2023-2024",  "game_results_raw_through_2024-04-14.rds",  82,
  # The later 2024-25 files hold 2025-26 games (see the 2025 page restore)
  2025,  "2024-2025",  "game_results_raw_through_2025-04-13.rds",  82,
  2026,  "2025-2026",  "game_results_raw_through_2026-04-12.rds",  82
)

load_past_season <- function(s) {
  label <- paste0(s$year - 1, "-", substr(s$year, 3, 4))
  dir <- paste0("nba-over-under-", s$folder)
  games <- readRDS(file.path(dir, "rds", s$games_file)) |>
    filter(slugSeason == label) |>
    mutate(team = if_else(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam),
           win = slugTeam != slugTeamLoser, date = as.Date(dateGame))
  per_team <- count(games, team)
  if (nrow(per_team) != 30 || any(per_team$n != s$n_games))
    stop(label, ": expected 30 teams with ", s$n_games, " games each")
  projections <- read_excel(file.path(dir, "picks.xlsx"), sheet = "projections") |>
    select(team, win_projection)
  picks <- read_csv("nba-over-under-META/all_picks_6seasons.csv", show_col_types = FALSE) |>
    filter(season == label) |>
    select(team, player, wage, choice)
  stopifnot(setequal(projections$team, per_team$team),
            setequal(picks$team, per_team$team))
  # Each team's record after every game date of the season
  dates <- sort(unique(games$date))
  standings_grid <- games |>
    group_by(team, date) |>
    summarise(w = sum(win), l = sum(!win), .groups = "drop") |>
    complete(team, date = dates, fill = list(w = 0L, l = 0L)) |>
    arrange(team, date) |>
    group_by(team) |>
    mutate(current_wins = cumsum(w), current_losses = cumsum(l)) |>
    ungroup()
  list(label = label, n_games = s$n_games, dates = dates,
       standings_grid = standings_grid, projections = projections, picks = picks)
}
