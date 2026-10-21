# Count each player's correct 14s, 13s, and 12s for the seasons whose built
# pages only carried 15s (2020-21 and 2021-22), for restyle_season_pages.py to
# add as columns. Run from the repo root:
#
#   Rscript --vanilla nba-over-under-META/site-style/past_tiebreaks.R
#
# Writes nba-over-under-META/site-style/past-tiebreaks/<year>.json. It scores
# the final standings the way each season's 03-data-wrangling.R does (win
# percentage against the line, rounded to 2 places, a push loses the wager),
# and keeps points, picks correct, and 15s so the page can check them against
# what the season page already shows.

suppressPackageStartupMessages({library(dplyr); library(stringr); library(readxl)
                                library(readr); library(jsonlite)})

seasons <- list(
  list(year = 2021, dir = "nba-over-under-2020-2021", last = "2021-05-16", games = 72),
  list(year = 2022, dir = "nba-over-under-2021-2022", last = "2022-04-10", games = 82))
out_dir <- "nba-over-under-META/site-style/past-tiebreaks"
dir.create(out_dir, showWarnings = FALSE)

for (s in seasons) {
  g <- read_rds(file.path(s$dir, "rds", paste0("game_results_raw_through_", s$last, ".rds")))
  final <- g |>
    mutate(win = if_else(slugTeam != slugTeamLoser, 1, 0)) |>
    group_by(nameTeam) |>
    summarize(wp = round(sum(win) / n() * 100, 2), .groups = "drop") |>
    mutate(team = str_replace_all(nameTeam, "LA ", "Los Angeles "))
  line <- read_excel(file.path(s$dir, "picks.xlsx"), sheet = "projections") |>
    mutate(pp = round(win_projection / s$games * 100, 2))
  picks <- read_excel(file.path(s$dir, "picks.xlsx"), sheet = "picks")
  res <- picks |>
    inner_join(final, by = "team") |> inner_join(select(line, team, pp), by = "team") |>
    mutate(ou = case_when(wp > pp ~ "OVER", wp < pp ~ "UNDER", TRUE ~ "PUSH"),
           pts = if_else(ou == choice, wage, -wage)) |>
    group_by(player) |>
    summarize(points = sum(pts), correct = sum(pts > 0), w15 = sum(pts == 15),
              w14 = sum(pts == 14), w13 = sum(pts == 13), w12 = sum(pts == 12),
              .groups = "drop")
  stopifnot(nrow(picks) == 30 * n_distinct(picks$player))
  write_json(list(date = s$last, players = split(select(res, -player), res$player) |>
                    lapply(as.list)),
             file.path(out_dir, paste0(s$year, ".json")), auto_unbox = TRUE, pretty = TRUE)
  cat("wrote", s$year, "\n")
}
