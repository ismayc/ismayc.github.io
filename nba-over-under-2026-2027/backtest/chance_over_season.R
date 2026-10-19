# Each player's chance to win the pool through 2021-22 to 2025-26, from the
# in-season model the daily page uses (chance_by_date in 09-season-graphics.R:
# records pulled toward the line, beta-binomial, 100,000 simulations), run
# weekly from opening night to the last day.
#
# Picks: nba-over-under-META/all_picks_6seasons.csv. Lines: each season's
# picks.xlsx. Games: the same April game files as prior_weight.R.
# Writes chance_over_season.csv.
#
# Run from this folder: Rscript --vanilla chance_over_season.R

suppressMessages({
  library(dplyr); library(readr); library(readxl); library(purrr); library(tidyr)
})
repo <- "/Users/chesterismay/repos/ismayc.github.io"
source(file.path(repo, "nba-over-under-2026-2027/09-season-graphics.R"))

seasons <- c(`2021-22` = "2021-2022", `2022-23` = "2022-2023",
             `2023-24` = "2023-2024", `2024-25` = "2024-2025",
             `2025-26` = "2025-2026")
game_files <- c(`2021-22` = "2021-2022/rds/game_results_raw_through_2022-04-10.rds",
                `2022-23` = "2022-2023/rds/game_results_raw_through_2023-04-09.rds",
                `2023-24` = "2023-2024/rds/game_results_raw_through_2024-04-14.rds",
                `2024-25` = "2024-2025/rds/game_results_raw_through_2025-04-13.rds",
                `2025-26` = "2025-2026/rds/game_results_raw_through_2026-04-12.rds")

all_picks <- read_csv(file.path(repo, "nba-over-under-META/all_picks_6seasons.csv"),
                      show_col_types = FALSE) |>
  mutate(player = if_else(player == "Mike", "Mike J", player))

run <- function(s) {
  projections <- read_excel(file.path(repo, paste0("nba-over-under-", seasons[[s]]), "picks.xlsx"),
                            sheet = "projections") |>
    select(team, win_projection)
  games <- readRDS(file.path(repo, paste0("nba-over-under-", game_files[[s]]))) |>
    filter(slugSeason == s) |>
    mutate(team = if_else(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam),
           date = as.Date(dateGame), win = slugTeam != slugTeamLoser)
  stopifnot(all(count(games, team)$n == 82))
  # Weekly from the day before opening night, plus the last day
  first <- min(games$date); last <- max(games$date)
  dates <- sort(unique(c(seq(first - 1, last, by = 7), last)))
  # Each team's record through each date
  standings_grid <- expand_grid(team = unique(games$team), date = dates) |>
    left_join(games |> select(team, gdate = date, win), by = "team",
              relationship = "many-to-many") |>
    group_by(team, date) |>
    summarise(current_wins = sum(win & gdate <= date),
              current_losses = sum(!win & gdate <= date), .groups = "drop")
  picks <- filter(all_picks, season == s) |> select(team, player, wage, choice)
  chance_by_date(standings_grid, projections, picks) |>
    mutate(season = s, day = as.integer(date - first + 1),
           games_played = map_dbl(date, \(d) mean(
             filter(standings_grid, date == d) |> with(current_wins + current_losses))))
}

res <- map_dfr(names(seasons), \(s) { message(s); run(s) })
write_csv(res, "chance_over_season.csv")

# Summary: the eventual winner's chance at a few checkpoints, and the
# favorite at each
winners <- res |> filter(date == max(date), .by = season) |> filter(final_rank == 1)
print(as.data.frame(winners |> select(season, player)))
res |>
  mutate(stage = case_when(day == 0 ~ "preseason", TRUE ~ NA)) |>
  invisible()
checkpoints <- c(10, 20, 41, 60, 70, 75)
for (s in names(seasons)) {
  d <- filter(res, season == s)
  w <- filter(winners, season == s)$player
  cat("\n==", s, "winner", paste(w, collapse = "/"), "==\n")
  at <- map_dfr(c(0, checkpoints), \(g) {
    dd <- d |> filter(games_played >= g) |> filter(date == min(date))
    tibble(games = round(dd$games_played[1], 1), date = dd$date[1],
           winner_chance = sum(dd$p_first[dd$player %in% w]),
           favorite = dd$player[which.max(dd$p_first)],
           favorite_chance = max(dd$p_first))
  })
  print(as.data.frame(at), digits = 3)
}
