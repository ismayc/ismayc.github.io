# Backtest the prior weight k in p = (w + k*line/82) / (g + k), 2021-22 to 2025-26
suppressMessages({library(dplyr); library(readxl); library(purrr); library(tidyr)})
repo <- "/Users/chesterismay/repos/ismayc.github.io"
files <- c(`2021-22` = "2021-2022/rds/game_results_raw_through_2022-04-10.rds",
           `2022-23` = "2022-2023/rds/game_results_raw_through_2023-04-09.rds",
           `2023-24` = "2023-2024/rds/game_results_raw_through_2024-04-14.rds",
           `2024-25` = "2024-2025/rds/game_results_raw_through_2025-04-13.rds",
           `2025-26` = "2025-2026/rds/game_results_raw_through_2026-04-12.rds")
teams <- imap_dfr(files, function(f, s) {
  g <- readRDS(file.path(repo, paste0("nba-over-under-", f))) |>
    filter(slugSeason == s) |>
    mutate(win = slugTeam != slugTeamLoser,
           nameTeam = if_else(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam)) |>
    arrange(nameTeam, numberGameTeamSeason) |>
    group_by(team = nameTeam) |>
    mutate(gp = numberGameTeamSeason, w = cumsum(win), final = sum(win), n = n()) |>
    ungroup()
  lines <- read_excel(file.path(repo, paste0("nba-over-under-", sub("/.*", "", f)), "picks.xlsx"),
                      sheet = "projections") |> select(team, line = win_projection)
  miss <- setdiff(unique(g$team), lines$team)
  if (length(miss)) stop(s, " unmatched: ", paste(miss, collapse = ", "))
  g |> left_join(lines, by = "team") |> mutate(season = s) |>
    select(season, team, gp, w, final, n, line)
})
stopifnot(all(teams$n == 82), n_distinct(paste(teams$season, teams$team)) == 150)
cat("team-seasons:", n_distinct(paste(teams$season, teams$team)),
    " over rate:", mean(distinct(teams, season, team, final, line) |> with(final > line)), "\n")

chk <- teams |> filter(gp %in% seq(5, 75, 5)) |>
  mutate(rem = 82 - gp, rem_w = final - w, over = final > line)
ks <- c(0, 5, 10, 15, 20, 25, 30, 40, 50, 60, 80, 100, 150, Inf)
score <- function(k, d) {
  p <- if (is.infinite(k)) d$line / 82 else (d$w + k * d$line / 82) / (d$gp + k)
  # P(final > line) under the binomial the page simulates
  need <- floor(d$line) + 1 - d$w
  p_over <- pbinom(need - 1, d$rem, p, lower.tail = FALSE)
  tibble(k = k, mae_rest = mean(abs(d$rem * p - d$rem_w)),
         brier = mean((p_over - d$over)^2),
         logloss = -mean(log(pmax(pmin(ifelse(d$over, p_over, 1 - p_over), 1 - 1e-6), 1e-6))))
}
overall <- map_dfr(ks, score, d = chk)
cat("\nAll checkpoints (games 5 to 75 by 5):\n"); print(as.data.frame(overall), digits = 4)

# Best k by stage of season
by_stage <- chk |> mutate(stage = cut(gp, c(0, 20, 45, 75), labels = c("games 5-20", "25-45", "50-75"))) |>
  group_split(stage) |>
  map_dfr(\(d) map_dfr(ks, score, d = d) |> mutate(stage = d$stage[1]))
cat("\nBest k by stage (Brier on over/under):\n")
print(by_stage |> group_by(stage) |> slice_min(brier, n = 1) |> as.data.frame(), digits = 4)
cat("\nk = 20 by stage:\n"); print(by_stage |> filter(k == 20) |> as.data.frame(), digits = 4)

# Leave-one-season-out: pick k on four seasons, score on the fifth
loso <- map_dfr(names(files), function(s) {
  tr <- map_dfr(ks, score, d = filter(chk, season != s))
  kb <- tr$k[which.min(tr$brier)]
  te <- filter(chk, season == s)
  tibble(held_out = s, best_k_on_others = kb,
         brier_best = score(kb, te)$brier, brier_k20 = score(20, te)$brier)
})
cat("\nLeave one season out:\n"); print(as.data.frame(loso), digits = 4)

# Calibration of the binomial at k = 20: is it overconfident?
k <- 20
cal <- chk |> mutate(p = (w + k * line / 82) / (gp + k),
                     p_over = pbinom(floor(line) - w, rem, p, lower.tail = FALSE),
                     bin = cut(p_over, c(0, .05, .2, .4, .6, .8, .95, 1), include.lowest = TRUE)) |>
  group_by(bin) |> summarise(n = n(), predicted = mean(p_over), actual = mean(over))
cat("\nCalibration at k = 20 (P(over) bins):\n"); print(as.data.frame(cal), digits = 3)
