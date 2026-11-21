# Backtest two changes to the team-strength estimate inside chance_by_date
# (09-season-graphics.R), on 2021-22 to 2025-26 (150 team-seasons):
#
#   center: where the 20-game prior sits. 0 = the pool's line (as published),
#           1 = DARKO's preseason wins, 0.5 = halfway.
#   pyth:   how the record so far is counted. 0 = wins and losses (as
#           published), 1 = the record its points scored and allowed imply
#           (games * PF^14 / (PF^14 + PA^14)), 0.5 = halfway.
#
# Everything else is the published model: prior worth 20 games, team rate
# drawn from a beta with the evidence at half weight, Over means final wins
# above the line. Checkpoints every 5 games from 5 to 75, as in prior_weight.R
# and betabin.R.
#
# Games and scores: game_scores_2021_26.csv (stats.nba.com LeagueGameLog,
# regular season, fetched November 21, 2026; wins match each season's pool
# game file). Lines: each season's picks.xlsx. DARKO:
# nba-over-under-META/preseason_projections.csv.
#
# Run from this folder: Rscript --vanilla strength_inputs.R
# Writes strength_inputs_team.csv (every variant's scores; strength_inputs.py
# recomputes it in Python).

suppressMessages({library(dplyr); library(readr); library(readxl); library(purrr); library(tidyr)})
repo <- "/Users/chesterismay/repos/ismayc.github.io"
prior_games <- 20; strength_weight <- 0.5; pyth_exp <- 14
folders <- c(`2021-22` = "2021-2022", `2022-23` = "2022-2023", `2023-24` = "2023-2024",
             `2024-25` = "2024-2025", `2025-26` = "2025-2026")

lines <- imap_dfr(folders, \(f, s)
  read_excel(file.path(repo, paste0("nba-over-under-", f), "picks.xlsx"), sheet = "projections") |>
    transmute(season = s, team, line = win_projection))
darko <- read_csv(file.path(repo, "nba-over-under-META/preseason_projections.csv"),
                  show_col_types = FALSE) |>
  filter(source == "darko") |> select(season, team, darko = proj_wins)

chk <- read_csv("game_scores_2021_26.csv", show_col_types = FALSE) |>
  arrange(season, team, date) |>
  group_by(season, team) |>
  mutate(gp = row_number(), w = cumsum(win), pf = cumsum(pts), pa = cumsum(opp_pts),
         final = sum(win)) |>
  ungroup() |>
  filter(gp %in% seq(5, 75, 5)) |>
  inner_join(lines, by = c("season", "team")) |>
  inner_join(darko, by = c("season", "team")) |>
  mutate(rem = 82 - gp, rem_w = final - w, over = final > line,
         w_pyth = gp * pf^pyth_exp / (pf^pyth_exp + pa^pyth_exp))
stopifnot(nrow(chk) == 150 * 15, !anyNA(chk))

# P(final wins > line) when the rate is Beta(a, b) and `rem` games remain
p_over_bb <- function(w, rem, line, a, b) {
  need <- floor(line) + 1 - w
  map_dbl(seq_along(w), function(i) {
    n <- rem[i]; if (need[i] <= 0) return(1); if (need[i] > n) return(0)
    x <- need[i]:n
    sum(exp(lchoose(n, x) + lbeta(x + a[i], n - x + b[i]) - lbeta(a[i], b[i])))
  })
}
predict_variant <- function(d, center, pyth, k = prior_games) {
  q <- (d$line + center * (d$darko - d$line)) / 82
  w_used <- d$w + pyth * (d$w_pyth - d$w)
  a <- strength_weight * (w_used + k * q)
  b <- strength_weight * (d$gp - w_used + k * (1 - q))
  tibble(rate = a / (a + b), p_over = p_over_bb(d$w, d$rem, d$line, a, b))
}
grid <- expand_grid(center = c(0, 0.5, 1), pyth = c(0, 0.5, 1))
preds <- pmap_dfr(grid, \(center, pyth)
  bind_cols(select(chk, season, team, gp, rem, rem_w, over),
            predict_variant(chk, center, pyth), center = center, pyth = pyth))

score <- function(d) summarise(d,
  n = n(), mae_rest = mean(abs(rem * rate - rem_w)),
  brier = mean((p_over - over)^2),
  logloss = -mean(log(pmax(pmin(ifelse(over, p_over, 1 - p_over), 1 - 1e-6), 1e-6))),
  right_side = mean((p_over > 0.5) == over), .groups = "drop")

overall <- preds |> group_by(center, pyth) |> score()
cat("All checkpoints (150 team-seasons x 15):\n"); print(as.data.frame(overall), digits = 4)

by_stage <- preds |>
  mutate(stage = cut(gp, c(0, 20, 45, 75), labels = c("games 5-20", "25-45", "50-75"))) |>
  group_by(stage, center, pyth) |> score()
cat("\nBy stage:\n"); print(as.data.frame(by_stage), digits = 4)

by_season <- preds |> group_by(season, center, pyth) |> score()
base <- by_season |> filter(center == 0, pyth == 0) |>
  select(season, brier0 = brier, mae0 = mae_rest)
cat("\nSeasons (of 5) where the variant beats the published model:\n")
by_season |> inner_join(base, by = "season") |> group_by(center, pyth) |>
  summarise(brier_better = sum(brier < brier0), mae_better = sum(mae_rest < mae0),
            .groups = "drop") |> as.data.frame() |> print()

# Uncertainty: resample the 150 team-seasons (all 15 checkpoints of a team
# stay together), 2,000 times, and take each variant's difference from the
# published model.
set.seed(20261121)
wide <- preds |>
  mutate(id = paste(season, team), sq = (p_over - over)^2, ae = abs(rem * rate - rem_w)) |>
  group_by(id, center, pyth) |> summarise(sq = sum(sq), ae = sum(ae), .groups = "drop")
ids <- unique(wide$id)
boot <- map_dfr(seq_len(2000), function(r) {
  draw <- tibble(id = sample(ids, replace = TRUE))
  draw |> inner_join(wide, by = "id", relationship = "many-to-many") |>
    group_by(center, pyth) |> summarise(brier = sum(sq) / (15 * length(ids)),
                                        mae = sum(ae) / (15 * length(ids)), .groups = "drop") |>
    mutate(d_brier = brier - brier[center == 0 & pyth == 0],
           d_mae = mae - mae[center == 0 & pyth == 0])
})
ci <- boot |> group_by(center, pyth) |>
  summarise(d_brier_lo = quantile(d_brier, .025), d_brier_hi = quantile(d_brier, .975),
            d_mae_lo = quantile(d_mae, .025), d_mae_hi = quantile(d_mae, .975), .groups = "drop")
cat("\nDifference from the published model, 95% interval (below 0 is better):\n")
print(as.data.frame(ci), digits = 3)

# Does the prior weight want to change with the new inputs?
cat("\nPrior weight check (Brier, MAE of remaining wins):\n")
expand_grid(center = c(0, 1), pyth = c(0, 1), k = c(10, 20, 30, 40)) |>
  pmap_dfr(\(center, pyth, k) bind_cols(select(chk, rem, rem_w, over),
                                         predict_variant(chk, center, pyth, k)) |>
             score() |> mutate(center = center, pyth = pyth, k = k)) |>
  select(center, pyth, k, brier, mae_rest) |> as.data.frame() |> print(digits = 4)

overall |> mutate(stage = "all") |> bind_rows(mutate(by_stage, stage = as.character(stage))) |>
  write_csv("strength_inputs_team.csv")
