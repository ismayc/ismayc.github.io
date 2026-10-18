# Backtest the preseason pool model (08-preseason-pool-analysis.R) on
# 2021-22 to 2025-26: run it on each season's real entries and lines, then
# score its preseason numbers against how the pool actually finished.
#
# Model as in 08: wins = line + Normal(0, 7.03 * 1.2533), each simulated
# season shifted to sum to 1,230, ranked by points, then number correct, then
# 15-wager picks correct. A second pass uses a leave-one-season-out SD (the
# line MAE from the other four seasons) since 7.03 was measured on these
# same seasons.
#
# Picks and outcomes: nba-over-under-META/all_picks_6seasons.csv. Lines: each
# season's picks.xlsx "projections" sheet. Final wins: the last April game
# file per season (same files as prior_weight.R).
#
# Run from this folder: Rscript --vanilla preseason_model.R

suppressMessages({
  library(dplyr); library(readr); library(readxl); library(purrr); library(tidyr)
})
repo <- "/Users/chesterismay/repos/ismayc.github.io"
set.seed(20261018)
n_sims <- 200000
league_wins <- 1230

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

lines <- imap_dfr(seasons, \(f, s)
  read_excel(file.path(repo, paste0("nba-over-under-", f), "picks.xlsx"),
             sheet = "projections") |>
    transmute(season = s, team, line = win_projection))

finals <- imap_dfr(game_files, \(f, s)
  readRDS(file.path(repo, paste0("nba-over-under-", f))) |>
    filter(slugSeason == s) |>
    mutate(team = if_else(nameTeam == "LA Clippers", "Los Angeles Clippers", nameTeam)) |>
    group_by(team) |>
    summarise(final = sum(slugTeam != slugTeamLoser), n = n(), .groups = "drop") |>
    mutate(season = s))
stopifnot(all(finals$n == 82))
team_seasons <- inner_join(lines, finals, by = c("season", "team"))
stopifnot(nrow(team_seasons) == 150)

# Line MAE overall and with each season held out
mae_all <- mean(abs(team_seasons$final - team_seasons$line))
mae_loso <- map_dbl(names(seasons), \(s)
  with(filter(team_seasons, season != s), mean(abs(final - line)))) |>
  set_names(names(seasons))

# Pool ranking key, matching 08
rank_key <- function(points, correct, correct15) points * 1e4 + correct * 1e2 + correct15

run_season <- function(s, noise_sd) {
  ln <- filter(lines, season == s) |> arrange(team)
  pk <- filter(all_picks, season == s) |>
    mutate(signed = if_else(choice == "OVER", 1, -1) * wage)
  S <- pk |> select(team, player, signed) |>
    pivot_wider(names_from = player, values_from = signed) |> arrange(team)
  stopifnot(identical(S$team, ln$team))
  players <- setdiff(names(S), "team")
  S <- as.matrix(S[, players]); W <- abs(S)
  n_teams <- nrow(S)

  noise <- matrix(rnorm(n_sims * n_teams, 0, noise_sd), n_sims, n_teams)
  wins <- sweep(noise, 2, ln$line, "+")
  wins <- wins - (rowSums(wins) - league_wins) / n_teams
  wins <- pmin(pmax(wins, 0), 82)
  dir <- ifelse(sweep(wins, 2, ln$line, ">"), 1, -1)
  points <- dir %*% S
  correct <- (dir %*% sign(S) + n_teams) / 2
  correct15 <- (dir %*% (sign(S) * (W == 15)) + 3) / 2
  key <- rank_key(points, correct, correct15)
  is_top <- key == apply(key, 1, max)
  share <- is_top / rowSums(is_top)
  ranks <- t(apply(-key, 1, rank, ties.method = "min"))

  # What happened
  act <- pk |> group_by(player) |>
    summarise(points = sum(points), correct = sum(correct),
              correct15 = sum(correct & wage == 15), .groups = "drop")
  act <- act[match(players, act$player), ]
  act_key <- rank_key(act$points, act$correct, act$correct15)

  tibble(
    season = s, player = players, n_players = length(players),
    win_prob = colMeans(share),
    mean_points = colMeans(points),
    p10 = apply(points, 2, quantile, .1), p90 = apply(points, 2, quantile, .9),
    mean_rank = colMeans(ranks),
    last_prob = colMeans(ranks == length(players)),
    actual_points = act$points,
    actual_rank = rank(-act_key, ties.method = "min"),
    won = actual_rank == 1
  )
}

summarise_runs <- function(res, label) {
  by_season <- res |> group_by(season) |>
    summarise(
      players = n(),
      winner = paste(player[won], collapse = "/"),
      winner_prob = sum(win_prob[won]),
      winner_pred_place = paste(rank(-win_prob, ties.method = "min")[won], collapse = "/"),
      favorite = player[which.max(win_prob)],
      favorite_prob = max(win_prob),
      favorite_finish = actual_rank[which.max(win_prob)],
      last = paste(player[actual_rank == max(actual_rank)], collapse = "/"),
      last_prob = sum(last_prob[actual_rank == max(actual_rank)]),
      rank_cor = cor(mean_rank, actual_rank, method = "spearman"),
      in_band = sum(actual_points >= p10 & actual_points <= p90),
      mean_abs_points_miss = mean(abs(actual_points - mean_points)),
      .groups = "drop")
  # Log score of the actual winner against picking a winner uniformly at random
  ll <- res |> group_by(season) |>
    summarise(model = log(sum(win_prob[won])), uniform = log(1 / n()), .groups = "drop")
  cat("\n=====", label, "=====\n")
  print(as.data.frame(by_season), digits = 3)
  cat(sprintf("\nWinner log score: model %.2f vs uniform %.2f (sum over seasons; higher is better)\n",
              sum(ll$model), sum(ll$uniform)))
  cat(sprintf("Actual points inside the model's 10th-90th band: %d of %d (80%% expected)\n",
              sum(res$actual_points >= res$p10 & res$actual_points <= res$p90), nrow(res)))
  cat(sprintf("Mean predicted points %.1f vs mean actual points %.1f\n",
              mean(res$mean_points), mean(res$actual_points)))
  # Calibration of win chances, coarse bins (only five winners, so thin)
  cal <- res |> mutate(bin = cut(win_prob, c(0, .05, .1, .2, 1), include.lowest = TRUE)) |>
    group_by(bin) |> summarise(n = n(), predicted_wins = sum(win_prob), actual_wins = sum(won),
                               .groups = "drop")
  cat("\nWin-chance bins (predicted wins = sum of chances in the bin):\n")
  print(as.data.frame(cal), digits = 3)
  invisible(by_season)
}

cat(sprintf("Line MAE, 150 team-seasons: %.2f. Held-out MAE by season: %s\n",
            mae_all, paste(sprintf("%s %.2f", names(mae_loso), mae_loso), collapse = ", ")))
cat(sprintf("Over rate: %.1f%%\n", 100 * mean(team_seasons$final > team_seasons$line)))

res_fixed <- map_dfr(names(seasons), \(s) run_season(s, 7.03 * 1.2533))
res_loso <- map_dfr(names(seasons), \(s) run_season(s, mae_loso[[s]] * 1.2533))
summarise_runs(res_fixed, "As published (SD from all five seasons)")
summarise_runs(res_loso, "Leave one season out SD")

cat("\nPer player, as published:\n")
res_fixed |> arrange(season, actual_rank) |>
  transmute(season, player, win_prob = round(win_prob, 3), mean_points = round(mean_points),
            p10 = round(p10), p90 = round(p90), actual_points, mean_rank = round(mean_rank, 1),
            actual_rank) |>
  as.data.frame() |> print()
write_csv(res_fixed, "preseason_model_backtest.csv")
