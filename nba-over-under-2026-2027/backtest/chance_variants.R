# Pool-level backtest of two changes to the team-strength estimate in
# chance_by_date (09-season-graphics.R), run weekly through 2021-22 to
# 2025-26 exactly as chance_over_season.R runs the published model:
#
#   center: 0 = the prior sits on the pool's line (published), 1 = on DARKO's
#           preseason wins, 0.5 = halfway.
#   pyth:   0 = the record is wins and losses (published), 1 = the record the
#           team's points imply (games * PF^14 / (PF^14 + PA^14)), 0.5 = halfway.
#
# The team-level version of this test is strength_inputs.R.
#
# One variant:  Rscript --vanilla chance_variants.R run <center> <pyth>
#               (writes chance_variants/c<center>_p<pyth>.csv; about 8 minutes)
# Score them:   Rscript --vanilla chance_variants.R score
#               (writes chance_variants_scores.csv)
# Run from this folder.

suppressMessages({
  library(dplyr); library(readr); library(readxl); library(purrr); library(tidyr)
})
repo <- "/Users/chesterismay/repos/ismayc.github.io"
args <- commandArgs(TRUE)
folders <- c(`2021-22` = "2021-2022", `2022-23` = "2022-2023", `2023-24` = "2023-2024",
             `2024-25` = "2024-2025", `2025-26` = "2025-2026")
pyth_exp <- 14

if (args[1] == "run") {
  center_w <- as.numeric(args[2]); pyth_w <- as.numeric(args[3])
  source(file.path(repo, "nba-over-under-2026-2027/09-season-graphics.R"))
  all_picks <- read_csv(file.path(repo, "nba-over-under-META/all_picks_6seasons.csv"),
                        show_col_types = FALSE) |>
    mutate(player = if_else(player == "Mike", "Mike J", player))
  darko <- read_csv(file.path(repo, "nba-over-under-META/preseason_projections.csv"),
                    show_col_types = FALSE) |>
    filter(source == "darko") |> select(season, team, darko = proj_wins)
  scores <- read_csv("game_scores_2021_26.csv", show_col_types = FALSE)

  run <- function(s) {
    projections <- read_excel(file.path(repo, paste0("nba-over-under-", folders[[s]]), "picks.xlsx"),
                              sheet = "projections") |>
      select(team, win_projection)
    games <- filter(scores, season == s)
    stopifnot(all(count(games, team)$n == 82))
    # Weekly from the day before opening night, plus the last day
    first <- min(games$date); last <- max(games$date)
    dates <- sort(unique(c(seq(first - 1, last, by = 7), last)))
    standings_grid <- expand_grid(team = unique(games$team), date = dates) |>
      left_join(games |> select(team, gdate = date, win, pts, opp_pts), by = "team",
                relationship = "many-to-many") |>
      group_by(team, date) |>
      summarise(current_wins = sum(win & gdate <= date),
                current_losses = sum(!win & gdate <= date),
                pf = sum(pts[gdate <= date]), pa = sum(opp_pts[gdate <= date]),
                .groups = "drop") |>
      mutate(g = current_wins + current_losses,
             w_pyth = if_else(g > 0, g * pf^pyth_exp / (pf^pyth_exp + pa^pyth_exp), 0),
             strength_wins = current_wins + pyth_w * (w_pyth - current_wins),
             strength_losses = g - strength_wins)
    center <- projections |>
      left_join(filter(darko, season == s), by = "team") |>
      transmute(team, wins = win_projection + center_w * (darko - win_projection))
    picks <- filter(all_picks, season == s) |> select(team, player, wage, choice)
    chance_by_date(standings_grid, projections, picks, center = center) |>
      mutate(season = s, day = as.integer(date - first + 1),
             games_played = map_dbl(date, \(d) mean(filter(standings_grid, date == d)$g)))
  }
  res <- map_dfr(names(folders), \(s) { message(s); run(s) }) |>
    mutate(center = center_w, pyth = pyth_w)
  dir.create("chance_variants", showWarnings = FALSE)
  write_csv(res, sprintf("chance_variants/c%s_p%s.csv", center_w, pyth_w))
}

if (args[1] == "score") {
  res <- map_dfr(list.files("chance_variants", full.names = TRUE),
                 \(f) read_csv(f, show_col_types = FALSE))
  # The published model's run must match chance_over_season.csv
  pub <- read_csv("chance_over_season.csv", show_col_types = FALSE)
  same <- inner_join(filter(res, center == 0, pyth == 0), pub,
                     by = c("season", "date", "player"))
  cat(sprintf("Published model: %d of %d readings rerun; largest difference in the chance to finish first %.2g, top 4 %.2g\n",
              nrow(same), nrow(pub), max(abs(same$p_first.x - same$p_first.y)),
              max(abs(same$p_top4.x - same$p_top4.y))))

  finals <- res |> filter(!is.na(final_rank)) |>
    distinct(season, player, fr = final_rank)
  place_cols <- names(res)[grepl("^p_place_", names(res))]
  d <- res |> select(-final_rank) |>
    inner_join(finals, by = c("season", "player")) |>
    group_by(season, center, pyth) |> filter(date < max(date)) |> ungroup() |>
    mutate(top4 = as.integer(fr <= 4), first = as.integer(fr == 1),
           phase = cut(games_played, c(-1, 20, 41, 62, 82),
                       labels = c("games 0-20", "21-41", "42-62", "63-82")))
  # Ranked probability score over places: squared gaps between the predicted
  # and the actual cumulative place distribution (lower is better)
  cum_pred <- t(apply(as.matrix(replace(d[place_cols], is.na(d[place_cols]), 0)), 1, cumsum))
  cum_act <- outer(d$fr, seq_along(place_cols), "<=") * 1
  d$rps <- rowSums((cum_pred - cum_act)^2)

  per_date <- d |> group_by(center, pyth, season, phase, date) |>
    summarise(rho = cor(exp_place, fr, method = "spearman"),
              top4_hits = sum(top4[rank(exp_place, ties.method = "first") <= 4]),
              winner_logp = log(sum(p_first[first == 1])),
              favorite_won = first[which.max(p_first)],
              brier_top4 = mean((p_top4 - top4)^2), brier_first = mean((p_first - first)^2),
              rps = mean(rps), .groups = "drop")
  summ <- function(g) summarise(g, readings = n(), rho = mean(rho),
                                top4_hits = mean(top4_hits), brier_top4 = mean(brier_top4),
                                brier_first = mean(brier_first), rps = mean(rps),
                                winner_logp = mean(winner_logp),
                                favorite_won = mean(favorite_won), .groups = "drop")
  overall <- per_date |> group_by(center, pyth) |> summ()
  by_phase <- per_date |> group_by(phase, center, pyth) |> summ()
  cat("\nAll weekly readings before the last day (a reading = one date in one season):\n")
  print(as.data.frame(overall), digits = 3)
  cat("\nBy stage of the season:\n"); print(as.data.frame(by_phase), digits = 3)

  by_season <- per_date |> group_by(season, center, pyth) |> summ()
  base <- by_season |> filter(center == 0, pyth == 0) |>
    select(season, rho0 = rho, rps0 = rps, b40 = brier_top4, b10 = brier_first, lp0 = winner_logp)
  cat("\nSeasons (of 5) where the variant beats the published model:\n")
  by_season |> inner_join(base, by = "season") |> group_by(center, pyth) |>
    summarise(rho = sum(rho > rho0), rps = sum(rps < rps0), brier_top4 = sum(brier_top4 < b40),
              brier_first = sum(brier_first < b10), winner_logp = sum(winner_logp > lp0),
              .groups = "drop") |> as.data.frame() |> print()
  cat("\nBy season:\n")
  print(as.data.frame(select(by_season, season, center, pyth, rho, top4_hits, rps, brier_top4,
                             brier_first, winner_logp)), digits = 3)
  bind_rows(mutate(overall, phase = "all"), mutate(by_phase, phase = as.character(phase))) |>
    write_csv("chance_variants_scores.csv")
}
