# Preseason pool analysis: who each player should root for, how similar the
# entries are, and which teams swing the pool. Rerun after every new entry.
#
# Model: each team's wins = Vegas line + noise. The lines are the center
# because no published projection beat them on average in 2021-26 (line MAE
# 7.03 wins; see nba-over-under-META). Noise is normal with SD = MAE * 1.2533
# (the normal-distribution ratio), then every simulated season is shifted so
# the league sums to 1,230 wins, which ties the teams together the way a real
# schedule does. Ranking uses the pool's order: points, then number correct,
# then number of 15-wager picks correct.
#
# Reads the submitter Sheet (public link, no login), projections.csv, and
# preseason-preview-template.html. Writes analysis/preseason_pool_analysis.json,
# four CSVs, and the standalone page analysis/preseason-preview.html.
#
# Run from this folder: Rscript --vanilla 08-preseason-pool-analysis.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(googlesheets4)
  library(jsonlite)
})

source("00-season-config.R")

set.seed(20261018)
n_sims <- 1000000
line_mae <- 7.03
noise_sd <- line_mae * 1.2533
league_wins <- 1230

# Display names: the pool's history keys on first names; Mike Jensen has
# always been "Mike J" in the group.
display_names <- c("Mike Jensen" = "Mike J", "Adonis Fuller" = "Adonis")

gs4_deauth()
submissions <- read_sheet(
  "1DJJtkDkYNe_WoYRGmc0XcrCPMun8WpTho66nZKYVr4c",
  sheet = season_label, col_types = "cccccc"
) |>
  mutate(user = str_squish(user), wager = as.numeric(wager))

picks <- submissions |>
  group_by(user) |>
  filter(submitted_at == max(submitted_at)) |>
  ungroup() |>
  mutate(player = coalesce(display_names[user],
                           str_extract(user, "^[^\\s]+")),
         sign = if_else(pick == "Over", 1, -1),
         signed = sign * wager)

stopifnot(all(count(picks, player)$n == 30))

lines <- read_csv("projections.csv", show_col_types = FALSE) |>
  arrange(team)
teams <- lines$team
players <- sort(unique(picks$player))
n_teams <- length(teams)
n_players <- length(players)

# Team x player matrices of signed wagers and raw wagers
signed <- picks |>
  select(team, player, signed) |>
  pivot_wider(names_from = player, values_from = signed) |>
  arrange(team)
stopifnot(identical(signed$team, teams))
S <- as.matrix(signed[, players])
W <- abs(S)

# Simulate seasons --------------------------------------------------------
simulate <- function(shift = 0) {
  noise <- matrix(rnorm(n_sims * n_teams, 0, noise_sd), n_sims, n_teams)
  wins <- sweep(noise, 2, lines$win_projection, "+")
  # Hold the league total at 1,230, then move every team by `shift` so a
  # scenario can model the whole board running Over or Under.
  wins <- wins - (rowSums(wins) - league_wins) / n_teams + shift
  pmin(pmax(wins, 0), 82)
}

score_sims <- function(wins) {
  over <- sweep(wins, 2, lines$win_projection, ">")   # sims x teams
  dir <- ifelse(over, 1, -1)
  points <- dir %*% S                                 # sims x players
  correct <- (dir %*% sign(S) + n_teams) / 2
  correct15 <- dir %*% (sign(S) * (W == 15))
  correct15 <- (correct15 + 3) / 2
  key <- points * 1e4 + correct * 1e2 + correct15
  best <- apply(key, 1, max)
  is_top <- key == best
  share <- is_top / rowSums(is_top)                   # split exact ties
  ranks <- t(apply(-key, 1, rank, ties.method = "min"))
  list(over = over, points = points, share = share, ranks = ranks)
}

base <- score_sims(simulate())

win_prob <- colMeans(base$share)
names(win_prob) <- players

# Rooting value: change in win probability when a team goes Over vs Under.
root <- sapply(seq_len(n_players), function(j) {
  sapply(seq_len(n_teams), function(i) {
    o <- base$over[, i]
    mean(base$share[o, j]) - mean(base$share[!o, j])
  })
})
dimnames(root) <- list(teams, players)

# Team sway: how far the whole winner distribution moves when a team flips
# (total variation distance between the Over and Under worlds).
sway <- sapply(seq_len(n_teams), function(i) {
  o <- base$over[, i]
  sum(abs(colMeans(base$share[o, ]) - colMeans(base$share[!o, ]))) / 2
})
names(sway) <- teams

# Similarity --------------------------------------------------------------
pair_rows <- combn(players, 2, simplify = FALSE) |>
  map(\(p) {
    a <- p[1]; b <- p[2]
    same <- sign(S[, a]) == sign(S[, b])
    tibble(
      a = a, b = b,
      same_side = sum(same),
      # Points at stake on teams where they disagree: when those teams go one
      # way, one gains and the other loses.
      contested_wager = sum((W[, a] + W[, b])[!same]),
      wager_corr = cor(S[, a], S[, b]),
      score_corr = cor(base$points[, a], base$points[, b]),
      a_beats_b = mean(base$ranks[, a] < base$ranks[, b])
    )
  }) |>
  list_rbind()

# Scenario: league-wide drift (history: 54% of team-seasons went Under)
scenario <- map(c(-2, 2), \(shift) {
  s <- score_sims(simulate(shift))
  set_names(as.list(colMeans(s$share)), players)
}) |>
  set_names(c("under_drift_2", "over_drift_2"))

# Per-player summary
player_summary <- tibble(
  player = players,
  win_prob = win_prob,
  mean_points = colMeans(base$points),
  sd_points = apply(base$points, 2, sd),
  mean_rank = colMeans(base$ranks),
  last_prob = colMeans(base$ranks == n_players),
  overs = colSums(S > 0),
  over_wager = colSums(W * (S > 0)),
  under_wager = colSums(W * (S < 0)),
  user = map_chr(players, \(p) picks$user[match(p, picks$player)])
)

# Points percentile bands per player
bands <- map(players, \(p) {
  q <- quantile(base$points[, p], c(.1, .25, .5, .75, .9))
  set_names(as.list(unname(q)), c("p10", "p25", "p50", "p75", "p90"))
}) |>
  set_names(players)

out <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  season = season_label,
  n_sims = n_sims,
  noise_sd = noise_sd,
  line_sum = sum(lines$win_projection),
  players = player_summary,
  bands = bands,
  submitted = picks |> distinct(player, user, submitted_at),
  teams = tibble(
    team = teams,
    conference = lines$conference,
    line = lines$win_projection,
    sway = sway[teams],
    overs = rowSums(S > 0),
    over_wager = rowSums(W * (S > 0)),
    under_wager = rowSums(W * (S < 0))
  ),
  signed = as_tibble(S) |> mutate(team = teams, .before = 1),
  root = as_tibble(root) |> mutate(team = teams, .before = 1),
  pairs = pair_rows,
  scenario = scenario
)

dir.create("analysis", showWarnings = FALSE)
write_json(out, "analysis/preseason_pool_analysis.json",
           auto_unbox = TRUE, digits = 6, pretty = TRUE)

# CSVs for the review page ------------------------------------------------
submitted_at <- picks |> distinct(player, submitted_at)

player_summary |>
  mutate(
    under_drift_win_prob = unlist(scenario$under_drift_2[player]),
    over_drift_win_prob = unlist(scenario$over_drift_2[player]),
    p10 = map_dbl(player, \(p) bands[[p]]$p10),
    p50 = map_dbl(player, \(p) bands[[p]]$p50),
    p90 = map_dbl(player, \(p) bands[[p]]$p90),
    unders = n_teams - overs
  ) |>
  left_join(submitted_at, by = "player") |>
  select(player, entry_name = user, win_prob, under_drift_win_prob,
         over_drift_win_prob, mean_points, p10, p50, p90, mean_rank,
         last_prob, overs, unders, over_wager, under_wager, submitted_at) |>
  arrange(desc(win_prob)) |>
  write_csv("analysis/players.csv")

# Who gains most when a team goes each way
best_if <- function(side) {
  sapply(seq_len(n_teams), \(i) players[which.max(side * root[i, ])])
}
as_tibble(S) |>
  mutate(team = teams, .before = 1) |>
  left_join(out$teams, by = "team") |>
  mutate(unders = n_players - overs,
         biggest_gainer_if_over = best_if(1),
         biggest_gainer_if_under = best_if(-1)) |>
  select(team, conference, line, sway, overs, unders, over_wager,
         under_wager, biggest_gainer_if_over, biggest_gainer_if_under,
         all_of(players)) |>
  arrange(desc(sway)) |>
  write_csv("analysis/teams.csv")

# Rooting: the side that raises each player's win chance, and by how much.
# "Against own pick" happens when the rest of the field bet more on the same
# side, so the team hitting helps rivals more than it helps the player.
as_tibble(root) |>
  mutate(team = teams) |>
  pivot_longer(-team, names_to = "player", values_to = "delta") |>
  left_join(picks |> select(team, player, pick, wager), by = c("team", "player")) |>
  mutate(root_for = if_else(delta > 0, "Over", "Under"),
         win_chance_swing = abs(delta),
         against_own_pick = root_for != pick,
         key = paste(player, team, sep = " | ")) |>
  select(key, player, team, root_for, win_chance_swing, pick, wager,
         against_own_pick) |>
  arrange(player, desc(win_chance_swing)) |>
  write_csv("analysis/rooting.csv")

pair_rows |>
  mutate(key = paste(a, b, sep = " | "), .before = 1) |>
  write_csv("analysis/pairs.csv")

# Carried over from 2025-26 picks-display and 04-most-important-per-player --

# Consensus: the average signed wager per team (Over positive)
consensus <- tibble(team = teams, consensus = rowMeans(S))

# Each pick's distance from the rest of the group: own signed wager minus the
# average of everyone else's. Top 10 per player by size.
most_different <- map(players, \(p) {
  others <- rowMeans(S[, setdiff(players, p), drop = FALSE])
  tibble(player = p, team = teams, signed = S[, p], others_avg = others,
         dev = S[, p] - others)
}) |>
  list_rbind() |>
  group_by(player) |>
  slice_max(abs(dev), n = 10, with_ties = FALSE) |>
  ungroup()

# 2D closeness map: classical MDS on 1 - cosine similarity of signed wagers
cosine <- crossprod(S) / outer(sqrt(colSums(S^2)), sqrt(colSums(S^2)))
mds <- cmdscale(as.dist(1 - cosine), k = 2)
closeness <- tibble(player = players, x = mds[, 1], y = mds[, 2])

# Last season's players who have no entry yet (matched on first name)
deadline_text <- "11:59 PM Central, Sunday, October 18"
prev_names <- read_sheet("1DJJtkDkYNe_WoYRGmc0XcrCPMun8WpTho66nZKYVr4c",
                         sheet = "2025-2026", col_types = "c", range = "A:A")[[1]]
first_name <- \(x) str_extract(str_squish(x), "^[^\\s]+")
missing <- setdiff(unique(first_name(prev_names)), first_name(picks$user)) |>
  sort()

# Standalone HTML page ----------------------------------------------------
page_data <- list(
  meta = list(
    generated = paste0(format(Sys.time(), "%B "),
                       as.integer(format(Sys.time(), "%d")),
                       format(Sys.time(), ", %Y at "),
                       as.integer(format(Sys.time(), "%I")),
                       format(Sys.time(), ":%M %p %Z")),
    n_sims = n_sims, noise_sd = noise_sd, line_mae = line_mae,
    line_sum = sum(lines$win_projection), lines_as_of = "October 13",
    missing = I(missing), deadline = deadline_text
  ),
  players = read_csv("analysis/players.csv", show_col_types = FALSE) |>
    mutate(submitted_at = as.character(submitted_at)),
  teams = read_csv("analysis/teams.csv", show_col_types = FALSE) |>
    left_join(consensus, by = "team"),
  rooting = read_csv("analysis/rooting.csv", show_col_types = FALSE),
  pairs = read_csv("analysis/pairs.csv", show_col_types = FALSE),
  most_different = most_different,
  closeness = closeness
)
template <- read_file("preseason-preview-template.html") |>
  str_replace(fixed("/*OU-THEME*/"),
              read_file("../nba-over-under-META/site-style/ou-theme.css"))
page <- str_replace(template, fixed("__DATA__"),
                    toJSON(page_data, dataframe = "rows", digits = 6,
                           auto_unbox = TRUE, na = "null"))
write_file(page, "analysis/preseason-preview.html")
message("Wrote analysis/preseason-preview.html")

# Publish copies in the site theme: 2027-nba-over-under-preview.html, plus
# 2027-nba-over-under.html until the in-season page takes over (root, docs/)
system2("python3", c("../nba-over-under-META/site-style/restyle_season_pages.py",
                     "preview", normalizePath("analysis/preseason-preview.html")))

player_summary |>
  arrange(desc(win_prob)) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |>
  print(width = 200)
tibble(team = teams, sway = round(sway, 3)) |> arrange(desc(sway)) |> print(n = 10)
