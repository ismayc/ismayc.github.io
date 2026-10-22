# Write the golden cases the Python package (ouanalytics-py) tests against:
# fixed inputs and the tables this R package computes from them.
#   R CMD INSTALL ouanalytics && Rscript ouanalytics/tools/make-golden.R
# Run from nba-over-under-2026-2027/. Re-run after any change to the R
# calculations, then run the Python tests: a difference means one side is wrong.

suppressMessages({
  library(ouanalytics)
  library(dplyr)
  library(tibble)
  library(readxl)
})

golden <- file.path("ouanalytics-py", "tests", "golden")
unlink(golden, recursive = TRUE)

# --- Case 1: the real saved data through October 21, 2026 -------------------
# Player names are replaced with P1, P2, ... (the order of first appearance).
num_games <- 82
picks_raw <- readRDS("rds/gs_picks_raw.rds")
picks <- picks_raw |>
  mutate(choice = toupper(pick),
         player = sub("\\s.*$", "", trimws(name))) |>
  select(team, player, wage = wager, choice) |>
  arrange(team, player)
players <- sort(unique(picks$player))
picks$player <- paste0("P", match(picks$player, players))

projections <- read_excel("picks.xlsx", sheet = "projections") |>
  select(-conference) |>
  mutate(percentage_projection = win_projection / num_games * 100)

game_results_raw <- readRDS("rds/game_results_raw_through_2026-10-21.rds")
standings <- readRDS("rds/standings_through_2026-10-21.rds")
scores <- tibble(game_id = character(), game_date = as.Date(character()),
                 away_team = character(), home_team = character(),
                 slug_away_team = character(), slug_home_team = character(),
                 away_score = numeric(), home_score = numeric())

params <- list(yesterday = as.Date("2026-10-21"), today = as.Date("2026-10-22"),
               season_start = as.Date("2026-10-20"), num_games = num_games)
pool <- compute_pool(game_results_raw, scores, projections, picks, standings,
                     params$yesterday, params$today, params$season_start,
                     params$num_games)
export_pool_case(file.path(golden, "oct21"), game_results_raw, scores,
                 projections, picks, standings, params, pool)

# --- Case 2: the small fixture the R tests use (has game scores) ------------
source("ouanalytics/tests/testthat/helper-fixtures.R", local = TRUE)
fx_standings <- tibble(team_name = c("Alpha", "Bravo", "Los Angeles Clippers"),
                       wins = c(1, 0, 1), losses = c(1, 1, 0))
fx_params <- list(yesterday = d2, today = d2 + 1, season_start = d1,
                  num_games = 82)
fx_pool <- compute_pool(game_results_raw, scores, projections, picks,
                        fx_standings, fx_params$yesterday, fx_params$today,
                        fx_params$season_start, fx_params$num_games)
export_pool_case(file.path(golden, "fixture"), game_results_raw, scores,
                 projections, picks, fx_standings, fx_params, fx_pool)

# --- Cases 3 to 8: random leagues for the playoff seeding ------------------
# Few games per team on purpose: many ties exercise every tie-break rule.
meta <- read_excel("picks.xlsx", sheet = "meta") |> as.data.frame()

random_league <- function(seed, games_per_team) {
  set.seed(seed)
  n_games <- games_per_team * nrow(meta) / 2
  g <- tibble(a = sample(meta$abbreviation, n_games, replace = TRUE),
              b = sample(meta$abbreviation, n_games, replace = TRUE)) |>
    filter(a != b) |>
    mutate(sa = sample(90:130, n(), replace = TRUE),
           sb = sample(90:130, n(), replace = TRUE)) |>
    filter(sa != sb)
  scores_tidy <- bind_rows(
    g |> transmute(team_slug = a, opponent_slug = b, score = sa, opponent_score = sb),
    g |> transmute(team_slug = b, opponent_slug = a, score = sb, opponent_score = sa)
  )
  rec <- scores_tidy |>
    group_by(team_slug) |>
    summarize(wins = sum(score > opponent_score),
              losses = sum(score < opponent_score), .groups = "drop")
  st <- meta |>
    transmute(team_name = team, conference, division, abbreviation) |>
    left_join(rec, by = c("abbreviation" = "team_slug")) |>
    mutate(wins = coalesce(wins, 0L), losses = coalesce(losses, 0L),
           `Winning Pct` = if_else(wins + losses > 0,
                                   round(wins / (wins + losses), 3), 0),
           `Games Back` = "-") |>
    select(team_name, conference, division, wins, losses, `Winning Pct`,
           `Games Back`, abbreviation)
  list(standings = st, scores_tidy = scores_tidy)
}

leagues <- list(list(1, 3), list(2, 3), list(3, 6), list(4, 6), list(5, 12),
                list(6, 20))
for (i in seq_along(leagues)) {
  lg <- random_league(leagues[[i]][[1]], leagues[[i]][[2]])
  seeded <- compute_playoff_seeding(lg$standings, lg$scores_tidy, meta)
  export_seeding_case(file.path(golden, sprintf("seeding_%d", i)), meta,
                      lg$standings, lg$scores_tidy, seeded)
}

cat("Wrote golden cases to", golden, "\n")
print(list.files(golden))
