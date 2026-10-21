#!/usr/bin/env Rscript
# 000-generate-rooting-guide.R
#
# Generates a self-contained HTML rooting guide dashboard.
# Reads:
#   - nba-over-under-2026-2027/determined_outcomes/determined_outcomes_{date}.rds   (from 03-data-wrangling.R)
#   - nba-over-under-2026-2027/rds/gs_picks_raw.rds                                (original picks source)
#   - nba-over-under-2026-2027/schedule-2026-27-after-ist.csv (or _initial.csv)    (from 00-get_schedule.R)
#   - nba-over-under-2026-2027/rds/standings_through_{date}.rds                     (from 02c)
#   - nba-over-under-2026-2027/rooting-guide-app.js                                 (dashboard JS logic)
#   - nba-over-under-2026-2027/scenario-explorer-template.html                      (scenario explorer template)
# Writes:
#   - docs/2027-nba-rooting-guide.html
#   - docs/nba-scenario-explorer.html

library(tidyverse)
library(jsonlite)
library(glue)

source("nba-over-under-2026-2027/00-season-config.R")

cat("Generating rooting guide...\n")

# -- 1. Read determined outcomes -----------------------------------------------
outcomes_file <- paste0(
  "nba-over-under-2026-2027/determined_outcomes/determined_outcomes_", Sys.Date(), ".rds"
)
if (!file.exists(outcomes_file)) {
  outcomes_file <- paste0(
    "nba-over-under-2026-2027/determined_outcomes/determined_outcomes_", Sys.Date() - 1, ".rds"
  )
}
stopifnot(file.exists(outcomes_file))
out_table <- read_rds(outcomes_file)
cat("  Read outcomes from:", outcomes_file, "\n")

# -- 2. Read picks from original source ----------------------------------------
picks_raw <- read_rds("nba-over-under-2026-2027/rds/gs_picks_raw.rds") %>%
  mutate(
    choice = str_to_upper(pick),
    player = str_extract(str_trim(name), "^[^\\s]+")
  ) %>%
  select(team, player, wage = wager, choice)

players <- sort(unique(picks_raw$player))
cat("  Read picks for players:", paste(players, collapse = ", "), "\n")

# -- 3. Read today's schedule --------------------------------------------------
schedule <- read_csv(current_schedule_file(),
                     show_col_types = FALSE)
todays_games <- schedule %>%
  filter(game_date == Sys.Date()) %>%
  select(away_team, home_team)
cat("  Today's games:", nrow(todays_games), "\n")

# -- 3b. Read standings (for rest-candidate computation) -----------------------
standings_file <- paste0(
  "nba-over-under-2026-2027/rds/standings_through_", Sys.Date() - 1, ".rds"
)
if (!file.exists(standings_file)) {
  standings_file <- paste0(
    "nba-over-under-2026-2027/rds/standings_through_", Sys.Date() - 2, ".rds"
  )
}
if (file.exists(standings_file)) {
  standings <- read_rds(standings_file)
  cat("  Read standings from:", standings_file, "\n")
} else {
  standings <- NULL
  message("  WARNING: No standings file found; rest-candidate computation will be skipped")
}

# -- 4. Build JSON data blob ---------------------------------------------------
# Built with the shared functions the 2025-26 and past seasons' guides use
source("nba-over-under-2026-2027/11-rooting-guide-history.R")
rest_candidates <- guide_rest_candidates(standings)
cat("  Rest candidates:", length(rest_candidates), "teams\n")
teams_list <- guide_teams(out_table, rest_candidates, schedule, Sys.Date())
picks_list <- guide_picks(picks_raw, players)
games_list <- lapply(seq_len(nrow(todays_games)), function(i) {
  list(todays_games[["away_team"]][i], todays_games[["home_team"]][i])
})

# Check for team name mismatches between outcomes, picks, and the schedule
sched_teams <- unique(c(schedule$away_team, schedule$home_team))
for (msg in c(
  if (length(setdiff(sched_teams, names(teams_list)))) "Schedule teams not in outcomes",
  if (!setequal(names(teams_list), names(picks_list))) "Outcome and pick teams differ"
)) message("  WARNING: ", msg)

data_json <- toJSON(
  list(teams = teams_list, picks = picks_list, todaysGames = games_list),
  auto_unbox = TRUE, null = "null", pretty = FALSE
)
cat("  JSON data size:", nchar(data_json), "bytes\n")

# -- 4d. Progression history ---------------------------------------------------
history_json <- toJSON(
  rooting_guide_history("nba-over-under-2026-2027", names(teams_list), explorer_max_open, schedule,
                        as.Date(str_extract(outcomes_file, "\\d{4}-\\d{2}-\\d{2}"))),
  auto_unbox = TRUE, na = "null", pretty = FALSE
)

# -- 5. Read JS template -------------------------------------------------------
js_file <- "nba-over-under-2026-2027/rooting-guide-app.js"
stopifnot(file.exists(js_file))
js_code <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
cat("  Read JS from:", js_file, "\n")

# -- 6. Assemble HTML ----------------------------------------------------------
rem_range <- out_table %>%
  summarize(lo = min(`Remaining Games`), hi = max(`Remaining Games`))
html_output <- guide_page(data_json, toJSON(players, auto_unbox = TRUE), history_json,
                          js_code, season_label, "2027-nba-over-under.html",
                          format(Sys.Date() - 1, "%B %d, %Y"), rem_range$lo, rem_range$hi)

# -- 7. Write rooting guide output ---------------------------------------------
out_dir <- "docs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "2027-nba-rooting-guide.html")
writeLines(html_output, out_path)
file.copy(from = out_path, to = "2027-nba-rooting-guide.html", overwrite = TRUE)
cat("  Wrote:", out_path, "\n")

cat("Rooting guide complete!\n")

# =============================================================================
# SCENARIO EXPLORER
# =============================================================================
# Every Over/Under combination of the open teams is listed in the browser, so
# the explorer covers only the dates with explorer_max_open or fewer teams
# open. It is embedded as the Scenarios tab of the season page. Each
# determined_outcomes file holds the games through the day before its date.

cat("\nGenerating scenario explorer...\n")
source("nba-over-under-2026-2027/10-scenario-explorer.R")

outcome_files <- sort(list.files("nba-over-under-2026-2027/determined_outcomes",
                                 pattern = "^determined_outcomes_.*\\.rds$",
                                 full.names = TRUE))
snapshots <- map(outcome_files, function(f) {
  through <- as.Date(str_extract(f, "\\d{4}-\\d{2}-\\d{2}")) - 1
  d <- read_rds(f)
  # A few files from failed builds have "NA-NA" records; skip those days
  if (!all(str_detect(d$`Current Record`, "^\\d+-\\d+$"))) return(NULL)
  # Early in the season some teams have no row yet (see nba_teams)
  if (nrow(d) < nba_teams) return(NULL)
  d %>%
    separate_wider_delim(`Current Record`, "-", names = c("w", "l")) %>%
    transmute(team = Team, w = as.integer(w), l = as.integer(l),
              line = `Win Projection`, n_games = w + l + `Remaining Games`) %>%
    explorer_snapshot(date = through)
}) %>%
  compact() %>%
  keep(\(x) x$n_open > 0 && x$n_open <= explorer_max_open)

explorer_name <- paste0(ending_season_year, "-nba-scenario-explorer.html")
if (length(snapshots) == 0) {
  cat("  No date yet with ", explorer_max_open, " or fewer teams open\n", sep = "")
} else {
  write_scenario_explorer(snapshots, picks_raw, season_label,
                          file.path(out_dir, explorer_name),
                          "nba-over-under-2026-2027/scenario-explorer-template.html")
  file.copy(file.path(out_dir, explorer_name), explorer_name, overwrite = TRUE)
  cat("  Wrote:", explorer_name, "with", length(snapshots), "dates\n")
}
