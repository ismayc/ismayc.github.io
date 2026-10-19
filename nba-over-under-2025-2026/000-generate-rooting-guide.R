#!/usr/bin/env Rscript
# 000-generate-rooting-guide.R
#
# Generates a self-contained HTML rooting guide dashboard.
# Reads:
#   - nba-over-under-2025-2026/determined_outcomes/determined_outcomes_{date}.rds   (from 03-data-wrangling.R)
#   - nba-over-under-2025-2026/rds/gs_picks_raw.rds                                (original picks source)
#   - nba-over-under-2025-2026/schedule-2025-26-after-ist.csv                       (from 00-get_schedule.R)
#   - nba-over-under-2025-2026/rds/standings_through_{date}.rds                     (from 02c)
#   - nba-over-under-2025-2026/rooting-guide-app.js                                 (dashboard JS logic)
#   - nba-over-under-2025-2026/scenario-explorer-template.html                      (scenario explorer template)
# Writes:
#   - docs/2026-nba-rooting-guide.html
#   - docs/nba-scenario-explorer.html

library(tidyverse)
library(jsonlite)
library(glue)

cat("Generating rooting guide...\n")

# The season is over, so rebuild it as of a past date: GUIDE_DATE=2026-04-13
# reproduces the final page (games through April 12, 2026)
guide_date <- as.Date(Sys.getenv("GUIDE_DATE", as.character(Sys.Date())))

# -- 1. Read determined outcomes -----------------------------------------------
outcomes_file <- paste0(
  "nba-over-under-2025-2026/determined_outcomes/determined_outcomes_", guide_date, ".rds"
)
if (!file.exists(outcomes_file)) {
  outcomes_file <- paste0(
    "nba-over-under-2025-2026/determined_outcomes/determined_outcomes_", guide_date - 1, ".rds"
  )
}
stopifnot(file.exists(outcomes_file))
out_table <- read_rds(outcomes_file)
cat("  Read outcomes from:", outcomes_file, "\n")

# -- 2. Read picks from original source ----------------------------------------
picks_raw <- read_rds("nba-over-under-2025-2026/rds/gs_picks_raw.rds") %>%
  mutate(
    choice = str_to_upper(pick),
    player = str_extract(str_trim(name), "^[^\\s]+")
  ) %>%
  select(team, player, wage = wager, choice)

players <- sort(unique(picks_raw$player))
cat("  Read picks for players:", paste(players, collapse = ", "), "\n")

# -- 3. Read today's schedule --------------------------------------------------
schedule <- read_csv("nba-over-under-2025-2026/schedule-2025-26-after-ist.csv",
                     show_col_types = FALSE)
todays_games <- schedule %>%
  filter(game_date == guide_date) %>%
  select(away_team, home_team)
cat("  Today's games:", nrow(todays_games), "\n")

# -- 3b. Read standings (for rest-candidate computation) -----------------------
standings_file <- paste0(
  "nba-over-under-2025-2026/rds/standings_through_", guide_date - 1, ".rds"
)
if (!file.exists(standings_file)) {
  standings_file <- paste0(
    "nba-over-under-2025-2026/rds/standings_through_", guide_date - 2, ".rds"
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
# Built with the shared functions the past seasons' guides use
source("nba-over-under-2026-2027/11-rooting-guide-history.R")
rest_candidates <- guide_rest_candidates(standings)
cat("  Rest candidates:", length(rest_candidates), "teams\n")
teams_list <- guide_teams(out_table, rest_candidates, schedule, guide_date)
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
  rooting_guide_history("nba-over-under-2025-2026", names(teams_list), 20, schedule,
                        as.Date(str_extract(outcomes_file, "\\d{4}-\\d{2}-\\d{2}"))),
  auto_unbox = TRUE, na = "null", pretty = FALSE
)

# -- 5. Read JS template -------------------------------------------------------
js_file <- "nba-over-under-2025-2026/rooting-guide-app.js"
stopifnot(file.exists(js_file))
js_code <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
cat("  Read JS from:", js_file, "\n")

# -- 6. Assemble HTML ----------------------------------------------------------
rem_range <- out_table %>%
  summarize(lo = min(`Remaining Games`), hi = max(`Remaining Games`))
html_output <- guide_page(data_json, toJSON(players, auto_unbox = TRUE), history_json,
                          js_code, "2025-26", "2026-nba-over-under.html",
                          format(guide_date - 1, "%B %d, %Y"), rem_range$lo, rem_range$hi)

# -- 7. Write rooting guide output ---------------------------------------------
out_dir <- "docs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "2026-nba-rooting-guide.html")
writeLines(html_output, out_path)
file.copy(from = out_path, to = "2026-nba-rooting-guide.html", overwrite = TRUE)
cat("  Wrote:", out_path, "\n")

cat("Rooting guide complete!\n")

# =============================================================================
# SCENARIO EXPLORER
# =============================================================================

cat("\nGenerating scenario explorer...\n")

# -- Classify teams into CLINCHED, LIKELY, UNDECIDED ---------------------------
# Clinched: outcome already determined
# Likely: win% needed is 20+ percentage points away from the team's current pace
#   e.g. a 60% team needing 25% = likely OVER (35pt gap, coasting)
#        a 30% team needing 25% = undecided (only 5pt gap, that IS their pace)
# Undecided: everything else

likely_gap_threshold <- 25  # percentage points between current pace and what's needed

clinched_list <- list()
likely_list <- list()
undecided_vec <- c()

for (i in seq_len(nrow(out_table))) {
  row <- out_table[i, ]
  team <- row[["Team"]]
  det <- row[["Outcome Determined"]]
  
  if (det == "OVER") {
    clinched_list[[team]] <- "O"
  } else if (det == "UNDER") {
    clinched_list[[team]] <- "U"
  } else {
    rec <- str_split(row[["Current Record"]], "-")[[1]]
    wins <- as.integer(rec[1])
    losses <- as.integer(rec[2])
    rem <- row[["Remaining Games"]]
    wtg <- row[["Wins To Go Over Vegas"]]
    current_wpct <- wins / (wins + losses) * 100
    
    if (!is.na(wtg) && rem > 0) {
      wpn <- wtg / rem * 100
    } else {
      wpn <- 50
    }
    
    gap <- wpn - current_wpct  # positive = needs to play better than pace
    
    cat(sprintf("    %-28s  pace=%4.1f%%  needed=%4.1f%%  gap=%+5.1f\n",
                team, current_wpct, wpn, gap))
    
    if (gap <= -likely_gap_threshold) {
      # Needs far less than current pace -> likely OVER
      likely_list[[team]] <- "O"
    } else if (gap >= likely_gap_threshold) {
      # Needs far more than current pace -> likely UNDER
      likely_list[[team]] <- "U"
    } else {
      undecided_vec <- c(undecided_vec, team)
    }
  }
}

cat("  Clinched:", length(clinched_list), "teams\n")
cat("  Likely:", length(likely_list), "teams\n")
cat("  Undecided:", length(undecided_vec), "teams\n")

clinched_json <- toJSON(clinched_list, auto_unbox = TRUE)
likely_json <- toJSON(likely_list, auto_unbox = TRUE)
# as.list() forces jsonlite to always emit an array, even for 0 or 1 elements
undecided_json <- toJSON(as.list(undecided_vec), auto_unbox = TRUE)

# -- Read template and inject data ---------------------------------------------
template_file <- "nba-over-under-2025-2026/scenario-explorer-template.html"
stopifnot(file.exists(template_file))
template <- paste(readLines(template_file, warn = FALSE), collapse = "\n")

# Use str_replace with fixed() for truly literal replacement
# (gsub can misinterpret backslash sequences in the replacement string)
scenario_html <- template
scenario_html <- str_replace(scenario_html, fixed("__CLINCHED_JSON__"), clinched_json)
scenario_html <- str_replace(scenario_html, fixed("__LIKELY_JSON__"), likely_json)
scenario_html <- str_replace(scenario_html, fixed("__UNDECIDED_JSON__"), undecided_json)

# Verify all placeholders were replaced
remaining <- sum(str_detect(scenario_html, fixed("__CLINCHED_JSON__")),
                 str_detect(scenario_html, fixed("__LIKELY_JSON__")),
                 str_detect(scenario_html, fixed("__UNDECIDED_JSON__")))
if (remaining > 0) {
  warning("  Template still has ", remaining, " unreplaced placeholder(s)!")
} else {
  cat("  All placeholders replaced successfully\n")
}

scenario_path <- file.path(out_dir, "nba-scenario-explorer.html")
writeLines(scenario_html, scenario_path)
cat("  Wrote:", scenario_path, "\n")

cat("Scenario explorer complete!\n")