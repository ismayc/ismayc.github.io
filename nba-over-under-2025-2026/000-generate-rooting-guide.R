#!/usr/bin/env Rscript
# 000-generate-rooting-guide.R
#
# Generates a self-contained HTML rooting guide dashboard.
# Reads:
#   - nba-over-under-2025-2026/determined_outcomes/determined_outcomes_{date}.rds   (from 03-data-wrangling.R)
#   - nba-over-under-2025-2026/rds/gs_picks_raw.rds                                (original picks source)
#   - nba-over-under-2025-2026/schedule-2025-26-after-ist.csv                       (from 00-get_schedule.R)
#   - nba-over-under-2025-2026/rooting-guide-app.js                                 (dashboard JS logic)
#   - nba-over-under-2025-2026/scenario-explorer-template.html                      (scenario explorer template)
# Writes:
#   - docs/2026-nba-rooting-guide.html
#   - docs/nba-scenario-explorer.html

library(tidyverse)
library(jsonlite)
library(glue)

cat("Generating rooting guide...\n")

# -- 1. Read determined outcomes -----------------------------------------------
outcomes_file <- paste0(
  "nba-over-under-2025-2026/determined_outcomes/determined_outcomes_", Sys.Date(), ".rds"
)
if (!file.exists(outcomes_file)) {
  outcomes_file <- paste0(
    "nba-over-under-2025-2026/determined_outcomes/determined_outcomes_", Sys.Date() - 1, ".rds"
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

# Reshape to wide for JSON
picks_wide_clean <- picks_raw %>%
  pivot_wider(
    id_cols = team,
    names_from = player,
    values_from = c(wage, choice),
    names_glue = "{player}_{.value}"
  )
cat("  Picks columns:", paste(names(picks_wide_clean), collapse = ", "), "\n")

# -- 3. Read today's schedule --------------------------------------------------
schedule <- read_csv("nba-over-under-2025-2026/schedule-2025-26-after-ist.csv",
                     show_col_types = FALSE)
todays_games <- schedule %>%
  filter(game_date == Sys.Date()) %>%
  select(away_team, home_team)
cat("  Today's games:", nrow(todays_games), "\n")

# -- 4. Build JSON data blob ---------------------------------------------------

teams_list <- list()
for (i in seq_len(nrow(out_table))) {
  row <- out_table[i, ]
  rec <- str_split(row[["Current Record"]], "-")[[1]]
  wins <- as.integer(rec[1])
  losses <- as.integer(rec[2])
  wp <- row[["Win Projection"]]
  rem <- row[["Remaining Games"]]
  det <- row[["Outcome Determined"]]
  wtg <- row[["Wins To Go Over Vegas"]]
  winPct <- round(wins / (wins + losses), 3)
  pWins <- round(winPct * 82, 1)
  ltg <- if (det == "not yet") {
    val <- (82 - ceiling(wp)) - losses + 1
    if (val > 0) val else NA_real_
  } else {
    NA_real_
  }

  teams_list[[row[["Team"]]]] <- list(
    w = wins, l = losses, wp = wp, rem = rem, det = det,
    wtg = if (is.na(wtg)) NULL else as.integer(wtg),
    ltg = if (is.na(ltg)) NULL else as.integer(ltg),
    pWins = pWins, winPct = winPct
  )
}

picks_list <- list()
for (i in seq_len(nrow(picks_wide_clean))) {
  team <- picks_wide_clean[["team"]][i]
  picks_list[[team]] <- list()
  for (p in players) {
    wage_col <- paste0(p, "_wage")
    choice_col <- paste0(p, "_choice")
    w_val <- picks_wide_clean[[wage_col]][i]
    c_val <- picks_wide_clean[[choice_col]][i]
    if (is.na(w_val) || is.na(c_val)) {
      message("  WARNING: Missing pick for ", p, " on ", team)
    }
    picks_list[[team]][[p]] <- list(as.integer(w_val), as.character(c_val))
  }
}

games_list <- lapply(seq_len(nrow(todays_games)), function(i) {
  list(todays_games[["away_team"]][i], todays_games[["home_team"]][i])
})

# ── Add remaining schedule to teams_list ──────────────────────────────────────
#
# It reads the schedule CSV, filters to future games, and attaches a `sched`
# array to each team in teams_list with the shape:
#   [{d: "2026-04-01", opp: "Boston Celtics", home: TRUE}, ...]
#
# The JS then looks up D.teams[oppName].winPct for opponent strength.

# -- Read schedule and standings -----------------------------------------------
# (schedule is probably already read earlier in the script; reuse if so)
if (!exists("schedule")) {
  schedule <- read_csv("schedule-2025-26-after-ist.csv", show_col_types = FALSE)
}

# Remaining games: today and forward
remaining_games <- schedule %>%
  filter(game_date >= Sys.Date())

cat("  Remaining games in schedule:", nrow(remaining_games), "\n")

# -- Build a lookup from team names used in schedule to team names in teams_list
# The schedule uses basketball-reference names; teams_list uses the same names
# from out_table. They should match, but let's verify.
sched_teams <- unique(c(remaining_games$away_team, remaining_games$home_team))
outcome_teams <- names(teams_list)
missing_from_outcomes <- setdiff(sched_teams, outcome_teams)
if (length(missing_from_outcomes) > 0) {
  message("  WARNING: Schedule teams not in teams_list: ",
          paste(missing_from_outcomes, collapse = ", "))
}

# -- Attach remaining schedule to each team ------------------------------------
for (team_name in names(teams_list)) {
  # Games where this team is home
  home_games <- remaining_games %>%
    filter(home_team == team_name) %>%
    transmute(d = as.character(game_date), opp = away_team, home = TRUE)
  
  # Games where this team is away
  away_games <- remaining_games %>%
    filter(away_team == team_name) %>%
    transmute(d = as.character(game_date), opp = home_team, home = FALSE)
  
  team_sched <- bind_rows(home_games, away_games) %>%
    arrange(d)
  
  # Convert to list-of-lists for JSON
  if (nrow(team_sched) > 0) {
    teams_list[[team_name]][["sched"]] <- lapply(seq_len(nrow(team_sched)), function(i) {
      list(
        d = team_sched$d[i],
        opp = team_sched$opp[i],
        home = team_sched$home[i]
      )
    })
  } else {
    teams_list[[team_name]][["sched"]] <- list()
  }
}

cat("  Attached remaining schedule to", length(teams_list), "teams\n")

# -- Verify a sample team has schedule data ------------------------------------
sample_team <- names(teams_list)[1]
sample_sched <- teams_list[[sample_team]][["sched"]]
cat("  Sample:", sample_team, "has", length(sample_sched), "remaining games\n")
if (length(sample_sched) > 0) {
  cat("    First game:", sample_sched[[1]]$d, "vs", sample_sched[[1]]$opp,
      if (sample_sched[[1]]$home) "(home)" else "(away)", "\n")
}

# Check for team name mismatches between outcomes and picks
teams_in_outcomes <- names(teams_list)
teams_in_picks <- names(picks_list)
missing_from_picks <- setdiff(teams_in_outcomes, teams_in_picks)
missing_from_outcomes <- setdiff(teams_in_picks, teams_in_outcomes)
if (length(missing_from_picks) > 0) {
  message("  WARNING: Teams in outcomes but NOT in picks: ",
          paste(missing_from_picks, collapse = ", "))
}
if (length(missing_from_outcomes) > 0) {
  message("  WARNING: Teams in picks but NOT in outcomes: ",
          paste(missing_from_outcomes, collapse = ", "))
}

data_json <- toJSON(
  list(teams = teams_list, picks = picks_list, todaysGames = games_list),
  auto_unbox = TRUE, null = "null", pretty = FALSE
)
cat("  JSON data size:", nchar(data_json), "bytes\n")

# -- 5. Read JS template -------------------------------------------------------
js_file <- "nba-over-under-2025-2026/rooting-guide-app.js"
stopifnot(file.exists(js_file))
js_code <- paste(readLines(js_file, warn = FALSE), collapse = "\n")
cat("  Read JS from:", js_file, "\n")

# -- 6. Assemble HTML ----------------------------------------------------------
data_date <- format(Sys.Date() - 1, "%B %d, %Y")
rem_range <- out_table %>%
  summarize(lo = min(`Remaining Games`), hi = max(`Remaining Games`))

# Build players array for JS
players_json <- toJSON(players, auto_unbox = TRUE)

css_block <- '
*{margin:0;padding:0;box-sizing:border-box}
:root{--bg:#0b0f19;--card:#111827;--card2:#1a2236;--border:#1e293b;--t1:#f1f5f9;--t2:#94a3b8;--t3:#475569;--over:#10b981;--under:#f43f5e;--over-bg:rgba(16,185,129,.1);--under-bg:rgba(244,63,94,.1);--over-border:rgba(16,185,129,.25);--under-border:rgba(244,63,94,.25);--accent:#3b82f6;--gold:#f59e0b;--purple:#a78bfa}
body{font-family:"Outfit",sans-serif;background:var(--bg);color:var(--t1);min-height:100vh}
.shell{max-width:1080px;margin:0 auto;padding:20px 16px 60px}
h1{font-size:28px;font-weight:800;letter-spacing:-.5px;margin-bottom:2px}
.date-line{font-size:12px;color:var(--t3);margin-bottom:24px;font-family:"JetBrains Mono",monospace}
.tabs{display:flex;gap:4px;margin-bottom:20px;border-bottom:1px solid var(--border);padding-bottom:0;overflow-x:auto}
.tab{padding:10px 16px;font-size:13px;font-weight:600;color:var(--t3);cursor:pointer;border-bottom:2px solid transparent;transition:.15s;white-space:nowrap}
.tab:hover{color:var(--t2)}.tab.active{color:var(--accent);border-color:var(--accent)}
.panel{display:none}.panel.active{display:block}
.player-row{display:flex;gap:6px;margin-bottom:16px;flex-wrap:wrap;align-items:center}
.player-row .label{font-size:10px;font-weight:700;color:var(--t3);text-transform:uppercase;letter-spacing:1px;margin-right:4px}
.pp{padding:5px 13px;border-radius:20px;font-size:12px;font-weight:600;cursor:pointer;border:1px solid var(--border);background:transparent;color:var(--t2);font-family:inherit;transition:.12s}
.pp:hover{border-color:var(--t3);color:var(--t1)}.pp.active{background:var(--accent);border-color:var(--accent);color:#fff}
.section-title{font-size:16px;font-weight:700;color:var(--t1);margin:20px 0 12px;display:flex;align-items:center;gap:8px}
.trow{display:grid;align-items:center;padding:7px 10px;border-radius:6px;font-size:13px;gap:8px}
.trow:nth-child(odd){background:rgba(255,255,255,.02)}.trow:nth-child(even){background:transparent}
.team-name{font-weight:600;color:var(--t1);font-size:12px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
.mono{font-family:"JetBrains Mono",monospace;font-size:12px}
.pill-sm{display:inline-block;padding:2px 8px;border-radius:10px;font-size:10px;font-weight:700;letter-spacing:.5px}
.pill-over{background:var(--over-bg);color:var(--over);border:1px solid var(--over-border)}
.pill-under{background:var(--under-bg);color:var(--under);border:1px solid var(--under-border)}
.prog-wrap{height:6px;background:var(--border);border-radius:3px;overflow:hidden;flex:1}
.prog-fill{height:100%;border-radius:3px;transition:width .3s}
.game-card{background:var(--card);border:1px solid var(--border);border-radius:10px;padding:16px;margin-bottom:10px}
.game-matchup{font-size:15px;font-weight:700;margin-bottom:10px;display:flex;align-items:center;gap:8px}
.game-at{color:var(--t3);font-weight:400;font-size:12px}
.chip{display:inline-block;padding:3px 8px;border-radius:10px;font-size:11px;font-weight:600;margin:2px}
.chip-o{background:var(--over-bg);color:var(--over);border:1px solid var(--over-border)}
.chip-u{background:var(--under-bg);color:var(--under);border:1px solid var(--under-border)}
.faded{opacity:.4}.bright{opacity:1}
.error-box{background:#2d1215;border:1px solid #7f1d1d;border-radius:8px;padding:14px;margin:12px 0;color:#fca5a5;font-size:13px;font-family:"JetBrains Mono",monospace;white-space:pre-wrap}
@media(max-width:640px){.tabs{gap:0}.tab{padding:8px 10px;font-size:12px}h1{font-size:22px}}
'

html_output <- paste0(
  '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
  '<meta charset="UTF-8">\n',
  '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
  '<title>NBA Over/Under Rooting Guide</title>\n',
  '<link href="https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600&display=swap" rel="stylesheet">\n',
  '<style>', css_block, '</style>\n',
  '</head>\n<body>\n',
  '<div class="shell">\n',
  '<h1>NBA Over/Under Rooting Guide</h1>\n',
  '<div class="date-line">Season 2025-26 &middot; Data through ', data_date,
  ' &middot; ', rem_range$lo, '-', rem_range$hi, ' games remaining per team</div>\n',
  '<div class="tabs" id="tabs"></div>\n',
  '<div id="panels"></div>\n',
  '</div>\n',
  '<script>\nconst D=', data_json, ';\n',
  'const P=', players_json, ';\n',
  js_code, '\n</script>\n',
  '</body>\n</html>'
)

# -- 7. Write rooting guide output ---------------------------------------------
out_dir <- "docs"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
out_path <- file.path(out_dir, "2026-nba-rooting-guide.html")
file.copy(from = out_path, to = "2026-nba-rooting-guide.html", overwrite = TRUE)
writeLines(html_output, out_path)
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

likely_gap_threshold <- 20  # percentage points between current pace and what's needed

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
undecided_json <- toJSON(undecided_vec, auto_unbox = TRUE)

# -- Read template and inject data ---------------------------------------------
template_file <- "nba-over-under-2025-2026/scenario-explorer-template.html"
stopifnot(file.exists(template_file))
template <- paste(readLines(template_file, warn = FALSE), collapse = "\n")

scenario_html <- template
scenario_html <- gsub("__CLINCHED_JSON__", clinched_json, scenario_html, fixed = TRUE)
scenario_html <- gsub("__LIKELY_JSON__", likely_json, scenario_html, fixed = TRUE)
scenario_html <- gsub("__UNDECIDED_JSON__", undecided_json, scenario_html, fixed = TRUE)

scenario_path <- file.path(out_dir, "nba-scenario-explorer.html")
writeLines(scenario_html, scenario_path)
cat("  Wrote:", scenario_path, "\n")

cat("Scenario explorer complete!\n")
