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

# -- 4b. Compute rest candidates from standings --------------------------------
# A team is a rest candidate if it has mathematically clinched its current
# top-6 conference seed: the team one spot below cannot catch them even by
# winning all remaining games.
#
# Specifically: team A (seed K) has clinched over team B (seed K+1) if
#   A_wins > B_wins + B_remaining_games

if (!is.null(standings) && "conference" %in% names(standings)) {
  conf_rankings <- standings %>%
    select(team_name, conference, wins, losses) %>%
    mutate(
      wins = as.integer(wins),
      losses = as.integer(losses),
      rem = 82L - wins - losses,
      max_possible_wins = wins + rem
    ) %>%
    arrange(conference, desc(wins), losses) %>%
    group_by(conference) %>%
    mutate(seed = row_number()) %>%
    ungroup()
  
  rest_candidates <- character(0)
  
  for (conf in c("East", "West")) {
    conf_teams <- conf_rankings %>%
      filter(conference == conf) %>%
      arrange(seed)
    
    for (i in seq_len(min(6, nrow(conf_teams)))) {
      team_row <- conf_teams[i, ]
      
      # The team one seed below
      challenger_row <- if (i < nrow(conf_teams)) conf_teams[i + 1, ] else NULL
      if (is.null(challenger_row)) next
      
      # Has the team clinched over the challenger?
      clinched <- team_row$wins > challenger_row$max_possible_wins
      
      if (clinched) {
        rest_candidates <- c(rest_candidates, team_row$team_name)
        cat("    Rest candidate:", team_row$team_name,
            "(seed", i, conf, "- clinched over",
            challenger_row$team_name, ")\n")
      }
    }
  }
  
  cat("  Rest candidates:", length(rest_candidates), "teams\n")
} else {
  rest_candidates <- character(0)
  cat("  Skipped rest-candidate computation (no standings)\n")
}

# -- 4c. Attach remaining schedule + restCandidate to each team ----------------
remaining_games <- schedule %>%
  filter(game_date >= Sys.Date())

cat("  Remaining games in schedule:", nrow(remaining_games), "\n")

# Verify team name alignment between schedule and teams_list
sched_teams <- unique(c(remaining_games$away_team, remaining_games$home_team))
outcome_teams <- names(teams_list)
missing_from_outcomes <- setdiff(sched_teams, outcome_teams)
if (length(missing_from_outcomes) > 0) {
  message("  WARNING: Schedule teams not in teams_list: ",
          paste(missing_from_outcomes, collapse = ", "))
}

for (team_name in names(teams_list)) {
  # Rest flag
  teams_list[[team_name]][["restCandidate"]] <- team_name %in% rest_candidates
  
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

# Verify a sample team
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
missing_from_outcomes2 <- setdiff(teams_in_picks, teams_in_outcomes)
if (length(missing_from_picks) > 0) {
  message("  WARNING: Teams in outcomes but NOT in picks: ",
          paste(missing_from_picks, collapse = ", "))
}
if (length(missing_from_outcomes2) > 0) {
  message("  WARNING: Teams in picks but NOT in outcomes: ",
          paste(missing_from_outcomes2, collapse = ", "))
}

data_json <- toJSON(
  list(teams = teams_list, picks = picks_list, todaysGames = games_list),
  auto_unbox = TRUE, null = "null", pretty = FALSE
)
cat("  JSON data size:", nchar(data_json), "bytes\n")

# -- 4d. Progression tab history -----------------------------------------------
source("nba-over-under-2026-2027/11-rooting-guide-history.R")
history_json <- toJSON(
  rooting_guide_history("nba-over-under-2026-2027/determined_outcomes",
                        names(teams_list), explorer_max_open,
                        as.Date(str_extract(outcomes_file, "\\d{4}-\\d{2}-\\d{2}"))),
  auto_unbox = TRUE, pretty = FALSE
)

# -- 5. Read JS template -------------------------------------------------------
js_file <- "nba-over-under-2026-2027/rooting-guide-app.js"
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
.back{display:inline-block;margin-bottom:10px;font-size:13px;color:var(--t2);text-decoration:none}.back:hover{color:var(--t1)}
.date-line{font-size:12px;color:var(--t3);margin-bottom:24px;font-family:"JetBrains Mono",monospace}
.tabs{display:flex;gap:4px;margin-bottom:20px;border-bottom:1px solid var(--border);padding-bottom:0;overflow-x:auto}
.tab{padding:10px 16px;font-size:13px;font-weight:600;color:var(--t3);cursor:pointer;border-bottom:2px solid transparent;transition:.15s;white-space:nowrap}
.tab:hover{color:var(--t2)}.tab.active{color:var(--accent);border-color:var(--accent)}
.panel{display:none}.panel.active{display:block}
/* Fixed-width table rows scroll inside the tab on a phone instead of widening the page */
#tab-content{overflow-x:auto}
#tab-content div[style*="grid-template-columns"]:not([style*="1fr 1fr"]){min-width:600px}
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
  '<a class="back" href="2027-nba-over-under.html">&larr; ', season_label, ' season page</a>\n',
  '<h1>NBA Over/Under Rooting Guide</h1>\n',
  '<div class="date-line">Season ', season_label, ' &middot; Data through ', data_date,
  ' &middot; ', rem_range$lo, '-', rem_range$hi, ' games remaining per team</div>\n',
  '<div class="tabs" id="tabs"></div>\n',
  '<div id="panels"></div>\n',
  '</div>\n',
  '<script>\nconst D=', data_json, ';\n',
  'const P=', players_json, ';\n',
  'const H=', history_json, ';\n',
  js_code, '\n</script>\n',
  '</body>\n</html>'
)

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
