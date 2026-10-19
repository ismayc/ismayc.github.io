# Rooting guide building blocks, shared by the 2025-26 generator and
# nba-over-under-META/site-style/past_rooting_guides.R so every season's guide
# is built the same way (000-generate-rooting-guide.R for 2026-27 still holds
# its own copy of guide_teams, guide_picks, and the page).
#
# A day's `out_table` has the determined_outcomes columns: Team, Outcome
# Determined, Current Record, Win Projection, Remaining Games, Wins To Go Over
# Vegas. `schedule` has game_date, away_team, home_team.

# A top-6 seed the team below can no longer catch, by that day's standings
# (team_name, conference, wins, losses)
guide_rest_candidates <- function(standings, n_games = 82) {
  if (is.null(standings) || !"conference" %in% names(standings)) return(character(0))
  standings %>%
    transmute(team_name, conference, wins = as.integer(wins),
              losses = as.integer(losses),
              max_possible_wins = wins + as.integer(n_games) - wins - losses) %>%
    arrange(conference, desc(wins), losses) %>%
    group_by(conference) %>%
    mutate(seed = row_number(),
           clinched = wins > lead(max_possible_wins)) %>%
    ungroup() %>%
    filter(seed <= 6, clinched %in% TRUE) %>%
    pull(team_name)
}

# D.teams: one entry per team, with its remaining schedule (games on or after
# guide_date) and whether it may rest starters
guide_teams <- function(out_table, rest_candidates, schedule, guide_date, n_games = 82) {
  remaining <- schedule %>% filter(game_date >= guide_date)
  teams <- list()
  for (i in seq_len(nrow(out_table))) {
    row <- out_table[i, ]
    team <- row[["Team"]]
    rec <- str_split(row[["Current Record"]], "-")[[1]]
    wins <- as.integer(rec[1])
    losses <- as.integer(rec[2])
    wp <- row[["Win Projection"]]
    det <- row[["Outcome Determined"]]
    wtg <- row[["Wins To Go Over Vegas"]]
    winPct <- round(wins / (wins + losses), 3)
    ltg <- if (det == "not yet") {
      val <- (n_games - ceiling(wp)) - losses + 1
      if (val > 0) val else NA_real_
    } else {
      NA_real_
    }
    sched <- bind_rows(
      remaining %>% filter(home_team == team) %>%
        transmute(d = as.character(game_date), opp = away_team, home = TRUE),
      remaining %>% filter(away_team == team) %>%
        transmute(d = as.character(game_date), opp = home_team, home = FALSE)
    ) %>%
      arrange(d)
    teams[[team]] <- list(
      w = wins, l = losses, wp = wp, rem = row[["Remaining Games"]], det = det,
      wtg = if (is.na(wtg)) NULL else as.integer(wtg),
      ltg = if (is.na(ltg)) NULL else as.integer(ltg),
      pWins = round(winPct * n_games, 1), winPct = winPct,
      restCandidate = team %in% rest_candidates,
      sched = lapply(seq_len(nrow(sched)), function(k) {
        list(d = sched$d[k], opp = sched$opp[k], home = sched$home[k])
      })
    )
  }
  teams
}

# D.picks: team -> player -> [wage, "OVER"/"UNDER"], from team, player, wage, choice
guide_picks <- function(picks_raw, players) {
  wide <- picks_raw %>%
    pivot_wider(id_cols = team, names_from = player, values_from = c(wage, choice),
                names_glue = "{player}_{.value}")
  picks <- list()
  for (i in seq_len(nrow(wide))) {
    team <- wide[["team"]][i]
    picks[[team]] <- list()
    for (p in players) {
      w_val <- wide[[paste0(p, "_wage")]][i]
      c_val <- wide[[paste0(p, "_choice")]][i]
      if (is.na(w_val) || is.na(c_val)) message("  WARNING: Missing pick for ", p, " on ", team)
      picks[[team]][[p]] <- list(as.integer(w_val), as.character(c_val))
    }
  }
  picks
}

# One day's history row. `through` is the last date of games in out_table.
guide_snap <- function(out_table, through, team_order) {
  # A few files from failed builds have "NA-NA" records; skip those days
  if (!all(str_detect(out_table$`Current Record`, "^\\d+-\\d+$"))) return(NULL)
  if (!setequal(out_table$Team, team_order)) return(NULL)
  # Some early-season 2025-26 files predate the wins-to-go column
  if (!"Wins To Go Over Vegas" %in% names(out_table)) return(NULL)
  d <- out_table[match(team_order, out_table$Team), ]
  rec <- str_split_fixed(d$`Current Record`, "-", 2)
  det <- recode(as.character(d$`Outcome Determined`), OVER = "O", UNDER = "U", .default = "")
  list(
    d = format(as.Date(through)),
    n = sum(det == ""),
    r = map(seq_along(team_order), \(i) list(
      as.integer(rec[i, 1]), as.integer(rec[i, 2]), det[i],
      as.integer(d$`Remaining Games`[i]),
      as.integer(d$`Wins To Go Over Vegas`[i])
    ))
  )
}

# The Progression history: `days` is a list of list(through, out_table,
# standings), oldest first. Keeps the days from the first with max_open or
# fewer teams open, so every tab can be shown as of any of them.
#
# Returns list(teams, maxOpen, snaps, games). Each snap is list(d = games-
# through date, n = teams open, r = one [wins, losses, "O"/"U"/"", remaining,
# wins to go] row per team in `team_order`, rest = 0-based indexes of rest
# candidates). `games` is [date, away index, home index] for every game after
# the first snapshot. Serialize with toJSON(na = "null").
guide_history <- function(days, team_order, max_open, schedule, n_games = 82) {
  snaps <- map(days, function(x) {
    s <- guide_snap(x$out_table, x$through, team_order)
    if (is.null(s)) return(NULL)
    rest <- guide_rest_candidates(x$standings(), n_games)
    c(s, list(rest = I(match(rest, team_order) - 1L)))
  }) %>%
    compact()

  # The window opens on the first date at or under max_open and runs to the end
  start <- detect_index(snaps, \(s) s$n <= max_open)
  snaps <- if (start == 0) list() else snaps[start:length(snaps)]

  games <- list()
  if (length(snaps)) {
    later <- schedule %>%
      filter(game_date > as.Date(snaps[[1]]$d)) %>%
      arrange(game_date)
    games <- map(seq_len(nrow(later)), \(i) list(
      format(later$game_date[i]),
      match(later$away_team[i], team_order) - 1L,
      match(later$home_team[i], team_order) - 1L
    ))
  }
  cat("  Progression:", length(snaps), "snapshots with", max_open,
      "or fewer teams open,", length(games), "games\n")
  list(teams = team_order, maxOpen = max_open, snaps = snaps, games = games)
}

# The history from a season's saved determined_outcomes and standings files,
# through the guide's own outcomes file. Each file holds the games through
# the day before its date.
rooting_guide_history <- function(season_dir, team_order, max_open, schedule,
                                  last_file_date = Sys.Date(), n_games = 82) {
  files <- sort(list.files(file.path(season_dir, "determined_outcomes"),
                           pattern = "^determined_outcomes_.*\\.rds$", full.names = TRUE))
  file_dates <- as.Date(str_extract(basename(files), "\\d{4}-\\d{2}-\\d{2}"))
  keep <- file_dates <= last_file_date
  days <- map2(files[keep], file_dates[keep], function(f, fd) {
    through <- fd - 1
    list(through = through, out_table = read_rds(f),
         # Standings through that date (or the day before), read only for kept days
         standings = function() {
           sf <- file.path(season_dir, "rds", paste0("standings_through_", c(through, through - 1), ".rds"))
           sf <- sf[file.exists(sf)]
           if (length(sf)) read_rds(sf[1]) else NULL
         })
  })
  guide_history(days, team_order, max_open, schedule, n_games)
}

# The page before restyle_season_pages.py puts it in the site theme (which
# replaces this stylesheet)
guide_page <- function(data_json, players_json, history_json, js_code,
                       season_label, back_href, data_date, rem_lo, rem_hi) {
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
  paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
    '<meta charset="UTF-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
    '<title>NBA Over/Under Rooting Guide</title>\n',
    '<link href="https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800&family=JetBrains+Mono:wght@400;500;600&display=swap" rel="stylesheet">\n',
    '<style>', css_block, '</style>\n',
    '</head>\n<body>\n',
    '<div class="shell">\n',
    '<a class="back" href="', back_href, '">&larr; ', season_label, ' season page</a>\n',
    '<h1>NBA Over/Under Rooting Guide</h1>\n',
    '<div class="date-line">Season ', season_label, ' &middot; Data through ', data_date,
    ' &middot; ', rem_lo, '-', rem_hi, ' games remaining per team</div>\n',
    '<div class="tabs" id="tabs"></div>\n',
    '<div id="panels"></div>\n',
    '</div>\n',
    '<script>\nconst D=', data_json, ';\n',
    'const P=', players_json, ';\n',
    'const H=', history_json, ';\n',
    js_code, '\n</script>\n',
    '</body>\n</html>'
  )
}
