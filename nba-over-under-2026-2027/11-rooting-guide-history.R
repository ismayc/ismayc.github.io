# Rooting guide Progression: the determined_outcomes snapshots from the first
# date with max_open or fewer teams open through the guide's own date, so
# every tab can be shown as of any of those dates. Used by
# 000-generate-rooting-guide.R for the current season and by the 2025-26
# generator. Each file holds the games through the day before its date.
#
# Returns list(teams, maxOpen, snaps, games). Each snap is list(d = games-
# through date, n = teams open, r = one [wins, losses, "O"/"U"/"", remaining,
# wins to go] row per team in `team_order`, rest = 0-based indexes of rest
# candidates). `games` is [date, away index, home index] for every game after
# the first snapshot. Serialize with toJSON(na = "null").

# Same rule as the generator's section 4b: a top-6 seed the team below can no
# longer catch, by that date's standings
guide_rest_candidates <- function(standings) {
  if (is.null(standings) || !"conference" %in% names(standings)) return(character(0))
  standings %>%
    transmute(team_name, conference, wins = as.integer(wins),
              losses = as.integer(losses),
              max_possible_wins = wins + 82L - wins - losses) %>%
    arrange(conference, desc(wins), losses) %>%
    group_by(conference) %>%
    mutate(seed = row_number(),
           clinched = wins > lead(max_possible_wins)) %>%
    ungroup() %>%
    filter(seed <= 6, clinched %in% TRUE) %>%
    pull(team_name)
}

rooting_guide_history <- function(season_dir, team_order, max_open, schedule,
                                  last_file_date = Sys.Date()) {
  outcomes_dir <- file.path(season_dir, "determined_outcomes")
  files <- sort(list.files(outcomes_dir, pattern = "^determined_outcomes_.*\\.rds$",
                           full.names = TRUE))
  file_dates <- as.Date(str_extract(basename(files), "\\d{4}-\\d{2}-\\d{2}"))
  files <- files[file_dates <= last_file_date]

  snaps <- map(files, function(f) {
    d <- read_rds(f)
    # A few files from failed builds have "NA-NA" records; skip those days
    if (!all(str_detect(d$`Current Record`, "^\\d+-\\d+$"))) return(NULL)
    if (!setequal(d$Team, team_order)) return(NULL)
    d <- d[match(team_order, d$Team), ]
    rec <- str_split_fixed(d$`Current Record`, "-", 2)
    det <- recode(d$`Outcome Determined`, OVER = "O", UNDER = "U", .default = "")
    list(
      d = format(as.Date(str_extract(basename(f), "\\d{4}-\\d{2}-\\d{2}")) - 1),
      n = sum(det == ""),
      r = map(seq_along(team_order), \(i) list(
        as.integer(rec[i, 1]), as.integer(rec[i, 2]), det[i],
        as.integer(d$`Remaining Games`[i]),
        as.integer(d$`Wins To Go Over Vegas`[i])
      ))
    )
  }) %>%
    compact()

  # The window opens on the first date at or under max_open and runs to the end
  start <- detect_index(snaps, \(s) s$n <= max_open)
  snaps <- if (start == 0) list() else snaps[start:length(snaps)]

  # Rest candidates from the standings through each date (or the day before)
  snaps <- map(snaps, function(s) {
    through <- as.Date(s$d)
    sf <- file.path(season_dir, "rds", paste0("standings_through_", c(through, through - 1), ".rds"))
    sf <- sf[file.exists(sf)]
    rest <- guide_rest_candidates(if (length(sf)) read_rds(sf[1]) else NULL)
    c(s, list(rest = I(match(rest, team_order) - 1L)))
  })

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
