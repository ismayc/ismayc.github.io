# Rooting guide Progression tab: the determined_outcomes snapshots from the
# first date with max_open or fewer teams open through the guide's own date.
# Used by 000-generate-rooting-guide.R for the current season and by the
# 2025-26 generator. Each file holds the games through the day before its date.
#
# Returns list(teams, maxOpen, snaps). Each snap is list(d = games-through
# date, n = teams open, r = one [wins, losses, "O"/"U"/""] row per team in
# `team_order`). The browser scores each snapshot with the guide's own rules.
rooting_guide_history <- function(outcomes_dir, team_order, max_open,
                                  last_file_date = Sys.Date()) {
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
      r = map(seq_along(team_order),
              \(i) list(as.integer(rec[i, 1]), as.integer(rec[i, 2]), det[i]))
    )
  }) %>%
    compact()

  # The window opens on the first date at or under max_open and runs to the end
  start <- detect_index(snaps, \(s) s$n <= max_open)
  snaps <- if (start == 0) list() else snaps[start:length(snaps)]
  cat("  Progression:", length(snaps), "snapshots with", max_open,
      "or fewer teams open\n")
  list(teams = team_order, maxOpen = max_open, snaps = snaps)
}
