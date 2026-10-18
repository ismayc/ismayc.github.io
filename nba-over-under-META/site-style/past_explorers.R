# Build each past season's scenario explorer, covering every date from the
# first with 20 or fewer teams open to the last game. Run from the repo root:
#
#   Rscript --vanilla nba-over-under-META/site-style/past_explorers.R
#
# Writes <year>-nba-scenario-explorer.html to the repo root and docs/, which
# restyle_season_pages.py embeds as the Scenarios tab of each season page.
# Each opens on the first date the explorer would have been published.

source("nba-over-under-META/site-style/past_seasons.R")
source("nba-over-under-2026-2027/10-scenario-explorer.R")
explorer_max_open <- 20
template <- "nba-over-under-2026-2027/scenario-explorer-template.html"

for (i in seq_len(nrow(past_seasons))) {
  s <- past_seasons[i, ]
  d <- load_past_season(s)
  lines <- d$projections |> rename(line = win_projection)
  snapshots <- map(d$dates, function(dt) {
    d$standings_grid |>
      filter(date == dt) |>
      transmute(team, w = current_wins, l = current_losses) |>
      left_join(lines, by = "team") |>
      mutate(n_games = d$n_games) |>
      explorer_snapshot(date = dt)
  })
  # Through the last date that still had a team open
  snapshots <- keep(snapshots, \(x) x$n_open <= explorer_max_open & x$n_open > 0)
  name <- paste0(s$year, "-nba-scenario-explorer.html")
  write_scenario_explorer(snapshots, d$picks, d$label, name, template,
                          default_date = snapshots[[1]]$date)
  file.copy(name, file.path("docs", name), overwrite = TRUE)
  cat(sprintf("%s: %d dates, %s (%d open) to %s (%d open)\n", d$label,
              length(snapshots), snapshots[[1]]$date, snapshots[[1]]$n_open,
              snapshots[[length(snapshots)]]$date,
              snapshots[[length(snapshots)]]$n_open))
}
