# Rebuild each past season's chance-to-win and top-4 charts with the 2026-27
# method (09-season-graphics.R), for restyle_season_pages.py to add to the
# 2021-2026 pages. As on the 2026-27 page, each team's starting estimate sits
# between the pool's line and DARKO's preseason wins (chance_center_weight in
# 00-season-config.R); 2020-21 has no DARKO projection on file and uses the
# line alone. Each remaining game is played between its two teams, from the
# season's real schedule. Run from the repo root:
#
#   Rscript --vanilla nba-over-under-META/site-style/past_chances.R [years]
#
# Writes nba-over-under-META/site-style/past-chances/<year>.json, one per
# season-ending year, holding the two plotly figures. Takes about 15 minutes
# a season at 100,000 simulations per date.

suppressMessages({library(ggplot2); library(jsonlite)})
source("nba-over-under-META/site-style/past_seasons.R")
source("nba-over-under-2026-2027/09-season-graphics.R")

center_weight <- local({
  source("nba-over-under-2026-2027/00-season-config.R", local = TRUE)
  chance_center_weight
})
out_dir <- "nba-over-under-META/site-style/past-chances"
dir.create(out_dir, showWarnings = FALSE)
only <- commandArgs(trailingOnly = TRUE)

for (i in seq_len(nrow(past_seasons))) {
  s <- past_seasons[i, ]
  if (length(only) && !(as.character(s$year) %in% only)) next
  t0 <- Sys.time()
  d <- load_past_season(s)
  center <- chance_prior_center(d$projections, d$label, center_weight,
                                "nba-over-under-META/preseason_projections.csv")
  chances <- chance_by_date(d$standings_grid, d$projections, d$picks,
                            n_games = d$n_games, center = center,
                            schedule = d$schedule)
  winner <- chances |> filter(date == max(date), final_rank == 1) |> pull(player)
  figs <- map(c(first = "p_first", top4 = "p_top4"), function(m) {
    fig <- fromJSON(plotly_json(chance_chart(chances, m), jsonedit = FALSE),
                    simplifyVector = FALSE)[c("data", "layout", "config")]
    # chance_chart leaves dtick unset on a long season; in JSON that comes out
    # as an empty list, which makes plotly label every date
    if (!length(fig$layout$xaxis$dtick)) fig$layout$xaxis$dtick <- NULL
    fig
  })
  write_json(c(list(season = d$label, winner = winner, darko = !is.null(center)), figs),
             file.path(out_dir, paste0(s$year, ".json")),
             auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  cat(sprintf("%s (%s): %d dates, final first place %s, %.0f s\n", d$label,
              if (is.null(center)) "line alone" else "line and DARKO",
              length(d$dates), paste(winner, collapse = " & "),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
