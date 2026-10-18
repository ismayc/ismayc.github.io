# Scenario explorer: lists every Over/Under combination of the teams still
# open (2^n), so it covers only dates with explorer_max_open or fewer open.
# Used by 000-generate-rooting-guide.R for the current season and by
# nba-over-under-META/site-style/past_explorers.R for past seasons.

library(jsonlite)

# One date's state. `teams` has team, w, l, line, n_games. A team is clinched
# under the pipeline's rule (03-data-wrangling.R): Over once its wins reach
# ceiling(line), Under once that is out of reach. An open team is "likely"
# when the win % it needs is 25+ points from its pace; the explorer starts
# those on that side but lets them be flipped.
explorer_snapshot <- function(date, teams, likely_gap = 25) {
  t <- teams |>
    mutate(rem = n_games - w - l,
           wtg = ceiling(line) - w,
           det = case_when(wtg <= 0 ~ "O", wtg > rem ~ "U", TRUE ~ NA_character_),
           gap = wtg / rem * 100 - w / pmax(w + l, 1) * 100)
  open <- filter(t, is.na(det))
  likely <- filter(open, abs(gap) >= likely_gap)
  list(
    date = format(as.Date(date)),
    through = format(as.Date(date), "%B %d, %Y") |> sub(pattern = " 0", replacement = " "),
    n_open = nrow(open),
    clinched = as.list(setNames(t$det[!is.na(t$det)], t$team[!is.na(t$det)])),
    likely = as.list(setNames(if_else(likely$gap < 0, "O", "U"), likely$team)),
    undecided = I(setdiff(open$team, likely$team))
  )
}

team_abbr <- c(
  `Atlanta Hawks` = "ATL", `Boston Celtics` = "BOS", `Brooklyn Nets` = "BKN",
  `Charlotte Hornets` = "CHA", `Chicago Bulls` = "CHI",
  `Cleveland Cavaliers` = "CLE", `Dallas Mavericks` = "DAL",
  `Denver Nuggets` = "DEN", `Detroit Pistons` = "DET",
  `Golden State Warriors` = "GSW", `Houston Rockets` = "HOU",
  `Indiana Pacers` = "IND", `Los Angeles Clippers` = "LAC",
  `Los Angeles Lakers` = "LAL", `Memphis Grizzlies` = "MEM",
  `Miami Heat` = "MIA", `Milwaukee Bucks` = "MIL",
  `Minnesota Timberwolves` = "MIN", `New Orleans Pelicans` = "NOP",
  `New York Knicks` = "NYK", `Oklahoma City Thunder` = "OKC",
  `Orlando Magic` = "ORL", `Philadelphia 76ers` = "PHI", `Phoenix Suns` = "PHX",
  `Portland Trail Blazers` = "POR", `Sacramento Kings` = "SAC",
  `San Antonio Spurs` = "SAS", `Toronto Raptors` = "TOR", `Utah Jazz` = "UTA",
  `Washington Wizards` = "WAS"
)

# `picks` has team, player, wage, choice (OVER/UNDER). Writes the page and
# returns its path; `default_date` is the snapshot shown without ?date=.
write_scenario_explorer <- function(snapshots, picks, season_label, path,
                                    template, default_date = NULL) {
  stopifnot(length(snapshots) > 0, all(picks$team %in% names(team_abbr)))
  players <- sort(unique(picks$player))
  teams <- picks |>
    arrange(team, player) |>
    group_by(team) |>
    group_map(function(d, key) list(
      t = key$team, ab = unname(team_abbr[key$team]),
      p = setNames(purrr::map2(d$wage, substr(d$choice, 1, 1), list), d$player)))
  default_date <- default_date %||% snapshots[[length(snapshots)]]$date
  html <- paste(readLines(template, warn = FALSE), collapse = "\n")
  fill <- c(
    "__SNAPSHOTS_JSON__" = toJSON(snapshots, auto_unbox = TRUE),
    "__DEFAULT_DATE__" = toJSON(default_date, auto_unbox = TRUE),
    "__PLAYERS_JSON__" = toJSON(players),
    "__TEAMS_JSON__" = toJSON(teams, auto_unbox = TRUE),
    "__SEASON__" = season_label
  )
  for (k in names(fill)) html <- gsub(k, fill[[k]], html, fixed = TRUE)
  left <- regmatches(html, gregexpr("__[A-Z_]+__", html))[[1]]
  if (length(left)) stop("unreplaced placeholders: ", paste(unique(left), collapse = ", "))
  writeLines(html, path)
  path
}
