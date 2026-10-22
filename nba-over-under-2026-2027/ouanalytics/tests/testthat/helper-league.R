# Fixtures shared by the seeding and export tests

# A small league: East has three divisions (A1-A5, C1-C5, S1-S3), West has
# two (W1-W3, X1-X3).
league_meta <- bind_rows(
  tibble(abbreviation = paste0("A", 1:5), division = "ATL", conference = "East"),
  tibble(abbreviation = paste0("C", 1:5), division = "CEN", conference = "East"),
  tibble(abbreviation = paste0("S", 1:3), division = "SE",  conference = "East"),
  tibble(abbreviation = paste0("W", 1:3), division = "SW",  conference = "West"),
  tibble(abbreviation = paste0("X", 1:3), division = "NW",  conference = "West")
) %>% mutate(team = abbreviation)

# One game, as two rows (one per team): a scored sa, b scored sb
sg <- function(a, b, sa, sb) {
  tibble(team_slug = c(a, b), opponent_slug = c(b, a),
         score = c(sa, sb), opponent_score = c(sb, sa))
}
no_games <- sg("A1", "A2", 0, 0)[0, ]
two <- function(t1, t2, scores, ...) {
  break_two_team_tie(t1, t2, NULL, scores, league_meta, ...)
}
multi <- function(teams, scores, ...) {
  break_multi_team_tie(teams, NULL, scores, league_meta, ...)
}

make_standings <- function(abbr, pct, conference, division) {
  tibble(team_name = paste("Team", abbr), conference = conference,
         division = division, wins = round(pct * 10), losses = 10 - round(pct * 10),
         `Winning Pct` = pct, `Games Back` = "-", abbreviation = abbr)
}

east <- make_standings(
  c("A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4", "C5", "S1", "S2", "S3"),
  c(0.9, 0.8, 0.6, 0.6, 0.55, 0.4, 0.4, 0.4, 0.3, 0.2, 0.1, 0.05, 0),
  "East",
  c(rep("ATL", 5), rep("CEN", 5), rep("SE", 3))
)
west <- make_standings(
  c("W1", "W2", "W3", "X1", "X2", "X3"),
  c(0.7, 0.7, 0.5, 0.4, 0.3, 0.1), "West", c(rep("SW", 3), rep("NW", 3))
)
league_standings <- bind_rows(east, west)
league_scores <- bind_rows(sg("A3", "A4", 100, 90), sg("W2", "W1", 100, 90))
