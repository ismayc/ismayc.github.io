# Tie-break building blocks

The pieces of the official NBA tie-break procedure. Teams are identified
by their abbreviations. `scores_tidy` has one row per team per game:
`team_slug`, `opponent_slug`, `score`, and `opponent_score`.

## Usage

``` r
record_vs(team_abbr, opponent_abbrs, scores_tidy)

point_differential(team_abbr, scores_tidy)

get_division_leaders(conf_standings, scores_tidy, meta)

break_two_team_tie(
  t1,
  t2,
  standings_df,
  scores_tidy,
  meta,
  division_leaders = character(0),
  playoff_eligible_own = NULL,
  playoff_eligible_other = NULL
)

break_multi_team_tie(
  team_abbrs,
  standings_df,
  scores_tidy,
  meta,
  division_leaders = character(0),
  playoff_eligible_own = NULL,
  playoff_eligible_other = NULL
)
```

## Arguments

- team_abbr, t1, t2:

  Team abbreviations.

- opponent_abbrs:

  Abbreviations of the opponents to count games against.

- scores_tidy:

  One row per team per game.

- conf_standings:

  Standings for one conference: `abbreviation`, `division`,
  `Winning Pct`.

- meta:

  Team details: `abbreviation`, `conference`, `division`.

- standings_df:

  Not used; kept so the call signature matches the seeding code.

- division_leaders:

  Abbreviations of the division leaders.

- playoff_eligible_own, playoff_eligible_other:

  Abbreviations of the teams in playoff position in the team's own and
  the other conference. `NULL` skips the criterion.

- team_abbrs:

  Abbreviations of three or more tied teams.

## Value

`record_vs()` returns a one-row tibble with `wins`, `losses`, and `pct`
(missing when no games were played). `point_differential()` returns the
net points over all of a team's games. `get_division_leaders()` returns
one abbreviation per division. `break_two_team_tie()` returns
`c(winner, loser)`. `break_multi_team_tie()` returns the teams in order.
Criteria are applied in the official order; if nothing separates the
teams they are ordered alphabetically, standing in for the random draw.

## Examples

``` r
with(seeding_example, {
  record_vs("W2", c("W1", "W3"), scores_tidy)
  point_differential("W2", scores_tidy)
  break_two_team_tie("W1", "W2", NULL, scores_tidy, meta)
  break_multi_team_tie(c("A1", "A2", "A3"), NULL, scores_tidy, meta)
})
#> [1] "A3" "A1" "A2"
```
