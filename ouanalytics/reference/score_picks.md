# Score the picks

Each player's points for each team they picked, every day: the wage when
the pick matches the team's status, minus the wage when it does not, 0
for a push, and missing before the team's first game.

## Usage

``` r
score_picks(standings_grid, picks)

add_changes(player_projections_by_team)

rows_for_day(changes, day)

projected_score_by_player(player_projections_by_team)

rank_players(projected_score, today)
```

## Arguments

- standings_grid:

  The result of
  [`build_standings_grid()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md).

- picks:

  One row per pick: `team`, `player`, `wage`, and `choice` ("OVER" or
  "UNDER").

- player_projections_by_team:

  The result of `score_picks()`.

- changes:

  The result of `add_changes()`.

- day:

  A `Date`.

- projected_score:

  The result of `projected_score_by_player()`.

- today:

  The day the page is built (a `Date`).

## Value

`score_picks()` returns one row per player, team, and day.
`add_changes()` returns the rows where a pick's points moved from the
day before. A team's points are missing until its first game, so a
missing value counts as 0: otherwise every team's first game would drop
out of the changes. `rows_for_day()` keeps one day of those.
`projected_score_by_player()` returns each player's total points and
counts of correct picks by day (missing points count as 0 and as not
correct). `rank_players()` returns the players, best first, by points,
then correct picks, then correct 15-point picks, on the date closest to
`today`.

## Examples

``` r
pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
  picks, standings, params$yesterday, params$today, params$season_start,
  params$num_games))
scored <- pool$scored_picks
ch <- add_changes(scored)
rows_for_day(ch, as.Date("2026-10-21"))
#> # A tibble: 3 × 7
#>   Date       Player Team                 `Current Record` `Current Win %`
#>   <date>     <chr>  <chr>                <chr>                      <dbl>
#> 1 2026-10-21 P1     Alpha                1-1                           50
#> 2 2026-10-21 P2     Alpha                1-1                           50
#> 3 2026-10-21 P2     Los Angeles Clippers 1-0                          100
#> # ℹ 2 more variables: `Vegas Win Projection %` <dbl>,
#> #   `Change in Points Since Yesterday` <dbl>
ps <- projected_score_by_player(scored)
rank_players(ps, today = as.Date("2026-10-22"))
#> [1] "P1" "P2"
```
