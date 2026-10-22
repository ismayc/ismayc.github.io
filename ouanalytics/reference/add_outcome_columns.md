# Wins and losses still needed

How many more wins a team needs to finish OVER, how many more losses
would make it finish UNDER, and whether the outcome is already settled.

## Usage

``` r
add_outcome_columns(player_projections_by_team, num_games)

add_projected_to_standings(standings, projections, yesterday, num_games)

build_wins_needed(player_projections_by_team, yesterday, num_games)
```

## Arguments

- player_projections_by_team:

  For `add_outcome_columns()`, the result of
  [`score_picks()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md).
  For `build_wins_needed()`, the result of `add_outcome_columns()`.

- num_games:

  Games in a season (82).

- standings:

  Official standings: `team_name`, `wins`, `losses`.

- projections:

  Win totals, as for
  [`add_over_under()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md).

- yesterday:

  The last day with results (a `Date`).

## Value

`add_outcome_columns()` adds `Wins To Go Over Vegas`,
`Losses To Go Under Vegas`, `Winning % In Remaining Games Needed`, and
`Outcome Determined` ("not yet", "OVER", or "UNDER"); the "to go"
columns are missing once the outcome is settled.
`add_projected_to_standings()` adds the wins needed for an OVER to the
official standings (no percentage when no games are left).
`build_wins_needed()` returns one row per team for `yesterday`.

## Examples

``` r
pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
  picks, standings, params$yesterday, params$today, params$season_start,
  params$num_games))
add_outcome_columns(pool$scored_picks, num_games = 82)
#> # A tibble: 8 × 14
#> # Groups:   Team [3]
#>   Date       Player Team                 `Current Record` `Current Win %`
#>   <date>     <chr>  <chr>                <chr>                      <dbl>
#> 1 2026-10-20 P1     Alpha                1-0                          100
#> 2 2026-10-20 P2     Alpha                1-0                          100
#> 3 2026-10-21 P1     Alpha                1-1                           50
#> 4 2026-10-21 P2     Alpha                1-1                           50
#> 5 2026-10-20 P1     Bravo                0-1                            0
#> 6 2026-10-21 P1     Bravo                0-1                            0
#> 7 2026-10-20 P2     Los Angeles Clippers 0-0                           NA
#> 8 2026-10-21 P2     Los Angeles Clippers 1-0                          100
#> # ℹ 9 more variables: `Vegas Win Projection %` <dbl>,
#> #   `Current Over/Under` <chr>, `Over/Under Pick` <chr>, `Win % - Vegas` <dbl>,
#> #   `Wins To Go Over Vegas` <dbl>, `Winning % In Remaining Games Needed` <dbl>,
#> #   `Losses To Go Under Vegas` <dbl>, `Current Projected Points` <dbl>,
#> #   `Outcome Determined` <fct>
build_wins_needed(pool$player_projections_by_team, as.Date("2026-10-21"), 82)
#> # A tibble: 3 × 9
#> # Groups:   Team [3]
#>   Date       Team             `Outcome Determined` `Current Record`  Wins Losses
#>   <date>     <chr>            <fct>                <chr>            <int>  <int>
#> 1 2026-10-21 Alpha            not yet              1-1                  1      1
#> 2 2026-10-21 Bravo            not yet              0-1                  0      1
#> 3 2026-10-21 Los Angeles Cli… not yet              1-0                  1      0
#> # ℹ 3 more variables: `Wins To Go Over Vegas` <dbl>,
#> #   `Losses To Go Under Vegas` <dbl>, `Remaining Games` <dbl>
```
