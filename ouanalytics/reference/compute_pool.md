# Run the whole pool calculation

Runs every step from the saved game data to the tables the page uses, in
order. The page build and the tests both use this function.

## Usage

``` r
compute_pool(
  game_results_raw,
  scores,
  projections,
  picks,
  standings,
  yesterday,
  today,
  season_start,
  num_games
)
```

## Arguments

- game_results_raw, scores:

  See
  [`compute_game_results()`](https://ismayc.github.io/ouanalytics/reference/compute_game_results.md).

- projections:

  Win totals, as for
  [`add_over_under()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md).

- picks:

  One row per pick, as for
  [`score_picks()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md).

- standings:

  Official standings, as for
  [`add_projected_to_standings()`](https://ismayc.github.io/ouanalytics/reference/add_outcome_columns.md).

- yesterday:

  The last day with results (a `Date`).

- today:

  The day the page is built (a `Date`).

- season_start:

  The first day of the season (a `Date`).

- num_games:

  Games in a season (82).

## Value

A named list: `game_results`, `scores_tidy`, `join_with_projections`,
`standings_grid`, `scored_picks`, `changes_in_player_projections`,
`rows_today`, `projected_score`, `player_projections_by_team`,
`current_rankings`, `standings_with_projected`, and `wins_needed`.

## Examples

``` r
pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
  picks, standings, params$yesterday, params$today, params$season_start,
  params$num_games))
pool$rows_today
#> # A tibble: 3 × 7
#>   Date       Player Team                 `Current Record` `Current Win %`
#>   <date>     <chr>  <chr>                <chr>                      <dbl>
#> 1 2026-10-21 P1     Alpha                1-1                           50
#> 2 2026-10-21 P2     Alpha                1-1                           50
#> 3 2026-10-21 P2     Los Angeles Clippers 1-0                          100
#> # ℹ 2 more variables: `Vegas Win Projection %` <dbl>,
#> #   `Change in Points Since Yesterday` <dbl>
pool$current_rankings
#> [1] "P1" "P2"
```
