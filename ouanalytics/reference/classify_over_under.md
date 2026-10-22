# Over, under, or push against the win total

A team is OVER when its winning percentage is above its line, UNDER when
it is below, and PUSH when the two are equal or either is missing.

## Usage

``` r
classify_over_under(current_win_pct, vegas_pct)

add_over_under(updated_schedule, projections, yesterday)

build_standings_grid(join_with_projections, season_start, yesterday)
```

## Arguments

- current_win_pct, vegas_pct:

  Winning percentages (0 to 100).

- updated_schedule:

  The result of
  [`add_game_scores()`](https://ismayc.github.io/ouanalytics/reference/compute_game_results.md).

- projections:

  Win totals: `team`, `win_projection`, and `percentage_projection` (the
  win total as a percentage of the season).

- yesterday:

  The last day with results (a `Date`).

- join_with_projections:

  The result of `add_over_under()`.

- season_start:

  The first day of the season (a `Date`).

## Value

`classify_over_under()` returns "OVER", "UNDER", or "PUSH".
`add_over_under()` returns one row per team game with `over_under`,
`flipped` (the status changed from the game before), and
`flipped_today`. `build_standings_grid()` returns one row per team per
day from `season_start` to `yesterday`. A team keeps its last status on
days it does not play, and has no status (and a record of 0-0) before
its first game.

## Examples

``` r
classify_over_under(c(60, 50, 40), 50)
#> [1] "OVER"  "PUSH"  "UNDER"

gr <- compute_game_results(pool_example$game_results_raw)
sched <- add_game_scores(gr, tidy_scores(pool_example$scores))
ou <- add_over_under(sched, pool_example$projections, as.Date("2026-10-21"))
build_standings_grid(ou, as.Date("2026-10-20"), as.Date("2026-10-21"))
#> # A tibble: 6 × 8
#> # Groups:   team [3]
#>   team   date       current_wins current_losses `Current Record` `Current Win %`
#>   <chr>  <date>            <dbl>          <dbl> <chr>                      <dbl>
#> 1 Alpha  2026-10-20            1              0 1-0                          100
#> 2 Alpha  2026-10-21            1              1 1-1                           50
#> 3 Bravo  2026-10-20            0              1 0-1                            0
#> 4 Bravo  2026-10-21            0              1 0-1                            0
#> 5 Los A… 2026-10-20            0              0 0-0                           NA
#> 6 Los A… 2026-10-21            1              0 1-0                          100
#> # ℹ 2 more variables: `Vegas Win Projection %` <dbl>, over_under <chr>
```
