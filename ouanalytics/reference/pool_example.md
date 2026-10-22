# Example pool: three teams, two days

A tiny pool used in the examples and tests: three teams (Alpha, Bravo,
and the Los Angeles Clippers), two players, and two days of games. Alpha
beats Bravo on October 20, 2026; the Clippers beat Alpha on October 21,
the Clippers' first game. Every team has a 50 percent line.

## Usage

``` r
pool_example
```

## Format

A list with the inputs of
[`compute_pool()`](https://ismayc.github.io/ouanalytics/reference/compute_pool.md):

- game_results_raw:

  One row per team game.

- scores:

  One row per game, with scores.

- projections:

  Win totals.

- picks:

  Two players' picks.

- standings:

  Official standings.

- params:

  A list: `yesterday`, `today`, `season_start`, `num_games`.

## Examples

``` r
str(pool_example, max.level = 1)
#> List of 6
#>  $ game_results_raw: tibble [4 × 8] (S3: tbl_df/tbl/data.frame)
#>  $ scores          : tibble [2 × 8] (S3: tbl_df/tbl/data.frame)
#>  $ projections     : tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ picks           : tibble [4 × 4] (S3: tbl_df/tbl/data.frame)
#>  $ standings       : tibble [3 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ params          :List of 4
```
