# Export a case as CSV files

Writes the inputs and the computed tables as CSV files in a fixed layout
(`inputs/` and `outputs/`), so the Python package can recompute every
table and compare. Missing values are empty fields, dates are
`YYYY-MM-DD`, and factors are text.

## Usage

``` r
export_pool_case(
  dir,
  game_results_raw,
  scores,
  projections,
  picks,
  standings,
  params,
  pool
)

export_seeding_case(dir, meta, standings, scores_tidy, seeded)
```

## Arguments

- dir:

  The folder to write into; created if needed.

- game_results_raw, scores, projections, picks, standings:

  The inputs given to
  [`compute_pool()`](https://ismayc.github.io/ouanalytics/reference/compute_pool.md).

- params:

  A named list: `yesterday`, `today`, `season_start`, `num_games`.

- pool:

  The result of
  [`compute_pool()`](https://ismayc.github.io/ouanalytics/reference/compute_pool.md).

- meta, scores_tidy:

  The inputs given to
  [`compute_playoff_seeding()`](https://ismayc.github.io/ouanalytics/reference/seed_conference.md).
  In `export_seeding_case()`, `standings` is the standings given to it
  too.

- seeded:

  The result of
  [`compute_playoff_seeding()`](https://ismayc.github.io/ouanalytics/reference/seed_conference.md).

## Value

`dir`, invisibly.

## Examples

``` r
dir <- tempfile()
pool <- with(pool_example, compute_pool(game_results_raw, scores, projections,
  picks, standings, params$yesterday, params$today, params$season_start,
  params$num_games))
with(pool_example, export_pool_case(dir, game_results_raw, scores, projections,
  picks, standings, params, pool))
list.files(dir, recursive = TRUE)
#>  [1] "inputs/game_results_raw.csv"              
#>  [2] "inputs/params.csv"                        
#>  [3] "inputs/picks.csv"                         
#>  [4] "inputs/projections.csv"                   
#>  [5] "inputs/scores.csv"                        
#>  [6] "inputs/standings.csv"                     
#>  [7] "outputs/changes_in_player_projections.csv"
#>  [8] "outputs/current_rankings.csv"             
#>  [9] "outputs/game_results.csv"                 
#> [10] "outputs/join_with_projections.csv"        
#> [11] "outputs/player_projections_by_team.csv"   
#> [12] "outputs/projected_score.csv"              
#> [13] "outputs/rows_today.csv"                   
#> [14] "outputs/scored_picks.csv"                 
#> [15] "outputs/standings_grid.csv"               
#> [16] "outputs/standings_with_projected.csv"     
#> [17] "outputs/wins_needed.csv"                  
```
