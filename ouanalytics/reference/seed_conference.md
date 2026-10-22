# Playoff seeding

Seeds one conference, or both, with the official NBA tie-break
procedure. Teams tied on winning percentage are ordered by head-to-head,
division leader, division record, conference record, record against
playoff-position teams, and point differential.

## Usage

``` r
seed_conference(conf, standings, scores_tidy, meta)

compute_playoff_seeding(standings, scores_tidy, meta)
```

## Arguments

- conf:

  "East" or "West".

- standings:

  All 30 teams: `team_name`, `conference`, `division`, `wins`, `losses`,
  `Winning Pct`, `Games Back`, `abbreviation`.

- scores_tidy:

  One row per team per game: `team_slug`, `opponent_slug`, `score`,
  `opponent_score`.

- meta:

  Team details: `abbreviation`, `conference`, `division`.

## Value

A tibble with `seed`, the standings columns, and `seed_note`: "Playoff"
for seeds 1 to 6, "Play-In" for 7 to 10, and empty after that.
`compute_playoff_seeding()` returns both conferences.

## Examples

``` r
with(seeding_example, seed_conference("West", standings, scores_tidy, meta))
#> # A tibble: 6 × 10
#>    seed team_name conference division  wins losses `Winning Pct` `Games Back`
#>   <int> <chr>     <chr>      <chr>    <dbl>  <dbl>         <dbl> <chr>       
#> 1     1 Team W2   West       SW           7      3           0.7 -           
#> 2     2 Team W1   West       SW           7      3           0.7 -           
#> 3     3 Team W3   West       SW           5      5           0.5 -           
#> 4     4 Team X1   West       NW           4      6           0.4 -           
#> 5     5 Team X2   West       NW           3      7           0.3 -           
#> 6     6 Team X3   West       NW           1      9           0.1 -           
#> # ℹ 2 more variables: abbreviation <chr>, seed_note <chr>
with(seeding_example, compute_playoff_seeding(standings, scores_tidy, meta))
#> # A tibble: 19 × 10
#>     seed team_name conference division  wins losses `Winning Pct` `Games Back`
#>    <int> <chr>     <chr>      <chr>    <dbl>  <dbl>         <dbl> <chr>       
#>  1     1 Team A1   East       ATL          9      1          0.9  -           
#>  2     2 Team A2   East       ATL          8      2          0.8  -           
#>  3     3 Team A3   East       ATL          6      4          0.6  -           
#>  4     4 Team A4   East       ATL          6      4          0.6  -           
#>  5     5 Team A5   East       ATL          6      4          0.55 -           
#>  6     6 Team C1   East       CEN          4      6          0.4  -           
#>  7     7 Team C2   East       CEN          4      6          0.4  -           
#>  8     8 Team C3   East       CEN          4      6          0.4  -           
#>  9     9 Team C4   East       CEN          3      7          0.3  -           
#> 10    10 Team C5   East       CEN          2      8          0.2  -           
#> 11    11 Team S1   East       SE           1      9          0.1  -           
#> 12    12 Team S2   East       SE           0     10          0.05 -           
#> 13    13 Team S3   East       SE           0     10          0    -           
#> 14     1 Team W2   West       SW           7      3          0.7  -           
#> 15     2 Team W1   West       SW           7      3          0.7  -           
#> 16     3 Team W3   West       SW           5      5          0.5  -           
#> 17     4 Team X1   West       NW           4      6          0.4  -           
#> 18     5 Team X2   West       NW           3      7          0.3  -           
#> 19     6 Team X3   West       NW           1      9          0.1  -           
#> # ℹ 2 more variables: abbreviation <chr>, seed_note <chr>
```
