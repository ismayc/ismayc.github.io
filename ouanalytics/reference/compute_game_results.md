# Game results and scores

Turn the saved game list into one row per team game with a running
record, and the scores table into one row per team per game.

## Usage

``` r
compute_game_results(game_results_raw)

tidy_scores(scores)

add_game_scores(game_results, scores_tidy)
```

## Arguments

- game_results_raw:

  One row per team game: `dateGame`, `numberGameTeamSeason`, `nameTeam`,
  `slugMatchup`, `slugTeam`, `slugTeamLoser`.

- scores:

  One row per game: `game_id`, `game_date`, `away_team`, `home_team`,
  `slug_away_team`, `slug_home_team`, `away_score`, `home_score`.

- game_results:

  The result of `compute_game_results()`.

- scores_tidy:

  The result of `tidy_scores()`.

## Value

`compute_game_results()` returns a grouped tibble with `current_wins`,
`current_losses`, and `current_win_perc` (a percentage from 0 to 100)
per team, "LA " spelled out as "Los Angeles ", sorted by team.
`tidy_scores()` returns one row per team per game with `team_slug`,
`opponent_slug`, `score`, and `opponent_score`. `add_game_scores()` adds
each game's score, record, and a readable result text to `game_results`.

## Examples

``` r
gr <- compute_game_results(pool_example$game_results_raw)
st <- tidy_scores(pool_example$scores)
add_game_scores(gr, st)
#> # A tibble: 4 × 19
#> # Groups:   Team Name [3]
#>   slugSeason `Game Date` game_id `Game Number` `Team Name`  slugMatchup slugTeam
#>   <chr>      <date>      <chr>           <int> <chr>        <chr>       <chr>   
#> 1 2026-27    2026-10-20  g1                  1 Alpha        ALP vs. BRA ALP     
#> 2 2026-27    2026-10-21  g2                  2 Alpha        ALP @ CLI   ALP     
#> 3 2026-27    2026-10-20  g1                  1 Bravo        BRA @ ALP   BRA     
#> 4 2026-27    2026-10-21  g2                  1 Los Angeles… CLI vs. ALP CLI     
#> # ℹ 12 more variables: slugOpponent <chr>, slugTeamLoser <chr>, win <dbl>,
#> #   loss <dbl>, current_wins <dbl>, current_losses <dbl>,
#> #   current_win_perc <dbl>, score <dbl>, opponent_score <dbl>,
#> #   final_score <chr>, `Current Record` <chr>, `Game Result` <chr>
```
