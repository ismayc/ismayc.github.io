# How the pool is scored

Each player picks OVER or UNDER for every team’s preseason win total and
puts a wage on each pick. A team is OVER when its winning percentage is
above the line, UNDER when it is below, and PUSH when the two are equal.
A right pick earns its wage, a wrong pick loses it, and a push earns
nothing.

This guide walks the small example pool that ships with the package:
three teams, two players, and two days of games.

``` r

pool_example$picks
#> # A tibble: 4 × 4
#>   team                 player  wage choice
#>   <chr>                <chr>  <dbl> <chr> 
#> 1 Alpha                P1        15 OVER  
#> 2 Bravo                P1        14 UNDER 
#> 3 Alpha                P2        15 UNDER 
#> 4 Los Angeles Clippers P2        13 OVER
```

## 1. Game results

[`compute_game_results()`](https://ismayc.github.io/ouanalytics/reference/compute_game_results.md)
turns the saved games into one row per team game, with a running record.

``` r

gr <- compute_game_results(pool_example$game_results_raw)
gr %>% select(`Team Name`, `Game Date`, current_wins, current_losses, current_win_perc)
#> # A tibble: 4 × 5
#> # Groups:   Team Name [3]
#>   `Team Name`          `Game Date` current_wins current_losses current_win_perc
#>   <chr>                <date>             <dbl>          <dbl>            <dbl>
#> 1 Alpha                2026-10-20             1              0              100
#> 2 Alpha                2026-10-21             1              1               50
#> 3 Bravo                2026-10-20             0              1                0
#> 4 Los Angeles Clippers 2026-10-21             1              0              100
```

## 2. Status against the line

[`add_over_under()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md)
compares each game’s winning percentage with the team’s line. Alpha is
OVER after its first game (100 percent against 50), then PUSH after its
second (50 against 50).

``` r

sched <- add_game_scores(gr, tidy_scores(pool_example$scores))
ou <- add_over_under(sched, pool_example$projections, as.Date("2026-10-21"))
ou %>% select(`Team Name`, `Game Date`, `Current Win %`, over_under, flipped)
#> # A tibble: 4 × 5
#> # Groups:   Team Name [3]
#>   `Team Name`          `Game Date` `Current Win %` over_under flipped
#>   <chr>                <date>                <dbl> <chr>      <lgl>  
#> 1 Alpha                2026-10-20              100 OVER       NA     
#> 2 Alpha                2026-10-21               50 PUSH       TRUE   
#> 3 Bravo                2026-10-20                0 UNDER      NA     
#> 4 Los Angeles Clippers 2026-10-21              100 OVER       NA
```

## 3. One row per team per day

[`build_standings_grid()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md)
fills in the days a team does not play. A team keeps its last status on
an off day. Before a team’s first game it has no status and a record of
0-0: the Clippers on October 20 below.

``` r

grid <- build_standings_grid(ou, as.Date("2026-10-20"), as.Date("2026-10-21"))
grid %>% select(team, date, `Current Record`, over_under)
#> # A tibble: 6 × 4
#> # Groups:   team [3]
#>   team                 date       `Current Record` over_under
#>   <chr>                <date>     <chr>            <chr>     
#> 1 Alpha                2026-10-20 1-0              OVER      
#> 2 Alpha                2026-10-21 1-1              PUSH      
#> 3 Bravo                2026-10-20 0-1              UNDER     
#> 4 Bravo                2026-10-21 0-1              UNDER     
#> 5 Los Angeles Clippers 2026-10-20 0-0              NA        
#> 6 Los Angeles Clippers 2026-10-21 1-0              OVER
```

## 4. Points

[`score_picks()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
gives each player’s points for each team they picked. Points are missing
until the team’s first game.

``` r

scored <- score_picks(grid, pool_example$picks)
scored %>% select(Date, Player, Team, `Over/Under Pick`, `Current Over/Under`, current_projected_points)
#> # A tibble: 8 × 6
#> # Groups:   Team [3]
#>   Date       Player Team                 `Over/Under Pick` `Current Over/Under`
#>   <date>     <chr>  <chr>                <chr>             <chr>               
#> 1 2026-10-20 P1     Alpha                OVER              OVER                
#> 2 2026-10-20 P2     Alpha                UNDER             OVER                
#> 3 2026-10-21 P1     Alpha                OVER              PUSH                
#> 4 2026-10-21 P2     Alpha                UNDER             PUSH                
#> 5 2026-10-20 P1     Bravo                UNDER             UNDER               
#> 6 2026-10-21 P1     Bravo                UNDER             UNDER               
#> 7 2026-10-20 P2     Los Angeles Clippers OVER              NA                  
#> 8 2026-10-21 P2     Los Angeles Clippers OVER              OVER                
#> # ℹ 1 more variable: current_projected_points <dbl>
```

## 5. Changes since yesterday

[`add_changes()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
lists the picks whose points moved. A missing value counts as 0, so a
team’s first game appears as a change. (Before October 22, 2026 it did
not, and the page said that no scores had changed on days when many
had.)

``` r

changes <- add_changes(scored)
rows_for_day(changes, as.Date("2026-10-21"))
#> # A tibble: 3 × 7
#>   Date       Player Team                 `Current Record` `Current Win %`
#>   <date>     <chr>  <chr>                <chr>                      <dbl>
#> 1 2026-10-21 P1     Alpha                1-1                           50
#> 2 2026-10-21 P2     Alpha                1-1                           50
#> 3 2026-10-21 P2     Los Angeles Clippers 1-0                          100
#> # ℹ 2 more variables: `Vegas Win Projection %` <dbl>,
#> #   `Change in Points Since Yesterday` <dbl>
```

## 6. Totals and ranking

``` r

ps <- projected_score_by_player(scored)
ps
#> # A tibble: 4 × 8
#>   Date       Player `Projected Total Points` `Number Correct`
#>   <date>     <chr>                     <dbl>            <int>
#> 1 2026-10-20 P1                           29                2
#> 2 2026-10-20 P2                          -15                0
#> 3 2026-10-21 P1                           14                1
#> 4 2026-10-21 P2                           13                1
#> # ℹ 4 more variables: `Number (Wage 15) Correct` <int>,
#> #   `Number (Wage 14) Correct` <int>, `Number (Wage 13) Correct` <int>,
#> #   `Number (Wage 12) Correct` <int>
rank_players(ps, today = as.Date("2026-10-22"))
#> [1] "P1" "P2"
```

## 7. Everything in one call

[`compute_pool()`](https://ismayc.github.io/ouanalytics/reference/compute_pool.md)
runs all of the steps above, plus the wins-needed columns, in the same
order the page build uses.

``` r

pool <- with(pool_example, compute_pool(
  game_results_raw, scores, projections, picks, standings,
  params$yesterday, params$today, params$season_start, params$num_games
))
names(pool)
#>  [1] "game_results"                  "scores_tidy"                  
#>  [3] "join_with_projections"         "standings_grid"               
#>  [5] "scored_picks"                  "changes_in_player_projections"
#>  [7] "rows_today"                    "projected_score"              
#>  [9] "player_projections_by_team"    "current_rankings"             
#> [11] "standings_with_projected"      "wins_needed"
```
