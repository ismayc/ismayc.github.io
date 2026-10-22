# Playoff seeding

[`compute_playoff_seeding()`](https://ismayc.github.io/ouanalytics/reference/seed_conference.md)
seeds both conferences with the official NBA tie-break procedure. Teams
are ordered by winning percentage. Teams that tie are separated by these
criteria, in order, and the first one that separates them decides:

1.  Head-to-head record (a two-team tie), or record among all the tied
    teams.
2.  Division leader over non-leader.
3.  Division record, when the tied teams share a division.
4.  Conference record.
5.  Record against teams in playoff position in their own conference.
6.  Record against teams in playoff position in the other conference.
7.  Point differential.

When one team separates from a larger tie, the rest start again at the
top of the list. If nothing separates the teams, they are ordered
alphabetically, standing in for the random draw.

## An example league

The example league has 13 East teams and 6 West teams, with a few games,
so some teams tie.

``` r

with(seeding_example, compute_playoff_seeding(standings, scores_tidy, meta)) %>%
  filter(conference == "West") %>%
  select(seed, team_name, wins, losses, `Winning Pct`, seed_note)
#> # A tibble: 6 × 6
#>    seed team_name  wins losses `Winning Pct` seed_note
#>   <int> <chr>     <dbl>  <dbl>         <dbl> <chr>    
#> 1     1 Team W2       7      3           0.7 Playoff  
#> 2     2 Team W1       7      3           0.7 Playoff  
#> 3     3 Team W3       5      5           0.5 Playoff  
#> 4     4 Team X1       4      6           0.4 Playoff  
#> 5     5 Team X2       3      7           0.3 Playoff  
#> 6     6 Team X3       1      9           0.1 Playoff
```

In the West, W1 and W2 tie on winning percentage. W2 won their game, so
head-to-head puts W2 first:

``` r

with(seeding_example, break_two_team_tie("W1", "W2", NULL, scores_tidy, meta))
#> [1] "W2" "W1"
```

## The building blocks

``` r

with(seeding_example, {
  list(
    record = record_vs("W2", c("W1", "W3"), scores_tidy),
    point_differential = point_differential("W2", scores_tidy)
  )
})
#> $record
#> # A tibble: 1 × 4
#>   abbreviation  wins losses   pct
#>   <chr>        <int>  <int> <dbl>
#> 1 W2               1      0     1
#> 
#> $point_differential
#> [1] 10
```
