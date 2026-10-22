# ouanalytics

The calculations behind the **NBA over/under pool** page. In the pool, each player
picks OVER or UNDER for every team's preseason win total and puts a wage on each
pick. This package scores those picks from the saved game data and seeds the
playoffs.

## What it does

- **Status against the line.** Each team is OVER, UNDER, or PUSH, every day.
- **Points.** Each player's points per pick, per day, and their running total.
- **Changes since yesterday.** Which picks moved, including a team's first game.
- **Wins still needed.** How many wins or losses settle each team's outcome.
- **Playoff seeding.** The official NBA tie-break procedure, for both conferences.

Every function is pure: nothing reads a file, calls the network, or looks at today's
date. The dates and constants come in as arguments, so the same inputs always
give the same tables.

## Try it

``` r
library(ouanalytics)

pool <- with(pool_example, compute_pool(
  game_results_raw, scores, projections, picks, standings,
  params$yesterday, params$today, params$season_start, params$num_games
))

pool$rows_today          # the picks that moved yesterday
pool$current_rankings    # the players, best first

with(seeding_example, compute_playoff_seeding(standings, scores_tidy, meta))
```

## Learn more

- [How the pool is scored](articles/how-the-pool-is-scored.html): a walk through the example pool, one step at a time.
- [Playoff seeding](articles/playoff-seeding.html): the tie-break criteria, in order.
- [Cross-checking with Python](articles/python-cross-check.html): a second, independent implementation that checks every table.
- [Reference](reference/index.html): every function.

## Install

Not on CRAN. From a clone of the repository (`nba-over-under-2026-2027/`):

``` r
# in a terminal: R CMD INSTALL ouanalytics
```
