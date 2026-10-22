# Package index

## Run the whole pool

- [`compute_pool()`](https://ismayc.github.io/ouanalytics/reference/compute_pool.md)
  : Run the whole pool calculation

## Game results and status

From saved games to each team’s status against its win total.

- [`compute_game_results()`](https://ismayc.github.io/ouanalytics/reference/compute_game_results.md)
  [`tidy_scores()`](https://ismayc.github.io/ouanalytics/reference/compute_game_results.md)
  [`add_game_scores()`](https://ismayc.github.io/ouanalytics/reference/compute_game_results.md)
  : Game results and scores
- [`classify_over_under()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md)
  [`add_over_under()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md)
  [`build_standings_grid()`](https://ismayc.github.io/ouanalytics/reference/classify_over_under.md)
  : Over, under, or push against the win total

## Scoring the picks

Points, changes since yesterday, and wins still needed.

- [`score_picks()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
  [`add_changes()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
  [`rows_for_day()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
  [`projected_score_by_player()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
  [`rank_players()`](https://ismayc.github.io/ouanalytics/reference/score_picks.md)
  : Score the picks
- [`add_outcome_columns()`](https://ismayc.github.io/ouanalytics/reference/add_outcome_columns.md)
  [`add_projected_to_standings()`](https://ismayc.github.io/ouanalytics/reference/add_outcome_columns.md)
  [`build_wins_needed()`](https://ismayc.github.io/ouanalytics/reference/add_outcome_columns.md)
  : Wins and losses still needed

## Playoff seeding

The official NBA tie-break procedure.

- [`seed_conference()`](https://ismayc.github.io/ouanalytics/reference/seed_conference.md)
  [`compute_playoff_seeding()`](https://ismayc.github.io/ouanalytics/reference/seed_conference.md)
  : Playoff seeding
- [`record_vs()`](https://ismayc.github.io/ouanalytics/reference/record_vs.md)
  [`point_differential()`](https://ismayc.github.io/ouanalytics/reference/record_vs.md)
  [`get_division_leaders()`](https://ismayc.github.io/ouanalytics/reference/record_vs.md)
  [`break_two_team_tie()`](https://ismayc.github.io/ouanalytics/reference/record_vs.md)
  [`break_multi_team_tie()`](https://ismayc.github.io/ouanalytics/reference/record_vs.md)
  : Tie-break building blocks

## Cross-check with Python

- [`export_pool_case()`](https://ismayc.github.io/ouanalytics/reference/export_pool_case.md)
  [`export_seeding_case()`](https://ismayc.github.io/ouanalytics/reference/export_pool_case.md)
  : Export a case as CSV files

## Example data

- [`pool_example`](https://ismayc.github.io/ouanalytics/reference/pool_example.md)
  : Example pool: three teams, two days
- [`seeding_example`](https://ismayc.github.io/ouanalytics/reference/seeding_example.md)
  : Example league for playoff seeding
