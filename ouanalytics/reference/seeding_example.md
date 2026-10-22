# Example league for playoff seeding

A small league used in the examples and tests: 13 East teams in three
divisions and 6 West teams in two, with a few games, so that some teams
tie on winning percentage.

## Usage

``` r
seeding_example
```

## Format

A list with the inputs of
[`compute_playoff_seeding()`](https://ismayc.github.io/ouanalytics/reference/seed_conference.md):

- meta:

  `abbreviation`, `division`, and `conference` for each team.

- standings:

  Standings with `Winning Pct`.

- scores_tidy:

  One row per team per game.

## Examples

``` r
str(seeding_example, max.level = 1)
#> List of 3
#>  $ meta       : tibble [19 × 3] (S3: tbl_df/tbl/data.frame)
#>  $ standings  : tibble [19 × 8] (S3: tbl_df/tbl/data.frame)
#>  $ scores_tidy: tibble [4 × 4] (S3: tbl_df/tbl/data.frame)
```
