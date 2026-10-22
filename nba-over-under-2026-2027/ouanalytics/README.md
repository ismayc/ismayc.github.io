# ouanalytics

The calculations behind the NBA over/under pool page: each team's status
against its win total, each player's points, the change since yesterday,
the wins a team still needs, and playoff seeding with the official NBA
tie-break procedure. Not for CRAN.

Every function is pure: nothing reads a file, calls the network, or looks at
`Sys.Date()`. Every date and constant comes in as an argument. The page build
(`03-data-wrangling.R`, `make_plots.Rmd`) reads the saved data and calls
`compute_pool()` and `compute_playoff_seeding()`.

## Install

From `nba-over-under-2026-2027/`, once and again after any change to the code:

    R CMD INSTALL ouanalytics

## Test

    Rscript ouanalytics/tools/check.R

This installs the package, runs the tests, and fails unless every line is
covered. The page build in GitHub Actions runs the same check first, and
skips the build when it fails.

## Cross-check with the Python package

`ouanalytics-py/` recomputes every table in polars and compares it with the R
result. The golden cases it tests against come from R:

    R CMD INSTALL ouanalytics
    Rscript ouanalytics/tools/make-golden.R

Re-run `make-golden.R` after any change to the R calculations, then run the
Python tests (see `ouanalytics-py/README.md`). A difference means one of the
two is wrong.

## Layout

| File | Contents |
|---|---|
| `R/standings_logic.R` | Over/under status, points, changes, wins needed |
| `R/seeding.R` | NBA tie-break procedure and playoff seeding |
| `R/pool.R` | `compute_pool()`: the whole pool calculation in order |
| `R/export.R` | CSV export of a case, for the Python cross-check |
| `tests/testthat/` | Tests (fixtures in `helper-*.R`) |
| `tools/check.R` | Tests plus the 100% coverage gate |
| `tools/make-golden.R` | Writes the golden cases for Python |
