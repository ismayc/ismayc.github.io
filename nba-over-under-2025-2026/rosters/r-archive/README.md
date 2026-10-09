# Archived: Shiny version of the NBA Player Finder

Until October 9, 2026, https://ismayc.github.io/nba-player-finder/ was this R Shiny
app, exported with shinylive so it ran in the browser through webR. The R startup
made the first load slow, so the page was rebuilt as a static HTML and JavaScript
page in `nba-player-finder/` at the repo root. This folder keeps the R version for
reference; nothing here is built or deployed.

- `finder_app/app.R` and `finder_app/filter_players.R`: the bslib UI and the
  filtering logic.
- `build_finder_site.R`: the `shinylive::export()` step the weekly workflow ran.
- `run_tests.R` and `tests/testthat/`: the testthat suite that gated the deploy.
- `shiny-tests.yaml`: the GitHub Actions workflow that ran those tests (moved
  out of `.github/workflows/`, so it no longer runs).

To run it again, copy `../finder_app/players_finder.csv` into `finder_app/`
and call `shiny::runApp("finder_app")` from this folder.
