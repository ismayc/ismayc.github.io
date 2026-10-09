# Build smoke test: source the real app.R against the live players_finder.csv
# and confirm the Shiny app object constructs without error.
#
# This is the test that would have caught the blank-page outage: the app builds
# its UI eagerly at load time (e.g. sliderInput("slider_age", ...) reads
# min()/max() of the age column), so a roster pull that introduced an NA age
# made min()/max() return NA and sliderInput() threw before the app could start.
# Sourcing app.R here reproduces that exact code path on the freshly pulled data,
# so a bad pull fails the build instead of shipping a blank page.

test_that("app.R builds a Shiny app object against the live roster data", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("dplyr")
  skip_if_not(file.exists(players_csv_path()), "players_finder.csv not present")

  app <- NULL
  # chdir = TRUE so app.R's relative read.csv("players_finder.csv") and
  # source("filter_players.R") resolve against finder_app/. regexp = NA asserts
  # the expression raised no error (portable across testthat versions).
  expect_error(
    app <- source(
      file.path(finder_app_dir(), "app.R"),
      local = new.env(parent = globalenv()),
      chdir = TRUE
    )$value,
    regexp = NA
  )
  expect_s3_class(app, "shiny.appobj")
})
