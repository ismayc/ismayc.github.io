# Contract tests for players_finder.csv — the file the weekly pull regenerates
# and the app reads. These catch a pull/format regression before it ships.

test_that("players_finder.csv exists and is non-trivial", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  expect_gt(nrow(players), 400)  # mirrors the build's MIN_PLAYERS guard
})

test_that("players_finder.csv has the columns the app requires", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  required <- c("player", "team", "conference", "division",
                "height_feet", "height_inches", "height_total_inches",
                "age", "number_jersey", "date_pulled")
  expect_true(all(required %in% names(players)))
})

test_that("height_total_inches is consistent with feet and inches", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  expect_equal(
    players$height_total_inches,
    players$height_feet * 12L + players$height_inches
  )
})

test_that("conference and division values are valid", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  expect_true(all(players$conference %in% c("East", "West")))
  valid_div <- c("Southwest", "Northwest", "Pacific",
                 "Southeast", "Atlantic", "Central")
  expect_true(all(players$division %in% valid_div))
})

test_that("key identifying fields are never missing", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  expect_false(any(is.na(players$player)))
  expect_false(any(is.na(players$team)))
  expect_equal(dplyr::n_distinct(players$team), 30L)
})
