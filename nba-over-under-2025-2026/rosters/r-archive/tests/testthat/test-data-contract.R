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

# The app's UI is built eagerly from the data: the age sliderInput() uses
# min()/max() of age, and the height selectInputs use sort(unique(...)) of the
# height columns. These assertions encode what that UI construction requires so
# a bad pull fails the test suite instead of shipping a blank page. (Age may be
# NA for a newly signed player — the app handles that with na.rm — so we only
# require a usable, finite range rather than zero NAs.)
test_that("age yields finite slider bounds", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  expect_true(is.numeric(players$age))
  expect_gt(sum(!is.na(players$age)), 0)              # at least one real age
  rng <- range(players$age, na.rm = TRUE)
  expect_true(all(is.finite(rng)))                    # sliderInput min/max
  expect_lt(rng[1], rng[2])
})

test_that("height columns are numeric and complete", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  for (col in c("height_feet", "height_inches", "height_total_inches")) {
    expect_true(is.numeric(players[[col]]), info = col)
    expect_false(any(is.na(players[[col]])), info = col)
  }
})

test_that("every division and both conferences are populated", {
  csv <- players_csv_path()
  skip_if_not(file.exists(csv), "players_finder.csv not present")
  players <- read.csv(csv, stringsAsFactors = FALSE)
  # Each division drives a per-division checkbox group; an empty one would leave
  # part of the conference unselectable.
  divisions <- c("Southwest", "Northwest", "Pacific",
                 "Southeast", "Atlantic", "Central")
  div_counts <- table(factor(players$division, levels = divisions))
  expect_true(all(div_counts > 0))
  expect_setequal(unique(players$conference), c("East", "West"))
})
