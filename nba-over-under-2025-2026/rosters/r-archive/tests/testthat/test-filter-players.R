# Unit tests for filter_players() — the functional core of the player finder.
# Each test exercises one filter dimension against the synthetic sample roster.

run_filter <- function(...) do.call(filter_players, west_all(...))

test_that("wide-open West selection returns all West players", {
  res <- run_filter()
  expect_setequal(res$player, c("Player A", "Player B", "Player D"))
})

test_that("conference filter excludes the other conference", {
  res <- do.call(filter_players, west_all(
    conf = "East",
    divisions = c("Atlantic", "Southeast", "Central")
  ))
  expect_setequal(res$player, c("Player C", "Player E"))
  expect_true(all(res$conference == "East"))
})

test_that("division filter narrows to selected divisions", {
  res <- run_filter(divisions = "Pacific")
  expect_setequal(res$player, c("Player A", "Player B"))  # Player D is Northwest
})

test_that("team filter narrows to selected teams", {
  res <- run_filter(teams = "Los Angeles Lakers")
  expect_setequal(res$player, c("Player A", "Player B"))  # excludes Nuggets' Player D
})

test_that("known (exact) height matches only that height", {
  res <- run_filter(known_height = TRUE, chosen_feet = 6, chosen_inches = 6)  # 78 in
  expect_setequal(res$player, "Player A")
})

test_that("height range filters inclusively", {
  # 60..78 inches keeps A (78) and D (75); excludes B (84)
  res <- run_filter(min_feet = 5, min_inches = 0, max_feet = 6, max_inches = 6)
  expect_setequal(res$player, c("Player A", "Player D"))
})

test_that("known (exact) age matches only that age", {
  res <- run_filter(known_age = TRUE, age_value = 30)
  expect_setequal(res$player, "Player B")
})

test_that("age range filters inclusively", {
  res <- run_filter(age_range = c(26, 31))  # keeps B (30) and D (28)
  expect_setequal(res$player, c("Player B", "Player D"))
})

test_that("known (exact) jersey matches only that number", {
  res <- run_filter(known_jersey = TRUE, jersey_value = 23)
  expect_setequal(res$player, "Player A")
})

test_that("jersey range filters inclusively", {
  res <- run_filter(jersey_range = c(5, 20))  # keeps B (6) and D (12); excludes A (23)
  expect_setequal(res$player, c("Player B", "Player D"))
})

test_that("players with NA jersey are dropped by a range filter", {
  players <- sample_players()
  players$number_jersey[players$player == "Player A"] <- NA
  res <- do.call(filter_players, west_all(players_data = players))
  expect_false("Player A" %in% res$player)
  expect_setequal(res$player, c("Player B", "Player D"))
})

test_that("combined filters intersect", {
  # West + Pacific + height <= 80 + age <= 26 -> only Player A
  res <- run_filter(
    divisions = "Pacific",
    max_feet = 6, max_inches = 8,   # <= 80 in
    age_range = c(0, 26)
  )
  expect_setequal(res$player, "Player A")
})

test_that("an empty result is a zero-row data frame, not an error", {
  res <- run_filter(teams = "Nonexistent Team")
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("output has exactly the display columns and formatted height", {
  res <- run_filter()
  expect_named(
    res,
    c("player", "team", "conference", "division", "height", "age", "number_jersey")
  )
  a <- res[res$player == "Player A", ]
  expect_equal(a$height, "6-6")
})
