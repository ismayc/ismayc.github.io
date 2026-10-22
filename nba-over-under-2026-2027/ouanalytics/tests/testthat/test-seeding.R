# --- record_vs and point_differential ---------------------------------------

test_that("record_vs counts wins, losses, and percentage against a set of teams", {
  sc <- bind_rows(sg("A1", "A2", 100, 90), sg("A1", "A3", 90, 100),
                  sg("A1", "A4", 100, 90), sg("A1", "W1", 100, 90))
  r <- record_vs("A1", c("A2", "A3", "A4"), sc)
  expect_equal(c(r$wins, r$losses, r$pct), c(2, 1, 2 / 3))
  expect_equal(r$abbreviation, "A1")
})

test_that("record_vs gives an NA percentage when no games were played", {
  r <- record_vs("A1", "A2", no_games)
  expect_equal(c(r$wins, r$losses), c(0, 0))
  expect_true(is.na(r$pct))
})

test_that("point_differential sums margins over all of a team's games", {
  sc <- bind_rows(sg("A1", "A2", 110, 100), sg("A1", "A3", 90, 95))
  expect_equal(point_differential("A1", sc), 5)
  expect_equal(point_differential("A2", sc), -10)
  expect_equal(point_differential("S1", sc), 0)
})

# --- two-team ties, one criterion at a time ---------------------------------

test_that("two-team tie: head-to-head decides first", {
  sc <- sg("A1", "A2", 100, 90)
  expect_equal(two("A1", "A2", sc), c("A1", "A2"))
  expect_equal(two("A2", "A1", sc), c("A1", "A2"))
})

test_that("two-team tie: a division leader beats a non-leader", {
  expect_equal(two("A1", "C1", no_games, division_leaders = "C1"), c("C1", "A1"))
  expect_equal(two("A1", "C1", no_games, division_leaders = "A1"), c("A1", "C1"))
})

test_that("two-team tie: division record decides for teams in one division", {
  sc <- bind_rows(sg("A1", "A3", 100, 90), sg("A2", "A3", 90, 100))
  expect_equal(two("A1", "A2", sc), c("A1", "A2"))
  expect_equal(two("A2", "A1", sc), c("A1", "A2"))
})

test_that("two-team tie: conference record decides for teams in different divisions", {
  sc <- bind_rows(sg("A1", "C2", 100, 90), sg("C1", "C2", 90, 100))
  expect_equal(two("A1", "C1", sc), c("A1", "C1"))
  expect_equal(two("C1", "A1", sc), c("A1", "C1"))
})

test_that("two-team tie: record against playoff teams in the own conference", {
  # Equal conference records (1-1 each), different records against A3
  sc <- bind_rows(sg("A1", "A3", 100, 90), sg("A1", "C2", 90, 100),
                  sg("C1", "A3", 90, 100), sg("C1", "C2", 100, 90))
  expect_equal(two("A1", "C1", sc, playoff_eligible_own = "A3"), c("A1", "C1"))
  expect_equal(two("C1", "A1", sc, playoff_eligible_own = "A3"), c("A1", "C1"))
})

test_that("two-team tie: record against playoff teams in the other conference", {
  sc <- bind_rows(sg("A1", "W1", 100, 90), sg("C1", "W1", 90, 100))
  expect_equal(two("A1", "C1", sc, playoff_eligible_other = c("W1", "W2")),
               c("A1", "C1"))
  expect_equal(two("C1", "A1", sc, playoff_eligible_other = c("W1", "W2")),
               c("A1", "C1"))
})

test_that("two-team tie: an empty list of playoff teams is skipped", {
  sc <- bind_rows(sg("A1", "W1", 110, 100), sg("C1", "W1", 105, 100))
  expect_equal(two("C1", "A1", sc, playoff_eligible_own = character(0),
                   playoff_eligible_other = character(0)),
               c("A1", "C1"))
})

test_that("two-team tie: point differential decides when records match", {
  sc <- bind_rows(sg("A1", "W1", 110, 100), sg("C1", "W1", 105, 100))
  expect_equal(two("A1", "C1", sc), c("A1", "C1"))
  expect_equal(two("C1", "A1", sc), c("A1", "C1"))
})

test_that("two-team tie: alphabetical order when nothing separates them", {
  sc <- bind_rows(sg("A1", "W1", 100, 90), sg("C1", "W2", 100, 90))
  expect_equal(two("C1", "A1", sc), c("A1", "C1"))
  expect_equal(two("C1", "A1", no_games), c("A1", "C1"))
})

# --- multi-team ties --------------------------------------------------------

test_that("multi-team tie: one team or two teams delegate", {
  expect_equal(multi("A1", no_games), "A1")
  expect_equal(multi(c("A2", "A1"), no_games), c("A1", "A2"))
})

test_that("multi-team tie: division leaders go above non-leaders", {
  expect_equal(multi(c("C1", "C2", "A1"), no_games, division_leaders = "A1"),
               c("A1", "C1", "C2"))
  expect_equal(multi(c("C2", "C1", "A1"), no_games, division_leaders = c("C1", "A1")),
               c("A1", "C1", "C2"))
})

test_that("multi-team tie: record among all the tied teams, one best team", {
  sc <- bind_rows(sg("A1", "A2", 100, 90), sg("A1", "A3", 100, 90),
                  sg("A2", "A3", 100, 90))
  expect_equal(multi(c("A3", "A2", "A1"), sc), c("A1", "A2", "A3"))
})

test_that("multi-team tie: record among all the tied teams, two best teams", {
  sc <- bind_rows(sg("A1", "A3", 100, 90), sg("A2", "A3", 100, 90))
  expect_equal(multi(c("A3", "A2", "A1"), sc), c("A1", "A2", "A3"))
})

test_that("multi-team tie: division record when all are in one division", {
  sc <- bind_rows(sg("A1", "A4", 100, 90), sg("A2", "A4", 90, 100))
  expect_equal(multi(c("A1", "A2", "A3"), sc), c("A1", "A3", "A2"))
})

test_that("multi-team tie: conference record across divisions", {
  sc <- bind_rows(sg("A1", "A4", 100, 90), sg("C1", "A4", 90, 100))
  expect_equal(multi(c("A1", "C1", "C2"), sc), c("A1", "C2", "C1"))
})

test_that("multi-team tie: record against playoff teams in the own conference", {
  sc <- bind_rows(sg("A1", "A4", 100, 90), sg("A1", "A5", 90, 100),
                  sg("C1", "A4", 90, 100), sg("C1", "A5", 100, 90))
  expect_equal(multi(c("A1", "C1", "C2"), sc, playoff_eligible_own = "A4"),
               c("A1", "C1", "C2"))
})

test_that("multi-team tie: point differential, one best team", {
  sc <- bind_rows(sg("A1", "W1", 110, 100), sg("C1", "W1", 105, 100))
  expect_equal(multi(c("C2", "C1", "A1"), sc), c("A1", "C1", "C2"))
})

test_that("multi-team tie: point differential, two best teams", {
  sc <- bind_rows(sg("A1", "W1", 110, 100), sg("C1", "W2", 110, 100))
  expect_equal(multi(c("C2", "C1", "A1"), sc), c("A1", "C1", "C2"))
})

test_that("multi-team tie: alphabetical order when nothing separates them", {
  expect_equal(multi(c("C2", "A1", "C1"), no_games), c("A1", "C1", "C2"))
})

# --- division leaders -------------------------------------------------------

test_that("get_division_leaders picks one leader per division, breaking ties", {
  conf <- tibble(
    abbreviation = c("A1", "A2", "C1", "C2", "S1", "S2", "S3"),
    division = c("ATL", "ATL", "CEN", "CEN", "SE", "SE", "SE"),
    `Winning Pct` = c(0.6, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  )
  sc <- bind_rows(
    sg("C2", "C1", 100, 90),                       # two-way tie: C2 won
    sg("S1", "S2", 110, 100), sg("S2", "S3", 120, 100),
    sg("S3", "S1", 105, 100)                       # three-way tie: S2 on margin
  )
  expect_equal(get_division_leaders(conf, sc, league_meta), c("A1", "C2", "S2"))
})

# --- seeding a conference ---------------------------------------------------

test_that("seed_conference orders by winning percentage and breaks ties", {
  out <- seed_conference("East", league_standings, league_scores, league_meta)
  expect_equal(out$abbreviation,
               c("A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4", "C5",
                 "S1", "S2", "S3"))
  expect_equal(out$seed, 1:13)
})

test_that("seed_conference marks playoff, play-in, and out", {
  out <- seed_conference("East", league_standings, league_scores, league_meta)
  expect_equal(out$seed_note, c(rep("Playoff", 6), rep("Play-In", 4), rep("", 3)))
})

test_that("seed_conference handles a conference with fewer than 10 teams", {
  out <- seed_conference("West", league_standings, league_scores, league_meta)
  expect_equal(out$abbreviation, c("W2", "W1", "W3", "X1", "X2", "X3"))
  expect_equal(unique(out$seed_note), "Playoff")
})

test_that("seed_conference returns the columns the page uses", {
  out <- seed_conference("East", league_standings, league_scores, league_meta)
  expect_equal(names(out),
               c("seed", "team_name", "conference", "division", "wins", "losses",
                 "Winning Pct", "Games Back", "abbreviation", "seed_note"))
})

test_that("compute_playoff_seeding seeds both conferences", {
  out <- compute_playoff_seeding(league_standings, league_scores, league_meta)
  expect_equal(nrow(out), 19)
  expect_equal(out %>% filter(conference == "East") %>% pull(seed), 1:13)
  expect_equal(out %>% filter(conference == "West") %>% pull(seed), 1:6)
})
