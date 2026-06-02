# Wiring test: drive the real Shiny server with shiny::testServer (no browser)
# to confirm inputs flow through the reactive into the filtered table. The
# detailed filter behavior is covered by test-filter-players.R.

test_that("server reactive responds to inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("DT")
  skip_if_not(file.exists(players_csv_path()), "players_finder.csv not present")

  # Derive team lists from the data file (the app's globals aren't exposed in
  # the testServer eval scope, but server-local reactives like filtered() are).
  players <- read.csv(players_csv_path(), stringsAsFactors = FALSE)
  west_teams <- unique(players$team[players$conference == "West"])
  east_teams <- unique(players$team[players$conference == "East"])

  shiny::testServer(finder_app_dir(), {
    session$setInputs(
      conference    = "West",
      division_west = c("Southwest", "Northwest", "Pacific"),
      sw_team = west_teams, nw_team = west_teams, pac_team = west_teams,
      known_height = FALSE, min_feet = 5, min_inches = 0, max_feet = 7, max_inches = 11,
      known_age = FALSE, slider_age = c(15, 50),
      known_jersey = FALSE, slider_jersey = c(0, 99)
    )

    res <- filtered()
    expect_s3_class(res, "data.frame")
    expect_named(
      res,
      c("player", "team", "conference", "division", "height", "age", "number_jersey")
    )
    expect_gt(nrow(res), 0)
    expect_true(all(res$conference == "West"))

    # Narrowing the conference should change the result set.
    session$setInputs(
      conference    = "East",
      division_east = c("Southeast", "Atlantic", "Central"),
      se_team = east_teams, atlantic_team = east_teams, cen_team = east_teams
    )
    res_east <- filtered()
    expect_true(all(res_east$conference == "East"))
  })
})

# focus_team_plan() is the logic behind the "Focus on one team" control: which
# conference to switch to and which team-group selections to apply. (The observer
# that applies these via update*Input can't be exercised by testServer, which
# doesn't simulate the input round-trip, so we test the pure plan directly.)
test_that("focus_team_plan targets the right group and clears the others", {
  div_group <- c(Southwest = "sw_team", Northwest = "nw_team", Pacific = "pac_team",
                 Southeast = "se_team", Atlantic = "atlantic_team", Central = "cen_team")
  groups_by_conf <- list(West = c("sw_team", "nw_team", "pac_team"),
                         East = c("se_team", "atlantic_team", "cen_team"))
  team_meta <- data.frame(
    team = c("Boston Celtics", "Los Angeles Lakers"),
    conference = c("East", "West"),
    division = c("Atlantic", "Pacific"),
    stringsAsFactors = FALSE
  )

  plan <- focus_team_plan("Boston Celtics", team_meta, div_group, groups_by_conf)
  expect_equal(plan$conference, "East")
  expect_equal(plan$selected$atlantic_team, "Boston Celtics")
  expect_equal(plan$selected$se_team, character(0))
  expect_equal(plan$selected$cen_team, character(0))

  west <- focus_team_plan("Los Angeles Lakers", team_meta, div_group, groups_by_conf)
  expect_equal(west$conference, "West")
  expect_equal(west$selected$pac_team, "Los Angeles Lakers")
  expect_equal(west$selected$sw_team, character(0))

  expect_null(focus_team_plan("", team_meta, div_group, groups_by_conf))
  expect_null(focus_team_plan(NULL, team_meta, div_group, groups_by_conf))
  expect_null(focus_team_plan("Nonexistent", team_meta, div_group, groups_by_conf))
})
