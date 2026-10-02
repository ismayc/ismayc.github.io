# Season constants for the 2026-27 over/under pipeline.
# Every other script sources this file, so a new season only needs edits here
# (plus the Python files, which keep their own copies; see the bottom).
#
# Key dates: https://www.nba.com/news/2026-27-schedule-announced

season_folder         <- "nba-over-under-2026-2027"
season_label          <- "2026-27"   # schedule CSV names, nba_api season
starting_season_year  <- 2026        # nba_api SEASON_ID suffix (22026)
ending_season_year    <- 2027        # basketball-reference NBA_2027, page names

nba_season_start_date <- "2026-10-20"
nba_season_end_date   <- "2027-04-11"
all_star_game_date    <- "2027-02-21"

# The NBA Cup championship is the one Cup game that does NOT count toward
# regular-season records. The knockout rounds before it do count.
nba_cup_champ_date    <- "2026-12-11"

# basketball-reference only lists 80 games per team until the Cup knockout
# field is set; the last two games per team get added after Group Play.
schedule_initial_file   <- paste0("schedule-", season_label, "_initial.csv")
schedule_after_cup_file <- paste0("schedule-", season_label, "-after-ist.csv")

# Resolve a file inside the season folder whether the working directory is
# the season folder (local runs) or the repo root (GitHub Actions).
season_path <- function(...) {
  if (grepl("nba-over-under", basename(getwd()))) {
    file.path(...)
  } else {
    file.path(season_folder, ...)
  }
}

# The schedule to use right now: the complete post-Cup schedule once it
# exists, the 80-game initial schedule before that.
current_schedule_file <- function() {
  if (file.exists(season_path(schedule_after_cup_file))) {
    season_path(schedule_after_cup_file)
  } else {
    season_path(schedule_initial_file)
  }
}

# Python copies of these values (update them together with this file):
#   02b-get_game_scores.py and update_nba_data.sh: YEAR, SEASON_ID, the
#     season start datetime(), and the NBA_<year>_games URL
#   06a-fetch-official-standings.py: season=
