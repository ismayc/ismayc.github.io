# Pull the participants' picks from the over-under-submitter Google Sheet and
# save them as rds/gs_picks_raw.rds, the input to 01-picks-formatting.R and
# the rooting guide. Run this once after submissions close (it needs a
# googlesheets4 login, so it is not part of the daily build). Committing the
# rds turns on the daily page build in .github/workflows/nba-over-under.yaml.
#
# Run from this folder: Rscript --vanilla 01a-pull-picks.R

library(tidyverse)
library(googlesheets4)

source("00-season-config.R")

picks_sheet <- "https://docs.google.com/spreadsheets/d/1DJJtkDkYNe_WoYRGmc0XcrCPMun8WpTho66nZKYVr4c/edit?usp=sharing"

# The submitter writes one tab per season, named like "2026-27", with
# columns user, submitted_at, team, conference, wager, pick.
submissions <- read_sheet(picks_sheet, sheet = season_label,
                          col_types = "cccccc") |>
  rename(name = user, submission_date = submitted_at) |>
  mutate(name = str_squish(name), wager = as.numeric(wager))

# Every submit appends 30 rows, so a participant who resubmitted has more
# than one entry. Keep each person's latest one.
# The app sends ISO times; parse so a Sheets-reformatted date also compares.
gs_picks_raw <- submissions |>
  mutate(submitted = lubridate::parse_date_time(
    submission_date, orders = c("Ymd HMS", "Ymd HM", "mdY HMS", "mdY HM"),
    quiet = TRUE
  )) |>
  group_by(name) |>
  filter(submitted == max(submitted)) |>
  ungroup() |>
  select(-submitted) |>
  arrange(name, team)

# The pipeline keys participants on first name, so two people sharing a first
# name would be merged.
first_names <- gs_picks_raw |>
  distinct(name) |>
  mutate(player = str_extract(name, "^[^\\s]+")) |>
  add_count(player) |>
  filter(n > 1)
if (nrow(first_names) > 0) {
  stop("Participants share a first name: ",
       paste(first_names$name, collapse = ", "),
       ". Edit their names in the Sheet so first names differ.")
}

# The app enforces these rules, but check what actually landed in the Sheet.
meta <- readxl::read_excel("picks.xlsx", sheet = "meta")
problems <- gs_picks_raw |>
  group_by(name) |>
  summarize(
    teams = n_distinct(team),
    unknown_teams = sum(!team %in% meta$team),
    bad_wager_counts = sum(table(factor(wager, levels = 6:15)) != 3),
    bad_picks = sum(!pick %in% c("Over", "Under"))
  ) |>
  filter(teams != 30 | unknown_teams > 0 | bad_wager_counts > 0 |
           bad_picks > 0)
if (nrow(problems) > 0) {
  print(problems)
  stop("Some entries break the pool rules (see table above).")
}

write_rds(gs_picks_raw, "rds/gs_picks_raw.rds")
message("Saved ", n_distinct(gs_picks_raw$name), " entries to ",
        "rds/gs_picks_raw.rds: ",
        paste(unique(gs_picks_raw$name), collapse = ", "))
