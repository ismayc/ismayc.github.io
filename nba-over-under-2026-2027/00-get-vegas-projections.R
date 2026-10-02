# Pull the preseason Vegas win totals (over/under lines) from VegasInsider and
# write them everywhere the season needs them:
#   - vegas_win_totals_all_books_<date>.csv  every sportsbook's line (snapshot)
#   - projections.csv                        the over-under-submitter's input
#   - picks.xlsx, "projections" sheet        read by 01-picks-formatting.R
#
# Lines move until opening night, so rerun this right before sending the
# submitter link out, then leave the lines alone for the rest of the season.
#
# Run from this folder: Rscript --vanilla 00-get-vegas-projections.R

library(rvest)
library(tidyverse)
library(jsonlite)
library(openxlsx)

# Which line the pool uses. "consensus" takes, for each team, the line posted
# by the most sportsbooks; a tie goes to the tied line closest to the median
# across books (then the lower line). It is always a line some book posted,
# and it covers teams a single book has not listed yet. To follow one book
# instead, use its VegasInsider column name: Bet365, BetMGM, DraftKings,
# Caesars, HardRock, or RiversCasino.
book <- "consensus"

# Where the submitter app lives (its projections.csv gets overwritten).
submitter_dir <- path.expand("~/repos/over-under-submitter")

# Lines are still settling earlier than one week before opening night, so
# the final pull waits until then. An earlier pull is allowed only as a
# preview: `Rscript --vanilla 00-get-vegas-projections.R --preliminary`, and
# the submitter page must then have PRELIMINARY_LINES = true.
source("00-season-config.R")
earliest_pull_date <- as.Date(nba_season_start_date) - 7
preliminary <- "--preliminary" %in% commandArgs(trailingOnly = TRUE)
if (Sys.Date() < earliest_pull_date && !preliminary) {
  stop("Too early to pull win totals: wait until ", earliest_pull_date,
       " (one week before the ", nba_season_start_date, " opener), or pass ",
       "--preliminary for preview lines.")
}

url <- "https://www.vegasinsider.com/nba/odds/win-totals/"
page <- read_html(url)
raw <- html_table(page)[[1]]

# The first column holds schema.org JSON with the full team name in
# "description"; each book's column reads like "o62.5   -110   +".
names(raw)[1] <- "team_cell"
raw <- raw[, names(raw) != ""]  # trailing unnamed column
win_totals <- raw |>
  filter(str_detect(team_cell, "SportsTeam")) |>
  mutate(team = map_chr(
    str_extract(team_cell, "\\{.*\\}"),
    \(x) fromJSON(x)$description
  ),
  team = recode(team, "LA Clippers" = "Los Angeles Clippers")) |>
  select(-team_cell) |>
  pivot_longer(-team, names_to = "sportsbook", values_to = "cell") |>
  mutate(win_total = as.numeric(str_match(cell, "^[ou]\\s*(\\d+(\\.\\d+)?)")[, 2]),
         odds = str_match(cell, "([+-]\\d+|even)")[, 2]) |>
  select(team, sportsbook, win_total, odds)

stopifnot(
  "Expected 30 teams on the VegasInsider table" =
    n_distinct(win_totals$team) == 30,
  "Requested book is not on the VegasInsider table" =
    book %in% c("consensus", win_totals$sportsbook)
)

consensus_line <- function(lines) {
  lines <- lines[!is.na(lines)]
  if (length(lines) == 0) return(NA_real_)
  counts <- table(lines)
  tied <- as.numeric(names(counts)[counts == max(counts)])
  tied[order(abs(tied - median(lines)), tied)][1]
}

all_books <- win_totals |>
  select(team, sportsbook, win_total) |>
  pivot_wider(names_from = sportsbook, values_from = win_total) |>
  arrange(team)
write_csv(all_books, paste0("vegas_win_totals_all_books_", Sys.Date(), ".csv"))

# Conference and team order come from the meta sheet so names match the
# rest of the pipeline exactly (for example "Los Angeles Clippers").
meta <- readxl::read_excel("picks.xlsx", sheet = "meta")
chosen <- if (book == "consensus") {
  win_totals |>
    group_by(team) |>
    summarize(win_projection = consensus_line(win_total),
              books_posting = sum(!is.na(win_total)))
} else {
  win_totals |>
    filter(sportsbook == book) |>
    select(team, win_projection = win_total)
}

projections <- meta |>
  select(team, conference) |>
  left_join(chosen |> select(team, win_projection), by = "team")

missing <- projections |> filter(is.na(win_projection))
if (nrow(missing) > 0) {
  stop(book, " has no line for: ", paste(missing$team, collapse = ", "),
       ". Pick another book or fill these in by hand.")
}
not_half <- projections |> filter(win_projection %% 1 != 0.5)
if (nrow(not_half) > 0) {
  warning("Whole-number lines can end in a push: ",
          paste0(not_half$team, " (", not_half$win_projection, ")",
                 collapse = ", "))
}

# picks.xlsx: rewrite only the win_projection column, matched by team, so the
# sheet's other columns and formatting stay as they are.
wb <- loadWorkbook("picks.xlsx")
sheet_rows <- read.xlsx(wb, "projections")
stopifnot(names(sheet_rows)[3] == "win_projection",
          setequal(sheet_rows$team, projections$team))
new_values <- projections$win_projection[match(sheet_rows$team,
                                               projections$team)]
writeData(wb, "projections", new_values, startCol = 3, startRow = 2,
          colNames = FALSE)
saveWorkbook(wb, "picks.xlsx", overwrite = TRUE)

write_csv(projections, "projections.csv")
if (dir.exists(submitter_dir)) {
  invisible(file.copy("projections.csv",
                      file.path(submitter_dir, "projections.csv"),
                      overwrite = TRUE))
}

message("Wrote ", book, " win totals for 30 teams (", Sys.Date(), ") to ",
        "picks.xlsx, projections.csv",
        if (dir.exists(submitter_dir)) ", and the submitter's projections.csv")
print(projections, n = 30)
