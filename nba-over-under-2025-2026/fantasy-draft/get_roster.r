# install.packages(c("pdftools", "stringr", "tibble", "readr", "dplyr", "purrr"))
library(pdftools)
library(stringr)
library(tibble)
library(readr)
library(dplyr)
library(purrr)

pdf_path <- "NBA Fantasy Draft 2025-26 Rosters.pdf"
txt <- pdftools::pdf_text(pdf_path) |> paste(collapse = "\n")

# 1) Find the header row that has all three titles on it, then take everything after it
#    This is robust whether they appear on one line or across lines with spacing.
header_idx <- str_locate(txt, "GUARDS[\\s\\S]*?WINGS[\\s\\S]*?POSTS")[1,2]
stopifnot(!is.na(header_idx))
block <- substr(txt, header_idx + 1, nchar(txt))

# 2) Split into lines and keep those that contain at least one numbered entry ("1. ", "2. ", etc.)
lines <- str_split(block, "\\n")[[1]] |> trimws()
lines <- lines[str_detect(lines, "\\b[0-9]+\\.")]

# 3) Each line has up to 3 entries separated by wide spaces. Split on 2+ spaces.
split3 <- function(s) {
  pieces <- str_split(s, "\\s{2,}")[[1]] |> trimws()
  # Keep only things that look like "12. Name, TEAM"
  pieces <- pieces[str_detect(pieces, "^[0-9]+\\.")]
  # pad to length 3
  length(pieces) <- 3
  pieces
}

mat <- map(lines, split3) |> do.call(rbind, args = _)

# 4) Strip the leading numbers ("12. ") and trailing spaces
strip_num <- function(x) sub("^[0-9]+\\.\\s*", "", x)
guards <- strip_num(mat[,1]) |> na.omit() |> as.character()
wings  <- strip_num(mat[,2]) |> na.omit() |> as.character()
posts  <- strip_num(mat[,3]) |> na.omit() |> as.character()

# 5) Optional: stop if counts aren't the expected 30 each
# If your PDF has different counts, comment these out or adjust.
stopifnot(length(guards) == 30, length(wings) == 30, length(posts) == 30)

players <- tibble(
  name = c(guards, wings, posts),
  position = c(
    rep("Guard", length(guards)),
    rep("Wing",  length(wings)),
    rep("Post",  length(posts))
  )
)

# 6) Write the CSV and RDS exactly like your sample (quotes around name due to commas)
write_csv(players, "players_ballot.csv")
write_rds(players, "players_ballot.rds")
