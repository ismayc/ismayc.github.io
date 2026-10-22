# Run the tests and fail unless every line in R/ is covered.
#   Rscript tests/run-tests.R          (from nba-over-under-2026-2027/)
# Needs: testthat, covr, dplyr, tidyr, tibble, stringr.
# R/game_logs.R is not measured: it is an unused copy of a stats.nba.com
# scraper (its only call in 02a is commented out).

args <- commandArgs(trailingOnly = FALSE)
here_dir <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
setwd(dirname(here_dir))

source_files <- c("R/standings_logic.R", "R/tiebreakers.R")
test_files <- sort(list.files("tests/testthat", pattern = "^(helper|test).*[.]R$",
                              full.names = TRUE))

# 1. Plain pass or fail, so a failed test names itself
results <- testthat::test_dir("tests/testthat", reporter = "summary",
                              stop_on_failure = FALSE)
failed <- sum(vapply(results, function(r) any(vapply(
  r$results, function(x) inherits(x, c("expectation_failure", "expectation_error")),
  logical(1))), logical(1)))

# 2. Line coverage of the files above, running the same tests
cov <- covr::file_coverage(source_files, test_files)
pct <- covr::percent_coverage(cov)
cat(sprintf("\nLine coverage of %s: %.2f%%\n",
            paste(source_files, collapse = " and "), pct))
if (pct < 100) {
  missed <- covr::zero_coverage(cov)
  print(missed[, c("filename", "line")])
}

if (failed > 0 || pct < 100) {
  cat(sprintf("FAIL: %d failing test(s), coverage %.2f%% (needs 100%%)\n", failed, pct))
  quit(status = 1)
}
cat("OK: all tests pass at 100% line coverage\n")
