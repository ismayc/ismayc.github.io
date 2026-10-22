# Install the package, run its tests, and fail unless every line is covered.
#   Rscript ouanalytics/tools/check.R      (from nba-over-under-2026-2027/)
# Needs: testthat, covr, dplyr, tidyr, tibble, stringr.

args <- commandArgs(trailingOnly = FALSE)
tools_dir <- dirname(normalizePath(sub("^--file=", "", args[grep("^--file=", args)])))
pkg <- dirname(tools_dir)

cov <- covr::package_coverage(pkg, type = "tests", quiet = FALSE)
pct <- covr::percent_coverage(cov)
cat(sprintf("\nLine coverage of ouanalytics: %.2f%%\n", pct))
if (pct < 100) {
  missed <- covr::zero_coverage(cov)
  print(missed[, c("filename", "line")])
  cat("FAIL: coverage is under 100%\n")
  quit(status = 1)
}
cat("OK: all tests pass at 100% line coverage\n")
