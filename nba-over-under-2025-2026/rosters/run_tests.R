# Run the player-finder test suite locally:
#   Rscript run_tests.R
# Auto-installs any missing test dependencies so it "just works".

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible())
  repos <- getOption("repos")
  if (is.null(repos[["CRAN"]]) || is.na(repos[["CRAN"]]) ||
      repos[["CRAN"]] == "@CRAN@") {
    repos <- c(CRAN = "https://cran.r-project.org")
  }
  message("Installing missing R package(s): ", paste(missing, collapse = ", "))
  install.packages(missing, repos = repos)
}

ensure_packages(c("testthat", "dplyr", "shiny", "DT"))

# Resolve the directory of this script so tests run from any working directory.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  getwd()
}

test_dir <- file.path(get_script_dir(), "tests", "testthat")
testthat::test_dir(test_dir, stop_on_failure = TRUE)
