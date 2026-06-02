# Export the lean Shiny app in finder_app/ to a fully static, browser-only site
# (WebAssembly via shinylive). The output is committed and served by GitHub Pages
# at <site>/nba-player-finder/; the Quarto build only copies it as a resource.
#
# Run from the rosters/ directory (locally or in the weekly Action):
#   Rscript build_finder_site.R

# Auto-install any required R packages that aren't available, so this "just
# works" when tested locally (and in CI). Only `shinylive` is needed locally:
# shinylive::export downloads the WebAssembly builds of shiny/DT/dplyr itself,
# so those do not need to be installed here.
ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) == 0) return(invisible())

  # Make sure a CRAN mirror is set even under a bare Rscript session.
  repos <- getOption("repos")
  if (is.null(repos[["CRAN"]]) || is.na(repos[["CRAN"]]) ||
      repos[["CRAN"]] == "@CRAN@") {
    repos <- c(CRAN = "https://cran.r-project.org")
  }
  message("Installing missing R package(s): ", paste(missing, collapse = ", "))
  install.packages(missing, repos = repos)

  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing)) {
    stop("Could not install required R package(s): ",
         paste(still_missing, collapse = ", "))
  }
}

ensure_packages("shinylive")

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  ofile <- sys.frame(1)$ofile
  if (!is.null(ofile)) return(dirname(normalizePath(ofile)))
  getwd()
}

script_dir <- get_script_dir()

app_dir <- file.path(script_dir, "finder_app")

# Repo root is two levels up from rosters/ (…/nba-over-under-2025-2026/rosters).
repo_root <- normalizePath(file.path(script_dir, "..", ".."))
dest_dir <- file.path(repo_root, "nba-player-finder")

message("Exporting ", app_dir, " -> ", dest_dir)
shinylive::export(appdir = app_dir, destdir = dest_dir)
message("Done. Open ", file.path(dest_dir, "index.html"))
