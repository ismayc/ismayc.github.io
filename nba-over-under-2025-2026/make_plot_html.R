# For running locally every day uncomment next line
# setwd("~/Desktop/ismayc.github.io/nba-over-under-2023-2024")

#here::i_am("nba-over-under-2021-2022.Rproj")
#Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
#Sys.setenv(RSTUDIO_PANDOC = "/usr/local/bin/pandoc")

# Do this to get it working
# usethis::edit_r_environ()
# Add RSTUDIO_PANDOC = /usr/local/bin/pandoc
# Save file

library(glue)
ending_season_year <- 2026

#update_page <- FALSE
update_page <- TRUE

cat("\n")
cat(glue("Starting at {Sys.time() - lubridate::hours(8)}"), "Pacific time", "\n")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

date_modified <- file.info(file.path("docs", glue("{ending_season_year}-nba-over-under.html"))) |>
  dplyr::pull(ctime) |>
  as.Date()


# system("git config --global user.name 'Chester Ismay'")
# system("git remote set-url origin git@github.com:ismayc/ismayc.github.io.git")
# system("git config pull.rebase false")
# system("git pull")

if (date_modified != Sys.Date() || update_page) {
  rmarkdown::render(
    # Remove directory if running locally
    input = "nba-over-under-2025-2026/make_plots.Rmd",  
    output_format = "html_document",
    output_file = glue("{ending_season_year}-nba-over-under.html"),
    output_dir = "docs",
    # Uncomment line above if running locally (Or maybe can just change
    # to ismayc.github.io as working directory?)
    quiet = TRUE
  )
  if(grepl("MacBook", Sys.info()["nodename"])) {
    file.copy(
      from = file.path(glue("{ending_season_year}-nba-over-under.html")),
      to = file.path("docs", glue("{ending_season_year}-nba-over-under.html")),
      overwrite = TRUE
    )
  }
} else {
  cat("Webpage already created today", "\n")
}
cat(glue("Completed at {Sys.time() - lubridate::hours(8)}"), "Pacific time", "\n")

# system("git add --all")
# system(paste0('git commit -m "Updated ', Sys.time(), '"'))
# system("git push origin master")
