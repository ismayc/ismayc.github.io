setwd("~/Desktop/ismayc.github.io/nba-over-under-2020-2021")
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")

update_page <- FALSE
#update_page <- TRUE

cat("\n")
cat(glue::glue("Starting at {Sys.time()}"), "Pacific time", "\n")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

date_modified <- file.info(file.path("..", "2021-nba-over-under.html")) %>% 
  pull(ctime) %>% 
  as.Date()

system("git config --global user.name 'Chester Ismay'")
system("git remote set-url origin git@github.com:ismayc/ismayc.github.io.git")
system("git config pull.rebase false")
system("git pull")

if (date_modified != Sys.Date() || update_page) {
  rmarkdown::render(
    input = "make_plots.Rmd",  
    output_format = "html_document",
    output_file = "2021-nba-over-under.html",
    output_dir = "..",
    quiet = TRUE
  )
} else {
  cat("Webpage already created today", "\n")
}
cat(glue::glue("Completed at {Sys.time()}"), "Pacific time", "\n")

system("git add --all")
system(paste0('git commit -m "Updated ', Sys.time(), '"'))
system("git push origin master")
