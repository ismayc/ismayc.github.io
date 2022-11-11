setwd("~/Desktop/ismayc.github.io/nba-over-under-2022-2023")
#here::i_am("nba-over-under-2021-2022.Rproj")
#Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")
#Sys.setenv(RSTUDIO_PANDOC = "/usr/local/bin/pandoc")

# Do this to get it working
# usethis::edit_r_environ()
# Add RSTUDIO_PANDOC = /usr/local/bin/pandoc
# Save file

#update_page <- FALSE
update_page <- TRUE

cat("\n")
cat(glue::glue("Starting at {Sys.time()}"), "Pacific time", "\n")

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))

date_modified <- file.info(file.path("..", "2023-nba-over-under.html")) %>% 
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
    output_file = "2023-nba-over-under.html",
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