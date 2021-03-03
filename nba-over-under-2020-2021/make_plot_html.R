setwd("~/Desktop/ismayc.github.io/nba-over-under-2020-2021")
Sys.setenv(RSTUDIO_PANDOC = "/Applications/RStudio.app/Contents/MacOS/pandoc")

cat(glue::glue("Starting at {Sys.time()}"))
rmarkdown::render(
  input = "make_plots.Rmd",  
  output_format = "html_document",
  output_file = "2021-nba-over-under.html",
  output_dir = "..",
  quiet = TRUE
)
cat(glue::glue("Completed at {Sys.time()}\n\n"))

# system("git config --global user.name 'Chester Ismay'")
system("git pull")
system("git add --all")
system(paste0('git commit -m "Updated ', Sys.time(), '"'))
system("git push origin master")
