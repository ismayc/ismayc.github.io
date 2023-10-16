# This file is no longer used since scores are pulled in
# using Python in the Rmd file
r_files <- list.files(pattern = "+(.R)$")[2:5]
purrr::walk(r_files, source)
