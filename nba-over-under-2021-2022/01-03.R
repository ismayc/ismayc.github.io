r_files <- list.files(pattern = "+(.R)$")[2:4]
purrr::walk(r_files, source, local = TRUE)