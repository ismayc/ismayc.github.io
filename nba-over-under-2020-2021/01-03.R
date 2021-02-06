r_files <- list.files(pattern = "+(.R)$")[c(-1, -5)]
purrr::walk(r_files, source)