## code to prepare `approved` dataset goes here

filename <- "tests/testthat/ref/ex1.R"
source(filename)
approved <- get_used_functions(filename)
usethis::use_data(approved, overwrite = TRUE)
