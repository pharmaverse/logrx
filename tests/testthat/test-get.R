test_that("metadata elements are specified correctly and loaded into a list", {

   logrx_session_info <- session_info()$packages %>%
      filter(.data$package == "logrx")

  expect_identical(get_logrx_metadata(),
               list(
                  info = paste0("This log was generated using logrx ",
                                logrx_session_info[['loadedversion']]),
                  version = logrx_session_info[['loadedversion']],
                  built = logrx_session_info[["source"]],
                  repository_link = "https://github.com/atorus-research/logrx"
               ))
})

test_that("when given a file as an argument a normalized file path to that file will be returned by default", {
   expect_equal(get_file_path(file = './test-get.R'), normalizePath('test-get.R'))
})

test_that("when given a file as an argument a non-normalized file path to that file will be returned when specified to not normalize", {
   expect_equal(get_file_path(file = './test-get.R', normalize = FALSE), './test-get.R')
})

test_that("session info is captured", {
   expect_identical(get_session_info(), capture.output(session_info(info = "all")))
})

test_that("all functions that are masked are found and returned", {
   expect_identical(names(get_masked_functions()), unique(unlist(conflicts(detail = TRUE))))
})

test_that("each masked function element contain source and masks elements", {
   expect_identical(unlist(unname(map(get_masked_functions(), ~ names(.x)))), rep(c("source","masks"), length(get_masked_functions())))
})

test_that("ex1.R parses correctly", {
   filename <- test_path("ref", "ex1.R")
   source(filename, local = TRUE)

   expected <- tibble::tribble(
      ~function_name,        ~library,
      "library",  "package:base",
      "%>%", "package:dplyr",
      "group_by", "package:dplyr",
      "summarize", "package:dplyr",
      "mean",  "package:base",
      "pivot_wider", "package:tidyr"
   )

   expect_identical(get_used_functions(filename), expected)
})

test_that("unapproved packages and functions found in ex2.R", {
   filename <- test_path("ref", "ex2.R")
   source(filename, local = TRUE)

   withr::local_options(logrx.approved = test_path("ref", "approved.rds"))

   approved_functions <- readRDS(getOption("logrx.approved"))

   used_functions <- get_used_functions(filename)

   expected <- tibble::tribble(
      ~function_name,        ~library,
      "glimpse",  "package:dplyr"
   )

   expect_identical(get_unapproved_use(used_functions, approved_functions), expected)
})

test_that("unapproved packages returns expected result when all packages and functions are approved", {
   filename <- test_path("ref", "ex1.R")
   source(filename, local = TRUE)

   withr::local_options(logrx.approved = test_path("ref", "approved.rds"))

   approved_functions <- readRDS(getOption("logrx.approved"))

   used_functions <- tibble::tibble(
      function_name = "mean",
      library = "package:base"
   )

   expected <- tibble::tibble(
      function_name = vector(mode = "character"),
      library = vector(mode = "character")
   )

   expect_identical(get_unapproved_use(used_functions, approved_functions), expected)
})

test_that("used functions returned correctly when file doesn't contain all token types", {
   filename <- test_path("ref", "ex3.R")
   source(filename, local = TRUE)

   expected <- tibble::tribble(
      ~function_name,        ~library,
      "print",  "package:base"
   )

   expect_identical(get_used_functions(filename), expected)
})

test_that("parse does not fatal error when syntax issue occurs", {
   filename <- test_path("ref", "ex4.R")

   expected <- tibble::tibble(
         function_name = "",
         library = "Syntax Error Found, Package and Function Identification Stopped"
      )

   expect_identical(get_used_functions(filename), expected)
})

test_that("lint returns expected result when option is set", {
   filename <- test_path("ref", "ex6.R")
   source(filename, local = TRUE)

   expected <- lint(filename, c(lintr::undesirable_operator_linter()))

   withr::local_options(logrx.lint = c(lintr::undesirable_operator_linter()))

   expect_identical(get_lint_results(filename), expected)
})

test_that("lint returns expected result when option is not set", {
   filename <- test_path("ref", "ex6.R")
   source(filename, local = TRUE)

   expect_identical(get_lint_results(filename), NULL)
})
