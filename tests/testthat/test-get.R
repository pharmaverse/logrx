test_that("metadata elements are specified correctly and loaded into a list", {
  logrx_session_info <- session_info()$packages %>%
    filter(.data$package == "logrx")

  expect_identical(
    get_logrx_metadata(),
    list(
      info = paste0(
        "This log was generated using logrx ",
        logrx_session_info[["loadedversion"]]
      ),
      version = logrx_session_info[["loadedversion"]],
      built = logrx_session_info[["source"]],
      repository_link = "https://github.com/pharmaverse/logrx"
    )
  )
})

test_that("when given a file as an argument a normalized file path to that file will be returned by default", {
  expect_equal(get_file_path(file = "./test-get.R"), normalizePath("test-get.R"))
})

test_that("when given a file as an argument a non-normalized file path to that file will be returned when specified to not normalize", {
  expect_equal(get_file_path(file = "./test-get.R", normalize = FALSE), "./test-get.R")
})

test_that("session info is captured", {
  expect_identical(capture.output(get_session_info()), capture.output(session_info(info = "all")))
})

test_that("all functions that are masked are found and returned", {
  expect_identical(names(get_masked_functions()), unique(unlist(conflicts(detail = TRUE))))
})

test_that("each masked function element contain source and masks elements", {
  expect_identical(unlist(unname(map(get_masked_functions(), ~ names(.x)))), rep(c("source", "masks"), length(get_masked_functions())))
})

test_that("ex1.R parses correctly", {
  filename <- test_path("ref", "ex1.R")
  source(filename, local = TRUE)

  expected <- tibble::tribble(
    ~function_name, ~library,
    "library", "package:base",
    "%>%", "package:dplyr",
    "group_by", "package:dplyr",
    "summarize", "package:dplyr",
    "mean", "package:base",
    "pivot_wider", "package:tidyr"
  )

  expect_identical(get_used_functions(filename), expected)
})

test_that("get used functions returns NULL when no functions are used", {
  r_path <- tempfile(fileext = ".R")
  withr::defer(unlink(r_path))

  writeLines(
    text = "1",
    con = r_path
  )

  expect_identical(get_used_functions(r_path), NULL)
})


test_that("::: parses correctly", {
  filename <- test_path("ref", "ex8.R")
  source(filename, local = TRUE)

  expected <- tibble::tribble(
    ~function_name, ~library,
    "library", "package:base",
    "%>%", "package:dplyr",
    "group_by", "package:dplyr",
    "summarize", "package:dplyr",
    "mean", "package:base",
    "pivot_wider", "package:tidyr",
    "commas", "package:dplyr",
    "c", "package:base"
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
    ~function_name, ~library,
    "glimpse", "package:dplyr"
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
    ~function_name, ~library,
    "print", "package:base"
  )

  expect_identical(get_used_functions(filename), expected)
})

test_that("get_library returns correct function when a non-function
          object of same name is available", {
  writeLines('search <- "dummy object"', "dummy.R")

  sys.source("dummy.R", envir = attach(NULL, name = "dummy"))

  actual <- get_library(tibble(function_name = "search", SYMBOL_PACKAGE = NA))

  unlink("dummy.R")
  detach("dummy")

  expected <- tibble::tribble(
    ~function_name, ~SYMBOL_PACKAGE, ~library,
    "search", NA, "package:base"
  )

  expect_identical(actual, expected)
})

test_that("parse does not fatal error when syntax issue occurs", {
  filename <- test_path("ref", "ex4.R")

  expected <- tibble::tibble(
    function_name = "",
    library = "Syntax Error Found, Package and Function Identification Stopped"
  )

  expect_identical(get_used_functions(filename), expected)
})

test_that("lint returns expected result when using the default log.rx.lint option", {
  skip_if_not_installed("lintr")

  options("log.rx" = NULL)
  filename <- test_path("ref", "ex7.R")

  # get is called within log_config
  log_config(filename)

  expect_identical(get_lint_results(filename), NULL)
})

test_that("get_lint_results does not return a message if the option set to FALSE", {
  options(log.rx.lint = FALSE)
  filename <- test_path("ref", "ex6.R")
  expect_no_message(get_lint_results(filename))
})

test_that("lint returns expected result when option is changed", {
  skip_if_not_installed("lintr")

  filename <- test_path("ref", "ex6.R")
  source(filename, local = TRUE)

  expected <- lintr::lint(filename, c(lintr::undesirable_operator_linter()))

  withr::local_options(log.rx.lint = c(lintr::undesirable_operator_linter()))

  expect_identical(get_lint_results(filename), expected)
})

test_that("library lint returns expected result when multiple linters are set", {
  skip_if_not_installed("lintr", "3.2.0")
  skip_if_not_installed("xml2")

  options("log.rx" = NULL)
  withr::local_options(log.rx.lint = c(lintr::library_call_linter(), lintr::undesirable_operator_linter()))
  filename <- test_path("ref", "ex7.R")

  # get is called within log_config
  log_config(filename)

  expected <- paste(
    "Line 6 [library_call_linter] Move all library calls to the top of the script.",
    "",
    "Line 8 [undesirable_operator_linter] Avoid undesirable operator `<<-`. It",
    "assigns outside the current environment in a way that can be hard to reason",
    "about. Prefer fully-encapsulated functions wherever possible, or, if",
    "necessary, assign to a specific environment with assign(). Recall that you",
    "can create an environment at the desired scope with new.env().",
    sep = "\n"
  )

  expect_identical(write_lint_results(), expected)
})

test_that("lint returns expected result when option is set to FALSE", {
  filename <- test_path("ref", "ex6.R")
  withr::local_options(log.rx.lint = FALSE)
  source(filename, local = TRUE)

  expect_identical(get_lint_results(filename), NULL)
})

test_that("functions used are returned correctly for rmd files", {
  filename <- test_path("ref", "ex1.Rmd")

  tmpfile <- tempfile(fileext = ".R")

  withr::local_options(list(knitr.purl.inline = TRUE))

  knitr::purl(filename, tmpfile)

  source(tmpfile, local = TRUE)

  expected <- tibble::tribble(
    ~function_name, ~library,
    "library", "package:base",
    "summary", "package:base",
    "plot", "package:graphics",
    "%>%", "package:dplyr",
    "filter", "package:dplyr",
    "print", "package:base"
  )

  expect_identical(get_used_functions(tmpfile), expected)
})

test_that("get_repo_urls returns correct list of repos", {
  original_repos <- options("repos")
  test_repos <- c(
    binary = "https://packagemanager.rstudio.com/all/__linux__/focal/latest",
    source = "https://packagemanager.rstudio.com/all/latest",
    CRAN = "https://cloud.r-project.org"
  )
  options("repos" = test_repos)
  expect_identical(get_repo_urls(), as.list(options("repos")$repos))
  options("repos" = original_repos)
})
