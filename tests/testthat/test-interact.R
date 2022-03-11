test_that("set_log_element works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "value",
      "list")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('timber.log'))
   }

   v <- 1
   set_log_element("value", v)

   l <- list(a = 1, b = 2, c = 3)
   set_log_element("list", l)

   testthat::expect_identical(getOption("timber.log")[["value"]],
                              v)

   testthat::expect_identical(getOption("timber.log")[["list"]],
                              l)

   log_remove()
})

test_that("get_log_element works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "value",
      "list")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('timber.log'))
   }

   v <- 1
   set_log_element("value", v)

   l <- list(a = 1, b = 2, c = 3)
   set_log_element("list", l)

   testthat::expect_identical(get_log_element("value"),
                              v)

   testthat::expect_identical(get_log_element("list"),
                              l)

   log_remove()
})

test_that("setting log name works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "file_name",
      "file_path",
      "log_name",
      "log_path")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('timber.log'))
   }

   log_name <- "test_log_name"

   set_log_name_path(log_name = log_name)

   testthat::expect_identical(getOption("timber.log")[["log_name"]], log_name)
   testthat::expect_warning(set_log_name_path(log_name = "test_log_name2"))

   log_remove()
})

test_that("setting path name works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "file_name",
      "file_path",
      "log_name",
      "log_path")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('timber.log'))
   }

   log_path <- "test_log_path"

   set_log_name_path(log_path = log_path)

   testthat::expect_identical(getOption("timber.log")[["log_path"]],
                              log_path)
   testthat::expect_warning(set_log_name_path(log_path = "."))

   log_remove()
})

test_that("run_safely_quietly works for warnings, errors, messages, and output", {
   fp <- testthat::test_path("ref", "safely_quietly_test_file.R")

   log_config(file = fp)

   run_safely_quietly(fp)

   testthat::expect_true(!is.null(getOption("timber.log")[["warnings"]]))
   testthat::expect_true(!is.null(getOption("timber.log")[["errors"]]))
   testthat::expect_true(!is.null(getOption("timber.log")[["messages"]]))
   testthat::expect_true(!is.null(getOption("timber.log")[["output"]]))

   log_remove()
})

test_that("run_safely_quietly works for result", {
   fp <- testthat::test_path("ref", "safely_quietly_test_file_result.R")

   log_config(file = fp)

   run_safely_quietly(fp)

   testthat::expect_true(!is.null(getOption("timber.log")[["result"]]))

   log_remove()
})
