test_that("set_log_element works", {
   # scriptPath <- tempfile()
   # logDir <- tempdir()
   #
   # writeLines("print('hello timber')", con = scriptPath)
   #
   # log_config(file = scriptPath)
   #
   # # Adding a value to the timber object works
   # testthat::expect_identical(getOption("timber.log")[["user"]],
   #                            Sys.info()[["user"]])
   #
   # # Adding a list to the timber object works
   # session_info <- sessionInfo()
   #
   # timber_metadata <- list(
   #    info = paste0("This log was generated using timber ",
   #                  session_info[["otherPkgs"]][["timber"]][["Version"]]),
   #    version = session_info[["otherPkgs"]][["timber"]][["Version"]],
   #    license = session_info[["otherPkgs"]][["timber"]][["License"]],
   #    built = session_info[["otherPkgs"]][["timber"]][["Built"]],
   #    repository_link = NULL
   # )
   #
   # testthat::expect_identical(getOption("timber.log")[["metadata"]],
   #                            timber_metadata)

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

   testthat::expect_identical(getOption("timber.log")[["log_name"]],
                              log_name)
   testthat::expect_warning(set_log_name_path(log_name = "test_log_name2"))
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
})
