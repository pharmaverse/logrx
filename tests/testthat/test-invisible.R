test_that("invisibly returned results are not written to the log", {
  options("log.rx" = NULL)
  scriptPath <- test_path("ref", "invisible_returned_obj.R")
  logDir <- tempdir()

  axecute(scriptPath, log_name = "log_invisible", log_path = logDir)

  con <- file(file.path(logDir, "log_invisible"), "r")
  flines <- readLines(con)
  close(con)

  expect_false(any(grepl("do not print this", flines)))

  rm(flines, con, logDir)
  log_remove()
})

test_that("visibly returned results are written to the log", {
  options("log.rx" = NULL)
  scriptPath <- test_path("ref", "safely_loudly_test_file_result.R")
  logDir <- tempdir()

  axecute(scriptPath, log_name = "log_visible", log_path = logDir)

  con <- file(file.path(logDir, "log_visible"), "r")
  flines <- readLines(con)
  close(con)

  expect_true(any(grepl("8, 6, 7, 5, 3, 0, 9|test", flines)))

  rm(flines, con, logDir)
  log_remove()
})
