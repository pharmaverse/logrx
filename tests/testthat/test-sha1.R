test_that("Test 1: File HashSum does not change for ex1.R", {
  scriptPath <- test_path("ref", "ex1.R")
  logDir <- tempdir()
  axecute(scriptPath, log_path = logDir)

  con <- file(file.path(logDir, "ex1.log"), "r")
  flines <- readLines(con)
  close(con)

  expect_true(any(grepl("File HashSum: 69bba3ee00038e547f0a95b6b62d16f9a56a946b", flines) == TRUE))

  # remove all the stuff we added
  rm(scriptPath, logDir)
  log_remove()
})

test_that("Test 2: File HashSum generated for temp file", {
  options("log.rx" = NULL)
  scriptPath <- tempfile()
  logDir <- tempdir()
  writeLines(
    c(
      "message('hello logrx')",
      "cat('this is output')",
      "data.frame(c(8, 6, 7, 5, 3, 0, 9))"
    ),
    con = scriptPath
  )

  log_config(scriptPath, log_name = "log_out_report", log_path = logDir)
  logrx:::run_safely_loudly(scriptPath)
  log_write(scriptPath, remove_log_object = FALSE, to_report = c("messages", "result"))

  con <- file(file.path(logDir, "log_out_report"), "r")
  flines <- readLines(con)
  close(con)

  expect_true(any(grepl("File HashSum: 1a850097971365b8846fd7a935cc792896e97fbe", flines) == TRUE))
  # remove all the stuff we added
  rm(scriptPath, logDir)
  log_remove()
})

test_that("Test 3: Different File HashSum generated for similar temp file with slight change", {
  options("log.rx" = NULL)
  scriptPath <- tempfile()
  logDir <- tempdir()
  writeLines(
    c(
      "message('hello again logrx')",
      "cat('this is output')",
      "data.frame(c(8, 6, 7, 5, 3, 0, 9))"
    ),
    con = scriptPath
  )

  log_config(scriptPath, log_name = "log_out_report", log_path = logDir)
  logrx:::run_safely_loudly(scriptPath)
  log_write(scriptPath, remove_log_object = FALSE, to_report = c("messages", "result"))


  con <- file(file.path(logDir, "log_out_report"), "r")
  flines <- readLines(con)
  close(con)

  expect_true(any(grepl("File HashSum: 1a850097971365b8846fd7a935cc792896e97fbe", flines) != TRUE))
  # remove all the stuff we added
  rm(scriptPath, logDir)
  log_remove()
})

test_that("Test 4: Same File HashSum generated for temp file in Test 2", {
  options("log.rx" = NULL)
  scriptPath <- tempfile()
  logDir <- tempdir()
  writeLines(
    c(
      "message('hello logrx')",
      "cat('this is output')",
      "data.frame(c(8, 6, 7, 5, 3, 0, 9))"
    ),
    con = scriptPath
  )

  log_config(scriptPath, log_name = "log_out_report", log_path = logDir)
  logrx:::run_safely_loudly(scriptPath)
  log_write(scriptPath, remove_log_object = FALSE, to_report = c("messages", "result"))

  con <- file(file.path(logDir, "log_out_report"), "r")
  flines <- readLines(con)
  close(con)

  expect_true(any(grepl("File HashSum: 1a850097971365b8846fd7a935cc792896e97fbe", flines) == TRUE))
  # remove all the stuff we added
  rm(scriptPath, logDir)
  log_remove()
})
