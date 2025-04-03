test_that("read_log_file will parse a logrx log file and create the necessary object", {
  skip_if_not_installed("readr")
  options("log.rx" = NULL)
  scriptPath <- tempfile()
  logDir <- tempdir()
  writeLines("print('hello logrx')", con = scriptPath)

  # check no log is currently written out
  filePath <- file.path(logDir, "log_out_parse")
  expect_warning(expect_error(file(filePath, "r"), "cannot open the connection"))

  axecute(scriptPath, log_name = "log_out_parse", log_path = logDir)

  # check that the log file can be parsed
  parsedFile <- read_log_file(filePath)

  expect_length(parsedFile, 9)
  expect_named(
    parsedFile,
    c(
      "logrx Metadata",
      "User and File Information",
      "Session Information",
      "Masked Functions",
      "Used Package and Functions",
      "Program Run Time Information",
      "Errors and Warnings",
      "Messages, Output, and Result",
      "Log Output File"
    )
  )
  expect_true(all(sapply(
    parsedFile[!names(parsedFile) %in%
      c(
        "Session Information",
        "Messages, Output, and Result",
        "Errors and Warnings"
      )],
    is.data.frame
  )))

  expect_true(
    all(sapply(
      parsedFile[!names(parsedFile) %in%
        c(
          "Session Information",
          "Messages, Output, and Result",
          "Errors and Warnings"
        )],
      nrow
    ) > 0)
  )

  # remove all the stuff we added
  rm(scriptPath, logDir, parsedFile)
  log_remove()
})
