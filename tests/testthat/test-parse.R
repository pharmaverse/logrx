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

test_that("nest_subsections handles both matched and unmatched subsection headers", {
  matched_headers <- c("- Session info -------")
  section_with_header <- list(
    `Session Information` = c("- Session info -------", " setting value", " foo bar")
  )

  matched_result <- logrx:::nest_subsections(matched_headers, section_with_header)
  expect_named(matched_result[[1]], "Session info")
  expect_identical(
    matched_result[[1]][["Session info"]],
    c(" setting value", " foo bar")
  )

  no_headers <- c("plain text")
  section_without_header <- list(`Session Information` = c("line one", "line two"))
  unmatched_result <- logrx:::nest_subsections(no_headers, section_without_header)
  expect_length(unmatched_result[[1]], 2)
  expect_identical(unmatched_result[[1]][[1]], "line one")
  expect_identical(unmatched_result[[1]][[2]], "line two")
})
