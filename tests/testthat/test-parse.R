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
  expect_warning(parsedFile <- read_log_file(filePath), NA)

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

test_that("package parsing excludes session information footnotes", {
  skip_if_not_installed("readr")
  package_lines <- c(
    "! package * version date (UTC) lib source",
    "P dplyr * 1.2.1 2026-04-03 [?] RSPM",
    "  readr 2.2.0 2026-02-19 [1] RSPM",
    "R logrx * 0.4.0 <NA> [?] <NA>",
    "VP example 1.0 2026-01-01 [1] CRAN (R 4.3.2)",
    "",
    "[1] /path/to/library",
    "[2] /another/library",
    "",
    "* -- Packages attached to the search path.",
    "P -- Loaded and on-disk path mismatch."
  )
  nested_log <- list(`Session Information` = list(
    `Session info` = c("setting value", "os Linux"),
    Packages = package_lines,
    `External software` = c("setting value", "zlib 1.3")
  ))

  expect_warning(parsed <- logrx:::parse_log(nested_log), NA)
  packages <- parsed$`Session Information`$Packages
  expect_named(packages, c("package", "version", "date", "lib", "source"))
  expect_equal(packages$package, c("dplyr", "readr", "logrx", "example"))
  expect_equal(packages$version, c("1.2.1", "2.2.0", "0.4.0", "1.0"))
  expect_equal(packages$lib, c("[?]", "[1]", "[?]", "[1]"))
  expect_equal(packages$source, c("RSPM", "RSPM", "<NA>", "CRAN (R 4.3.2)"))

  nested_log$`Session Information`$Packages <- c(
    "package * version date (UTC) lib source",
    "dplyr * 1.1.4 2023-11-17 [1] CRAN (R 4.3.2)"
  )
  expect_warning(parsed <- logrx:::parse_log(nested_log), NA)
  packages <- parsed$`Session Information`$Packages
  expect_named(packages, c("package", "version", "date", "lib", "source", "lang", "r_version"))
  expect_equal(packages$source, "CRAN")
  expect_equal(packages$lang, "R")
  expect_equal(packages$r_version, "4.3.2")

  nested_log$`Session Information`$Packages <- c(
    "package * version date (UTC) lib source",
    "malformed package row"
  )
  expect_error(logrx:::parse_log(nested_log), "Malformed package row")
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
