test_that("axecute will run a file and create the necessary log", {
   options("log.rx" = NULL)
   scriptPath_sha <- tempfile()
   logDir_sha <- tempdir()
   writeLines("print('hello logrx')", con = scriptPath_sha)

   # check no log is currently written out
   expect_warning(expect_error(
      file(file.path(logDir_sha, "log_out_sha"), "r"),
      "cannot open the connection")
      )

   axecute(scriptPath_sha,
           log_name = "log_out_sha",
           log_path = logDir_sha,
           remove_log_object = FALSE)


   con <- file(file.path(logDir_sha, "log_out_sha"), "r")
   flines <- readLines(con)
   close(con)

   # check that the output file is populated
   expect_gt(length(flines), 1)
   # check all the elements are there
   expect_true(grepl(paste(write_log_header("User and File Information"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Session Information"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Masked Functions"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Program Run Time Information"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Errors and Warnings"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Messages, Output, and Result"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Log Output File"), collapse = ','),
                     paste(flines,collapse = ',')))

   # remove all the stuff we added
   rm(flines, con, scriptPath_sha, logDir_sha)
   log_remove()

})

test_that("to_report works to control log output elements", {
   options("log.rx" = NULL)
   scriptPath_sha <- tempfile()
   logDir_sha <- tempdir()
   writeLines(
      c("message('hello logrx')",
        "cat('this is output')",
        "data.frame(c(8, 6, 7, 5, 3, 0, 9))"),
      con = scriptPath_sha)

   # check no log is currently written out
   expect_warning(expect_error(file(file.path(logDir_sha, "log_out_report_sha"), "r"), "cannot open the connection"))

   axecute(scriptPath_sha,
           log_name = "log_out_report_sha",
           log_path = logDir_sha,
           remove_log_object = FALSE,
           to_report = c("messages", "result"))
   con <- file(file.path(logDir_sha, "log_out_report_sha"), "r")
   flines <- readLines(con)
   close(con)

   expect_true(any(grepl("^Messages:", flines) == TRUE))
   expect_true(all(grepl("^Output:", flines) != TRUE))
   expect_true(any(grepl("^Result:", flines) == TRUE))

   rm(flines, con, scriptPath_sha, logDir_sha)
   log_remove()

})
