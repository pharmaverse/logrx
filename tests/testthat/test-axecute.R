test_that("axecute will run a file and create the necessary log", {
   options("log.rx" = NULL)
   scriptPath <- tempfile()
   logDir <- tempdir()
   writeLines("print('hello logrx')", con = scriptPath)

   # check no log is currently written out
   expect_warning(expect_error(file(file.path(logDir, "log_out"), "r"), "cannot open the connection"))

   axecute(scriptPath, log_name = "log_out", log_path = logDir, remove_log_object = FALSE)
   con <- file(file.path(logDir, "log_out"), "r")
   flines <- readLines(con)
   close(con)

   # check that the output file is populated
   expect_gt(length(flines), 1)
   # check all the elements are there
   expect_true(grepl(paste(write_log_header("logrx Metadata"), collapse = ','),
                     paste(flines,collapse = ',')))
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
   expect_true(grepl(paste(write_log_header("Messages and Result"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Log Stream"), collapse = ','),
                     paste(flines,collapse = ',')))
   expect_true(grepl(paste(write_log_header("Log Output File"), collapse = ','),
                     paste(flines,collapse = ',')))

   # remove all the stuff we added
   rm(flines, con, scriptPath, logDir)

})

test_that("to_report works to control log output elements", {
   options("log.rx" = NULL)
   scriptPath <- tempfile()
   logDir <- tempdir()
   writeLines(
      c("message('hello logrx')",
        "cat('this is output')",
        "data.frame(c(8, 6, 7, 5, 3, 0, 9))"),
      con = scriptPath)

   # check no log is currently written out
   expect_warning(expect_error(file(file.path(logDir, "log_out_report"), "r"), "cannot open the connection"))

   axecute(scriptPath,
           log_name = "log_out_report",
           log_path = logDir,
           remove_log_object = FALSE,
           to_report = c("messages", "result"))
   con <- file(file.path(logDir, "log_out_report"), "r")
   flines <- readLines(con)
   close(con)

   expect_true(any(grepl("^Messages:", flines) == TRUE))
   expect_true(all(grepl("^Log Stream:", flines) != TRUE))
   expect_true(any(grepl("^Result:", flines) == TRUE))


})
