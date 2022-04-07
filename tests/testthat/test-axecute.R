test_that("axecute will run a file and create the necessary log", {
   options("timber.log" = NULL)
   scriptPath <- tempfile()
   logDir <- tempdir()
   writeLines("print('hello timber')", con = scriptPath)

   # check no log is currently written out
   expect_warning(expect_error(file(file.path(logDir, "log_out"), "r"), "cannot open the connection"))

   axecute(scriptPath, log_name = "log_out", log_path = logDir, remove_log_object = FALSE)
   con <- file(file.path(logDir, "log_out"), "r")
   flines <- readLines(con)
   close(con)

   # check that the output file is populated
   expect_gt(length(flines), 1)
   # check all the elements are there
   expect_true(grepl(paste(write_log_header("timber Metadata"), collapse = ','),
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
   expect_true(grepl(paste(write_log_header("Log Output File"), collapse = ','),
                     paste(flines,collapse = ',')))

   # remove all the stuff we added
   rm(flines, con, scriptPath, logDir)

})

test_that("axecute returns 1 when there are errors", {
   options("timber.log" = NULL)
   scriptPath <- tempfile()
   logDir <- tempdir()
   writeLines("stop('hello timber')", con = scriptPath)

   rc <- axecute(scriptPath, log_name = "log_out", log_path = logDir, remove_log_object = FALSE)

   expect_equal(rc , 1)

   # remove all the stuff we added
   rm(rc, scriptPath, logDir)

})
