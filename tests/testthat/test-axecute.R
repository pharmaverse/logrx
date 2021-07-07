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

   expect_gt(length(flines), 1)

   rm(flines, con, scriptPath, logDir)

})
