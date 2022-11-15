test_that("Test 1: File HashSum does not change for ex1.R", {
   scriptPath <- test_path("ref", "ex1.R")
   logDir <- tempdir()
   axecute(scriptPath, log_path = logDir)

   con <- file(file.path(logDir, "ex1.log"), "r")
   flines <- readLines(con)
   close(con)

   expect_true(any(grepl("File HashSum: e3b7438926d8fc6e55d927db377c9670a057e0a8", flines) == TRUE))

   # remove all the stuff we added
   rm(scriptPath, logDir)
   log_remove()

})

test_that("Test 2: File HashSum generated for temp file", {
   options("log.rx" = NULL)
   scriptPath <- tempfile()
   logDir <- tempdir()
   writeLines(
      c("message('hello logrx')",
        "cat('this is output')",
        "data.frame(c(8, 6, 7, 5, 3, 0, 9))"),
      con = scriptPath)

   axecute(scriptPath,
           log_name = "log_out_report",
           log_path = logDir,
           remove_log_object = FALSE,
           to_report = c("messages", "result"))
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
      c("message('hello again logrx')",
        "cat('this is output')",
        "data.frame(c(8, 6, 7, 5, 3, 0, 9))"),
      con = scriptPath)

   axecute(scriptPath,
           log_name = "log_out_report",
           log_path = logDir,
           remove_log_object = FALSE,
           to_report = c("messages", "result"))
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
      c("message('hello logrx')",
        "cat('this is output')",
        "data.frame(c(8, 6, 7, 5, 3, 0, 9))"),
      con = scriptPath)

   axecute(scriptPath,
           log_name = "log_out_report",
           log_path = logDir,
           remove_log_object = FALSE,
           to_report = c("messages", "result"))
   con <- file(file.path(logDir, "log_out_report"), "r")
   flines <- readLines(con)
   close(con)

   expect_true(any(grepl("File HashSum: 1a850097971365b8846fd7a935cc792896e97fbe", flines) == TRUE))
   # remove all the stuff we added
   rm(scriptPath, logDir)
   log_remove()
})
