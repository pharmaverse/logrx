test_that("File HashSum does not change for ex1.R", {
   log_remove()
   scriptPath <- test_path("ref", "ex1.R")
   logDir <- test_path("ref")
   log_config(scriptPath)

   expect_identical(getOption("log.rx")[['hash_sum']], "2a6ebfb47a42e9d0a889b2710458c91b8d33a4f3")

   # remove all the stuff we added
   rm(scriptPath, logDir)
   log_remove()

})

