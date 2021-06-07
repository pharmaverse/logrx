test_that("interact works", {
   scriptPath <- tempfile()
   logDir <- tempdir()

   writeLines("print('hello timber')", con = scriptPath)

   log_config(file = scriptPath)

   # Adding a value to the timber object works
   testthat::expect_identical(getOption("timber.log")[["user"]],
                              Sys.info()[["user"]])

   # Adding a list to the timber object works
   session_info <- sessionInfo()

   timber_metadata <- list(
      info = paste0("This log was generated using timber ",
                    session_info[["otherPkgs"]][["timber"]][["Version"]]),
      version = session_info[["otherPkgs"]][["timber"]][["Version"]],
      license = session_info[["otherPkgs"]][["timber"]][["License"]],
      built = session_info[["otherPkgs"]][["timber"]][["Built"]],
      repository_link = NULL
   )

   testthat::expect_identical(getOption("timber.log")[["metadata"]],
                              timber_metadata)
})
