test_that("log_init creates an empty log environment if one is not available", {
   options("timber.log" = NULL)
   log_init()
   expect_type(getOption("timber.log"), 'environment')
   expect_identical(ls(getOption("timber.log")), character(0))
})

test_that("log_config configures the log and all the necessary elements", {
   options("timber.log" = NULL)
   log_config('./test-get.R')
   expect_setequal(names(getOption("timber.log")),
                   c("metadata","session_info","warnings","errors","start_time",
                     "end_time","run_time","file_name","file_path","user",
                     "masked_functions","used_packages","log_name","log_path"))

   expect_identical(getOption("timber.log")[['metadata']], get_timber_metadata())
   expect_identical(getOption("timber.log")[['session_info']], get_session_info())
   expect_identical(getOption("timber.log")[['masked_functions']], get_masked_functions())
   expect_identical(getOption("timber.log")[['file_path']], dirname(get_file_path('./test-get.R')))
   expect_identical(getOption("timber.log")[['file_name']], basename(get_file_path('./test-get.R')))
   expect_identical(getOption("timber.log")[['user']], Sys.info()[["user"]])

})

test_that("log_config errors if a populated log extists", {
   options("timber.log" = NULL)
   log_init()
   assign('user', Sys.info()[["user"]], envir = getOption('timber.log'))
   expect_identical(getOption("timber.log")[['user']], Sys.info()[["user"]])
   expect_error(log_config(),"a timber.log environment already exists")

})

test_that("log_remove removes a log if one exists", {
   options("timber.log" = NULL)
   log_init()
   expect_type(getOption("timber.log"), 'environment')

   log_remove()
   expect_null(getOption("timber.log"))
})
