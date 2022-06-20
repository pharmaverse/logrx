test_that("log_init creates an empty log environment if one is not available", {
   options("log.rx" = NULL)
   log_init()
   expect_type(getOption("log.rx"), 'environment')
   expect_identical(ls(getOption("log.rx")), character(0))
})

test_that("log_config configures the log and all the necessary elements", {
   options("log.rx" = NULL)
   log_config('./test-get.R')
   expect_setequal(names(getOption("log.rx")),
                   c("metadata","session_info","warnings","errors","messages",
                     "result","stream","start_time", "end_time", "run_time",
                     "file_name","file_path","user", "masked_functions",
                     "used_packages_functions", "unapproved_packages_functions",
                     "log_name","log_path"))

   expect_identical(getOption("log.rx")[['file_path']], dirname(get_file_path('./test-get.R')))
   expect_identical(getOption("log.rx")[['file_name']], basename(get_file_path('./test-get.R')))
   expect_identical(getOption("log.rx")[['user']], Sys.info()[["user"]])

})

test_that("log_config errors if a populated log extists", {
   options("log.rx" = NULL)
   log_init()
   assign('user', Sys.info()[["user"]], envir = getOption('log.rx'))
   expect_identical(getOption("log.rx")[['user']], Sys.info()[["user"]])
   expect_error(log_config(),"a log.rx environment already exists")

})

test_that("log_remove removes a log if one exists", {
   options("log.rx" = NULL)
   log_init()
   expect_type(getOption("log.rx"), 'environment')

   log_remove()
   expect_null(getOption("log.rx"))
})
