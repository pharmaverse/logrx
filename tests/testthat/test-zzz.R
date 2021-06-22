test_that("log_init is run on package load", {
   detach("package:timber", unload=TRUE)
   options("timber.log" = NULL)
   library(timber)
   expect_type(getOption("timber.log"), 'environment')
})
