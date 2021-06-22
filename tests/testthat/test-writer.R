test_that("write_log_element will return a formatted log element", {
   options("timber.log" = NULL)
   log_config("./test-writer.R")
   expect_identical(getOption("timber.log")[['user']], Sys.info()[["user"]])
   expect_identical(write_log_element('user'), Sys.info()[["user"]])
   expect_identical(write_log_element("user", "User: "), paste0("User: ", Sys.info()[["user"]]))
})


test_that("write_log_header will return a correctly padded header", {
   expect_identical(write_log_header("Test"),
                    c(paste(rep('-', 80), collapse = ""),
                      paste0('-', paste(rep(' ', 37), collapse = ""), "Test", paste(rep(' ', 37), collapse = ""), '-'),
                      paste(rep('-', 80), collapse = "")))
   expect_identical(write_log_header("Testing"),
                    c(paste(rep('-', 80), collapse = ""),
                      paste0('-', paste(rep(' ', 35), collapse = ""), "Testing", paste(rep(' ', 36), collapse = ""), '-'),
                      paste(rep('-', 80), collapse = "")))
})

test_that("write_metadata will return a formatted log metadata element",{
   options("timber.log" = NULL)
   log_config("./test-writer.R")
   metadata <- list(info = paste0("This log was generated using timber ",
                    sessionInfo()[["otherPkgs"]][["timber"]][["Version"]]),
               version = sessionInfo()[["otherPkgs"]][["timber"]][["Version"]],
               license = sessionInfo()[["otherPkgs"]][["timber"]][["License"]],
               built = sessionInfo()[["otherPkgs"]][["timber"]][["Built"]],
               repository_link = "https://github.com/atorus-research/timber"
            )
   expect_identical(write_metadata(),
                    c(metadata$info,
                      paste0("timber package version: ", metadata$version),
                      paste0("timber license: ", metadata$license),
                      paste0("timber build: ", metadata$built),
                      paste0("timber link to repository: ", metadata$repository_link)))
})

test_that("write_masked_functions will return a formatted log masked functions element",{
   options("timber.log" = NULL)
   log_config("./test-writer.R")
   masked_functions <- list("plot" = list("source" = "package:graphics", "masks" = c("package:base")),
                            "tribble" = list("source" = "package:dplyr", "masks" = c("package:tidyr", "package:tibble")))
   assign('masked_functions', masked_functions, envir = getOption('timber.log'))

   expect_identical(write_masked_functions(),
                    c("function `plot` from {package:base} by package:graphics",
                      "function `tribble` from {package:tidyr, package:tibble} by package:dplyr"))
})

test_that("write_errors will return a formatted log errors element", {
   options("timber.log" = NULL)
   log_config("./test-writer.R")
   errors <- list("<simpleError in eval(ei, envir): object 'a' not found>")
   assign('errors', errors, envir = getOption('timber.log'))
   expect_identical(write_errors(), paste0("errors:\n\t",capture.output(errors)))
})

test_that("write_warnings will return a formatted log errors element", {
   options("timber.log" = NULL)
   log_config("./test-writer.R")
   assign('warnings', c("this is a warning"), envir = getOption('timber.log'))
   expect_identical(write_warnings(), "Warnings:\n\tthis is a warning")
})
