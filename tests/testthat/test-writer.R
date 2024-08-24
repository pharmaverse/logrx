test_that("write_log_element will return a formatted log element", {
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   expect_identical(getOption("log.rx")[['user']], Sys.info()[["user"]])
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
   options("log.rx" = NULL)
   log_config("./test-writer.R")

   logrx_session_info <- session_info()$packages %>%
      filter(.data$package == "logrx")

   metadata <- list(info = paste0("This log was generated using logrx ",
                                  logrx_session_info[['loadedversion']]),
                    version = logrx_session_info[['loadedversion']],
                    built = logrx_session_info[['source']],
                    repository_link = "https://github.com/pharmaverse/logrx"
            )
   expect_identical(write_metadata(),
                    c(metadata$info,
                      paste0("logrx package version: ", metadata$version),
                      paste0("logrx build: ", metadata$built),
                      paste0("logrx link to repository: ", metadata$repository_link)))
})

test_that("write_masked_functions will return a formatted log masked functions element",{
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   masked_functions <- list("plot" = list("source" = "package:graphics", "masks" = c("package:base")),
                            "tribble" = list("source" = "package:dplyr", "masks" = c("package:tidyr", "package:tibble")))
   assign('masked_functions', masked_functions, envir = getOption('log.rx'))

   expect_identical(write_masked_functions(),
                    c("function `plot` from {package:base} by package:graphics",
                      "function `tribble` from {package:tidyr, package:tibble} by package:dplyr"))
})

test_that("write_file_name_path will return a formatted log file name and path element", {
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   assign('file_name', "safely_loudly_test_file_result.R", envir = getOption('log.rx'))
   assign('file_path', "tests/testthat/ref", envir = getOption('log.rx'))
   expect_identical(write_file_name_path(),
                    c("File Name: safely_loudly_test_file_result.R",
                      "File Path: tests/testthat/ref"))
})

test_that("write_file_name_path will return an informative message if no file_name and file_path exists", {
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   assign('file_name', NA, envir = getOption('log.rx'))
   assign('file_path', NA, envir = getOption('log.rx'))
   expect_identical(write_file_name_path(),
                    c("File Name: File name not able to be determined",
                      "File Path: File path not able to be determined"))
})

test_that("write_used_functions will return a formatted log of used pacakges and functions element",{
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   used_functions <- tibble::tribble(
      ~function_name,        ~library,
      "library",  "package:base",
      "%>%", "package:dplyr",
      "group_by", "package:dplyr",
      "summarize", "package:dplyr",
      "mean",  "package:base",
      "print",  "package:base",
      "pivot_wider", "package:tidyr"
   )
   assign('used_packages_functions', used_functions, envir = getOption('log.rx'))

   expect_identical(write_used_functions(),
                    c("{package:base} library, mean, print",
                      "{package:dplyr} %>%, group_by, summarize",
                      "{package:tidyr} pivot_wider")
   )
})

test_that("write_unapproved_functions will return a formatted log of unapproved pacakges and functions element",{
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   unapproved_functions <- tibble::tribble(
      ~function_name,        ~library,
      "library",  "package:base",
      "%>%", "package:dplyr",
      "group_by", "package:dplyr",
      "summarize", "package:dplyr",
      "mean",  "package:base",
      "print",  "package:base",
      "pivot_wider", "package:tidyr"
   )
   assign('unapproved_packages_functions', unapproved_functions, envir = getOption('log.rx'))

   expect_identical(write_unapproved_functions(),
                    c("{package:base} library, mean, print",
                      "{package:dplyr} %>%, group_by, summarize",
                      "{package:tidyr} pivot_wider")
   )
})

test_that("write_unapproved_functions will return expect results when all packages and functions are approved",{
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   unapproved_functions <- tibble::tibble(
      function_name = vector(mode = "character"),
      library = vector(mode = "character")
   )
   assign('unapproved_packages_functions', unapproved_functions, envir = getOption('log.rx'))

   expect_identical(write_unapproved_functions(),
                    "No unapproved packages or functions used"
   )
})


test_that("write_errors will return a formatted log errors element", {
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   errors <- list(message = "object 'a' not found")
   assign('errors', errors, envir = getOption('log.rx'))
   expect_identical(write_errors(), "Errors:\n\tobject 'a' not found")
})

test_that("write_warnings will return a formatted log warnings element", {
   options("log.rx" = NULL)
   log_config("./test-writer.R")
   assign('warnings', c("this is a warning"), envir = getOption('log.rx'))
   expect_identical(write_warnings(), "\nWarnings:\n\tthis is a warning")
   log_remove()
})

test_that("write_output will return a formatted log output element", {
   fp <- test_path("ref", "safely_loudly_test_file.R")

   log_config(file = fp)

   run_safely_loudly(fp)

   expect_identical(write_output(), "Output:\n\t[1] \"log print\"\n\t[1] \"log print 2\"\n\tlog catlog cat 2")

   log_remove()
})

test_that("write_messages will return a formatted log messages element", {
   fp <- test_path("ref", "safely_loudly_test_file.R")

   log_config(file = fp)

   run_safely_loudly(fp)

   expect_identical(write_messages(), "Messages:\n\tlog inform\n\tlog inform 2\n\tlog message\n\tlog message 2")

   log_remove()
})

test_that("write_result will return a formatted log result element", {
   fp <- test_path("ref", "safely_loudly_test_file_result.R")

   log_config(file = fp)

   run_safely_loudly(fp)

   expect_identical(write_result(fp),
                    c("\nResult:", paste0("\t", capture.output(data.frame(test = c(8, 6, 7, 5, 3, 0, 9))))))

   log_remove()
})

test_that("write_lint_results will return a formatted lint results element", {
   skip_if_not_installed("lintr")

   filename <- test_path("ref", "ex6.R")
   source(filename, local = TRUE)

   options("log.rx" = NULL)
   log_config("./test-writer.R")
   lint_results <- lintr::lint(filename, c(lintr::undesirable_operator_linter()))
   assign('lint_results', lint_results, envir = getOption('log.rx'))

   expect_identical(
      write_lint_results(),
      paste0(
         "Line 3 [undesirable_operator_linter] Operator `<<-` is undesirable.",
         " It\nassigns outside the current environment in a way that can be hard",
         " to reason\nabout. Prefer fully-encapsulated functions wherever possible,",
         " or, if\nnecessary, assign to a specific environment with assign(). ",
         "Recall that you\ncan create an environment at the desired scope with",
         " new.env().\n\nLine 4 [undesirable_operator_linter] Operator `<<-` is",
         " undesirable. It\nassigns outside the current environment in a way that",
         " can be hard to reason\nabout. Prefer fully-encapsulated functions",
         " wherever possible, or, if\nnecessary, assign to a specific environment",
         " with assign(). Recall that you\ncan create an environment at the",
         " desired scope with new.env()."
            )
      )

   log_remove()
})

test_that("write_lint_results works when linter is used but no lints found", {
   skip_if_not_installed("lintr")
   skip_if_not_installed("xml2")

   filename <- test_path("ref", "ex6.R")
   source(filename, local = TRUE)

   options("log.rx" = NULL)
   log_config(filename)
   lint_results <- lintr::lint(filename, c(library_call_linter()))
   assign('lint_results', lint_results, envir = getOption('log.rx'))

   expect_identical(
      write_lint_results(),
      ""
   )
})

