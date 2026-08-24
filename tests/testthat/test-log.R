test_that("log_init creates an empty log environment if one is not available", {
  # ensure no existing log environment
  options("log.rx" = NULL)
  log_init()
  # log.rx should be an environment and contain no elements
  expect_type(getOption("log.rx"), "environment")
  expect_identical(ls(getOption("log.rx")), character(0))
})

test_that("log_config configures the log and all the necessary elements", {
  # ensure no existing log environment
  options("log.rx" = NULL)
  log_config("./test-get.R")
  # all expected keys should be present
  expect_setequal(
    names(getOption("log.rx")),
    c(
      "metadata", "session_info", "warnings", "errors", "messages",
      "result", "output", "start_time", "end_time", "run_time",
      "file_name", "file_path", "user", "hash_sum", "masked_functions",
      "used_packages_functions", "unapproved_packages_functions",
      "lint_results", "log_name", "log_path", "repo_urls", "extra_info"
    )
  )
  # file path, file name, and user should be set correctly
  expect_identical(getOption("log.rx")[["file_path"]], dirname(get_file_path("./test-get.R")))
  expect_identical(getOption("log.rx")[["file_name"]], basename(get_file_path("./test-get.R")))
  expect_identical(getOption("log.rx")[["user"]], Sys.info()[["user"]])
})

test_that("log_config errors with helpful message if a populated log exists (non-interactive)", {
  # 1. Setup the environment to trigger the error condition
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))
  expect_identical(getOption("log.rx")[["user"]], Sys.info()[["user"]])

  # 2. Use expect_snapshot(error = TRUE) to capture the complete error message
  #    This will call log_config(), trigger the error, and save its output to a snapshot.
  expect_snapshot(
    {
      log_config()
    },
    error = TRUE
  ) # Indicate that an error is expected and should be snapshotted

  # 3. Clean up
  log_remove()
})

test_that("handle_existing_environment removes env when user chooses option 1", {
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))

  # Mock interactive() to return TRUE and menu() to return 1
  with_mocked_bindings(
    {
      # Capture messages
      expect_message(
        result <- handle_existing_environment(),
        "A log.rx environment already exists"
      )
      expect_message(
        handle_existing_environment(),
        "Removing existing log.rx environment"
      )

      # Simulate user choosing option 1 again for snapshot test
      log_init()
      assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))

      # Snapshot test for messages when user chooses to proceed
      expect_snapshot({
        handle_existing_environment()
      })
    },
    interactive = function() TRUE,
    menu = function(...) 1
  )

  # Environment should be removed after these operations
  expect_null(getOption("log.rx"))
})

test_that("handle_existing_environment errors when user chooses option 2", {
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))

  # Mock interactive() to return TRUE and menu() to return 2
  with_mocked_bindings(
    {
      expect_error(
        handle_existing_environment(),
        "Execution cancelled"
      )
      expect_error(
        handle_existing_environment(),
        "log_remove()"
      )

      # Snapshot test for the error message when user cancels
      expect_snapshot(
        handle_existing_environment(),
        error = TRUE
      )
    },
    interactive = function() TRUE,
    menu = function(...) 2
  )

  # Clean up
  log_remove()
})

test_that("handle_existing_environment errors when user cancels menu", {
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))

  # Mock interactive() to return TRUE and menu() to return 0 (cancelled)
  with_mocked_bindings(
    {
      expect_error(
        handle_existing_environment(),
        "Execution cancelled"
      )
    },
    interactive = function() TRUE,
    menu = function(...) 0
  )
})

test_that("log_config works after environment is removed interactively", {
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))

  # Mock interactive mode with user choosing to remove environment
  with_mocked_bindings(
    {
      # This should succeed as the environment will be removed
      log_config("./test-get.R")

      # Verify the environment was re-initialized properly
      expect_setequal(
        names(getOption("log.rx")),
        c(
          "metadata", "session_info", "warnings", "errors", "messages",
          "result", "output", "start_time", "end_time", "run_time",
          "file_name", "file_path", "user", "hash_sum", "masked_functions",
          "used_packages_functions", "unapproved_packages_functions",
          "lint_results", "log_name", "log_path", "repo_urls", "extra_info"
        )
      )
    },
    interactive = function() TRUE,
    menu = function(...) 1
  )

  # Clean up
  log_remove()
})

test_that("log_write reads approved list from yaml", {
  # get packages used in the reference script to build the approved list
  fp <- test_path("ref", "ex1.R")
  used <- logrx:::get_used_functions(fp)
  pkgs <- unique(sub("^package:", "", used$library[grepl("^package:", used$library)]))

  # configure the log and run the script
  options("log.rx" = NULL)
  log_out <- tempfile(fileext = ".log")
  log_config(fp, log_path = dirname(log_out), log_name = basename(log_out))
  run_safely_loudly(fp)

  # write approved list as .yaml and point the option to it
  approved <- tempfile(fileext = ".yaml")
  build_approved(setNames(lapply(pkgs, function(x) "_all_"), pkgs), approved)
  withr::local_options(list(log.rx.approved = approved))
  log_write(fp)

  # log should report no unapproved packages or functions
  expect_true(any(grepl("No unapproved packages or functions used", readLines(log_out))))
})

test_that("log_write reads approved list from yml", {
  # get packages used in the reference script to build the approved list
  fp <- test_path("ref", "ex1.R")
  used <- logrx:::get_used_functions(fp)
  pkgs <- unique(sub("^package:", "", used$library[grepl("^package:", used$library)]))

  # configure the log and run the script
  options("log.rx" = NULL)
  log_out <- tempfile(fileext = ".log")
  log_config(fp, log_path = dirname(log_out), log_name = basename(log_out))
  run_safely_loudly(fp)

  # write approved list as .yml and point the option to it
  approved <- tempfile(fileext = ".yml")
  build_approved(setNames(lapply(pkgs, function(x) "_all_"), pkgs), approved)
  withr::local_options(list(log.rx.approved = approved))
  log_write(fp)

  # log should report no unapproved packages or functions
  expect_true(any(grepl("No unapproved packages or functions used", readLines(log_out))))
})

test_that("log_write reads approved list from rds", {
  # get packages used in the reference script to build the approved list
  fp <- test_path("ref", "ex1.R")
  used <- logrx:::get_used_functions(fp)
  pkgs <- unique(sub("^package:", "", used$library[grepl("^package:", used$library)]))

  # configure the log and run the script
  options("log.rx" = NULL)
  log_out <- tempfile(fileext = ".log")
  log_config(fp, log_path = dirname(log_out), log_name = basename(log_out))
  run_safely_loudly(fp)

  # write approved list as .rds and point the option to it
  approved <- tempfile(fileext = ".rds")
  build_approved(setNames(lapply(pkgs, function(x) "_all_"), pkgs), approved)
  withr::local_options(list(log.rx.approved = approved))
  log_write(fp)
  
  # log should report no unapproved packages or functions
  expect_true(any(grepl("No unapproved packages or functions used", readLines(log_out))))
})

test_that("log_remove removes a log if one exists", {
  # ensure a log environment exists before removal
  options("log.rx" = NULL)
  log_init()
  expect_type(getOption("log.rx"), "environment")
  # after removal the option should be NULL
  log_remove()
  expect_null(getOption("log.rx"))
})
