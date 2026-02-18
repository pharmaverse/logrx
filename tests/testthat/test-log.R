test_that("log_init creates an empty log environment if one is not available", {
  options("log.rx" = NULL)
  log_init()
  expect_type(getOption("log.rx"), "environment")
  expect_identical(ls(getOption("log.rx")), character(0))
})

test_that("log_config configures the log and all the necessary elements", {
  options("log.rx" = NULL)
  log_config("./test-get.R")
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

  expect_identical(getOption("log.rx")[["file_path"]], dirname(get_file_path("./test-get.R")))
  expect_identical(getOption("log.rx")[["file_name"]], basename(get_file_path("./test-get.R")))
  expect_identical(getOption("log.rx")[["user"]], Sys.info()[["user"]])
})

test_that("log_config errors with helpful message if a populated log exists (non-interactive)", {
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))
  expect_identical(getOption("log.rx")[["user"]], Sys.info()[["user"]])
  
  # Test that error contains helpful information
  expect_error(
    log_config(),
    "A log.rx environment already exists"
  )
  expect_error(
    log_config(),
    "log_remove()"
  )
  expect_error(
    log_config(),
    "restart your R session"
  )
})

test_that("handle_existing_environment removes env when user chooses option 1", {
  options("log.rx" = NULL)
  log_init()
  assign("user", Sys.info()[["user"]], envir = getOption("log.rx"))
  
  # Mock interactive() to return TRUE and menu() to return 1
  with_mocked_bindings(
    {
      # Simulate user choosing option 1 (remove and proceed)
      result <- handle_existing_environment()
      expect_true(result)
      # Environment should be removed
      expect_null(getOption("log.rx"))
    },
    interactive = function() TRUE,
    menu = function(...) 1
  )
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
    },
    interactive = function() TRUE,
    menu = function(...) 2
  )
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

test_that("log_remove removes a log if one exists", {
  options("log.rx" = NULL)
  log_init()
  expect_type(getOption("log.rx"), "environment")

  log_remove()
  expect_null(getOption("log.rx"))
})
