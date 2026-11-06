test_that("set_log_element works", {
  log_init()

  # list of attributes to add to the log
  keys <- list(
    "value",
    "list"
  )

  for (key in 1:length(keys)) {
    assign(keys[[key]], NA, envir = getOption("log.rx"))
  }

  v <- 1
  set_log_element("value", v)

  l <- list(a = 1, b = 2, c = 3)
  set_log_element("list", l)

  expect_identical(
    getOption("log.rx")[["value"]],
    v
  )

  expect_identical(
    getOption("log.rx")[["list"]],
    l
  )

  log_remove()
})

test_that("get_log_element works", {
  log_init()

  # list of attributes to add to the log
  keys <- list(
    "value",
    "list"
  )

  for (key in 1:length(keys)) {
    assign(keys[[key]], NA, envir = getOption("log.rx"))
  }

  v <- 1
  set_log_element("value", v)

  l <- list(a = 1, b = 2, c = 3)
  set_log_element("list", l)

  expect_identical(
    get_log_element("value"),
    v
  )

  expect_identical(
    get_log_element("list"),
    l
  )

  log_remove()
})

test_that("setting log name works", {
  log_init()

  # list of attributes to add to the log
  keys <- list(
    "file_name",
    "file_path",
    "log_name",
    "log_path"
  )

  for (key in 1:length(keys)) {
    assign(keys[[key]], NA, envir = getOption("log.rx"))
  }

  log_name <- "test_log_name"

  set_log_name_path(log_name = log_name)

  expect_identical(getOption("log.rx")[["log_name"]], log_name)
  expect_warning(set_log_name_path(log_name = "test_log_name2"))

  log_remove()
})

test_that("setting path name works", {
  log_init()

  # list of attributes to add to the log
  keys <- list(
    "file_name",
    "file_path",
    "log_name",
    "log_path"
  )

  for (key in 1:length(keys)) {
    assign(keys[[key]], NA, envir = getOption("log.rx"))
  }

  log_path <- "test_log_path"

  set_log_name_path(log_path = log_path)

  expect_identical(
    getOption("log.rx")[["log_path"]],
    log_path
  )
  expect_warning(set_log_name_path(log_path = "."))

  log_remove()
})

test_that("run_safely_loudly captures output, messages, warnings, and errors separately", {
  fp <- test_path("ref", "safely_loudly_test_file.R")

  log_config(file = fp)

  run_safely_loudly(fp)

  log <- getOption("log.rx")
  expect_identical(log[["output"]], "[1] \"log print\"\n[1] \"log print 2\"\nlog catlog cat 2")
  expect_identical(
    log[["messages"]],
    c("log inform", "log inform 2", "log message", "log message 2")
  )
  expect_identical(log[["warnings"]], c("log warning", "log warning2"))
  expect_identical(log[["errors"]][["message"]], "log error")

  log_remove()
})

test_that("run_safely_loudly captures the value returned by the script", {
  fp <- test_path("ref", "safely_loudly_test_file_result.R")

  log_config(file = fp)

  run_safely_loudly(fp)

  expect_equal(
    getOption("log.rx")[["result"]][["value"]],
    data.frame(test = c(8, 6, 7, 5, 3, 0, 9))
  )

  log_remove()
})

test_that("run_file uses a child of the global environment for execution", {
  expect_identical(
    capture.output(globalenv()),
    capture.output(logrx:::run_file(test_path("ref", "run_file_test_parent_env.R")))
  )
})

test_that("run_file makes no changes are made to the global environment or options during execution", {
  pre_global <- ls(globalenv())
  pre_ops <- options()

  logrx:::run_file(test_path("ref", "run_file_test.R"))

  post_global <- ls(globalenv())
  post_ops <- options()

  expect_identical(pre_global, post_global)
  expect_identical(pre_ops, post_ops)

  # run_file adds magrittr to the search path via run_file_test.R, so remove it
  detach("package:magrittr")
})

test_that("run_file uses a different env if set using log.rx.exec.env", {
  run_env <- new.env(parent = as.environment(search()[2]))

  options("log.rx.exec.env" = run_env)

  expect_identical(
    capture.output(run_env),
    capture.output(logrx:::run_file(test_path("ref", "run_file_test_current_env.R")))
  )

  expect_identical(
    capture.output(parent.env(run_env)),
    capture.output(logrx:::run_file(test_path("ref", "run_file_test_parent_env.R")))
  )
})
