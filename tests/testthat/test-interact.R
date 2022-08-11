test_that("set_log_element works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "value",
      "list")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('log.rx'))
   }

   v <- 1
   set_log_element("value", v)

   l <- list(a = 1, b = 2, c = 3)
   set_log_element("list", l)

   expect_identical(getOption("log.rx")[["value"]],
                              v)

   expect_identical(getOption("log.rx")[["list"]],
                              l)

   log_remove()
})

test_that("get_log_element works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "value",
      "list")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('log.rx'))
   }

   v <- 1
   set_log_element("value", v)

   l <- list(a = 1, b = 2, c = 3)
   set_log_element("list", l)

   expect_identical(get_log_element("value"),
                              v)

   expect_identical(get_log_element("list"),
                              l)

   log_remove()
})

test_that("setting log name works", {
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "file_name",
      "file_path",
      "log_name",
      "log_path")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('log.rx'))
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
      "log_path")

   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('log.rx'))
   }

   log_path <- "test_log_path"

   set_log_name_path(log_path = log_path)

   expect_identical(getOption("log.rx")[["log_path"]],
                              log_path)
   expect_warning(set_log_name_path(log_path = "."))

   log_remove()
})

test_that("run_safely_loudly works for warnings, errors, messages, and output", {
   fp <- test_path("ref", "safely_loudly_test_file.R")

   log_config(file = fp)

   run_safely_loudly(fp)

   expect_true(!is.null(getOption("log.rx")[["warnings"]]))
   expect_true(!is.null(getOption("log.rx")[["errors"]]))
   expect_true(!is.null(getOption("log.rx")[["messages"]]))
   expect_true(!is.null(getOption("log.rx")[["output"]]))

   log_remove()
})

test_that("run_safely_loudly works for result", {
   fp <- test_path("ref", "safely_loudly_test_file_result.R")

   log_config(file = fp)

   run_safely_loudly(fp)

   expect_true(!is.null(getOption("log.rx")[["result"]]))

   log_remove()
})

test_that("run_file uses a child of the global environment for execution", {

   expect_equal(capture.output(globalenv()),
                capture.output(logrx:::run_file(test_path("ref", "run_file_test_parent_env.R"))))

})

test_that("run_file makes no changes are made to the global environment or options during execution", {

   pre_global <- globalenv()
   pre_ops <- options()

   logrx:::run_file(test_path("ref", "run_file_test.R"))

   post_global <- globalenv()
   post_ns <- search()
   post_ops <- options()

   expect_identical(pre_global, post_global)
   expect_identical(pre_ops, post_ops)

})

test_that("run_file uses a different env if set using log.rx.exec.env", {
   run_env <- new.env(parent = as.environment(search()[3]))

   options("log.rx.exec.env" = run_env)

   expect_equal(capture.output(run_env),
                capture.output(logrx:::run_file(test_path("ref", "run_file_test_current_env.R"))))

   expect_equal(capture.output(parent.env(run_env)),
                capture.output(logrx:::run_file(test_path("ref", "run_file_test_parent_env.R"))))

})
