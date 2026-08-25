test_that("build_approved returns the expected results", {
  approved_pkgs <- list(
    base = c("library", "mean"),
    sessioninfo = "All"
  )
  expect_equal(
    build_approved(approved_pkgs),
    tibble::tribble(
      ~function_name, ~library,
      "library", "package:base",
      "mean", "package:base",
      "external_info", "package:sessioninfo",
      "package_info", "package:sessioninfo",
      "platform_info", "package:sessioninfo",
      "os_name", "package:sessioninfo",
      "session_diff", "package:sessioninfo",
      "session_info", "package:sessioninfo",
      "python_info", "package:sessioninfo"
    )
  )
})

test_that("build_approved saves the expected results", {
  dir <- tempdir()
  withr::defer(unlink("dir", recursive = TRUE))

  approved_pkgs <- list(
    base = c("library", "mean"),
    sessioninfo = "All"
  )

  build_approved(approved_pkgs, file.path(dir, "approved.rds"))

  expect_equal(
    readRDS(file.path(dir, "approved.rds")),
    tibble::tribble(
      ~function_name, ~library,
      "library", "package:base",
      "mean", "package:base",
      "external_info", "package:sessioninfo",
      "package_info", "package:sessioninfo",
      "platform_info", "package:sessioninfo",
      "os_name", "package:sessioninfo",
      "session_diff", "package:sessioninfo",
      "session_info", "package:sessioninfo",
      "python_info", "package:sessioninfo"
    )
  )
})

test_that("build_approved writes block yaml with package: prefix keys", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = c("library", "mean")), out)
  result <- yaml::read_yaml(out)
  expect_equal(sort(result[["package:base"]]), c("library", "mean"))
})

test_that("build_approved writes block yaml with bare keys", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = c("library", "mean")), out, yaml_prefix = FALSE)
  result <- yaml::read_yaml(out)
  expect_equal(sort(result[["base"]]), c("library", "mean"))
  expect_null(result[["package:base"]])
})

test_that("build_approved writes inline yaml with package: prefix keys", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = c("library", "mean"), dplyr = c("mutate", "filter")), out, yaml_style = "inline")
  result <- yaml::read_yaml(out)
  expect_equal(sort(result[["package:base"]]), c("library", "mean"))
  expect_equal(sort(result[["package:dplyr"]]), c("filter", "mutate"))
})

test_that("build_approved writes inline yaml with bare keys", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = c("library", "mean"), dplyr = c("mutate", "filter")), out, yaml_prefix = FALSE, yaml_style = "inline")
  result <- yaml::read_yaml(out)
  expect_equal(sort(result[["base"]]), c("library", "mean"))
  expect_equal(sort(result[["dplyr"]]), c("filter", "mutate"))
  expect_null(result[["package:base"]])
})

test_that("build_approved preserves _all_ in yaml with package: prefix", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = "_all_"), out)
  result <- yaml::read_yaml(out)
  expect_equal(result[["package:base"]], "_all_")
})

test_that("build_approved preserves _all_ in yaml with bare keys", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = "_all_"), out, yaml_prefix = FALSE)
  result <- yaml::read_yaml(out)
  expect_equal(result[["base"]], "_all_")
})

test_that("build_approved round-trips rds", {
  out <- tempfile(fileext = ".rds")
  approved_pkgs <- list(base = c("library", "mean"))
  build_approved(approved_pkgs, out)
  result <- readRDS(out)
  expect_equal(sort(result$function_name), c("library", "mean"))
  expect_true(all(result$library == "package:base"))
})

test_that("build_approved round-trips yaml (block, prefix)", {
  out <- tempfile(fileext = ".yaml")
  approved_pkgs <- list(base = c("library", "mean"))
  build_approved(approved_pkgs, out)
  result <- logrx:::normalize_approved_yaml(yaml::read_yaml(out))
  expect_equal(sort(result$function_name), c("library", "mean"))
  expect_true(all(result$library == "package:base"))
})

test_that("build_approved round-trips yml extension", {
  out <- tempfile(fileext = ".yml")
  approved_pkgs <- list(base = c("library", "mean"))
  build_approved(approved_pkgs, out)
  result <- logrx:::normalize_approved_yaml(yaml::read_yaml(out))
  expect_equal(sort(result$function_name), c("library", "mean"))
  expect_true(all(result$library == "package:base"))
})

test_that("build_approved round-trips yaml (block, bare)", {
  out <- tempfile(fileext = ".yaml")
  approved_pkgs <- list(base = c("library", "mean"))
  build_approved(approved_pkgs, out, yaml_prefix = FALSE)
  result <- logrx:::normalize_approved_yaml(yaml::read_yaml(out))
  expect_equal(sort(result$function_name), c("library", "mean"))
  expect_true(all(result$library == "package:base"))
})

test_that("build_approved round-trips yaml (inline, prefix)", {
  out <- tempfile(fileext = ".yaml")
  approved_pkgs <- list(base = c("library", "mean"), dplyr = c("mutate", "filter"))
  build_approved(approved_pkgs, out, yaml_style = "inline")
  result <- logrx:::normalize_approved_yaml(yaml::read_yaml(out))
  expect_equal(sort(result$function_name[result$library == "package:base"]), c("library", "mean"))
  expect_equal(sort(result$function_name[result$library == "package:dplyr"]), c("filter", "mutate"))
})

test_that("build_approved round-trips yaml (inline, bare)", {
  out <- tempfile(fileext = ".yaml")
  approved_pkgs <- list(base = c("library", "mean"), dplyr = c("mutate", "filter"))
  build_approved(approved_pkgs, out, yaml_prefix = FALSE, yaml_style = "inline")
  result <- logrx:::normalize_approved_yaml(yaml::read_yaml(out))
  expect_equal(sort(result$function_name[result$library == "package:base"]), c("library", "mean"))
  expect_equal(sort(result$function_name[result$library == "package:dplyr"]), c("filter", "mutate"))
})

test_that("build_approved round-trips yaml with _all_", {
  out <- tempfile(fileext = ".yaml")
  build_approved(list(base = "_all_"), out)
  result <- logrx:::normalize_approved_yaml(yaml::read_yaml(out))
  expect_true(nrow(result) > 0)
  expect_true(all(result$library == "package:base"))
  expect_true("mean" %in% result$function_name)
})

test_that("normalize_approved_yaml handles inline format with bare names and _all_", {
  yaml_list <- yaml::read_yaml(testthat::test_path("ref", "approved_example.yaml"))
  result <- logrx:::normalize_approved_yaml(yaml_list)
  expect_true(all(result$library %in% c("package:base", "package:dplyr", "package:tidyr")))
  expect_true("mutate" %in% result$function_name)
  expect_true("pivot_wider" %in% result$function_name)
  expect_true(nrow(result[result$library == "package:base", ]) > 1)
})

test_that("normalize_approved_yaml handles block format with prefixed names and _all_", {
  yaml_list <- yaml::read_yaml(testthat::test_path("ref", "approved_example2.yaml"))
  result <- logrx:::normalize_approved_yaml(yaml_list)
  expect_true(all(result$library %in% c("package:base", "package:dplyr", "package:tidyr")))
  expect_true("mutate" %in% result$function_name)
  expect_true("pivot_wider" %in% result$function_name)
  expect_true(nrow(result[result$library == "package:base", ]) > 1)
})
