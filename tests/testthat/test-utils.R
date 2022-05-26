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
   withr::defer(unlink("dir", recursive=TRUE))

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
