test_that("metadata elements are specified correctly and loaded into a list", {
  expect_identical(get_timber_metadata(),
               list(
                  info = paste0("This log was generated using timber ",
                                sessionInfo()[["otherPkgs"]][["timber"]][["Version"]]),
                  version = sessionInfo()[["otherPkgs"]][["timber"]][["Version"]],
                  license = sessionInfo()[["otherPkgs"]][["timber"]][["License"]],
                  built = sessionInfo()[["otherPkgs"]][["timber"]][["Built"]],
                  repository_link = "https://github.com/atorus-research/timber"
               ))
})

test_that("when given a file as an argument a normalized file path to that file will be returned by default", {
   expect_equal(get_file_path(file = './test-get.R'), paste0(getwd(),'/test-get.R'))
})

test_that("when given a file as an argument a non-normalized file path to that file will be returned when specified to not normalize", {
   expect_equal(get_file_path(file = './test-get.R', normalize = FALSE), './test-get.R')
})

test_that("session info is captured", {
   expect_identical(get_session_info(), capture.output(sessionInfo()))
})

test_that("all functions that are masked are found and returned", {
   expect_identical(names(get_masked_functions()), unique(unlist(conflicts(detail = TRUE))))
})

test_that("each masked function element contain source and masks elements", {
   expect_identical(unlist(unname(map(get_masked_functions(), ~ names(.x)))), rep(c("source","masks"), length(get_masked_functions())))
})

