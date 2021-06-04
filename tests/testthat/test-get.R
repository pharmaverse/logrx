test_that("metadata elements are specified correctly and loaded into a list", {
  expect_identical(get_timber_metadata(),
               list(
                  info = paste0("This log was generated using timber ",
                                sessionInfo()[["otherPkgs"]][["timber"]][["Version"]]),
                  version = sessionInfo()[["otherPkgs"]][["timber"]][["Version"]],
                  license = sessionInfo()[["otherPkgs"]][["timber"]][["License"]],
                  built = sessionInfo()[["otherPkgs"]][["timber"]][["Built"]],
                  repository_link = NULL
               ))
})

test_that("when given a file as an argument a normalized file path to that file will be returned", {
   expect_equal(get_file_path(file = './test-get.R'), paste0(getwd(),'/test-get.R'))
})


test_that("when given a file as an argument a non-normalized file path to that file will be returned", {
   expect_equal(get_file_path(file = './test-get.R', normalize = FALSE), './test-get.R')
})
