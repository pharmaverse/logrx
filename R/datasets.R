#' Approved packages and functions
#'
#' @description
#' A dataset that stores approved packages and functions for use.
#' Each row contains a `library` and `function_name`.
#' This dataset is used to illustrate the data format to be stored in
#' the log.rx.approved option.
#'
#' @format A tibble with 6 rows and 2 variables:
#' \describe{
#' \item{function_name}{Name of the function}
#' \item{library}{Name of the package}
#' }
#'
#' @examples
#' logrx::approved
#'
"approved"
