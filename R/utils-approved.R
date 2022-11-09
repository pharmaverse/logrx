#' Build approved packages and functions tibble
#'
#' A utility function to help you build your approved packages and functions
#' list. This can be used by logrx to log unapproved use of packages and
#' functions.
#'
#' For more details see the vignette:
#' \code{vignette("approved", package = "logrx")}
#'
#' @param pkg_list Named list of character vectors where the name is the
#' package name with a character vector of approved functions or 'All'
#' @param pkg_list Named list of character vectors:
#' * Name is the package name
#' * Value is a character vector of approved functions or 'All'
#' @param file String. Name of file where the approved tibble will be written
#' to. If not specified, the tibble is returned
#'
#' Default: NULL
#'
#' Permitted Files: .RDS
#'
#' @return Tibble with two columns (library, function) and one row per function
#' @importFrom purrr map2_dfr
#' @export
#'
#' @examples
#' approved_pkgs <- list(
#'   base = c("library", "mean"),
#'   dplyr = "All"
#' )
#'
#' # build and return
#' build_approved(approved_pkgs)
#'
#' # build and save
#' dir <- tempdir()
#' build_approved(approved_pkgs, file.path(dir, "approved.rds"))
#'
build_approved <- function(pkg_list, file = NULL) {
  approved <- purrr::map2_dfr(
    names(pkg_list),
    pkg_list,
    ~ {
      all <- tibble::tibble(
        function_name = getNamespaceExports(.x),
        library = paste0("package:", .x)
      )

      if (.y[1] %in% c("All", "all")) {
        all
      } else {
        all[all$function_name %in% .y, ]
      }
    }
  )

  if (is.null(file)) {
    approved
  } else {
    saveRDS(approved, file)
  }
}
