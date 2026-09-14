#' Build approved packages and functions
#'
#' A utility function to help you build your approved packages and functions
#' list. This can be used by logrx to log unapproved use of packages and
#' functions.
#'
#' For more details see the vignette:
#' \code{vignette("approved", package = "logrx")}
#'
#' @param pkg_list Named list of character vectors:
#' * Name is the package name
#' * Value is a character vector of approved functions, 'All', or '_all_'
#' @param file String. Path where the approved list will be written.
#' If not specified, a tibble is returned.
#'
#' Default: NULL
#'
#' Permitted Files: .rds, .yaml, .yml
#' @param yaml_prefix Logical. When writing a yaml file, should package keys
#' be written with the `package:` prefix (e.g. `package:base`)? If `FALSE`,
#' bare names are used (e.g. `base`). Ignored for .rds output.
#'
#' Default: TRUE
#' @param yaml_style String. When writing a yaml file, `"block"` writes each
#' function on its own line; `"inline"` writes functions as a flow sequence
#' on one line. Ignored for .rds output.
#'
#' Default: `"block"`
#'
#' @return If `file` is NULL, a tibble with two columns (library, function) and
#' one row per function. If `file` is provided, the approved list is written to
#' the specified file and nothing is returned.
#' @importFrom purrr map2_dfr
#' @importFrom yaml write_yaml read_yaml
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
build_approved <- function(pkg_list, file = NULL, yaml_prefix = TRUE, yaml_style = c("block", "inline")) {
  approved <- purrr::map2_dfr(
    names(pkg_list),
    pkg_list,
    ~ {
      all <- tibble::tibble(
        function_name = getNamespaceExports(.x),
        library = paste0("package:", .x)
      )

      if (.y[1] %in% c("All", "all", "_all_")) {
        all
      } else {
        all[all$function_name %in% .y, ]
      }
    }
  )

  if (is.null(file)) {
    approved
  } else {
    ext <- tolower(tools::file_ext(file))
    if (ext %in% c("yaml", "yml")) {
      yaml_style <- match.arg(yaml_style)
      yaml_list <- lapply(names(pkg_list), function(pkg) {
        val <- pkg_list[[pkg]]
        if (val[1] %in% c("All", "all", "_all_")) "_all_" else as.list(val)
      })
      key_names <- if (yaml_prefix) {
        paste0("package:", names(pkg_list))
      } else {
        names(pkg_list)
      }
      names(yaml_list) <- key_names
      if (yaml_style == "inline") {
        yaml_list <- lapply(yaml_list, function(v) {
          if (identical(v, "_all_")) v else as.character(unlist(v))
        })
      }
      yaml::write_yaml(yaml_list, file)
    } else {
      saveRDS(approved, file)
    }
  }
}

#' Normalize a yaml approved list to a tibble
#'
#' Accepts keys as bare package names (e.g. `base`) or prefixed
#' (e.g. `package:base`). Values of `"all"`, `"All"`, or `"_all_"` are expanded
#' to all exported functions for that package.
#'
#' @param yaml_list Named list as returned by `yaml::read_yaml()`
#'
#' @return Tibble with columns `function_name` and `library`
#'
#' @noRd
normalize_approved_yaml <- function(yaml_list) {
  purrr::map2_dfr(
    names(yaml_list),
    yaml_list,
    ~ {
      pkg <- sub("^package:", "", .x)
      library <- paste0("package:", pkg)
      fns <- if (length(.y) == 1 && tolower(.y) %in% c("all", "_all_")) {
        getNamespaceExports(pkg)
      } else {
        unlist(.y)
      }
      tibble::tibble(function_name = fns, library = library)
    }
  )
}
