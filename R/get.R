### Functions to get data for elements of the log


#' Returns named list of logrx metadata attributes
#'
#' @return Named list of logrx package metadata attributes
#'
#' @importFrom sessioninfo session_info
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' get_logrx_metadata()
#' }
#'
#' @noRd
#'
get_logrx_metadata <- function() {
  logrx_session_info <- session_info()$packages %>%
    filter(.data[["package"]] == "logrx")

  logrx_metadata <- list(
    info = paste0(
      "This log was generated using logrx ",
      logrx_session_info[["loadedversion"]]
    ),
    version = logrx_session_info[["loadedversion"]],
    built = ifelse(!is.na(logrx_session_info[["source"]]),
      logrx_session_info[["source"]],
      "Build not able to be determined"
    ),
    repository_link = "https://github.com/pharmaverse/logrx"
  )

  return(logrx_metadata)
}

#' Gets full path of file being run
#'
#' @param file File path of file being run, optional
#' @param normalize If the returned path should be normalized
#'
#' @return full path of file being run
#'
#' @examples
#' \dontrun{
#' get_file_path()
#' }
#'
#' @noRd
#'
get_file_path <- function(file = NA, normalize = TRUE) {
  if (!is.na(file)) {
    ofile <- file
  } else {
    # This will populate if the file is sourced
    ofile <- sys.frame(1)$ofile

    # If not, go further
    if (is.null(ofile)) {
      # Interactively you can't be sure of location
      if (interactive()) {
        ofile <- NA
      } else {
        # If run in batch, use command line arguments
        initial.options <- commandArgs(trailingOnly = FALSE)
        # File command line argument to search for
        file.arg.name <- "--file="
        # Pick that off and remove the argument syntax
        ofile <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)])
      }
    }
  }

  # normalize the file path
  if (length(ofile) > 0 &&
    (!is.null(ofile)) &&
    (!is.na(ofile)) &&
    normalize) {
    ofile <- normalizePath(ofile)
  }

  # return the full path
  return(ofile)
}

#' Returns Session Info
#'
#' @return Formatted Session Info
#'
#' @importFrom sessioninfo session_info
#'
#' @examples
#' \dontrun{
#' get_session_info()
#' }
#'
#' @noRd
#'
get_session_info <- function() {
  return(session_info(info = "all"))
}


#' Returns named list of masked functions
#'
#' @return Named list of masked functions, source package, and what they mask
#'
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' get_masked_functions()
#' }
#'
#' @noRd
#'
get_masked_functions <- function() {
  # get conflicts into stable object
  conf <- conflicts(detail = TRUE)
  # Get the vector of package names into a vector
  items <- unname(unlist(imap(conf, ~ rep(.y, length(.x))))) %>%
    # Flatten the list of the function names and set them
    # as the names of the vector of packages
    set_names(unname(unlist(conf)))
  # get keys for mapping
  keys <- unique(names(items))
  # map items to list with names corresponding to keys
  conflict_list <- map(keys, ~ unique(unname(items[names(items) == .x]))) %>%
    set_names(keys) %>%
    map(~ list("source" = .x[1], "masks" = .x[2:length(.x)]))

  return(conflict_list)
}

#' Get functions used within a file
#'
#' @param file File path of file to run
#'
#' @return tibble with `library` and `function_name`
#' @importFrom dplyr select distinct mutate coalesce group_by ungroup
#' @importFrom tidyr pivot_wider complete all_of
#' @importFrom purrr safely
#' @importFrom tibble tibble
#' @importFrom utils getParseData
#'
#' @examples
#' \dontrun{
#' file <- "ex1.R"
#' get_functions_used(file)
#' }
#'
#' @noRd
#'
get_used_functions <- function(file) {
  # if markdown, write R code, including inline, to a script
  # use this script to find functions used
  if (grepl("*.Rmd$", file, ignore.case = TRUE)) {
    tmpfile <- tempfile(fileext = ".R")
    on.exit(unlink(tmpfile))
    withr::local_options(list(knitr.purl.inline = TRUE))
    knitr::purl(file, tmpfile)
    file <- tmpfile
  }

  # catch error
  retfun <- safely(parse,
    quiet = FALSE,
    otherwise = "Syntax Error Found, Package and Function Identification Stopped"
  )
  ret <- retfun(file, keep.source = TRUE)

  if (!is.expression(ret$result)) {
    return(
      tibble(
        function_name = "",
        library = ret$result
      )
    )
  }

  tokens <- getParseData(ret$result) %>%
    filter(.data[["token"]] %in% c("SYMBOL_FUNCTION_CALL", "SPECIAL", "SYMBOL_PACKAGE"))

  if (nrow(tokens) == 0) {
    return(NULL)
  }

  # grouping and complete to ensure all three columns carry through after pivot
  # regardless if seen in the parsed data
  filtered_tokens <- tokens %>%
    mutate(token = factor(
      .data[["token"]],
      c("SYMBOL_FUNCTION_CALL", "SPECIAL", "SYMBOL_PACKAGE")
    )) %>%
    group_by(.data[["line1"]], .data[["parent"]]) %>%
    complete(token = .data[["token"]])

  wide_tokens <- pivot_wider(filtered_tokens,
    id_cols = all_of(c("line1", "parent")),
    values_from = "text",
    names_from = "token"
  ) %>%
    ungroup()

  # if package is present, but symbol or special is not, a function did not follow the ::
  # ex. knitr::opts_chunk$set()
  # for this case, remove row that contains the package
  # set will still be captured but we will be able to link it to a package in this current version
  wide_tokens_wo_orphans <- wide_tokens[!(!is.na(wide_tokens$SYMBOL_PACKAGE) & is.na(wide_tokens$SYMBOL_FUNCTION_CALL) & is.na(wide_tokens$SPECIAL)), ]

  combine_tokens <- wide_tokens_wo_orphans %>%
    mutate(function_name = coalesce(
      .data[["SYMBOL_FUNCTION_CALL"]],
      .data[["SPECIAL"]]
    ))

  distinct_use <- get_library(combine_tokens) %>%
    select(all_of(c("function_name", "library"))) %>%
    distinct()

  distinct_use[is.na(distinct_use)] <- "!!! NOT FOUND !!!"

  distinct_use
}


#' Add libraries to functions
#'
#' Each script should be independent so we can use the search path since this
#' would be just for this script.
#' This must also be run after script execution.
#'
#' @param df dataframe containing variables `function_name` and `SYMBOL_PACKAGE`
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom purrr map
#' @importFrom utils lsf.str
#'
#' @return tibble that includes `library`
#'
#' @noRd
#'
get_library <- function(df) {
  functions_only <- function(.x) {
    intersect(ls(.x), lsf.str(.x))
  }

  # do not search CheckExEnv, this is created while examples are executed
  # during build
  # T and F are given a delayedAssign within the CheckExEnv environment,
  # and when we check this environments objects, the promise for T and F
  # are evaluated, and return:
  # stop("T used instead of TRUE"), stop("F used instead of FALSE")
  search_environ <- search()[search() != "CheckExEnv"]

  search_lookup <- map(search_environ, functions_only)
  names(search_lookup) <- search_environ
  df$library <- unlist(map(df$function_name, ~ get_first(., search_lookup)))

  df %>%
    mutate(library = ifelse(
      !is.na(df$SYMBOL_PACKAGE),
      paste0("package:", df$SYMBOL_PACKAGE),
      .data[["library"]]
    ))
}


get_first <- function(func, search_lookup) {
  flag_found <- map(search_lookup, ~ func %in% .)
  if (any(unlist(flag_found))) {
    names(flag_found[flag_found == TRUE][1])
  } else {
    NA
  }
}

#' Get unapproved packages and functions used
#'
#' Compare two dataframes that contain approved and used packages and functions.
#'
#' @param approved_packages dataframe containing variables `function_name` and `library`
#' @param used_packages dataframe containing variables `function_name` and `library`
#'
#' @importFrom dplyr anti_join
#'
#' @return tibble that includes packages and functions used, but not approved
#'
#' @noRd
#'
get_unapproved_use <- function(approved_packages, used_packages) {
  anti_join(approved_packages, used_packages, by = c("library", "function_name"))
}


#' Get lint results
#'
#' Pass linters specified in the `log.rx.lint` option to `lintr::lint`
#'
#' @param file File path of file being run
#'
#' @return results from `lintr::lint()`
#'
#' @noRd
#'
get_lint_results <- function(file) {
  # lint file if option is turned on
  if (!is.logical(getOption("log.rx.lint"))) {
    if (!requireNamespace("lintr", quietly = TRUE)) {
      message(strwrap("Linting will not be included in the log. Install the
         lintr package to use the log.rx.lint feature.",
        prefix = " ", initial = ""
      ))
      return()
    }

    lintr::lint(file, getOption("log.rx.lint"))
  }
}

#' Get repository URLs
#'
#' Obtain repository URLs possibly used to install packages in session
#'
#' @return results from `getOption("repos")` as list
#'
#' @noRd
#'
get_repo_urls <- function() {
  as.list(getOption("repos"))
}
