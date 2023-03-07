#' Reformat subsections in log lines
#'
#' @param log_txt String vector. Object with log text lines
#'
#' @importFrom stringr str_detect
#' @importFrom stringr str_count
#' @importFrom stringr str_remove
#'
#' @return tibble that ensures formatted subsections
#'
#' @examples
#' \dontrun{
#' reformat_subsections(readlines(log_file_path))
#' }
#'
#' @noRd
#'
reformat_subsections <- function(log_txt) {
  adj_log_txt <- c()
  for (i in log_txt) {
    adj_tf <- stringr::str_detect(
      i,
      "Errors:|Warnings:|Messages:|Output:|Result:"
    )
    if (adj_tf) {
      nrem <- stringr::str_count(i)
      i <- stringr::str_remove(i, ":")
      i <-
        paste("-", i, paste(rep("-", 54 - nrem), collapse = ""),
          collapse = ""
        )
    }
    adj_log_txt <- c(adj_log_txt, i)
  }
  return(adj_log_txt)
}

#' Nest sections in log lines vector
#'
#' @param adj_log_txt String vector. Object with formatted log text lines
#'
#' @importFrom stringr str_remove_all
#'
#' @return list that includes nested log sections
#'
#' @noRd
#'
nest_sections <- function(adj_log_txt) {
  sect_headers <- c()
  sect_status <- FALSE
  sect_info <- list()
  for (i in adj_log_txt) {
    if (i == paste(rep("-", 80), collapse = "")) {
      sect_status <- !sect_status
    } else if (sect_status == TRUE) {
      sect_headers <- c(sect_headers, i)
    } else {
      cur_pos <- length(sect_headers)
      if (length(sect_info) == cur_pos) {
        sect_info[[cur_pos]] <- c(sect_info[[cur_pos]], i)
      } else {
        sect_info[[cur_pos]] <- i
      }
    }
  }
  sect_headers <-
    stringr::str_remove_all(sect_headers, "-?\\s{3,}-?")
  names(sect_info) <- sect_headers

  return(sect_info)
}

#' Nest subsections in log lines vector
#'
#' @param adj_log_txt String vector. Object with formatted log text lines
#' @param sect_info String vector. Object with nested sections
#'
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
#' @importFrom stringr str_remove_all
#'
#' @return list that includes nested log subsections
#'
#' @noRd
#'
nest_subsections <- function(adj_log_txt, sect_info) {
  subsect_headers <- na.omit(
    stringr::str_extract(adj_log_txt, "\\-\\s\\w+\\s(\\w+\\s)?\\-{3,70}")
  )
  subset_sections <- function(section) {
    subsect_status <- FALSE
    subsect_info <- list()
    for (i in section) {
      if (i %in% subsect_headers) {
        latest_subsect <- stringr::str_trim(
          stringr::str_remove_all(i, "\\-")
        )
        subsect_status <- TRUE
      } else if (subsect_status) {
        subsect_info[[latest_subsect]] <-
          c(subsect_info[[latest_subsect]], i)
      } else {
        subsect_info <- c(subsect_info, i)
      }
    }
    subsect_info
  }
  nested_log <- lapply(sect_info, subset_sections)
  return(nested_log)
}

#' Nest sections and subsections in log lines vector
#'
#' @param adj_log_txt String vector. Object with formatted log text lines
#'
#' @return list that includes nested log sections and subsections
#'
#' @noRd
#'
nest_log <- function(adj_log_txt) {
  nest_subsections(
    adj_log_txt,
    nest_sections(adj_log_txt)
  )
}

#' Parse nested log list to tibbles for object where appropriate
#'
#' @param nested_log String vector.
#' Object with nested log output (from `nest_log()`)
#'
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom stringr str_replace_all
#' @importFrom dplyr rename_with
#' @importFrom readr read_table
#' @importFrom dplyr mutate
#'
#' @return list with objects coerced as tibbles
#'
#' @noRd
#'
parse_log <- function(nested_log) {
  parsed_log <- list()

  if ("logrx Metadata" %in% names(nested_log)) {
    parsed_log$`logrx Metadata` <-
      nested_log$`logrx Metadata` %>%
      unlist() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\: ",
        into = c("Variable", "Value"),
        extra = "merge"
      )
  }

  if ("User and File Information" %in% names(nested_log)) {
    parsed_log$`User and File Information` <-
      nested_log$`User and File Information` %>%
      unlist() %>%
      stringr::str_trim() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\: ",
        into = c("Variable", "Value")
      )
  }

  if ("Session Information" %in% names(nested_log)) {
    parsed_log$`Session Information`$`Session info` <-
      nested_log$`Session Information`$`Session info` %>%
      unlist() %>%
      stringr::str_trim() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\s",
        into = c("setting", "value"),
        extra = "merge",
      ) %>%
      mutate(across(where(is.character), stringr::str_trim))

    parsed_log$`Session Information`$`Packages` <-
      nested_log$`Session Information`$`Packages` %>%
      # remove indicator whether the package is attached to the search path
      stringr::str_replace_all("\\*", " ") %>%
      # account for loaded packages due to load_all()
      stringr::str_replace_all(" P ", "   ") %>%
      readr::read_table(skip = 1, col_names = FALSE) %>%
      dplyr::rename_with(~ c(
        "package",
        "version",
        "date",
        "lib",
        "source",
        "lang",
        "r_version"
      )) %>%
      dplyr::mutate(
        lang = stringr::str_remove(lang, "\\("),
        r_version = stringr::str_remove(r_version, "\\)")
      )

    parsed_log$`Session Information`$`External software` <-
      nested_log$`Session Information`$`External software` %>%
      stringr::str_trim() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\s",
        into = c("setting", "value"),
        extra = "merge",
      ) %>%
      mutate(across(where(is.character), stringr::str_trim))
  }

  if ("Masked Functions" %in% names(nested_log)) {
    parsed_log$`Masked Functions` <-
      nested_log$`Masked Functions` %>%
      unlist() %>%
      tibble::tibble("Masked Functions" = .)
  }

  if ("Used Package and Functions" %in% names(nested_log)) {
    parsed_log$`Used Package and Functions` <-
      nested_log$`Used Package and Functions` %>%
      unlist() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\} ",
        into = c("library", "function_names")
      ) %>%
      dplyr::mutate(library = stringr::str_remove(library, "\\{"))
  }

  if ("Program Run Time Information" %in% names(nested_log)) {
    parsed_log$`Program Run Time Information` <-
      nested_log$`Program Run Time Information` %>%
      unlist() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\: ",
        into = c("Variable", "Value")
      )
  }

  if ("Log Output File" %in% names(nested_log)) {
    parsed_log$`Log Output File` <-
      nested_log$`Log Output File` %>%
      unlist() %>%
      tibble::tibble() %>%
      tidyr::separate(".",
        sep = "\\: ",
        into = c("Variable", "Value")
      )
  }

  return(parsed_log)
}

#' Read and parse previous logrx file
#'
#' @param file String. Path to a logrx log file
#'
#' @return Tibble. Object that includes nested and parsed content
#'
#' @examples
#' \dontrun{
#' read_log_file(previous_log_filepath)
#' }
#'
read_log_file <- function(file) {
  if (!file.exists(file)) {
    stop("Path does not exist:", sQuote(file))
  }
  con <- file(file.path(file), "r")
  flines <- readLines(con)
  close(con)

  parsed_log <- flines %>%
    reformat_subsections() %>%
    nest_log() %>%
    parse_log()
  return(parsed_log)
}
