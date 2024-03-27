### Functions to format log elements to facilitate the writing of the output log file

#' Generic function to format log.rx elements for writing
#'
#' @param el_key the key of the element in log.rx to be fetched
#' @param prefix string to be placed before element value during formatting
#'
#' @return formatted element including prefix
#'
#' @noRd
#'
write_log_element <- function(el_key, prefix = NULL) {
   # get element from log
   el <- get_log_element(el_key)

   # format element with prefix
   fmtd_el <- paste0(prefix, el)

   # return formatted element
   return(fmtd_el)
}

#' Format log.rx's metadata attributes for writing
#'
#' @return A vector of log.rx's metadata attributes
#'
#' @noRd
#'
write_metadata <- function(){
   metadata <- get_log_element("metadata")

   metadata <- c(metadata$info,
                 paste0("logrx package version: ", metadata$version),
                 paste0("logrx build: ", metadata$built),
                 paste0("logrx link to repository: ", metadata$repository_link))

   return(metadata)
}


#' Format log.rx's session info attribute for writing
#'
#' @importFrom purrr map_chr
#' @importFrom stringi stri_wrap
#' @importFrom stringr str_c
#'
#' @return A vector of log.rx's session info attribute
#'
#' @noRd
#'
write_session_info <- function(){
   session_info <- get_log_element("session_info") %>%
      capture.output() %>%
      # remove extra dashes on title lines
      map_chr(~ ifelse(nchar(.x) > 80 & grepl("\u2500\u2500\u2500\u2500", .x),
                   substring(.x, 1, 80),
                   .x)) %>%
      # wrap any other elements over 80 characters
      map_chr(~ stri_wrap(.x, width = 80, exdent = 2, simplify = FALSE, use_length = TRUE,
                          normalize = FALSE, whitespace_only = TRUE) %>%
                 map_chr(~ str_c(.x, collapse = "\n\t", character(1))))

   return(session_info)
}

#' Format repo URLs for writing
#'
#' @return A vector of file name and path prefixed
#'
#' @noRd
#'
write_repo_urls <- function(){
   repo_urls <- ifelse(is.na(get_log_element("repo_urls")),
                       "Repo URLs not able to be determined",
                       map2(
                          names(get_log_element("repo_urls")),
                          get_log_element("repo_urls"),
                          ~paste(paste0(.x, ": "),
                                 paste0(.y, collapse = ", "))
                       ) %>%
                          unname() %>%
                          unlist()
   )

   return(repo_urls)
}

#' Format file name and path for writing
#'
#' @return A vector of file name and path prefixed
#'
#' @noRd
#'
write_file_name_path <- function(){
   file_name <- ifelse(is.na(get_log_element("file_name")),
                       "File name not able to be determined",
                       get_log_element("file_name"))
   file_path <- ifelse(is.na(get_log_element("file_path")),
                       "File path not able to be determined",
                       get_log_element("file_path"))

   file_name_path <- c(
      paste0("File Name: ", file_name),
      paste0("File Path: ", file_path)
   )
}

#' Format hashsums for writing
#'
#' @return A string
#' @noRd
#'
write_hash_sum <- function(){
      paste0("File HashSum: ", get_log_element("hash_sum"))
}

#' Format masked functions attribute for writing
#'
#' @return A formatted vector of masked functions
#'
#' @importFrom purrr imap
#'
#' @noRd
#'
write_masked_functions <- function(){
   masked_functions_list <- get_log_element("masked_functions")

   masked_functions <- imap(masked_functions_list, ~ paste0("function `", .y,
      "` from {", paste(.x$masks, collapse = ", "), "} by ", .x$source)) %>%
      unname() %>%
      unlist()


   return(masked_functions)
}

#' Formats and returns a vector of used package functions
#'
#' `write_used_functions()` gets log.rx used_packages_functions attribute,
#' formats and returns the list of used package functions
#'
#' @return Formatted vector of used package functions
#' @export
#'
#' @importFrom dplyr summarize
#' @importFrom purrr map2
#' @importFrom stats aggregate
#'
#' @examples
#' \dontrun{
#' write_used_functions()
#' }
#'
write_used_functions <- function(){
   used_functions_list <- get_log_element("used_packages_functions")

   combined <- used_functions_list %>%
      group_by(library) %>%
      summarize(function_name = paste0(function_name, collapse = ", "))

   map2(combined$library, combined$function_name, ~paste(paste0("{", .x, "}"), paste0(.y, collapse = ", "))) %>%
      unname() %>%
      unlist()
}


#' Formats and returns a vector of unapproved functions
#'
#' `write_unapproved_functions()` gets log.rx unapproved_packages_functions
#' attribute, formats and returns the list of unapproved functions
#'
#' @return Formatted vector of unapproved functions
#' @export
#'
#' @importFrom purrr map2
#' @importFrom stats aggregate
#'
#' @examples
#' \dontrun{
#' write_unapproved_functions()
#' }
write_unapproved_functions <- function(){
   unapproved_functions_list <- get_log_element("unapproved_packages_functions")

   if(nrow(unapproved_functions_list) == 0) {
      return("No unapproved packages or functions used")
   }

   combined <- unapproved_functions_list %>%
      group_by(library) %>%
      summarize(function_name = paste0(function_name, collapse = ", "))

   map2(combined$library, combined$function_name, ~paste(paste0("{", .x, "}"), paste0(.y, collapse = ", "))) %>%
      unname() %>%
      unlist()
}

#' Formatting of log file section headers
#'
#' `write_log_header` formats a string and returns it as a formatted log file
#' section header
#'
#' @param title_string String. Used as section title
#'
#' @return Vector of strings. Formatted log file section header
#' @export
#'
#' @examples
#' \dontrun{
#' write_log_header("Section Header")
#' }
write_log_header <- function(title_string){
   # create left and right pad length
   rpad <- ceiling((78 - nchar(title_string))/2)
   lpad <- floor((78 - nchar(title_string))/2)
   # make the vector
   head_vec <- c(paste(rep('-', 80), collapse = ""),
                 paste0('-', paste(rep(' ', lpad), collapse = ""), title_string, paste(rep(' ', rpad), collapse = ""), '-'),
                 paste(rep('-', 80), collapse = ""))
   # return the formatted vector
   return(head_vec)
}

#' Format errors attribute for writing
#'
#' @return A formatted vector of errors
#'
#' @importFrom utils capture.output
#'
#' @noRd
#'
write_errors <- function() {
   errors <- get_log_element("errors")

   paste0("Errors:\n\t",
          str_replace_all(errors$message, "\n", "\n\t"))
}

#' Format warnings attribute for writing
#'
#' @return A formatted vector of warnings
#'
#' @noRd
#'
write_warnings <- function() {
   warnings <- get_log_element("warnings")

   paste0("\nWarnings:\n\t",
          paste0(warnings, collapse = "\n\t"))
}

#' Format messages attribute for writing
#'
#' @importFrom purrr map
#' @importFrom stringr str_remove_all
#'
#' @return A formatted vector of messages
#'
#' @noRd
#'
write_messages <- function() {
   messages <- get_log_element("messages") %>%
      map(~ str_remove_all(.x, "\n"))

   paste0("Messages:\n\t",
          paste0(messages, collapse = "\n\t"))
}

#' Format output attribute for writing
#'
#' @importFrom stringr str_replace_all
#'
#' @return A formatted vector of output
#'
#' @noRd
#'
write_output <- function() {
   output <- get_log_element("output")

   paste0("Output:\n\t",
          str_replace_all(output, "\n", "\n\t"))
}

#' Format result attribute for writing
#'
#' @return A formatted vector of results
#'
#' @noRd
#'
write_result <- function(file) {
   result <- get_log_element("result")

   if (is_rmarkdown(file)) {
      c("\nResult:", paste0("\t", capture.output(result)))
   } else {
      c("\nResult:", paste0("\t", capture.output(result$value)))
   }
}

#' Format lint results for writing
#'
#' @return A formatted vector of results
#'
#' @noRd
#'
write_lint_results <- function(){
   lint_results <- get_log_element("lint_results")

   if (length(lint_results) == 0) {
      return("")
   }

   lint_df <- as.data.frame(lint_results)

   lint_df$lint_messages <- paste0("Line ",
                                   lint_df$line_number,
                                   " [",
                                   lint_df$linter,
                                   "] ",
                                   lint_df$message)

   break_rows <- paste(lint_df$lint_messages, collapse = "\n\n")

   paste(strwrap(break_rows, width = 78), collapse = "\n")
}
