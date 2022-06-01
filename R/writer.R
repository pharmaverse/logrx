### Functions to format log elements to facilitate the writing of the output log file

#' Generic function to format log.rx elements for writing
#'
#' @param el_key the key of the element in log.rx to be fetched
#' @param prefix string to be placed before element value during formatting
#'
#' @return formatted element including prefix
#' @export
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
#' @export
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
#' @export
#'
write_session_info <- function(){
   session_info <- get_log_element("session_info") %>%
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

#' Format file name and path for writing
#'
#' @return A vector of file name and path prefixed
#' @export
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

#' Format masked functions attribute for writing
#'
#' @return A formatted vector of masked functions
#' @export
#'
#' @importFrom purrr imap
#'
write_masked_functions <- function(){
   masked_functions_list <- get_log_element("masked_functions")

   masked_functions <- imap(masked_functions_list, ~ paste0("function `", .y,
      "` from {", paste(.x$masks, collapse = ", "), "} by ", .x$source)) %>%
      unname() %>%
      unlist()


   return(masked_functions)
}

#' Format used functions attribute for writing
#'
#' @return A formatted vector of used functions
#' @export
#'
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

   combined <- aggregate(function_name~library, used_functions_list, paste)

   map2(combined$library, combined$function_name, ~paste(paste0("{", .x, "}"), paste0(.y, collapse = ", "))) %>%
      unname() %>%
      unlist()
}

#' Format unapproved functions attribute for writing
#'
#' @return A formatted vector of unapproved functions
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

   combined <- aggregate(function_name~library, unapproved_functions_list, paste)

   map2(combined$library, combined$function_name, ~paste(paste0("{", .x, "}"), paste0(.y, collapse = ", "))) %>%
      unname() %>%
      unlist()
}

#' Generic function to format log section headers
#'
#' @param title_string String to be used as section title
#'
#' @return A vector with the header including title
#' @export
#'
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
#' @export
#'
#' @importFrom utils capture.output
#'
write_errors <- function() {
   errors <- get_log_element("errors")

   paste0("Errors:\n\t",
          str_replace_all(errors$message, "\n", "\n\t"))
}

#' Format warnings attribute for writing
#'
#' @return A formatted vector of warnings
#' @export
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
#' @export
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
#' @export
#'
write_output <- function() {
   output <- get_log_element("output")

   paste0("Output:\n\t",
          str_replace_all(output, "\n", "\n\t"))
}

#' Format result attribute for writing
#'
#' @return A formatted vector of results
#' @export
#'
write_result <- function() {
   result <- get_log_element("result")

   c("\nResult:", paste0("\t", capture.output(result$value)))
}
