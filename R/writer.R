### Functions to format log elements to facilitate the writing of the output log file

#' Generic function to format timber.log elements for writing
#'
#' @param el_key the key of the element in timber.log to be fetched
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

#' Format timber.log's metadata attributes for writing
#'
#' @return A vector of timber.log's metadata attributes
#' @export
#'
write_metadata <- function(){
   metadata <- get_log_element("metadata")

   metadata <- c(metadata$info,
                 paste0("timber package version: ", metadata$version),
                 paste0("timber build: ", metadata$built),
                 paste0("timber link to repository: ", metadata$repository_link))

   return(metadata)
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

   purrr::map2(combined$library, combined$function_name, ~paste(paste0("{", .x, "}"), paste0(.y, collapse = ", "))) %>%
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

   purrr::map2(combined$library, combined$function_name, ~paste(paste0("{", .x, "}"), paste0(.y, collapse = ", "))) %>%
      unname() %>%
      unlist()
}

#' Generic function to format log section headers
#'
#' @param title_string string to be used as section title
#'
#' @return a vector with the header including title
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
          capture.output(errors$message))
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

   paste0("\nResult:\n\t",
          paste0(result$value, collapse = "\n\t"))
}
