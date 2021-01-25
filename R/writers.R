### Functions to format log elements to facilitate the writing of the output log file

#' Writes
#'
#' @param el_key the key of the element in timber.log to be fetched
#' @param prefix string to be placed before element value during formatting
#'
#' @return formatted element including prefix
#'
#' @examples
#' write_log_element(user, "user running program: ")
#'
write_log_element <- function(el_key, prefix) {
   # get element from log
   el <- get_log_element(el_key)

   # format element with prefix
   fmtd_el <- paste0(prefix, el)

   # return formatted element
   return(fmtd_el)
}
