### Functions to format log elements to facilitate the writing of the output log file

#' Generic function to format timber.log elements for writing
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

#' Format timber.log's metadata attributes for writing
#'
#' @return A vector of timber.log's metadata attributes
#' @export
#'
#' @examples
#' write_metadata()
#'
write_metadata <- function(){
   metadata <- get_log_element("metadata")

   metadata <- c(metadata$info,
                 paste0("timber package version: ", metadata$version),
                 paste0("timber license: ", metadata$license),
                 paste0("timber build: ", metadata$built),
                 paste0("timber link to repository: ", metadata$repository_link))

   return(metadata)
}
