# Writer functions used to format timber.log attributes

#' Format timer.log's metadata attributes
#'
#' @return A vector of timer.log's metadata attributes
#' @export
#'
#' @examples
#' write_metadata()
#'
write_metadata <- function(){
   metadata <- get_log_element("metadata")

   metadata <- c(paste0("timber package version: ", metadata$version),
                 paste0("timber license: ", metadata$license),
                 paste0("timber build: ", metadata$built),
                 paste0("timber link to repository: ", metadata$repository_link))

   return(metadata)
}
