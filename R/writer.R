### Functions to format log elements to facilitate the writing of the output log file

#' Generic function to format timber.log elements for writing
#'
#' @param el_key the key of the element in timber.log to be fetched
#' @param prefix string to be placed before element value during formatting
#'
#' @return formatted element including prefix
#' @export
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

#' Format masked functions attribute for writing
#'
#' @return A formatted vector of masked functions
#' @export
#'
#' @examples
#' write_masked_functions()
#'
write_masked_functions <- function(){
   masked_functions_list <- get_log_element("masked_functions")

   masked_functions <- c()

   for (i in 1:length(masked_functions_list)){
      name <- names(masked_functions_list)[[i]]
      source <- masked_functions_list[[i]]$source
      masks <- masked_functions_list[[i]]$masks
      fmask <- paste(masks, collapse = ", ")
      fmtd <- paste0("function `", name, "` from {", fmask, "} by ", source)
      masked_functions <- append(masked_functions, fmtd)
   }


   return(masked_functions)
}

#' Generic function to format log section headers
#'
#' @param title_string string to be used as section title
#'
#' @return a vector with the header including title
#' @export
#'
#' @examples
#' write_log_header("timber metadata")
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
