### Functions to get data for elements of the log


#' Returns named list of timber metadata attributes
#'
#' @return Named list of timber package metadata attributes
#' @export
#'
#' @importFrom utils sessionInfo
#'
#' @examples
#' get_timber_metadata()
#'
get_timber_metadata <- function(){
   session_info <- sessionInfo()

   timber_metadata <- list(
      info = paste0("This log was generated using timber ",
                    session_info[["otherPkgs"]][["timber"]][["Version"]]),
      version = session_info[["otherPkgs"]][["timber"]][["Version"]],
      license = session_info[["otherPkgs"]][["timber"]][["License"]],
      built = session_info[["otherPkgs"]][["timber"]][["Built"]],
      repository_link = "https://github.com/atorus-research/timber"
   )

   return(timber_metadata)
}

#' Gets full path of file being run
#'
#' @param file File path of file being run, optional
#' @param normalize If the returned path should be normalized
#'
#' @return full path of file being run
#' @export
#'
#' @examples
#' get_file_path()
#'
get_file_path <- function(file = NA, normalize = TRUE){
   if (!is.na(file)){
      ofile <- file
   } else {
      # This will populate if the file is sourced
      ofile <- sys.frame(1)$ofile

      # If not, go further
      if (is.null(ofile)){
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
#' @export
#'
#' @importFrom utils sessionInfo
#'
#' @examples
#' get_session_info()
#'
get_session_info <- function(){
   return(capture.output(sessionInfo()))
}


#' Returns named list of masked functions
#'
#' @return Named list of masked functions, source package, and what they mask
#' @export
#'
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom magrittr "%>%"
#'
#' @examples
#' get_masked_functions()
#'
get_masked_functions <- function(){
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
