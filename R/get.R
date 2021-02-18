### Functions to get data for elements of the log


#' Returns named list of timber metadata attributes
#'
#' @return Named list of timber package metadata attributes
#' @export
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
      repository_link = NULL
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
   if ((!is.null(ofile)) & (!is.na(ofile)) & normalize) {
      ofile <- normalizePath(ofile)
   }

   # return the full path
   return(ofile)
}
