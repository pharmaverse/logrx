### Functions to setup and run code using timber

#' Create a log and run a file
#'
#' @param file File path of file to run
#' @param log_name Name of log file
#' @param log_path Path to output log to
#' @param remove_log_object Should the log object be removed after writing, defaults to TRUE
#'
#' @return 0 if there are no errors and 1 if any error
#' @export
#'
axecute <- function(file, log_name = NA, log_path = NA, remove_log_object = TRUE){
   # initialize log
   log_config(file = file, log_name = log_name, log_path = log_path)

   # run the code
   run_safely_loudly(file)

   # check for errors prior to log_write() since this can remove the log
   any_errors <- get_log_element("errors")

   # write log
   log_write(file = file, remove_log_object = remove_log_object)

   # return 0 if no error, 1 otherwise
   invisible(
      if(is.null(any_errors)) {
         return(0)
      } else (return(1))
   )

}
