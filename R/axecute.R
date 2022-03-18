### Functions to setup and run code using timber

#' Create a log and run a file
#'
#' @param file File path of file to run
#' @param log_name Name of log file
#' @param log_path Path to output log to
#' @param remove_log_object Should the log object be removed after writing, defaults to TRUE
#'
#' @return Nothing
#' @export
#'
axecute <- function(file, log_name = NA, log_path = NA, remove_log_object = TRUE){
   # initialize log
   log_config(file = file, log_name = log_name, log_path = log_path)

   # run the code
   run_safely_quietly(file)
   sink_log_stream(file)

   # write log
   log_write(remove_log_object = remove_log_object)
}
