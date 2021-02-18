### Functions to setup and run code using timber

#' Create a log and run a file
#'
#' @param file File path of file to run
#' @param log_name Name of log file
#' @param log_path Path to output log to
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_init("script.R")
#' log_init("script.R", log_name = "log_out", log_path = "./logs")
#'
axecute <- function(file, log_name = NA, log_path = NA){
   # initialize log
   log_config(file)

   # set log_name and log_path if passed in
   if (!is.na(log_name) | !is.na(log_path)){
      set_log_name_path(log_name, log_path)
   }

   # run the code
   source(file)

   # write log
   log_write()
}
