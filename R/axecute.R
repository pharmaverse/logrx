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
#'
#' scriptPath <- tempfile()
#' logDir <- tempdir()
#'
#' writeLines("print('hello timber')", con = scriptPath)
#'
#' axecute(scriptPath)
#' axecute(scriptPath, log_name = "log_out", log_path = logDir)
#'
axecute <- function(file, log_name = NA, log_path = NA){
   # initialize log
   log_config(file)

   # set log_name and log_path if passed in
   if (!is.na(log_name) | !is.na(log_path)){
      set_log_name_path(log_name, log_path)
   }

   # run the code
   run_safely_n_quietly(file)

   # write log
   log_write()
}
