### Functions to manipulate timber_log

#' initialise.timber_log
#'
#' Initialises the timber_log object
#'
#' @return timber_log object as a named list
#'
#' @examples
#' initialise.timber_log()
initialise.timber_log <- function() {
   timber_log <- list(
      system_info = list(),
      session_info = list(),
      namespace = list(),
      warnings = list(),
      errors = list(),
      start_time = lubridate::as_datetime(numeric()),
      end_time = lubridate::as_datetime(numeric()),
      run_time = lubridate::duration(),
      file_path = character(),
      user = character(),
      masked_functions = list(),
      used_packages = list(),
      log_name = character(),
      log_path = character()
   )

   return(timber_log)
}
