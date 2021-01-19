### Functions to manipulate timber_log

#' log_init
#'
#' Initialises the timber_log object
#'
#' @return timber_log object as an empty named list
#'
#' @examples
#' log_init()
log_init <- function(){
   timber_log <- list()

   return(timber_log)
}

#' log_config
#'
#' Configures the timber_log object
#'
#' @return timber_log object as a named list of attributes
#'
#' @examples
#' log_config()
log_config <- function(){
   # Initialise timber_log object
   timber_log <- log_init()

   # Add timber_log attributes to the timber_log object, and set them to NA
   keys <- list(
      "system_info",
      "session_info",
      "namespace",
      "warnings",
      "errors",
      "start_time",
      "end_time",
      "run_time",
      "file_path",
      "user",
      "masked_functions",
      "used_packages",
      "log_name",
      "log_path"
   )

   for (key in 1:length(keys)){
      timber_log[[keys[[key]]]] <- NA
   }

   # Set some tiber_log attributes
   # TBD

   return(timber_log)
}
