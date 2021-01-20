### Functions to initialise, configure, and manipulate the timber.log environment

#' Initialises the timber.log environment
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_init()
#'
log_init <- function(){
   timber.log <- new.env()

   if(!('timber.log' %in% names(options()))) {
      options('timber.log' = timber.log)
   }

   invisible()
}

#' Configures the timber.log environment
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_config()
#'
log_config <- function(){
   # Initialise timber.log environment
   # This should already be done onLoad but redundant here
   log_init()

   # list of attributes to add to the log
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
      "log_path")

   # Add attributes to the timber.log environment, and set them to NA
   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('timber.log'))
   }

   # Set some timber_log attributes
   # TBD
}


#' Adds values to existing named elements in the timber.log environment
#'
#' @param el_key the key of the element in timber.log to be updated
#' @param el_value the value to be added to the timber.log element
#'
#' @return Nothing
#' @export
#'
#' @examples
#' set_log_element("user", Sys.info()[["user"]])
#'
set_log_element <- function(el_key, el_value){
   # check if key is currently in the timber.log environment
   if (!(el_key %in% names(getOption('timber.log')))) {
      stop("element key provided must already exist in timber.log")
   }

   # check if element is currently not empty
   if (!is.na(getOption('timber.log')[[el_key]])) {
      stop("element can not already have a value")
   }

   # assign element value to specified element key
   assign(el_key, el_value, envir = getOption('timber.log'))
}

