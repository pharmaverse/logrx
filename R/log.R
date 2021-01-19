### Functions to initialise, configure, and manipulate timber_log

#' Initialises the timber_log object
#'
#' @return timber_log object as an empty named list
#' @export
#'
#' @examples
#' log_init()
#'
log_init <- function(){
   timber_log <- list()

   return(timber_log)
}

#' Configures the timber_log object
#'
#' @return timber_log object as a named list of attributes
#' @export
#'
#' @examples
#' log_config()
#'
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

   # Set some timber_log attributes
   # TBD

   return(timber_log)
}


#' Adds values to existing named elements in the timber_log object
#'
#' @param el_key the key of the element in timber_log to be updated
#' @param el_value the value to be added to the timber_log element
#'
#' @return
#' @export
#'
#' @examples
#' set_log_element("user", Sys.info()[["user"]])
set_log_element <- function(el_key, el_value){
   # check if key is currently in the timber_log_object
   if(!el_key %in% names(timber_log)) {
      stop("el_key provided must already exist in timber_log object")
   }

   # check if element is currently not empty
   if (!is.na(timber_log[el_key])) {
      stop("timber_log element can not already have a value")
   }

   # assign element value to specified element key
   timber_log[el_key] <- el_value
}


