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

   # Set some timber_log attributes
   # TBD

   return(timber_log)
}


#' set_log_element
#'
#' Adds values to existing named elements in the timber_log object
#'
#' @param el_key the key of the element in timber_log to be updated
#' @param el_value the value to be added to the timber_log element
#'
#' @examples
#' set_log_element("user", Sys.info()[["user"]])
set_log_element <- function(el_key, el_value){
   # check if key is currently in the timber_log_object
   assert_that(el_key %in% names(timber_log),
               msg = "el_key provided must already exist in timber_log object")

   # check if element is currently not null
   if (!is.na(timber_log[el_key])) {

   }


   # assign element value to specified element key
   timber_log[el_key] <- el_value
}
