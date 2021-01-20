### Functions to initialise, configure, and manipulate timber_log

#' Initialises the timber_log object
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_init()
#'
log_init <- function(){
   timber.log <- new.env()
   timber.log$timber_log <- list()

   if(!('timber.log' %in% names(options()))) {
      options('timber.log' = timber.log)
   }

   invisible()
}

#' Configures the timber_log object within the timber.log environment
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_config()
#'
log_config <- function(){
   # Initialise timber.log environment and timber_log object inside environment
   # This should already be done onLoad but redundant here
   log_init()

   # Evaluate all of this inside the timber.log environment
   evalq({
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

      # Add timber_log attributes to the timber_log object, and set them to NA
      for (key in 1:length(keys)){
         timber_log[[keys[[key]]]] <- NA
      }

      # Set some timber_log attributes
      # TBD

      # cleanup environment
      rm(keys)
      rm(key)

   }, envir = getOption('timber.log'))
}


#' Adds values to existing named elements in the timber_log object
#'
#' @param el_key the key of the element in timber_log to be updated
#' @param el_value the value to be added to the timber_log element
#'
#' @return Nothing
#' @export
#'
#' @examples
#' set_log_element("user", Sys.info()[["user"]])
set_log_element <- function(el_key, el_value){
   evalq({
      # check if key is currently in the timber_log_object
      if (!(el_key %in% names(timber_log))) {
         stop("el_key provided must already exist in timber_log object")
      }

      # check if element is currently not empty
      if (!is.na(timber_log[el_key])) {
         stop("timber_log element can not already have a value")
      }

      # assign element value to specified element key
      timber_log[el_key] <- el_value
   }, envir = getOption('timber.log'))
}


