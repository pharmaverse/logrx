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
      "metadata",
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
   # Metadata
   set_log_element("metadata", get_timber_metadata())
   # User
   set_log_element("user", Sys.info()[["user"]])
}

#' Cleans up log and does checks against elements
#' TODO add end time setting and runtime calculation
#'
#' @return List of non-NA elements in timber.log environment
#' @export
#'
#' @examples
#'
log_cleanup <- function() {
   # check the timber.log environment exists
   if (!('timber.log' %in% names(options()))) {
      stop("environment timber.log must exist")
   }

   # get all names of elements in the log
   log_env <- getOption('timber.log')
   el_names <- names(log_env)

   # check if element is not NA and add to list to return
   el_populated <- list()
   for (i in 1:length(el_names)) {
      el_name <- el_names[i]
      el_value <- get_log_element(el_names[i])
      if (!(anyNA(el_value))) {
         el_populated[[el_name]] <- el_value
      }
   }

   # return list of populated elements
   return(el_populated)
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


#' Gets the value of a named element in the timber.log environment
#'
#' @param el_key the key of the element in timber.log to be fetched
#'
#' @return Value of corresponding element from timber.log environment
#' @export
#'
#' @examples
#' get_log_element("user")
#'
get_log_element <- function(el_key){
   if (!(el_key %in% names(getOption('timber.log')))) {
      stop("element key provided must already exist in timber.log")
   }

   # assign element value to specified element key
   el_value <- getOption('timber.log')[[el_key]]

   # return value to user
   return(el_value)
}


#' Returns named list of timber metadata attributes
#'
#' @return Named list of timber package metadata attributes
#' @export
#'
#' @examples
#' get_timber_metadata()
#'
get_timber_metadata <- function(){
   session_info <- sessionInfo()

   timber_metadata <- list(
      version = session_info[["otherPkgs"]][["timber"]][["Version"]],
      license = session_info[["otherPkgs"]][["timber"]][["License"]],
      built = session_info[["otherPkgs"]][["timber"]][["Built"]],
      repository_link = NULL
   )

   return(timber_metadata)
}
