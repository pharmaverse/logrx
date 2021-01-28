### Functions to initialise, configure, cleanup, and write the timber.log environment

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
   # Start time
   set_log_element("start_time", Sys.time())
}

#' Cleans up log and does checks against elements
#' TODO add end time setting and runtime calculation
#'
#' @return List of non-NA elements in timber.log environment
#' @export
#'
#' @examples
#' log_cleanup()
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


#' Write the formatted timber.log to a file
#'
#' @param log_name Log file name
#' @param log_path Log file path
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_write()
#'
log_write <- function(log_name = "timber_log.log", log_path = "."){
   # Set end time and run time
   set_log_element("end_time", Sys.time())
   set_log_element("run_time",
                   get_log_element("end_time") - get_log_element("start_time"))

   cleaned_log <- log_cleanup()
   cleaned_log_vec <- c()

   if ("metadata" %in% names(log_cleanup())) {
      cleaned_log_vec <- write_metadata()

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "metadata"]
   }

   cleaned_log_vec <- c(cleaned_log_vec, write_log_element("user", "User: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("start_time", "Start time: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("end_time", "End time: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("run_time", "Run time: "))

   writeLines(cleaned_log_vec, con = file.path(log_path, log_name))
}
