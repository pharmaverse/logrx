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
#' @param file File path of file being run, optional
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_config()
#'
log_config <- function(file = NA){
   # If the timber.log environment is not NULL or empty, warn the user
   if (!is.null(getOption("timber.log"))) {
      if (!(identical(ls(getOption("timber.log")), character(0)))) {
         stop("a timber.log environment already exists")}
   }

   # Initialise timber.log environment
   # This should already be done onLoad but redundant here
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "metadata",
      "session_info",
      "warnings",
      "errors",
      "start_time",
      "end_time",
      "run_time",
      "file_name",
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
   # Session Info
   set_log_element("session_info", get_session_info())
   # Masked Functions
   set_log_element("masked_functions", get_masked_functions())
   # Source file path and name
   set_log_element("file_path", dirname(get_file_path(file)))
   set_log_element("file_name", basename(get_file_path(file)))
   # User
   set_log_element("user", Sys.info()[["user"]])
   # Start time
   set_log_element("start_time", strftime(Sys.time(), usetz = TRUE))
}

#' Cleans up log and does checks against elements
#'
#' @return List of non-NA elements and their value in timber.log environment
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
log_write <- function(){
   # Set end time and run time
   set_log_element("end_time", strftime(Sys.time(), usetz = TRUE))
   set_log_element("run_time",
                   paste0(as.numeric(
                      difftime(
                         as.POSIXct(get_log_element("end_time")),
                         as.POSIXct(get_log_element("start_time")),
                         units = "secs")),
                      " seconds"))

   # Set log name and path
   set_log_name_path()

   cleaned_log <- log_cleanup()
   cleaned_log_vec <- c()

   if ("metadata" %in% names(log_cleanup())) {
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("timber Metadata"),
                           write_metadata())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "metadata"]
   }

   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("User Information"),
                        write_log_element("user", "User: "))


   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Session Information"),
                        write_log_element("session_info", ""))

   if ("masked_functions" %in% names(log_cleanup())) {
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Masked Functions"),
                           write_masked_functions())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "masked_functions"]
   }



   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("start_time", "Start time: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("end_time", "End time: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("run_time", "Run time: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Errors and Warnings"),
                        write_errors())
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_warnings())

   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("log_name", "Log name: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_element("log_path", "Log path: "))

   writeLines(cleaned_log_vec,
              con = file.path(get_log_element("log_path"),
                              get_log_element("log_name")))

   log_remove()
}


#' Remove the timber.log environment by setting options("timber.log") to NULL
#'
#' @return Nothing
#' @export
#'
#' @examples
#' log_remove()
#'
log_remove <- function() {
   if (!is.null(getOption("timber.log"))) {
      options("timber.log" = NULL)
   }
}
