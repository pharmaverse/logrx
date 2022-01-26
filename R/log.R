### Functions to initialise, configure, cleanup, and write the timber.log environment

#' Initialises the timber.log environment
#'
#' @return Nothing
#' @export
#'
#'
log_init <- function(){
   timber.log <- new.env()

   if(!('timber.log' %in% names(options()))) {
      options('timber.log' = timber.log)
   }
}

#' Initialises the timber.approved option
#'
#' Defaults to working directory.  This should point to the location of the
#' dataframe storing approved packages and functions.
#'
#' See ?approved for an example dataframe.
#'
#' @return Nothing
#' @export
#'
#'
approved_functions_init <- function(){
   if(!('timber.approved' %in% names(options()))) {
      options('timber.approved' = './approved.rds')
   }
}


#' Configures the timber.log environment
#'
#' @param file File path of file being run, optional
#' @param log_name The log name
#' @param log_path The log path
#'
#' @return Nothing
#' @export
#'
log_config <- function(file = NA, log_name = NA, log_path = NA){
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
      "used_packages_functions",
      "unapproved_packages_functions",
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
   # log name and path
   set_log_name_path(log_name, log_path)
}

#' Cleans up log and does checks against elements
#'
#' @return List of non-NA elements and their value in timber.log environment
#' @export
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
#' @param file File path of file being run
#' @param remove_log_object Should the log object be removed after writing, defaults to TRUE
#'
#' @return Nothing
#' @export
#'
log_write <- function(file = NA, remove_log_object = TRUE){
   # Set end time and run time
   set_log_element("end_time", strftime(Sys.time(), usetz = TRUE))
   set_log_element("run_time",
                   paste0(as.numeric(
                      difftime(
                         as.POSIXct(get_log_element("end_time")),
                         as.POSIXct(get_log_element("start_time")),
                         units = "secs")),
                      " seconds"))

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

   used_functions <- get_used_functions(file)
   set_log_element("used_packages_functions", used_functions)

   if ("used_packages_functions" %in% names(log_cleanup())) {
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Used Package and Functions"),
                           write_used_functions())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "used_packages_functions"]

   }

   if (file.exists(getOption("timber.approved"))) {
      approved_functions <- readRDS(getOption("timber.approved"))
      unapproved_functions <- get_unapproved_use(used_functions, approved_functions)
      set_log_element("unapproved_packages_functions", unapproved_functions)

      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Unapproved Package and Functions"),
                           write_unapproved_functions())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "unapproved_packages_functions"]
   }



   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Program Run Time Information"),
                        write_log_element("start_time", "Start time: "),
                        write_log_element("end_time", "End time: "),
                        write_log_element("run_time", "Run time: "))
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Errors and Warnings"),
                        write_errors())
   cleaned_log_vec <- c(cleaned_log_vec,
                        write_warnings())

   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Log Output File"),
                        write_log_element("log_name", "Log name: "),
                        write_log_element("log_path", "Log path: "))

   writeLines(cleaned_log_vec,
              con = file.path(get_log_element("log_path"),
                              get_log_element("log_name")))
   if (remove_log_object) {
      log_remove()
   }
}


#' Remove the timber.log environment by setting options("timber.log") to NULL
#'
#' @return Nothing
#' @export
#'
log_remove <- function() {
   if (!is.null(getOption("timber.log"))) {
      options("timber.log" = NULL)
   }
}
