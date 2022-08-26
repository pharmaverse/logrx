### Functions to initialise, configure, cleanup, and write the log.rx environment

#' Initialises the log.rx environment
#'
#' @return Nothing
#' @export
#'
#'
log_init <- function(){
   log.rx <- new.env()

   if(!('log.rx' %in% names(options()))) {
      options('log.rx' = log.rx)
   }
}

#' Initialises the logrx.approved option
#'
#' Defaults to working directory. This should point to the location of the
#' dataframe storing approved packages and functions.
#'
#' See ?approved for an example dataframe.
#'
#' @return Nothing
#' @export
#'
#'
approved_functions_init <- function(){
   if(!('logrx.approved' %in% names(options()))) {
      options('logrx.approved' = './approved.rds')
   }
}

#' Initialises the logrx.lint option
lint_init <- function(){
   if(!('logrx.lint' %in% names(options()))) {
      options('logrx.lint' = FALSE)
   }
}


#' Configures the log.rx environment
#'
#' @param file File path of file being run, optional
#' @param log_name The log name
#' @param log_path The log path
#'
#' @return Nothing
#' @export
#'
log_config <- function(file = NA, log_name = NA, log_path = NA){
   # If the log.rx environment is not NULL or empty, warn the user
   if (!is.null(getOption("log.rx"))) {
      if (!(identical(ls(getOption("log.rx")), character(0)))) {
         stop("a log.rx environment already exists")}
   }

   # Initialise log.rx environment
   # This should already be done onLoad but redundant here
   log_init()

   # list of attributes to add to the log
   keys <- list(
      "metadata",
      "session_info",
      "warnings",
      "errors",
      "messages",
      "result",
      "stream",
      "start_time",
      "end_time",
      "run_time",
      "file_name",
      "file_path",
      "user",
      "masked_functions",
      "used_packages_functions",
      "unapproved_packages_functions",
      "lint_results",
      "log_name",
      "log_path")

   # Add attributes to the log.rx environment, and set them to NA
   for (key in 1:length(keys)){
      assign(keys[[key]], NA, envir = getOption('log.rx'))
   }

   # Set some logrx_log attributes
   # Metadata
   set_log_element("metadata", get_logrx_metadata())
   # Masked Functions
   set_log_element("masked_functions", get_masked_functions())
   # Source file path and name
   try({
      set_log_element("file_path", dirname(get_file_path(file)))
      set_log_element("file_name", basename(get_file_path(file)))
      }, silent = TRUE)
   # User
   set_log_element("user", Sys.info()[["user"]])
   # Start time
   set_log_element("start_time", strftime(Sys.time(), usetz = TRUE))
   # log name and path
   set_log_name_path(log_name, log_path)
   # lint results
   set_log_element("lint_results", get_lint_results(file))
}

#' Cleans up log and does checks against elements
#'
#' @return List of non-NA elements and their value in log.rx environment
#' @export
#'
log_cleanup <- function() {
   # check the log.rx environment exists
   if (!('log.rx' %in% names(options()))) {
      stop("environment log.rx must exist")
   }

   # get all names of elements in the log
   log_env <- getOption('log.rx')
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


#' Write the formatted log.rx to a file
#'
#' @param file File path of file being run
#' @param remove_log_object Should the log object be removed after writing,
#'   defaults to TRUE
#' @param to_report toggle for optional reporting objects, additional
#'   information in \code{\link{axecute}}
#'
#'
#' @return Nothing
#' @export
#'
log_write <- function(file = NA,
                      remove_log_object = TRUE,
                      to_report = c("messages", "output", "result")){
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
                           write_log_header("logrx Metadata"),
                           write_metadata())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "metadata"]
   }

   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("User and File Information"),
                        write_log_element("user", "User: "),
                        write_file_name_path())


   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Session Information"),
                        write_session_info())

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

   if (file.exists(getOption("logrx.approved"))) {
      approved_functions <- readRDS(getOption("logrx.approved"))
      unapproved_functions <- get_unapproved_use(used_functions, approved_functions)
      set_log_element("unapproved_packages_functions", unapproved_functions)

      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Unapproved Package and Functions"),
                           write_unapproved_functions())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "unapproved_packages_functions"]
   }

   if ("lint_results" %in% names(log_cleanup())) {
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Linter Results"),
                           write_lint_results())

      cleaned_log <- cleaned_log[!(names(cleaned_log)) %in% "lint_results"]
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

   if (any(c("messages", "result") %in% to_report)){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Messages and Result"))
   }

   if ("messages" %in% to_report){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_messages())
   }
   if ("result" %in% to_report){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_result())
   }
   if ("stream" %in% to_report){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Log Stream"),
                           write_stream())
   }

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


#' Remove the log.rx environment by setting options("log.rx") to NULL
#'
#' @return Nothing
#' @export
#'
log_remove <- function() {
   if (!is.null(getOption("log.rx"))) {
      options("log.rx" = NULL)
   }
}
