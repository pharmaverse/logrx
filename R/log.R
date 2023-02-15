### Functions to initialise, configure, cleanup, and write the log.rx environment

#' Initialisation of the log.rx environment
#'
#' `log_init()` initialises the log.rx environment
#'
#' @return Nothing
#' @export
#'
#' @examples
#' # Initialise the log.rx environment
#' log_init()
#'
#' # Remove the log.rx environment
#' log_remove()
log_init <- function(){
   log.rx <- new.env()

   if(!('log.rx' %in% names(options()))) {
      options('log.rx' = log.rx)
   }
}


#' Configuration of the log.rx environment
#'
#' `log_config()` initialises the log.rx environment, adds its attributes, and
#' sets them
#'
#' @param file String. Path to file executed. Optional
#' @param log_name String. Name of log file. Optional
#' @param log_path String. Path to log file. Optional
#'
#' @return Nothing
#' @export
#'
#' @examples
#' dir <- tempdir()
#' text <- 'print("Hello, Timberperson!")'
#' fileConn <- file(file.path(dir, "hello.R"))
#' writeLines(text, fileConn)
#' close(fileConn)
#'
#' file <- file.path(dir, "hello.R")
#'
#' # Initialise and configure the log.rx environment
#' log_config(file)
#'
#' # Run the script and record results, outputs, messages, errors, and warnings
#' logrx:::run_safely_loudly(file)
#'
#' # Write the log
#' log_write(file)
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
      "output",
      "start_time",
      "end_time",
      "run_time",
      "file_name",
      "file_path",
      "user",
      "hash_sum",
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

#' Cleaning-up of log.rx object
#'
#' `log_cleanup()` compiles a list of non-NA log.rx attributes and their
#' values, and return it
#'
#' @return List of non-NA log.rx attributes and their values
#'
#' @noRd
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


#' Formatting and writing of the log.rx object to a log file
#'
#' `log_write()` gets and formats the content of the log.rx before writing it
#' to a log file
#'
#' @param file String. Path to file executed
#' @param include_rds Boolean. Option to export log object as Rds file
#' @param remove_log_object Boolean. Should the log object be removed after
#' writing the log file? Defaults to TRUE
#' @param to_report String vector. Objects to optionally report; additional
#' information in \code{\link{axecute}}
#'
#' @return Nothing
#' @export
#'
#' @examples
#' dir <- tempdir()
#' text <- 'print("Hello, Timberperson!")'
#' fileConn <- file(file.path(dir, "hello.R"))
#' writeLines(text, fileConn)
#' close(fileConn)
#'
#' file <- file.path(dir, "hello.R")
#'
#' # Initialise and configure the log.rx environment
#' log_config(file)
#'
#' # Run the script and record results, outputs, messages, errors, and warnings
#' logrx:::run_safely_loudly(file)
#'
#' # Write the log
#' log_write(file)
log_write <- function(file = NA,
                      remove_log_object = TRUE,
                      include_rds = FALSE,
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
                        write_file_name_path(),
                        write_hash_sum())


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

   if (file.exists(getOption("log.rx.approved"))) {
      approved_functions <- readRDS(getOption("log.rx.approved"))
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

   if (any(c("messages", "output", "result") %in% to_report)){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_log_header("Messages, Output, and Result"))
   }

   if ("messages" %in% to_report){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_messages())
   }
   if ("output" %in% to_report){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_output())
   }
   if ("result" %in% to_report){
      cleaned_log_vec <- c(cleaned_log_vec,
                           write_result())
   }

   cleaned_log_vec <- c(cleaned_log_vec,
                        write_log_header("Log Output File"),
                        write_log_element("log_name", "Log name: "),
                        write_log_element("log_path", "Log path: "))

   writeLines(cleaned_log_vec,
                 con = file.path(get_log_element("log_path"),
                                 get_log_element("log_name")))
   if (include_rds){
      rds_fields <- c(
         "end_time", "start_time", "run_time", "user", "hash_sum",
         "log_path", "log_name", "file_path", "file_name",
         "unapproved_packages_functions", "errors", "warnings"
      )
      log_options <- as.list(getOption('log.rx'))
      cleaned_log_list <- Map(
         function(i, x){
            if(x %in% c("messages", "output", "result")){
               if(x %in% to_report){
                  return(i)
               }
            } else if(x %in% c(names(log_cleanup()), rds_fields)){
               return(i)
            }
         },
         log_options,
         names(log_options)
      )
      cleaned_log_list$session_info <- session_info(info = "all")
      saveRDS(cleaned_log_list,
              file = file.path(
                 get_log_element("log_path"),
                 paste0(tools::file_path_sans_ext(
                    get_log_element("log_name")
                    ),".Rds")
                 )
              )
   }

   if (remove_log_object) {
      log_remove()
   }
}


#' log.rx object removal
#'
#' `log_remove()` removes the log.rx object by setting `options("log.rx")` to
#' NULL
#'
#' @return Nothing
#' @export
#'
#' @examples
#' # Initialise the log.rx environment
#' log_init()
#'
#' # Remove the log.rx environment
#' log_remove()
log_remove <- function() {
   if (!is.null(getOption("log.rx"))) {
      options("log.rx" = NULL)
   }
}
