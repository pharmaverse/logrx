### Functions to interact with elements in the log.rx environment

#' Adds values to existing named elements in the log.rx environment
#'
#' @param el_key the key of the element in log.rx to be updated
#' @param el_value the value to be added to the log.rx element
#'
#' @return Nothing
#'
#' @noRd
#'
set_log_element <- function(el_key, el_value){
   # check if key is currently in the log.rx environment
   if (!(el_key %in% names(getOption('log.rx')))) {
      stop("element key provided must already exist in log.rx")
   }

   # check if element is currently not empty
   if (!is.na(getOption('log.rx')[[el_key]])) {
      stop("element can not already have a value")
   }

   # assign element value to specified element key
   assign(el_key, el_value, envir = getOption('log.rx'))
}


#' Gets the value of a named element in the log.rx environment
#'
#' @param el_key the key of the element in log.rx to be fetched
#'
#' @return Value of corresponding element from log.rx environment
#'
#' @noRd
#'
get_log_element <- function(el_key){
   if (!(el_key %in% names(getOption('log.rx')))) {
      stop("element key provided must already exist in log.rx")
   }

   # assign element value to specified element key
   el_value <- getOption('log.rx')[[el_key]]

   # return value to user
   return(el_value)
}


#' Set the log name and path
#'
#' \enumerate{
#'   \item As the name and path if supplied
#'   \item As the file name with .log extension and path if specified or
#'     if they can be determined by the function
#'   \item As logrx_log.log and . if none of the above are specified
#' }
#'
#' @param log_name The log name
#' @param log_path The log path
#'
#' @return Nothing
#'
#' @noRd
#'
set_log_name_path <- function(log_name = NA, log_path = NA) {
   # If log_name was previously assigned, generate warning
   # else assign log_name
   log_name_value <- get_log_element("log_name")
   if (!is.na(log_name_value)) {
      warning("log_name already assigned, will not be overwritten")
   } else {
      if (!is.na(log_name)) {
         set_log_element("log_name", log_name)
      } else {
         file_name <- get_log_element("file_name")

         if (!is.na(file_name)) {
            set_log_element("log_name",
                            sub(pattern = "(?<=\\.).*",
                                replacement = "log",
                                x = file_name,
                                perl = TRUE))
         } else {
            set_log_element("log_name", "logrx_log.log")
         }
      }
   }

   # If log_path was previously assigned, generate warning
   # else assign log_path
   log_path_value <- get_log_element("log_path")
   if (!is.na(log_path_value)) {
      warning("log_path already assigned, will not be overwritten")
   } else {
      if (!is.na(log_path)) {
         set_log_element("log_path", log_path)
      } else {
         file_path <- get_log_element("file_path")

         if (!is.na(file_path)) {
            set_log_element("log_path", file_path)
         } else {
            set_log_element("log_path", ".")
         }
      }
   }
}

#' Dummy function for running a file safely
#' @noRd
run_safely <- function(file) "dummy"

#' Is this a R Markdown file#'
#' @param file String. Path to file to execute
#' @noRd
is_rmarkdown <- function(file) {
   grepl("*.Rmd$", file, ignore.case = TRUE)
}

#' Dummy function for running a file
#' @noRd
run_file <- function(file){
   if (is.null(getOption("log.rx.exec.env"))){
      exec_env <- new.env(parent=globalenv())
   } else{
      exec_env <- getOption("log.rx.exec.env")
   }

   if (is_rmarkdown(file)) {
      rmarkdown::render(file, envir = exec_env)
   } else (
      source(file, local = exec_env)
   )

}

#' Safely run an R script and record results, outputs, messages, errors, warnings
#'
#' @importFrom purrr safely discard
#' @importFrom stringr str_starts
#'
#' @param file File to run
#'
#' @return Nothing
#'
#' @noRd
#'
run_safely_loudly <- function(file) {
   ret <- loudly(run_safely(file))
   set_log_element("messages", discard(ret$messages, ~ str_starts(.x, "Error")))
   set_log_element("output", ret$output)
   set_log_element("result", ret$result$result)
   set_log_element("warnings", ret$warnings)
   set_log_element("errors", ret$result$error)
   set_log_element("hash_sum", digest::sha1(readLines(file)))

   # Session Info
   set_log_element("session_info", get_session_info())
}

