### Functions to interact with elements in the timber.log environment

#' Adds values to existing named elements in the timber.log environment
#'
#' @param el_key the key of the element in timber.log to be updated
#' @param el_value the value to be added to the timber.log element
#'
#' @return Nothing
#' @export
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
get_log_element <- function(el_key){
   if (!(el_key %in% names(getOption('timber.log')))) {
      stop("element key provided must already exist in timber.log")
   }

   # assign element value to specified element key
   el_value <- getOption('timber.log')[[el_key]]

   # return value to user
   return(el_value)
}


#' Set the log name and path:
#' 1. As the name and path if specified
#' 2. As the file name and path if specified
#' 3. As timber_log.log and . if none of the above are specified
#'
#' @param log_name The log name
#' @param log_path The log path
#'
#' @return Nothing
#' @export
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
            set_log_element("log_name", "timber_log.log")
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


#' Safely run an R script and record results, outputs, messages, errors, warnings
#'
#' @param file_name Function to run
#'
#' @return Nothing
#' @export
#' @importFrom purrr quietly safely discard
#' @importFrom stringr str_detect
#'
run_safely_n_quietly <- function(file_name) {
   retfun <- purrr::quietly(purrr::safely(source, quiet = FALSE))
   ret <- retfun(file_name, local = TRUE)

   set_log_element("warnings", ret$warnings)
   set_log_element("errors", ret$result$error)
}
