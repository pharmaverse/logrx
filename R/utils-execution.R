#' Capture errors, warnings, messages, and a stream output from
#' executing R code
#'
#' @importFrom purrr as_mapper
#'
#' @param code All code to be run loudly
#'
#' @return Wrapped function returns a list with components
#'   `result`, `stream`, `messages` and `warnings`.
#'
#' @export
loudly <- function(code){
   warnings <- character()
   wHandler <- function(w) {
      warnings <<- c(warnings, w$message)
   }

   messages <- character()
   mHandler <- function(m) {
      messages <<- c(messages, m$message)
   }

   temp <- file()
   sink(temp, split = TRUE)
   on.exit({
      sink()
      close(temp)
   })

   result <- withCallingHandlers(
      code,
      warning = wHandler,
      message = mHandler
   )

   output <- paste0(readLines(temp, warn = FALSE), collapse = "\n")

   list(
      result = result,
      output = output,
      warnings = warnings,
      messages = messages
   )
}
