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
   temp <- file()
   warnings <- character()
   wHandler <- function(w) {
      capture.output(writeLines(w$message, con = temp))
      warnings <<- c(warnings, w$message)
   }

   messages <- character()
   mHandler <- function(m) {
      capture.output(writeLines(m$message, con = temp))
      messages <<- c(messages, m$message)
   }

   sink(temp, append = TRUE, split = TRUE)
   on.exit({
      sink()
      close(temp)
   })

   result <- withCallingHandlers(
      code,
      warning = wHandler,
      message = mHandler
   )

   stream <<- paste(readLines(temp, warn = FALSE), collapse = "\n")

   list(
      result = result,
      warnings = warnings,
      messages = messages,
      stream = stream
   )
}
