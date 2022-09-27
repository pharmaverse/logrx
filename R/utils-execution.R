#' Capture errors, warnings, messages, and a stream output from
#' executing R code
#'
#' @importFrom purrr as_mapper
#'
#' @param code All code to be run loudly
#' @param to_report toggle for optional reporting objects, additional
#'   information in \code{\link{axecute}}
#'
#' @return Wrapped function returns a list with components
#'   `result`, `output`, `stream`, `messages` and `warnings`.
#'
#' @export
loudly <- function(code, to_report){
   temp <- file()
   warnings <- character()
   wHandler <- function(w) {
      if("stream" %in% to_report){
         capture.output(writeLines(w$message, con = temp))
      }
      warnings <<- c(warnings, w$message)
   }

   messages <- character()
   mHandler <- function(m) {
      if("stream" %in% to_report){
         capture.output(writeLines(m$message, con = temp))
      }
      messages <<- c(messages, m$message)
   }


   if("stream" %in% to_report){
      sink(temp, append = TRUE, split = TRUE)
   }else{
      sink(temp, split = TRUE)
   }
   on.exit({
      sink()
      close(temp)
   })

   result <- withCallingHandlers(
      code,
      warning = wHandler,
      message = mHandler
   )
   if("stream" %in% to_report){
      stream <<- paste(readLines(temp, warn = FALSE), collapse = "\n")
      output <- NULL
   }else{
      output <- paste(readLines(temp, warn = FALSE), collapse = "\n")
      stream <- NULL
   }

   list(
      result = result,
      output = output,
      warnings = warnings,
      messages = messages,
      stream = stream
   )
}
