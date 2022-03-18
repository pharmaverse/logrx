
# Create the log environment on load
.onLoad <- function(libname, pkgname) {
   log_init()

   approved_functions_init()

   # This overwrite is to correctly build purrr adverb function
   # outlined in purrr best practices for exporting adverb-wrapped functions
   safely_quietly <<- purrr::quietly(purrr::safely(run_file, quiet = FALSE))
}
