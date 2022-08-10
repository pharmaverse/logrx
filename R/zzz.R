
# Create the log environment on load
.onLoad <- function(libname, pkgname) {
   # init functions for other logrx functionality
   log_init()
   approved_functions_init()
   lint_init()

   # This overwrite is to correctly build purrr adverb function
   # outlined in purrr best practices for exporting adverb-wrapped functions
   run_safely <<- purrr::safely(run_file, quiet = FALSE)

   # set warn to 1 to have warnings be output as they happen
   options(warn = 1)
}
