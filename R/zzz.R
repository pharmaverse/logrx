
# Create the log environment on load
.onLoad <- function(libname, pkgname) {
   log_init()

   safely_quietly <<- purrr::quietly(purrr::safely(run_file, quiet = FALSE))
}
