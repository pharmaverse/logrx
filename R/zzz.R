
# Create the log environment on load
.onLoad <- function(libname, pkgname) {
   log_init()
   approved_functions_init()
}
