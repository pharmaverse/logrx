# Default options
logrx_default_options <- list(
  # execution environment
  log.rx.exec.env = NULL,

  # Initializes the logrx.lint option
  log.rx.lint = FALSE,

  # Initialises the logrx.approved option
  log.rx.approved = "./approved.rds"
)

# Implement on load processes
.onLoad <- function(libname, pkgname) {
  # init functions for other logrx functionality
  log_init()

  # This overwrite is to correctly build purrr adverb function
  # outlined in purrr best practices for exporting adverb-wrapped functions
  run_safely <<- purrr::safely(run_file, quiet = FALSE)

  # set warn to 1 to have warnings be output as they happen
  options(warn = 1)

  # store existing options
  op <- options()

  # Set any options that haven't been set
  toset <- !(names(logrx_default_options) %in% names(op))
  if (any(toset)) options(logrx_default_options[toset])
}
