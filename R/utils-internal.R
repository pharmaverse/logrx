# R/utils-internal.R
# Explicitly create a binding for 'interactive' in the logrx namespace.
# This allows testthat::with_mocked_bindings to find and replace it.
interactive <- base::interactive
