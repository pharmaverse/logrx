# Formatting and writing of the log.rx object to a log file

`log_write()` gets and formats the content of the log.rx before writing
it to a log file

## Usage

``` r
log_write(
  file = NA,
  remove_log_object = TRUE,
  show_repo_url = FALSE,
  include_rds = FALSE,
  extra_info = NA,
  to_report = c("messages", "output", "result")
)
```

## Arguments

- file:

  String. Path to file executed

- remove_log_object:

  Boolean. Should the log object be removed after writing the log file?
  Defaults to TRUE

- show_repo_url:

  Boolean. Should the repo URLs be reported Defaults to FALSE

- include_rds:

  Boolean. Option to export log object as Rds file. Defaults to FALSE

- extra_info:

  List. Objects to add on to end of log in a special extra info section.
  Optional

- to_report:

  String vector. Objects to optionally report; additional information in
  [`axecute`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)

## Value

Nothing

## Examples

``` r
dir <- tempdir()
text <- 'print("Hello, Timberperson!")'
fileConn <- file(file.path(dir, "hello.R"))
writeLines(text, fileConn)
close(fileConn)

file <- file.path(dir, "hello.R")

# Initialise and configure the log.rx environment
log_config(file)

# Run the script and record results, outputs, messages, errors, and warnings
logrx:::run_safely_loudly(file)
#> [1] "Hello, Timberperson!"

# Write the log
log_write(file)
```
