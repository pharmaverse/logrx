# Configuration of the log.rx environment

`log_config()` initialises the log.rx environment, adds its attributes,
and sets them

## Usage

``` r
log_config(file = NA, log_name = NA, log_path = NA, extra_info = NA)
```

## Arguments

- file:

  String. Path to file executed. Optional

- log_name:

  String. Name of log file. Optional

- log_path:

  String. Path to log file. Optional

- extra_info:

  List. Objects to add on to end of log in a special extra info section.
  Optional

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
