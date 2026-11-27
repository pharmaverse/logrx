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

# For typical script execution, use axecute() to configure, run, and log the file
axecute(file)
#> [1] "Hello, Timberperson!"
```
