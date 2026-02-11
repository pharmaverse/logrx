# Build approved packages and functions tibble

A utility function to help you build your approved packages and
functions list. This can be used by logrx to log unapproved use of
packages and functions.

## Usage

``` r
build_approved(pkg_list, file = NULL)
```

## Arguments

- pkg_list:

  Named list of character vectors:

  - Name is the package name

  - Value is a character vector of approved functions or 'All'

- file:

  String. Name of file where the approved tibble will be written to. If
  not specified, the tibble is returned

  Default: NULL

  Permitted Files: .RDS

## Value

Tibble with two columns (library, function) and one row per function

## Details

For more details see the vignette:
[`vignette("approved", package = "logrx")`](https://pharmaverse.github.io/logrx/dev/articles/approved.md)

## Examples

``` r
approved_pkgs <- list(
  base = c("library", "mean"),
  dplyr = "All"
)

# build and return
build_approved(approved_pkgs)
#> # A tibble: 286 × 2
#>    function_name library      
#>    <chr>         <chr>        
#>  1 library       package:base 
#>  2 mean          package:base 
#>  3 db_drop_table package:dplyr
#>  4 group_split   package:dplyr
#>  5 mutate_at     package:dplyr
#>  6 db_commit     package:dplyr
#>  7 tally_        package:dplyr
#>  8 dense_rank    package:dplyr
#>  9 summarise_all package:dplyr
#> 10 any_vars      package:dplyr
#> # ℹ 276 more rows

# build and save
dir <- tempdir()
build_approved(approved_pkgs, file.path(dir, "approved.rds"))
```
