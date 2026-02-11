# Approved packages and functions

A dataset that stores approved packages and functions for use. Each row
contains a `library` and `function_name`. This dataset is used to
illustrate the data format to be stored in the log.rx.approved option.

## Usage

``` r
approved
```

## Format

A tibble with 6 rows and 2 variables:

- function_name:

  Name of the function

- library:

  Name of the package

## Examples

``` r
logrx::approved
#> # A tibble: 6 × 2
#>   function_name library      
#>   <chr>         <chr>        
#> 1 library       package:base 
#> 2 %>%           package:dplyr
#> 3 group_by      package:dplyr
#> 4 summarize     package:dplyr
#> 5 mean          package:base 
#> 6 pivot_wider   package:tidyr
```
