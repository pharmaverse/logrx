# Logging Unapproved Package and Function Use

## Why would I use this feature?

The main use case of this feature is to support traceability of package
and function use within a validated environment.

Following the guidance outlined in [A Risk-Based Approach for Assessing
R Package Accuracy within a Validated
Infrastructure](https://www.pharmar.org/white-paper/), packages are
selected to support use cases, risk is assessed and mitigated, and
packages are included in your validated environment for use. **Section
4.3** of this paper nicely outlines the need to trace what is used and
the need to identify package and function use where risk was not
assessed for direct use within the validated environment.

  

> *4.3. Traceability*
>
> *“One of the core concepts presented [in this
> paper](https://www.pharmar.org/white-paper/) is that Imports are not
> typically loaded by users and need not therefore be directly
> risk-assessed. If adopting this risk-based approach then measures need
> to be taken to ensure that users do not directly load the Package
> Imports. It is suggested that this is handled mainly through process,
> although tools could be developed to check using sessionInfo or
> devtools::session_info that check the loaded packages against packages
> lists of Intended for Use and Imports. In any case the use of these
> tools within a standard, logged, workflow is highly recommended to
> ensure traceability of the work.”*

  

{logrx} provides you this tool! It even goes a step further by not just
logging the packages you’ve use, but it will log use at a function
level. This gives you the flexibility of assessing the risk of functions
and approving functions, rather than having to assess the risk of the
entire package for use within your validated environment.

  

## How do I use this feature?

### 1. Create a named list

The named list contains the functions approved for use for each package.
If all functions for a package are approved for use, list “All”.

``` r
approved_pkgs <- list(
  base = "mean",
  dplyr = "All"
)
approved_pkgs
#> $base
#> [1] "mean"
#> 
#> $dplyr
#> [1] "All"
```

### 2. Build `approved.rds`

Pass the named list through
[`build_approved()`](https://pharmaverse.github.io/logrx/dev/reference/build_approved.md)
to build your tibble. We create a temp directory to save this for
illustration.

``` r
build_approved(approved_pkgs)
#> # A tibble: 285 × 2
#>    function_name library      
#>    <chr>         <chr>        
#>  1 mean          package:base 
#>  2 db_drop_table package:dplyr
#>  3 group_split   package:dplyr
#>  4 mutate_at     package:dplyr
#>  5 db_commit     package:dplyr
#>  6 tally_        package:dplyr
#>  7 dense_rank    package:dplyr
#>  8 summarise_all package:dplyr
#>  9 any_vars      package:dplyr
#> 10 as.tbl        package:dplyr
#> # ℹ 275 more rows
```

### 3. Save as `approved.rds`

You can use the `file` argument in
[`build_approved()`](https://pharmaverse.github.io/logrx/dev/reference/build_approved.md)
to save `approved.rds` instead of returning the tibble.

``` r
dir <- tempdir()

build_approved(approved_pkgs, file = file.path(dir, "approved.rds"))
```

### 4. Update the `logrx.approved` option

Update the `logrx.approved` option to point to your `approved.rds`
location. We recommend setting this in your `.Rprofile`.

``` r
options(log.rx.approved = file.path(dir, "approved.rds"))
```

### 5. You’re done. Let’s axecute!

`logrx` will take it from there. When each program is executed, packages
and functions will be compared to `approved.rds` and if any unapproved
use is found, it will be logged within the “Unapproved Package and
Functions” section of the log file.

## Example

Let’s write a simple script summarizing mean mpg from mtcars. We save
this as `mpg.R` in the temporary directory `dir` and
[`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
it.

``` r
library(dplyr, warn.conflicts = FALSE)

results <- mtcars %>%
  group_by(cyl) %>%
  summarize(mean = mean(mpg)) %>%
  mutate(label = "Mean MPG")

results %>%
  tidyr::pivot_wider(names_from = cyl, values_from = mean, id_cols = label)
#> # A tibble: 1 × 4
#>   label      `4`   `6`   `8`
#>   <chr>    <dbl> <dbl> <dbl>
#> 1 Mean MPG  26.7  19.7  15.1
```

Here we have the log elements for “Used Package and Functions” and
“Unapproved Package and Functions”. We can see we used
[`library()`](https://rdrr.io/r/base/library.html) from `package:base`
and `pivot_wider` from `package:tidyr`. We did not include the base
library or tidyr functions in our approved list, so this has been
logged!

    #> --------------------------------------------------------------------------------
    #> -                          Used Package and Functions                          -
    #> --------------------------------------------------------------------------------
    #> {package:base} library, mean
    #> {package:dplyr} %>%, group_by, summarize, mutate
    #> {package:tidyr} pivot_wider
    #> --------------------------------------------------------------------------------
    #> -                       Unapproved Package and Functions                       -
    #> --------------------------------------------------------------------------------
    #> {package:base} library
    #> {package:tidyr} pivot_wider

## A Few Words of Caution

All packages should be attached at the top of the script to set a
consistent `?base::searchpaths()` throughout the entire script. This
will ensure the functions you use in your script are linked to the
correct package. A lint feature is available to test your scripts follow
this best practice.

Some functions are stored within a list, for example
`knitr::opts_chunck$get()` and `knitr::opts_current$get()`. We do not
currently identify [`get()`](https://rdrr.io/r/base/get.html) as a knitr
function since it is not exported.
