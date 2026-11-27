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

### 2. Build your approved list

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

### 3. Save as `approved.rds` or `approved.yaml`

You can use the `file` argument in
[`build_approved()`](https://pharmaverse.github.io/logrx/dev/reference/build_approved.md)
to save the approved list to a file. Both `.rds` and `.yaml` / `.yml`
formats are supported.

``` r
dir <- tempdir()

# save as rds
build_approved(approved_pkgs, file = file.path(dir, "approved.rds"))

# save as yaml
build_approved(approved_pkgs, file = file.path(dir, "approved.yaml"))
```

When writing a YAML file, package keys are written with the `package:`
prefix by default (e.g. `package:base`). Both the prefixed form
(`package:base`) and the bare form (`base`) are accepted when reading
the file back, so either style can be used when editing the YAML by
hand.

For example, both of these are valid YAML approved lists:

``` yaml
# bare package names
base: "_all_"
dplyr: ["mutate", "filter", "select", "summarize", "group_by", "arrange"]
```

``` yaml
# package: prefix
package:base:
  - _all_
package:dplyr:
  - mutate
  - filter
  - select
  - summarize
  - group_by
  - arrange
```

### 4. Update the `logrx.approved` option

Update the `logrx.approved` option to point to your approved list file.
We recommend setting this in your `.Rprofile`.

``` r
# point to rds
options(log.rx.approved = file.path(dir, "approved.rds"))

# or point to yaml
options(log.rx.approved = file.path(dir, "approved.yaml"))
```

### 5. You’re done. Let’s axecute!

`logrx` will take it from there. When each program is executed, packages
and functions will be compared to `approved.rds` and if any unapproved
use is found, it will be logged within the “Unapproved Package and
Functions” section of the log file.

## Stopping Execution on Unapproved Packages

By default,
[`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
will log unapproved package use *after* the script runs. If you want to
prevent execution entirely when unapproved packages are detected, use
`error_on_unapproved = TRUE`.

This performs a **pre-execution check** by parsing the script’s AST
(Abstract Syntax Tree) to identify packages referenced via
[`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html), or `pkg::fun()`
namespacing, before any code is run. If any of those packages are not in
your `approved.rds` or `approved.yml`, execution is stopped and a log is
written capturing the unapproved packages found.

``` r
axecute("mpg.R", error_on_unapproved = TRUE)
```

This is useful in validated environments where running a script with
unapproved dependencies is not acceptable, even transiently.

> **Note:** `error_on_unapproved` requires the `log.rx.approved` option
> to be set. If no approved list is configured, the pre-execution check
> is skipped and execution proceeds normally.

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
    #> -                                logrx Metadata                                -
    #> --------------------------------------------------------------------------------
    #> This log was generated using logrx 0.4.0.9001
    #> logrx package version: 0.4.0.9001
    #> logrx build: local
    #> logrx link to repository: https://github.com/pharmaverse/logrx
    #> --------------------------------------------------------------------------------
    #> -                          User and File Information                           -
    #> --------------------------------------------------------------------------------
    #> User: runner
    #> File Name: mpg.R
    #> File Path: /tmp/RtmpTHBWp6
    #> File HashSum: 59ba8c053689b13c2aa20b633807cce9e762d5a2
    #> --------------------------------------------------------------------------------
    #> -                             Session Information                              -
    #> --------------------------------------------------------------------------------
    #> ─ Session info ────────────────────────────────────────────────────────
    #> setting  value
    #> version R version 4.6.2 (2026-10-29)
    #> os Ubuntu 26.04.1 LTS
    #> system x86_64, linux-gnu
    #> ui X11
    #> language en
    #> collate C.UTF-8
    #> ctype C.UTF-8
    #> tz UTC
    #> date 2026-11-27
    #> pandoc 3.7.0.2 @ /usr/bin/ (via rmarkdown)
    #> quarto NA
    #> 
    #> ─ Packages ────────────────────────────────────────────────────────────
    #> package  * version  date
    #>    (UTC) lib source
    #> bslib 0.12.0 2026-08-04 [1] RSPM (R 4.6.0)
    #> cachem 1.1.0 2024-05-16 [1] RSPM (R 4.6.0)
    #> cli 3.6.6 2026-04-09 [1] RSPM (R 4.6.0)
    #> desc 1.4.3 2023-12-10 [1] RSPM (R 4.6.0)
    #> digest 0.6.39 2025-11-19 [1] RSPM (R 4.6.0)
    #> dplyr * 1.2.1 2026-04-03 [1] RSPM (R 4.6.0)
    #> evaluate 1.0.5 2025-08-27 [1] RSPM (R 4.6.0)
    #> fastmap 1.2.0 2024-05-15 [1] RSPM (R 4.6.0)
    #> fs 2.1.0 2026-04-18 [1] RSPM (R 4.6.0)
    #> generics 0.1.4 2025-05-09 [1] RSPM (R 4.6.0)
    #> glue 1.8.1 2026-04-17 [1] RSPM (R 4.6.0)
    #> htmltools 0.5.9 2025-12-04 [1] RSPM (R 4.6.0)
    #> jquerylib 0.1.4 2021-04-26 [1] RSPM (R 4.6.0)
    #> jsonlite 2.0.0 2025-03-27 [1] RSPM (R 4.6.0)
    #> knitr 1.52 2026-09-06 [1] RSPM (R 4.6.0)
    #> lifecycle 1.0.5 2026-01-08 [1] RSPM (R 4.6.0)
    #> logrx * 0.4.0.9001 2026-11-27 [1]
    #>    local
    #> magrittr 2.0.5 2026-04-04 [1] RSPM (R 4.6.0)
    #> otel 0.3.0 2026-11-07 [1] RSPM (R 4.6.0)
    #> pillar 1.11.1 2025-09-17 [1] RSPM (R 4.6.0)
    #> pkgconfig 2.0.3 2019-09-22 [1] RSPM (R 4.6.0)
    #> pkgdown 2.2.1 2026-07-07 [1] RSPM (R 4.6.0)
    #> purrr 1.2.2 2026-04-10 [1] RSPM (R 4.6.0)
    #> R6 2.6.1 2025-02-15 [1] RSPM (R 4.6.0)
    #> ragg 1.5.2 2026-03-23 [1] RSPM (R 4.6.0)
    #> rlang 1.3.0 2026-07-05 [1] RSPM (R 4.6.0)
    #> rmarkdown 2.32 2026-09-01 [1] RSPM (R 4.6.0)
    #> sass 0.5.0 2026-11-20 [1] RSPM (R 4.6.0)
    #> sessioninfo 1.2.4 2026-06-04 [1] RSPM (R 4.6.0)
    #> systemfonts 1.3.4 2026-11-21 [1] RSPM (R 4.6.0)
    #> textshaping 1.1.0 2026-11-20 [1] RSPM (R 4.6.0)
    #> tibble 3.3.1 2026-01-11 [1] RSPM (R 4.6.0)
    #> tidyr 1.3.2 2025-12-19 [1] RSPM (R 4.6.0)
    #> tidyselect 1.2.1 2024-03-11 [1] RSPM (R 4.6.0)
    #> utf8 1.2.6 2025-06-08 [1] RSPM (R 4.6.0)
    #> vctrs 0.7.3 2026-04-11 [1] RSPM (R 4.6.0)
    #> withr 3.0.3 2026-06-19 [1] RSPM (R 4.6.0)
    #> xfun 0.62 2026-10-16 [1] RSPM (R 4.6.0)
    #> yaml 2.3.12 2025-12-10 [1] RSPM (R 4.6.0)
    #> 
    #>  [1] /home/runner/work/_temp/Library
    #>  [2] /opt/R/4.6.2/lib/R/library
    #> * ── Packages attached to the search path.
    #> 
    #> ─ External software ───────────────────────────────────────────────────
    #> setting  value
    #> cairo 1.18.4
    #> cairoFT
    #> pango 1.57.0
    #> png 1.6.57
    #> jpeg 8.0
    #> tiff LIBTIFF, Version 4.7.0
    #> tcl 8.6
    #> curl 8.18.0
    #> zlib 1.3.1
    #> bzlib 1.0.8, 13-Jul-2019
    #> xz 5.8.3
    #> deflate 1.23
    #> zstd 1.5.7
    #> PCRE 10.46 2025-08-27
    #> ICU 78.2
    #> TRE TRE 0.8.0 R_fixes (BSD)
    #> iconv glibc 2.43
    #> readline 8.3
    #> BLAS /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3
    #> lapack /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.32.so
    #> lapack_version 3.12.0
    #> 
    #> ─ Python configuration ────────────────────────────────────────────────
    #> Python is not available
    #> 
    #> ───────────────────────────────────────────────────────────────────────
    #> --------------------------------------------------------------------------------
    #> -                               Masked Functions                               -
    #> --------------------------------------------------------------------------------
    #> function `text` from {package:graphics} by .GlobalEnv
    #> function `dir` from {package:base} by .GlobalEnv
    #> function `filter` from {package:stats} by package:dplyr
    #> function `lag` from {package:stats} by package:dplyr
    #> function `intersect` from {package:base} by package:dplyr
    #> function `setdiff` from {package:base} by package:dplyr
    #> function `setequal` from {package:base} by package:dplyr
    #> function `union` from {package:base} by package:dplyr
    #> function `plot` from {package:base} by package:graphics
    #> function `body<-` from {package:base} by package:methods
    #> function `kronecker` from {package:base} by package:methods
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
    #> --------------------------------------------------------------------------------
    #> -                         Program Run Time Information                         -
    #> --------------------------------------------------------------------------------
    #> Start time: 2026-11-27 17:56:18 UTC
    #> End time: 2026-11-27 17:56:18 UTC
    #> Run time: 0 seconds
    #> --------------------------------------------------------------------------------
    #> -                             Errors and Warnings                              -
    #> --------------------------------------------------------------------------------
    #> Errors:
    #>  
    #> 
    #> Warnings:
    #>  
    #> --------------------------------------------------------------------------------
    #> -                         Messages, Output, and Result                         -
    #> --------------------------------------------------------------------------------
    #> Messages:
    #>  
    #> Output:
    #>  
    #> 
    #> Result:
    #>  # A tibble: 1 × 4
    #>    label      `4`   `6`   `8`
    #>    <chr>    <dbl> <dbl> <dbl>
    #>  1 Mean MPG  26.7  19.7  15.1
    #> --------------------------------------------------------------------------------
    #> -                               Log Output File                                -
    #> --------------------------------------------------------------------------------
    #> Log name: mpg.log
    #> Log path: /tmp/RtmpTHBWp6

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
