
<!-- README.md is generated from README.Rmd. Please edit that file -->

# timber <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R build
status](https://github.com/atorus-research/timber/workflows/R-CMD-check/badge.svg)](https://github.com/atorus-research/timber/actions?workflow=R-CMD-check)
[<img src="https://img.shields.io/codecov/c/github/atorus-research/timber">](https://codecov.io/gh/atorus-research/timber)
[<img src="https://img.shields.io/badge/License-MIT-blue.svg">](https://github.com/atorus-research/timber/blob/master/LICENSE.md)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental-1)
<!-- badges: end -->

The goal of timber is to facilitate logging in a clinical environment
with the goal of making code easily traceable and reproducible.

## Installation

This package is still in development and currently is only available
from GitHub. To install the development version run the following:

``` r
devtools::install_github("atorus-research/timber")
```

## What is `timber` ?

Simple! The timber package makes a log file for a R script! Below we
have a short gif of making a log file for a `adsl.R` script. More
details on the parts of the log script are found in the [Get
Started](https://atorus-research.github.io/timber/articles/timber.html)
and we have several vignettes that show specific use cases for using the
package.

<img src="man/figures/timber.gif" title="Caption for this figure 1" alt="Caption for this figure 1" width="100%" style="display: block; margin: auto;" />

While many packages to facilitate the logging of code already exist in
the R ecosystem, it is hard to find a solution that works well for
clinical programming applications. Many logging implementations are more
implicit and rely on user input to create the log for the execution of a
script. While this is useful for logging specific events of an
application, in clinical programming a log has a set purpose.

`timber` is built around the concept of creating a log for the execution
of an R script that provides an overview of what happened as well as the
environment that it happened in. We set out to create a flexible logging
utility that could provide the necessary information to anyone reviewing
the code execution so they can recreate the execution environment and
run the code for themselves. `timber` is purpose-built for the needs of
clinical programming to capture necessary elements of the environment
and execution to be able to create a complete view of the program
execution.

## Using timber

`timber` can be used in a file or through scripting. More information on
how to use `timber` can be found in [Get
Started](https://atorus-research.github.io/timber/articles/timber.html).
