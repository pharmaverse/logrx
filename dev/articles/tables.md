# Logging for Summary Display Table

In this section, we write a script to create a risk-difference table
from the [PHUSE Test Data Factory ADaM
dataset](https://github.com/phuse-org/TestDataFactory), using the
[`Typlr`](https://atorus-research.github.io/Tplyr/) package. We use the
`logrx` package to generate a log upon the script execution for
reproducibility and traceability.

## 1. Load the packages

First, we load the packages that we will use to generate the
risk-difference table.

``` r
library(haven)
library(magrittr)
library(Tplyr)
```

## 2. Load the data

Then we load the Adverse Event Analysis Dataset from the [PHUSE Test
Data Factory ADaM
dataset](https://github.com/phuse-org/TestDataFactory). The data are
stored in a transport file format and we use the
[`haven`](https://haven.tidyverse.org) package to load them.

``` r
adae <- read_xpt("adae.xpt")
```

## 3. Build the risk-difference table

Finally, we write a script (`risk_diff.R`) to build the risk-difference
table, using the [`Typlr`](https://atorus-research.github.io/Tplyr/)
package.

``` r
t <- tplyr_table(adae, TRTA) %>%
  add_layer(
    group_count(RACE) %>%
      set_distinct_by(USUBJID) %>%
      add_risk_diff(
        c("Xanomeline High Dose", "Placebo"),
        c("Xanomeline Low Dose", "Placebo")
      )
  )

build(t)
```

## 4. Axecute the script

We will `axecute` the following script.

``` r
axecute(file.path(dir, "risk_diff.R"))
```

Below is the log. Pay special attention to the Warnings and Result
sections.
