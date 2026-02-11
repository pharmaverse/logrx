# Logging with {tidylog}

In this vignette, we will create a log with
[`axecute()`](https://pharmaverse.github.io/logrx/dev/reference/axecute.md)
while using the [tidylog](https://github.com/elbersb/tidylog/) package.
The goal of [tidylog](https://github.com/elbersb/tidylog/) is to provide
feedback about [dplyr](https://dplyr.tidyverse.org) and
[tidyr](https://tidyr.tidyverse.org) operations.
[tidylog](https://github.com/elbersb/tidylog/) provides simple wrapper
functions that provide feedback for almost all
[dplyr](https://dplyr.tidyverse.org) and
[tidyr](https://tidyr.tidyverse.org) functions, such as `filter`,
`mutate`, `select`, `full_join`, and `group_by`.

Using [logrx](https://pharmaverse.github.io/logrx/) we will now capture
this feedback on [dplyr](https://dplyr.tidyverse.org) and
[tidyr](https://tidyr.tidyverse.org) functions and place it into the
`Messages, Output, and Result` of our
[logrx](https://pharmaverse.github.io/logrx/) log file. We will look at
two cases in this vignette.

## Logging a simple script with `{logrx}` and `{tidylog}`

Below we have a simple script using the `us_rent_income` dataset. We
will explore just a few functions available in
[tidyr](https://tidyr.tidyverse.org) and
[dplyr](https://dplyr.tidyverse.org): `pivot_wider`, `arrange`,
`distinct`, `bind_cols` and `left_join`.

  

  

Using `axecute(ex1_tidylog.R)` we produce a log file. Below we snapshot
just the pertinent information for users interested in the
[tidylog](https://github.com/elbersb/tidylog/) feedback. This feedback
is placed by the [logrx](https://pharmaverse.github.io/logrx/) package
into the `Messages, Output, and Result` section of the log.

  

  

## Logging an ADSL script with `{logrx}` and `{tidylog}`

Now we use an R script that creates an `ADSL` dataset and get a log
file. This file was generated using the
[admiral](https://pharmaverse.github.io/admiral/) package for creating
ADaM datasets. We can quickly generate this file by using the following
command `admiral::use_ad_template("adsl")`.

Feel free to explore the script. The next section showcases the output
for the log file after we have used `axecute` on the script.

  

  

We only showcase the feedback in the script relevant to our discussion
on [tidylog](https://github.com/elbersb/tidylog/). Again, the
[logrx](https://pharmaverse.github.io/logrx/) package places feedback
from [tidylog](https://github.com/elbersb/tidylog/) into the
`Messages, Output, and Result`section of the log file.

  

  

Success!! We just `axecuted` two scripts using
[logrx](https://pharmaverse.github.io/logrx/) and
[tidylog](https://github.com/elbersb/tidylog/). Remember that feedback
from [tidylog](https://github.com/elbersb/tidylog/) is placed into the
`Messages, Output, and Result` section of the log file.
