library(haven)
library(magrittr)
library(Tplyr)

adae <- read_xpt("data-raw/risk_diff/adae.xpt")


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
