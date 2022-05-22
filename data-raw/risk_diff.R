library(haven)
library(magrittr)
library(Tplyr)

adae <- read_xpt("data-raw/adae.xpt")


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

timber::axecute("data-raw/risk_diff.R", log_path = "data-raw", remove_log_object = TRUE)
