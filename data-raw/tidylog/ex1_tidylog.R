library(dplyr)
library(tidyr)
library(tibble)
library(tidylog)

data_wide <- us_rent_income %>%
   pivot_wider(
      names_from = variable,
      names_glue = "{variable}_{.value}",
      values_from = c(estimate, moe)) %>%
   arrange(desc(NAME))

get_states <- distinct(us_rent_income, NAME)

contrived_data <- bind_cols(get_states, 50) %>%
   rename(number = ...2 )

joined_data <- left_join(data_wide, contrived_data)
