library(dplyr)
library(magrittr)

mtcars %>%
  select(cyl, am) %>%
  arrange(cyl)
