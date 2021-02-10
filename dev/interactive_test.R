# general imports
library(tidyverse)

# log import and config
library(timber)
log_config()

# user code
dat <- group_by(mtcars, cyl) %>%
   summarise(n = n())

# write log
log_write()
