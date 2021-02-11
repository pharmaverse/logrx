# general imports
library(tidyverse)

# log import and config
library(timber)
log_config()

# user code
dat <- group_by(mtcars, cyl) %>%
   summarise(n = n())

# set_log_name_path(log_name = "test.log", log_path = "./dev")

# write log
log_write()
