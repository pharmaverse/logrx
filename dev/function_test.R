# bring in timber
library(timber)

# configure the log
log_config()

# add user
user <- Sys.info()[["user"]]
set_log_element("user", user)

# do a bunch of stuff here





log_write()
