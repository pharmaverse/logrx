# bring in timber
library(timber)

# configure the log
log_config()

# write log
log_write()

# check the log elements
for (i in 1:length(names(getOption('timber.log')))) {
   name <- names(getOption('timber.log'))[i]
   element <- getOption('timber.log')[[name]]
   cat(paste0(name, ': ', element, '\n'))
}

