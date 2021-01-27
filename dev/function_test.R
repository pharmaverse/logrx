# bring in timber
library(timber)

# configure the log
log_config()

# do a bunch of stuff here
for (i in 1:length(names(getOption('timber.log')))) {
   name <- names(getOption('timber.log'))[i]
   element <- getOption('timber.log')[[name]]
   cat(paste0(name, ': ', element, '\n'))
}

