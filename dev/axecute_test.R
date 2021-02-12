#imports
library(timber)

# test for sourcing a file
files <- c("no_log.R", "interactive_test.R")

for (i in 1:length(files)){
   file <- files[i]
   print(paste0("Running: ", file))
   axecute(file.path("./dev", file))
}

