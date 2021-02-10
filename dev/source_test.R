#imports
library(evaluate)
library(timber)

# test for sourcing a file
files <- c("interactive_test.R", "function_test.R")

for (i in 1:length(files)){
   file <- files[i]
   print(paste0("Running: ", file))
   source(file.path("./dev", file))
}

