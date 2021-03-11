# test file that has no timber log functions internally

# imports
library(dplyr)

Sys.sleep(2)

# code
test <- "test"

log(-1)

test_stats <- mtcars %>%
   group_by(cyl) %>%
   summarise(n = n(), mean = mean(mpg))
