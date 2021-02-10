# test file that has no timber log functions internally

# imports
library(dplyr)

# code
test <- "test"

test_stats <- mtcars %>%
   group_by(cyl) %>%
   summarise(n = n(), mean = mean(mpg))
