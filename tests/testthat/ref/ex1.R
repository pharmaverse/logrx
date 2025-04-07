library(dplyr)

results <- mtcars %>%
  group_by(cyl) %>%
  summarize(mean = mean(mpg))

results %>%
  tidyr::pivot_wider(names_from = cyl, values_from = mean)

mtcars
