library(dplyr)

results <- mtcars %>%
  group_by(cyl) %>%
  summarize(mean = mean(mpg))

glimpse(results)

results %>%
  tidyr::pivot_wider(names_from = cyl, values_from = mean)
