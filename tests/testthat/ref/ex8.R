library(dplyr)

results <- mtcars %>%
  group_by(cyl) %>%
  summarize(mean = mean(mpg))

wide_results <- results %>%
  tidyr::pivot_wider(names_from = cyl, values_from = mean)

# add ::: example
dplyr:::commas(c("a", "b"))
