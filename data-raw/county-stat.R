
library(tidyverse)

county_median_age %>%
  full_join(county_race_gender) %>%
  full_join(county_educ) %>%
  print() -> county_stat

devtools::use_data(county_stat, overwrite = TRUE)
