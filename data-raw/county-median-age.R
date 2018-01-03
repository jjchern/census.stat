library(tidycensus)
library(tidyverse)

# 2000 --------------------------------------------------------------------

# load_variables(2000, "sf1", cache = TRUE) %>% View("sf1_2000") # median
get_decennial("county", "P013001", year = 2000, sumfile = "sf1", cache_table = TRUE) %>%
  select(county_code = GEOID, median_age_2000 = value) %>%
  print() -> median_age_2000

# 2010 --------------------------------------------------------------------

# load_variables(2010, "sf1", cache = TRUE) %>% View("sf1_2010") # median
get_decennial("county", "P0130001", year = 2010, sumfile = "sf1", cache_table = TRUE) %>%
  select(county_code = GEOID, median_age_2010 = value) %>%
  print() -> median_age_2010

# 2011-2015 ---------------------------------------------------------------

# load_variables(2015, "acs5", cache = TRUE) %>% View("v15") # median
get_acs("county", "B01002_001E", year = 2015, survey = "acs5", cache_table = TRUE) %>%
  transmute(county_code = GEOID,
            median_age_2011 = estimate,
            median_age_2012 = estimate,
            median_age_2013 = estimate,
            median_age_2014 = estimate,
            median_age_2015 = estimate) %>%
  print() -> median_age_2011_2015

# 1999-2015 ---------------------------------------------------------------

median_age_2000 %>%
  full_join(median_age_2010, by = "county_code") %>%
  full_join(median_age_2011_2015, by = "county_code") %>%
  gather(year, median_age, -county_code) %>%
  arrange(county_code, year) %>%
  mutate(year = parse_number(year)) %>%
  {right_join(., expand(., county_code, year = 1999:2016))} %>%
  group_by(county_code) %>%
  arrange(county_code, year) %>%
  filter(sum(!is.na(median_age)) >= 2) %>%
  mutate(median_age_appx = approx(year, median_age, year, rule = 2)$y) %>%
  ungroup() %>%
  print(n = 20) -> county_median_age

devtools::use_data(county_median_age, overwrite = TRUE)
