
library(tidycensus)
library(tidyverse)

# Note: No long form for 2010 decennial census
# Note: Population 25 years and over

# 2000 --------------------------------------------------------------------

load_variables(2000, "sf3", cache = TRUE) %>% # View("sf3_2000")
  filter(grepl("P037", name)) %>%
  print(n = 35) %>%
  # Load everything except total population as variables
  {get_decennial("county", .$name, year = 2000,
                 summary_var = "P037001", sumfile = "sf3", cache_table = TRUE)} %>%
  # Confirm that the value of total population matched correctly
  rename(name = variable) %>%
  arrange(GEOID, name) %>%
  mutate(name = parse_number(name)) %>%
  mutate(label = case_when(
    name %in% c(37001)                    ~ "pop_age_25p",
    name %in% c(37003:37011, 37020:37028) ~ "hs_or_less",
    name %in% c(      37015,       37032) ~ "ba_degree"
  )) %>%
  group_by(GEOID, label) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  na.omit(value) %>%
  spread(label, value) %>%
  mutate(pct_ba_degree  = ba_degree / pop_age_25p * 100,
         pct_hs_or_less = hs_or_less / pop_age_25p * 100,
         year           = 2000) %>%
  select(county_code = GEOID, year, pct_ba_degree, pct_hs_or_less) %>%
  print() -> educ_2000

# 2006-2010 ---------------------------------------------------------------

load_variables(2010, "acs5", cache = TRUE) %>% # View("acs5_2006_2010")
  filter(grepl("B15002", name)) %>%
  filter(grepl("E", name)) %>%
  print(n = 35) %>% # View("acs5_2006_2010_B15002")
  {get_acs("county", .$name, year = 2015,
           summary_var = "B15002_001", survey = "acs5", cache_table = TRUE)} %>%
  # Confirm that the value of total population matched correctly
  rename(name = variable) %>%
  arrange(GEOID, name) %>%
  mutate(name = substr(name, 8, 10) %>% as.numeric()) %>%
  mutate(label = case_when(
    name %in% 1 ~ "pop_age_25p",
    name %in% c(3:11, 20:28) ~ "hs_or_less", # by male and female
    name %in% c(15, 32) ~ "ba_degree"            # by male and female
  )) %>%
  print(n = 35) %>%
  group_by(GEOID, label) %>%
  summarise(value = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  na.omit(value) %>%
  spread(label, value) %>%
  mutate(pct_ba_degree  = ba_degree / pop_age_25p * 100,
         pct_hs_or_less = hs_or_less / pop_age_25p * 100) %>%
  select(county_code = GEOID, pct_ba_degree, pct_hs_or_less) %>%
  {bind_rows(., ., ., ., ., .id = "id")} %>%
  arrange(county_code, id) %>%
  mutate(year = 2005 + as.numeric(id)) %>%
  select(-id) %>%
  print() -> educ_2006_2010

# 2011-2015 ---------------------------------------------------------------

load_variables(2015, "acs5", cache = TRUE) %>% # View("v15")
  filter(grepl("B15003", name)) %>%
  filter(grepl("E", name)) %>%
  print(n = 25) %>%
  {get_acs("county", .$name, year = 2015,
           summary_var = "B15003_001", survey = "acs5", cache_table = TRUE)} %>%
  # Confirm that the value of total population matched correctly
  rename(name = variable) %>%
  arrange(GEOID, name) %>%
  mutate(name = substr(name, 8, 10) %>% as.numeric()) %>%
  mutate(label = case_when(
    name %in% 1    ~ "pop_age_25p",
    name %in% 2:18 ~ "hs_or_less",
    name %in% 22   ~ "ba_degree"
  )) %>%
  group_by(GEOID, label) %>%
  summarise(value = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  na.omit(value) %>%
  spread(label, value) %>%
  mutate(pct_ba_degree  = ba_degree / pop_age_25p * 100,
         pct_hs_or_less = hs_or_less / pop_age_25p * 100) %>%
  select(county_code = GEOID, pct_ba_degree, pct_hs_or_less) %>%
  {bind_rows(., ., ., ., ., .id = "id")} %>%
  arrange(county_code, id) %>%
  mutate(year = 2010 + as.numeric(id)) %>%
  select(-id) %>%
  print() -> educ_2011_2015

# 1999-2015 ---------------------------------------------------------------

educ_2000 %>%
  bind_rows(educ_2006_2010) %>%
  bind_rows(educ_2011_2015) %>%
  arrange(county_code, year) %>%
  print(n = 35) %>%
  mutate(pct_ba_degree = round(pct_ba_degree, 1),
         pct_hs_or_less = round(pct_hs_or_less, 1)) %>%
         {right_join(., expand(., county_code, year = 1999:2016))} %>%
  print(n = 35) %>%
  group_by(county_code) %>%
  arrange(county_code, year) %>%
  filter(sum(!is.na(pct_ba_degree)) >= 2) %>%
  mutate(pct_ba_degree_appx = approx(year, pct_ba_degree, year, rule = 2)$y) %>%
  filter(sum(!is.na(pct_hs_or_less)) >= 2) %>%
  mutate(pct_hs_or_less_appx = approx(year, pct_hs_or_less, year, rule = 2)$y) %>%
  ungroup() %>%
  select(county_code,    year,
         pct_ba_degree,  pct_ba_degree_appx,
         pct_hs_or_less, pct_hs_or_less_appx) %>%
  print(n = 20) -> county_educ

devtools::use_data(county_educ, overwrite = TRUE)
