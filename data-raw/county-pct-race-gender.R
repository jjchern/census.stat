
library(tidycensus)
library(tidyverse)

# pct black, pct white, pct female

# 2000 --------------------------------------------------------------------

load_variables(2000, "sf1", cache = TRUE) %>% # View("sf1_2000")
  filter(name %in% c(
    "P001001",
    "P003001", "P003002", "P003003", "P003004",
    "P012001", "P012002", "P012026"
  )) %>%
  mutate(variable = case_when(
    name == "P003003" ~ "pct_white",
    name == "P003004" ~ "pct_black",
    name == "P012002" ~ "pct_male",
    name == "P012026" ~ "pct_female"
  )) %>%
  print() %>%
  # Load everything except total population as variables
  {get_decennial("county", .$name[-1], year = 2000,
                 summary_var = "P001001", sumfile = "sf1", cache_table = TRUE)} %>%
  # Confirm that the value of total population matched correctly
  rename(name = variable) %>%
  arrange(GEOID, name) %>%
  # Pick only the variables needed
  mutate(name = case_when(
    name == "P003003" ~ "pct_white",
    name == "P003004" ~ "pct_black",
    name == "P012002" ~ "pct_male",
    name == "P012026" ~ "pct_female"
  )) %>%
  filter(!is.na(name)) %>%
  transmute(county_code = GEOID,
            year = 2000,
            name = name,
            rate = value / summary_value * 100) %>%
  print() -> race_gender_2000

# 2010 --------------------------------------------------------------------

load_variables(2010, "sf1", cache = TRUE) %>% # View("sf1_2010")
  filter(name %in% c(
    "P0010001",
    "P0080001", "P0080002", "P0080003", "P0080004",
    "P0120001", "P0120002" # No variable for female count in Table P12?
  )) %>%
  mutate(variable = case_when(
    name == "P0080003" ~ "pct_white",
    name == "P0080004" ~ "pct_black",
    name == "P0120002" ~ "pct_male"
  )) %>%
  print() %>%
  # Load everything except total population as variables
  {get_decennial("county", .$name[-1], year = 2010,
                 summary_var = "P0010001", sumfile = "sf1", cache_table = TRUE)} %>%
  # Confirm that the value of total population matched correctly
  rename(name = variable) %>%
  arrange(GEOID, name) %>%
  # Pick only the variables needed
  mutate(name = case_when(
    name == "P0080003" ~ "pct_white",
    name == "P0080004" ~ "pct_black",
    name == "P0120002" ~ "pct_male"
  )) %>%
  filter(!is.na(name)) %>%
  transmute(county_code = GEOID,
            year = 2010,
            name = name,
            rate = value / summary_value * 100) %>%
  spread(name, rate) %>%
  mutate(pct_female = 100 - pct_male) %>%
  gather(name, rate, -county_code, -year) %>%
  arrange(county_code, name) %>%
  print() -> race_gender_2010

# 2011-2015 ---------------------------------------------------------------

load_variables(2015, "acs5", cache = TRUE) %>% # View("v15") # race:
  filter(name %in% c(
    "B01003_001E",
    "C02003_001E", "C02003_002E", "C02003_003E", "C02003_004E",
    "B01001_001E", "B01001_002E", "B01001_026E"
  )) %>%
  mutate(variable = case_when(
    name == "C02003_003E" ~ "pct_white",
    name == "C02003_004E" ~ "pct_black",
    name == "B01001_002E" ~ "pct_male",
    name == "B01001_026E" ~ "pct_female"
  )) %>%
  print() %>%
  {get_acs("county", .$name[-1], year = 2015,
           summary_var = "B01003_001", survey = "acs5", cache_table = TRUE)} %>%
  # Confirm that the value of total population matched correctly
  rename(name = variable) %>%
  arrange(GEOID, name) %>%
  # Pick only the variables needed
  mutate(name = case_when(
    name == "C02003_003" ~ "pct_white",
    name == "C02003_004" ~ "pct_black",
    name == "B01001_002" ~ "pct_male",
    name == "B01001_026" ~ "pct_female"
  )) %>%
  filter(!is.na(name)) %>%
  transmute(county_code = GEOID,
            name = name,
            rate = estimate / summary_est * 100) %>%
            {bind_rows(., ., ., ., ., .id = "id")} %>%
  arrange(county_code, id) %>%
  mutate(year = 2010 + as.numeric(id)) %>%
  select(-id) %>%
  print() -> race_gender_2011_2015

# 1999-2015 ---------------------------------------------------------------

race_gender_2000 %>%
  bind_rows(race_gender_2010) %>%
  bind_rows(race_gender_2011_2015) %>%
  arrange(county_code, year) %>%
  mutate(rate = round(rate, 1)) %>%
  spread(name, rate) %>%
  {right_join(., expand(., county_code, year = 1999:2016))} %>%
  group_by(county_code) %>%
  arrange(county_code, year) %>%
  filter(sum(!is.na(pct_black)) >= 2) %>%
  mutate(pct_black_appx = approx(year, pct_black, year, rule = 2)$y) %>%
  filter(sum(!is.na(pct_white)) >= 2) %>%
  mutate(pct_white_appx = approx(year, pct_white, year, rule = 2)$y) %>%
  filter(sum(!is.na(pct_female)) >= 2) %>%
  mutate(pct_female_appx = approx(year, pct_female, year, rule = 2)$y) %>%
  filter(sum(!is.na(pct_male)) >= 2) %>%
  mutate(pct_male_appx = approx(year, pct_male, year, rule = 2)$y) %>%
  ungroup() %>%
  select(county_code, year,
         pct_black,   pct_black_appx,
         pct_white,   pct_white_appx,
         pct_female,  pct_female_appx,
         pct_male,    pct_male_appx) %>%
  print(n = 20) -> county_race_gender

devtools::use_data(county_race_gender, overwrite = TRUE)

