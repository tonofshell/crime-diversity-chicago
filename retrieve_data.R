library(tidyverse)
library(ggplot2)
library(lubridate)
library(here)
library(sf)
library(ggmap)
library(maps)
library(mapdata)
library(tidycensus)
library(RSocrata)
source("api_keys.R")
census_api_key(census_key)

save_key = TRUE

if (save_key) {
  acs_vars_18 = load_variables(year = 2018, dataset = "acs5")
  #saveRDS(acs_vars_17, file = "Datasets/acs_vars_17.rds")
}
acs_18 = get_acs(geography = "tract", year = 2018, geometry = TRUE, state = "17", county = "31",  moe_level = 95,
                 variables = c(total_pop = "B01003_001", pop_white = "B02001_002", 
                               pop_black = "B02001_003", pop_asian = "B02001_005", 
                               pop_hispanic = "B03001_003", pop_native_amer = "B02001_004", 
                               pop_not_hisp_white = "B03002_003", pop_not_hisp_black = "B03002_004",
                               pop_not_hisp_na = "B03002_005", pop_not_hisp_asian = "B03002_006",
                               pop_not_hisp_nh_pi = "B03002_007", pop_not_hisp_other = "B03002_008",
                               pop_not_hisp_two_more = "B03002_009",
                               pop_hisp_white = "B03002_013", pop_hisp_black = "B03002_014",
                               pop_hisp_na = "B03002_015", pop_hisp_asian = "B03002_016",
                               pop_hisp_nh_pi = "B03002_017", pop_hisp_other = "B03002_018",
                               pop_hisp_two_more = "B03002_019",
                               med_age = "B01002_001", pop_female = "B01001_026", med_income = "B19326_001", 
                               pop_below_poverty ="B17001_002", pop_employed = "B27011_003", 
                               month_housing_costs = "B25105_001", pop_work_out_res_area = "B08008_003", 
                               pop_rented = "B25033_008", pop_owned = "B25008_002", 
                               pop_occupied = "B25008_001", total_hu = "B25001_001",
                               pop_under_18 = "B09002_001", total_families = "B19123_001",
                               pop_native_born = "B99051_002", pop_foreign_born = "B99051_005",
                               pop_educ_lt_hs = "B16010_002", pop_educ_hs = "B16010_015",
                               pop_educ_some_col = "B16010_028", pop_educ_mt_ba = "B16010_041"))
geometry_data = get_acs(geography = "tract", year = 2018, geometry = TRUE, state = "17", county = "31",  moe_level = 95, variables = c(total_pop = "B01003_001")) %>% select(GEOID) %>% select(GEOID)

acs_18_vals = acs_18 %>% select(-moe) %>% st_drop_geometry() %>% pivot_wider(names_from = "variable", values_from = "estimate") %>% left_join(geometry_data) %>% mutate(land_area = st_area(geometry) %>% units::set_units(mi^2)) %>% mutate_at(vars(-c("GEOID", "NAME", "total_pop", "med_age", "med_income", "total_hu", "total_families", "month_housing_costs", "geometry", "land_area")), (function(x) x / .$total_pop)) %>% mutate(pop_density = total_pop / land_area, total_hu = total_hu / land_area, total_families = total_families / land_area) %>% rename(hu_density = total_hu, family_density = total_families)

acs_18_vals %>% saveRDS(here::here("Data", "acs_tracts_2018_prop.rds"))

cpd_crime_data = RSocrata::read.socrata("https://data.cityofchicago.org/resource/ijzp-q8t2.json?$where=year = 2018", socrata_app_token) %>% 
  select(-c(x_coordinate, y_coordinate, location.latitude, location.longitude, location.human_address)) %>%
  mutate(date = as_datetime(date), 
         updated_on = as_datetime(updated_on), 
         arrest = as.logical(arrest), 
         domestic = as.logical(domestic), 
         beat = factor(beat), 
         district = factor(district), 
         ward = factor(ward), 
         community_area = factor(community_area), 
         fbi_code = factor(fbi_code), 
         year = as.numeric(year), 
         latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude),
         aggravated = str_detect(str_to_lower(description), "agg") & !str_detect(str_to_lower(description), "non-agg"),
         attempted = str_detect(str_to_lower(description), "att ") | str_detect(str_to_lower(description), "att:") | str_detect(str_to_lower(description), "attempt"), 
         armed = str_detect(str_to_lower(description), "armed"))
cpd_crime_data %>% saveRDS(here::here("Data", "cpd_crime_data_2018.rds"))

crime_sf = cpd_crime_data %>% filter(!is.na(latitude), !is.na(longitude)) %>% st_as_sf(coords = c("longitude", "latitude"))
st_crs(crime_sf) = st_crs(geometry_data)

calc_div_index = function(data_set) {
  parse(text = data_set %>% select(contains("_hisp_")) %>% names() %>% paste0("^2", collapse = " + ") %>% {paste("1 - (", ., ")")})
}

crimes_individ_merged = crime_sf %>% 
  filter(!is.na(geometry)) %>% 
  st_join(geometry_data) %>% 
  st_drop_geometry() %>% 
  group_by(GEOID) %>% 
  summarise(num_crimes = length(id), num_arrests = sum(arrest), num_domestic = sum(domestic), num_aggr = sum(aggravated), num_attempt = sum(attempted), num_armed = sum(armed)) %>% 
  left_join(acs_18_vals) %>% 
  mutate(num_arrests = 1000 * (num_arrests / total_pop), 
         num_domestic = 1000 * (num_domestic / total_pop), 
         num_aggr = 1000 * (num_aggr / total_pop), 
         num_attempt = 1000 * (num_attempt / total_pop), 
         num_armed = 1000 * (num_armed / total_pop), 
         num_crimes = 1000 * (num_crimes / total_pop), 
         diversity_index = eval(calc_div_index(.))) %>% 
  rename(arrests = num_arrests, domestic = num_domestic, aggravated = num_aggr, attempted = num_attempt, armed = num_armed, crime_density = num_crimes) %>% 
  select(-contains("_hisp_"))

saveRDS(crimes_individ_merged, here::here("Data", "merged_data_final.rds"))
