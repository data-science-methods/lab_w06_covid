## This script constructs the subset of the Google Mobility data
library(tidyverse)
library(covdata)

mobility_df = google_mobility %>% 
    filter(sub_region_1 == 'California', 
           !is.na(sub_region_2), is.na(metro_area), 
           date >= '2020-04-01', date <= '2020-08-31') %>% 
    select(county = sub_region_2, fips = census_fips_code, date, type, pct_diff) %>% 
    mutate(county = str_remove(county, ' County'))

write_csv(mobility_df, file.path('mobility.csv'))
