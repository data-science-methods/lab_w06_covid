## This script combines the three datasets
library(tidyverse)
library(here)

covid_file = here('data', 'counties.csv')
covid_url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
if (!file.exists(covid_file)) {
    download.file(covid_url, covid_file)
}

pop_df = read_csv(here('data', 'county_population.csv'))
mob_df = read_csv(here('data', 'mobility.csv')) |> 
    select(state = sub_region_1, 
           fips = census_fips_code, 
           date, 
           type, pct_diff) |> 
    filter(!is.na(fips)) |> 
    pivot_wider(names_from = type, values_from = pct_diff)
    

covid_df = read_csv(covid_file) |> 
    filter(state == 'California', county != 'Unknown') |> 
    left_join(pop_df, by = c('fips', 'state', 'county')) |> 
    left_join(mob_df, by = c('fips', 'state', 'date'))

write_rds(covid_df, here('data', 'combined.csv'))

