## This script constructs the county population file
library(tidyverse)
library(tidycensus)

## County populations, 2018 estimate
pop = get_acs(geography = 'county', 
              state = 'CA', 
              variables = 'B01001_001', 
              year = 2018, show_call = FALSE, 
              cache = TRUE) %>% 
    separate(NAME, into = c('county', 'state'), sep = ' County, ') %>% 
    select(state, county, fips = GEOID, population = estimate)

write_csv(pop, file.path('county_population.csv'))
