## This script constructs the subset of the Google Mobility data
library(tidyverse)
# devtools::install_github('kjhealy/covmobility')
library(covmobility)

data('google_mobility')

mobility_df = google_mobility %>% 
    filter(sub_region_1 == 'California')

write_csv(mobility_df, file.path('mobility.csv'))
