source("../lab.R", chdir = TRUE)
library(testthat)

warning('Problem 1 not checked automatically')
warning('Problem 2 not checked automatically')

df_3_1 = nytcovcounty %>% 
    filter(state == 'California', 
           date >= '2020-04-01', date <= '2020-08-31')
df_3_2 = df_3_1 %>% 
    group_by(county, fips) %>% 
    arrange(date) %>%
    mutate(cases = daily_diff(cases, date), 
           deaths = daily_diff(deaths, date)) %>% 
    ungroup()
df_3_3 = df_3_2 %>% 
    left_join(pop, by = c('county', 'state', 'fips')) %>% 
    filter(!is.na(fips)) %>% 
    mutate(cases_per_pop = cases / population * per_pop, 
           deaths_per_pop = deaths / population * per_pop)

test_that("3.1. Length of `filtered_df`", {
    expect_equivalent(nrow(filtered_df), 8585L)
})
test_that("3.2. Daily change in cases (compare to fixed reference)", {
    cases_reference = df_3_2$cases
    expect_equivalent(daily_df, cases_reference)
})
test_that("3.3. Length of `covid_df`", {
    expect_equivalent(nrow(covid_df), 8578L)
})

warning('Problem 4 not checked automatically')
