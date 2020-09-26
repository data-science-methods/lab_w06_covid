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
test_that("3.2. Compare mean of daily change in cases to fixed reference", {
    cases_reference = mean(df_3_2$cases, na.rm = TRUE)
    expect_equivalent(mean(daily_df$cases, na.rm = TRUE), cases_reference)
})
test_that("3.3. Length of `covid_df`", {
    expect_equivalent(nrow(covid_df), 8578L)
})
test_that("3.3. `covid_df` is not grouped", {
    expect_false(inherits(covid_df, 'grouped_df'))
})
test_that("3.3. Compare mean deaths per population to fixed reference", {
    deaths_per_pop_ref = mean(df_3_3$deaths_per_pop, na.rm = TRUE)
    expect_equivalent(mean(covid_df$deaths_per_pop, na.rm = TRUE), 
                      deaths_per_pop_ref)
})

warning('Problem 4 not checked automatically')

warning('Problem 5 not checked automatically')

df_6_2 = mob_df %>% 
    filter(date >= '2020-06-01', date <= '2020-06-30', 
           type == 'parks') %>% 
    group_by(county, fips) %>% 
    summarize(parks = mean(pct_diff, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(!is.na(parks))

test_that("6.2. Dimensions of `parks_june`", {
    expect_equivalent(nrow(parks_june), 46L)
    expect_equivalent(ncol(parks_june), 3L)
})
test_that("6.2. `parks_june` is not grouped", {
    expect_false(inherits(parks_june, 'grouped_df'))
})
test_that("6.2. Compare mean value of `parks` to a reference value", {
    parks_reference = mean(df_6_2$parks, na.rm = TRUE)
    expect_equivalent(mean(parks_june$parks, na.rm = TRUE), 
                      parks_reference)
})

df_6_3 = df_3_3 %>% 
    filter(date >= '2020-07-01', date <= '2020-07-31') %>% 
    group_by(county, fips) %>% 
    summarize(cases_per_pop = sum(cases_per_pop)) %>% 
    ungroup()

test_that("6.3. Dimensions of `cases_july`", {
    expect_equivalent(nrow(cases_july), 58L)
    expect_equivalent(ncol(cases_july), 3L)
})
test_that("6.3. `cases_july` is not grouped", {
    expect_false(inherits(cases_july, 'grouped_df'))
})
test_that("6.3. Compare mean cases per pop to a reference value", {
    cases_per_pop_ref = mean(df_6_3$cases_per_pop, na.rm = TRUE)
    expect_equivalent(mean(cases_july$cases_per_pop, na.rm = TRUE), 
                      cases_per_pop_ref)
})

df_6_4 = inner_join(df_6_3, df_6_2, by = c('county', 'fips'))

test_that("6.4. Compare `summer_df` FIPS codes to a reference list", {
    # expect_equivalent(nrow(summer_df), 56L)
    counties_6_4 = df_6_4$county
    expect_setequal(summer_df$county, counties_6_4)
})

# ggplot(df_6_4, aes(parks, cases_per_pop)) +
#     geom_point() +
#     geom_smooth(method = 'lm')

