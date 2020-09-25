#' ---
#' title: "Data Science Methods, Lab for Week 06"
#' author: "Your Name"
#' email: Your Email
#' output:
#'   html_document:
#'     toc: true
#' ---

#' *In this lab, we'll be exploring county-level data on the Covid-19 pandemic in California, including both case and mortality data as well as "mobility data" from Google.* 
#' 
#' *Specifically, we'll be exploring the way disease counts and mobility have changed over the course of the pandemic, and the relationship between social distancing and disease at the county level during the major outbreak in July 2020.* 
#' 
#' *Besides the `tidyverse` tools, we'll also be using the `covdata` package, a collection of Covid-related datasets created by sociologist Kieran Healy, and the `tidycensus` package for retrieving county-level population.*  
#' 


## Setup
## **IMPORTANT**: Add all dependencies to `DESCRIPTION`
library(tidyverse)
## `covdata` has to be installed from GitHub. Don't put it in `DESCRIPTION`.
## If you get errors on installation, see the file `covdata_install.md`. 
# devtools::install_github('kjhealy/covdata')
library(covdata)

## To check your answers locally, run the following: 
## testthat::test_dir('tests')
## (But leave it commented when you submit, otherwise it can cause an infinite loop)

#' *Because counties have different populations, we'll want to normalize cases and deaths.  Reporting per 100,000 and 1,000,000 residents are both pretty common.  We'll set this denominator here, so it's easy to change.* 
#' 

per_pop = 1e6

#' *We'll also need county-level population statistics. This file gives 5-year estimates from the American Community Survey as of 2018.  I retrieved them using the `tidycensus` package on 2020-09-23.*
#' 
 
pop = read_csv(file.path('data', 'county_population.csv'))



#' # Problem 1 #
#' *Take a few minutes to read about the provenance of the data we'll be using:*
#' 
#' - The `covdata` package itself: <https://kjhealy.github.io/covdata/>
#' - *New York Times* county-level cumulative data: <https://kjhealy.github.io/covdata/articles/new-york-times.html>
#' - Mobility data from Google:  <https://kjhealy.github.io/covdata/articles/mobility-data.html#google-s-mobility-reports>
#' 

#' 1. *For the county-level cumulative data, are there any limitations or caveats that we should keep in mind for our analysis?* 
#' 
#' 
#' 

#' 2. *How about the Google Mobility data?* 
#' 
#' 



#' # Problem 2 #
#' *The county-level Covid case and mortality data are in `nytcovcounty`.* 
data("nytcovcounty")
#' *Use tools such as `skimr::skim()` to answer the following questions.  Remember that, if you use any packages in the final script, you should (a) use a `library()` call at the top to load them and (b) add them to the list in the `DESCRIPTION` file (remember the comma between items).*
#' 

#' 1. *How current are these data?* 
#' 
#'  
#' 

#' 2. *What is the `fips` variable?* 
#' 
#' 
#' 

#' 3. *`cases` and `deaths` are nominally 100% complete.  Does this mean that we have case and death counts for every county over the entire time span of the data?*  
#' 
#' 
#' 



#' # Problem 3 #
#' *For our analysis, the `nytcovcounty` dataset needs three things:* 
#' - Filtered down to California between April 1 and August 31, 2020
#' - Daily changes in cases and deaths, rather than cumulative counts
#' - Cases and deaths as rates per 1 million residents
#' 

#' 1. *Write a pipe that starts with `nytcovcounty` as input, filters down to California and April 1-August 31, 2020, and assigns the result to a variable `filtered_df`.  Hint: You can compare dates as though they were strings, e.g., `date <= '1980-05-17'` gives you dates on or before May 17, 1980.* 
#' 

#' 2. To go from daily changes to cumulative counts, we'll use the following function. 

daily_diff = function(x, order_var) {
    diff = x - lag(x, order_by = order_var)
    return(diff)
}

#' *Write a pipe that takes `filter_df` as input, groups the data by county and FIPS code, sorts the dataframe by date, and then converts the cumulative cases and death counts to daily changes using `daily_diff`.  (`date` is the order variable.)  Assign the result to `daily_df`. Hint: `mutate()` can replace the value of existing variables.* 
#' 

#' 3. *Finally we need to calculate rates per 1 million residents.  Write a pipe that takes `daily_diff` as input, joins it with the `pop` dataframe using appropriate variables, removes any rows with missing FIPS codes, and constructs the variables `cases_per_pop` and `deaths_per_pop`.  When constructing these variables, multiply by `per_pop` to get rates per 1 million residents.  Assign the result to `covid_df`, since this contains the Covid data for our analysis.*  
#' 



#' # Problem 4 #
#' 1. *To explore these time-series data visually, we'll want to use line plots of cases or deaths over time.  The line group needs the `group` aesthetic to determine which values should be treated as part of a single line.  Uncomment and fill in the blanks to plot cases per 1,000,000 residents over time for each county.*  
#' 

# ggplot(covid_df, aes(---, ---, group = ---)) +
#     geom_line()

#' 2. *Because there are so many counties, the lines are heavily overplotted.  Modify your code from the last problem to facet by county.  Try both `scales = 'fixed'` and `scales = 'free_y'`.* 
#' 

#' 3. *The plot indicates that, on a few days, some counties gain or lose thousands of cases per million residents.  What's up this that?  Hints:  `plotly::ggplotly()` to create an interactive version of the most recent plot.  Use `filter()` to narrow down the data to particular counties during short periods of time, and `View()` to peruse the data after filtering.*  
#' 
#' 
#' 

#' 4. *We can pass data through a pipe before calling `ggplot()`.  Let's focus on 4 counties of interest, two rural and two urban:  Butte, Merced, Sacramento, and Santa Clara.  Uncomment and fill in the blanks:* 

focal_counties = c('Butte', 'Merced', 'Sacramento', 'Santa Clara')

# --- %>% 
#     filter(county %in% focal_counties) %>% 
#     ggplot(aes(---------)) +
#     -------

#' Note that we need to use plus `+` to connect ggplot layers, not the pipe `%>%`. You can get weird errors if you accidentally use the wrong one.  I do this all the time. 
#' 

#' 5. *The common narrative of Covid-19 in California runs something like this:  "Despite being one of the locations where Covid-19 was detected early on, California mostly avoided the large outbreak that hit New York in the spring.  About a month after stay-at-home rules were relaxed in late May, cases began to increase in June, leading to a large outbreak across the state that peaked in July.  This outbreak has largely faded by September."  Based on your (brief) visual EDA of the data, does this narrative seem accurate? * 
#' 
#' 
#' 

#' *(Just an aside.  Most presentations of Covid-19 data use 7-day rolling averages.  Either they don't show raw counts at all, or they emphasize the rolling averages rather than the raw counts.  In the `plots` folder, `chronicle.png` shows an example from the _San Francisco Chronicle_.  Because this lab is already super long and complicated, I decided to skip the rolling averages.  Two common packages for calculating rolling averages are (`zoo`)[https://cran.r-project.org/web/packages/zoo/index.html] and (`slider`)[https://cran.r-project.org/web/packages/slider/].)*
#' 



#' # Problem 5 #
#' *Now let's turn to the Google mobility data.  The full version in `covdata` has nearly 15 million rows, and couldn't be loaded in RStudio Cloud when I was testing this lab.  (The memory cap for a free RStudio Cloud instance is 1 GB.)  So I've included a prefiltered version in the `data` folder: only counties in California, between April 1 and August 31, 2020.*   
#' 

# data("google_mobility")
# google_mobility
mob_df = read_csv(file.path('data', 'mobility.csv'))

#' 1. *How many distinct values of `type` are there?  What does this variable indicate?*  
#' 
#' 
#' 

#' 2. *How about `pct_diff`?* 
#' 
#' 
#' 

#' 3. *During our time period of interest, does `mob_df` contain mobility data for every county in California?  If some counties are missing data, which ones are they?  Hints: There are 58 counties in California.  Try counting and then filtering to identify counties with outlying row counts.*  
#' 
#' 
#' 

#' 4. *In the `plots` folder, take a look at `mobility.png`.  Recreate this plot.  (Use whatever theme and colors that you like.  To create a horizontal line: `geom_hline(yintercept = 0, alpha = .5)`.  You don't need to save to disk.)* 
#' 

#' 5. *Again, the standard narrative of Covid-19 in California says that people were staying home in the spring, then going out more in May-June as stay-at-home orders were lifted.  Does this data support that narrative?*  
#' 
#' 
#' 

#' 6. *What other potentially interesting patterns do you see in these mobility data?* 
#' 
#' 



#' # Problem 6 #
#' *Our specific interest in this data is the relationship between the relaxation of stay-at-home rules in June and the outbreak that peaked in July.  The standard narrative of the summer outbreak focuses on the kind of individual behavior measured by the Google Mobility data.  According to this narrative, individuals became more relaxed about staying at home and maintaining social distancing in June, and this caused an increase of cases starting 2-4 weeks later and peaking in July.  Media coverage focused especially on parks, as in this _San Francisco Chronicle_ story: <https://www.sfchronicle.com/bayarea/article/Bay-Area-residents-mostly-wear-masks-and-follow-15452707.php>*. 
#' 
#' *We'll investigate this by taking month-long averages for social distancing in June (as measured by the `parks` type in the mobility data) and Covid-19 cases and deaths in July, then drawing a scatterplot.*
#' 

#' 1. *This is just one way we could get at the relationship between stay-at-home in June and the peak of the outbreak in July.  What are some other approaches we might take?*
#' 
#' 
#' 

#' 2. *Construct a dataframe `parks_june` that reports the mean level of "parks" mobility for each county in June 2020.  Call the variable `parks`.  (Just so the automatic checks know where to look.)  Note that the Google data has a lot of gaps for the `parks` type.  Use the `na.rm = TRUE` argument in `mean()` to handle missing values, and then filter out missing values.  The final dataframe should have three columns: county name, FIPS code, and `parks`.  And it should have one row for each county in the mobility data for which we have an estimate for "parks".*
#' 

#' 3. *Construct a dataframe `cases_july` that reports the total level of new cases per 1 million residents of each county in July 2020.  (Don't worry about negative values.  I'm just asking you to do `sum(cases_per_pop)`.)  This dataframe should have three columns and one row for each county in the Covid-19 data.*
#' 

#' 4. *Combine `retail_june` with `cases_july` using an inner join and appropriate matching columns.  Assign the result to `summer_df`.  (Note that the automatic checks will be looking at the `county` column, derived from the Covid-19 data.)* 
#' 

#' 5. *Construct a scatterplot of July cases against June "parks."  The standard narrative suggests that there should be a positive correlation between these variables: as people spent more time at parks in June, this led to more cases in July.  Does the scatterplot support this?* 
#' 
#' 
#' 



#' # Problem 7 #
#' *Please answer these questions in the course Slack.*
#' 
#' *If you haven't done so yet, read Rex Douglass' blog post "How to be Curious Instead of Contrarian About COVID-19" (<https://rexdouglass.github.io/TIGR/Douglass_2020_How_To_Be_Curious_Instead_of_Contrarian_About_Covid19.nb.html>).  Douglass presents eight "lessons" for non-epidemiologists who are working with epidemiological data.  I assume you're not an epidemiologist.  If you are an epidemiologist, please pretend that you're not for the sake of this exercise.*
#' 
#' *Suppose you were exploring Covid data for fun in late July or early August 2020 (cf this blog post by Kieran Healy: <https://kieranhealy.org/blog/archives/2020/05/21/the-kitchen-counter-observatory/>), and that you found the pattern that we identified in Problem 6.  You're contemplating writing a blog post, op-ed, or even short research letter to share this finding.  How can Douglass' 8 lessons help us determine whether and how to share this finding?* 
#' 