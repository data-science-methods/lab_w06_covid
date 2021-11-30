#' ---
#' title: "Data Science Methods, Lab for Week 06"
#' author: "Aramis D. M. Valverde"
#' email: amunoz-valverde@ucmerced.edu
#' output:
#'   html_document:
#'     toc: true
#' ---

#' *In this lab, we'll be exploring county-level data on the Covid-19 pandemic in California, including both case and mortality data as well as "mobility data" from Google.* 
#' 
#' *Specifically, we'll be exploring the relationship between case counts and mobility in the context of the Summer 2020 wave.* 
#' 
#' *This lab reproduces some of my own EDA of Covid-19 data from Summer 2020, in a more streamlined fashion.  Because it's still significantly longer and more complex than our *
#' *other labs, I decided not to include much open-ended exploration.  As a result this might not feel very "exploratory," but it does do some other valuable things.  I encourage *
#' *you to do spend a few hours exploring these data on your own, chasing after your own questions.*
#' 

#' # Reflexivity #
#' *Before starting the lab, spend 3 minutes writing a response to each reflexivity question.  Use a timer.  Answer these questions off the top of your head: don't worry about *
#' *consulting or citing outside sources or about getting the answers "right" or "wrong."* 
#' 
#' 1. *What do I already know about this subject?*
#' 2. *Why am I studying this?*
#' 3. *What do I expect or hope to find/learn, and why?*
#' 4. *Who is affected by this topic, and how am I connected to them?* 
#' 


## Setup ----
## **IMPORTANT**: Add all dependencies to `DESCRIPTION`
## Note:  I'm copying the relevant setup from class.  `covid_df` includes both the new cases/deaths per day as well as the rates per 100,000 residents. 

library(tidyverse)
library(tidylog)
library(skimr)
library(visdat)

library(assertthat)

theme_set(theme_bw())

daily_new = function(x, order_var) {
    diff = x - dplyr::lag(x, order_by = order_var)
    return(diff)
}

pop_df = read_csv(file.path('data', 'county_population.csv'))

covid_file = file.path('data', 'counties.csv')
covid_url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
if (!file.exists(covid_file)) {
    download.file(covid_url, covid_file)
}
covid_df = read_csv(covid_file) |> 
    filter(state == 'California', county != 'Unknown') |> 
    group_by(county) |>
    mutate(across(.cols = c(cases, deaths),
                  .fns = list(new = daily_new), date)) |> 
    ungroup() |> 
    inner_join(pop_df,
                   by = c('state', 'county', 'fips')) |>
    mutate(across(.cols = c(cases, deaths, cases_new, deaths_new),
                  .fns = list(rate = ~ .x/population * 100000)))

mob_df = read_csv(file.path('data', 'mobility.csv'))

## Data validation ----
dataf |> 
    group_by(county) |> 
    slice(-1) |> 
    pull(cases_new) |> 
    is.na() |> 
    any() |> 
    magrittr::not() |> 
    assert_that(msg = 'missing values in cases_new')


#' # Problem 1 #
#  problem 1 ----
#' *Take a few minutes to read about the provenance of the data we'll be using:*
#' 
#' - *New York Times* county-level cumulative data: <https://github.com/nytimes/covid-19-data/blob/master/README.md>
#' - Mobility data from Google:  <https://kjhealy.github.io/covmobility/reference/google_mobility.html>
#' 

#' 1. *For the county-level cumulative data, are there any limitations or caveats that we should keep in mind for our analysis?* 
#' There are many, the case reporting is not normalized per county, as each county has different reporting standards. Cases were also reported at different rates, 
#' and by differing avenues, meaning that spikes reported at one week may have happened weeks before, or may be artificial spikes or ruts due to case reporting that is 
#' plainly inconsistent. 
#' There is also the issue that labs are closed on weekends, meaning that reports of covid nearly always fall at the weekends. This is very obvious, yet much reporting at the 
#' early pandemic tried to say that every weekend dip indicated the prompt end of the pandemic. The painful obviousness of the labs being closed and that driving weekend covid 
#' numbers down drove me mad at the beginning of the pandemic, because; 1. There is no way that the people writing these stories were so oblivious, meaning that there were likely
#' ulterior motives, and 2. this created an inconsitency that people would latch onto and which predictably damaged later discourse (see also the blatant lies about masks being 
#' detrimental early in the pandemic.)
#' 

#' 2. *How about the Google Mobility data?* 
#' Some of the privacy protections could be an issue, especially in places with smaller populations. Also this is alphabet/google/android data, so it excluded people who arent
#' users of those (primarally iphone users). There is a page of limitations on the site, however for our purposes those are the main issues.
#' 


#' # Problem 2 #
#  problem 2 ----
#' *We spent some time in class looking at the Covid case data, and combining it with the county populations to get rates per 100,000 residents.  So here we'll start with the Google Mobility data.*
#' 
#' *The original dataset is huge.  The full version is very large, so I've included a prefiltered version in `data` that's limited to California.  It's already been loaded above as `mob_df`.*  
#' 1. *How many distinct values of `type` are there?  What does this variable indicate?*  
#' 
#' *residential, workplace, retail, grocery, parks, and transit*. it indicates the following, "
#' The data shows how visitors to (or time spent in) categorized places change compared to our 
#' baseline days. A baseline day represents a normal value for that day of the week. The baseline 
#' day is the median value from the 5‑week period Jan 3 – Feb 6, 2020."-Google
#' 

# Misc Code
#test1 <- group_by('census_fips_code')

#count(mob_df, variable_name='census_fips_code')
#count(mob_df[c('census_fips_code')])


#' 2. *How about `pct_diff`?* 
#' 
#' This is difference compared to the normal baseline day, which is calculates as a median value from the period of jan 3rd. to feb 6th, 2020.
#' 

#' 3. *During our time period of interest, does `mob_df` contain mobility data for every county in California?  If some counties are missing data, which ones are they?*
#'    *Hints: There are 58 counties in California.  Try counting and then filtering to identify counties with outlying row counts.*  
#' There are 56 fips codes and 57 place ids based on the skim below. sub_region_2 also holds 56 variables, meaning that there are 2 missing counties, probably the tiny ones.
#' 

skim(mob_df)



#' 4. *In the `plots` folder, take a look at `mobility.png`.  Recreate this plot.  (Use whatever theme and colors that you like. *
#'    *To create a horizontal line: `geom_hline(yintercept = 0, alpha = .5)`.  You don't need to save to disk.)* 
#' 

# df_bmss <- filter(mob_df, cut == 'Merced County', 'Butte County', 'Sacramento County', 'Santa Clara County')

#regionsUsed <- c('Merced County', 'Butte County', 'Sacramento County', 'Santa Clara County')
#df_bmss <- filter(mob_df, sub_region_2 %in% regionsUsed)

#ggplot(data = df_bmss, mapping = aes(x = date, y = pct_diff)) +
  #  geom_point() +
  #  geom_line(color = "blue")

###AV create seperate df for each county, will combine plots later
    
df_butte <- filter(mob_df, sub_region_2 == 'Butte County') 

df_merced <- filter(mob_df, sub_region_2 == 'Merced County')

df_sacramento <- filter(mob_df, sub_region_2 == 'Sacramento County')

df_santaclara <- filter(mob_df, sub_region_2 == 'Santa Clara County')

group_by(mob_df, type)

df_butte2 <- group_by(mob_df, type)

df_butte3 <- filter(df_butte2, type %in% c('parks', 'residential', 'retail'))

###AV create plots per county (I know this isn't efficient and will not be the same, however I think it may be more instructive for me to go this route than with a 
###AV facet wrap that I don't quite get yet)

ggplot(data = df_butte2, mapping = aes(x = date, y = pct_diff)) +
  geom_hline(yintercept = 0, alpha = .5) +
  geom_point() +
  geom_line(color = variable)

summarize(df_butte2)

ggplot(data = df_butte3, mapping = aes(x = date, y = pct_diff, group = type)) +
geom_line(aes(linetype=type, color=type)) +
    geom_point()


###AV the data is in the wrong format for me to be able to use this bit, they need ot be seperate y axies, but that isnt what I have done. I will check.
ggplot(data = df_butte, mapping = aes(x = date, y = pct_diff)) +
    geom_hline(yintercept = 0, alpha = .5) +
    geom_line(aes(y = parks), color = "red") + 
    geom_line(aes(y = residential), color="green", linetype="twodash") 


#' 5. *Again, the standard narrative of Covid-19 in California says that people were staying home in the spring, then going out more in May-June as stay-at-home orders were lifted.  *
#'    *Do this data support that narrative?*
#'   
#'   
#'   

#' 6. *What other potentially interesting patterns do you see in these mobility data?* 
#' 
#' 
#' 


#' Problem 3
# problem 3 ----
#' *Our specific interest in this data is the relationship between the relaxation of stay-at-home rules in June and the outbreak that peaked in July.  *
#' *The standard narrative of the summer outbreak focuses on the kind of individual behavior measured by the Google Mobility data.  *
#' *According to this narrative, individuals became more relaxed about staying at home and maintaining social distancing in June, and this caused an *
#' *increase of cases starting 2-4 weeks later and peaking in July.  Media coverage focused especially on parks, as in this *
#' *_San Francisco Chronicle_ story: <https://www.sfchronicle.com/bayarea/article/Bay-Area-residents-mostly-wear-masks-and-follow-15452707.php>*. 
#' 
#' *We'll investigate this by taking month-long averages for social distancing in June (as measured by the `parks` type in the mobility data) and *
#' *Covid-19 cases and deaths in July, then drawing a scatterplot.*
#' 
#' *(Just an aside.  Most presentations of Covid-19 data use 7-day rolling averages.  Either they don't show raw counts at all, or they emphasize the *
#' *rolling averages rather than the raw counts.  In the `plots` folder, `chronicle.png` shows an example from the _San Francisco Chronicle_.  Because this *
#' *lab is already super long and complicated, I decided to skip the rolling averages.  Two common packages for calculating rolling averages are (`zoo`)*
#' *[https://cran.r-project.org/web/packages/zoo/index.html] and (`slider`)[https://cran.r-project.org/web/packages/slider/].)*
#' 

#' 1. *This is just one way we could get at the relationship between stay-at-home in June and the peak of the outbreak in July.  What are some other approaches we might take?*
#' 
#' 
#' 

#' 2. *Construct a dataframe `parks_june` that reports the mean level of "parks" mobility for each county in June 2020.  Call the variable `parks`.  *
#' *(Just so the automatic checks know where to look.)*
#' 
#' *Hints:*
#' 
#' - You can compare dates as though they were strings, e.g., `date <= '1980-05-17'` gives you dates on or before May 17, 1980.*
#' - Use the `na.rm = TRUE` argument in `mean()` to handle missing values.
#' - The final dataframe should have three columns: county name, FIPS code, and `parks`.  
#' - And it should have one row for each county in the mobility data for which we have an estimate for "parks".*
#' 

#' 3. *Construct a dataframe `cases_july` that reports the total level of new cases per 1 million residents of each county in July 2020.  (Don't worry about negative values.  *
#' *I'm just asking you to do `sum(cases_per_pop)`.)  This dataframe should have three columns and one row for each county in the Covid-19 data.*
#' 

#' 4. *Combine `parks_june` with `cases_july` using an inner join and appropriate matching columns.  Assign the result to `summer_df`.  *
#' *(Note that the automatic checks will be looking at the `county` column.)* 
#' 

#' 5. *Construct a scatterplot of July cases against June "parks."  The standard narrative suggests that there should be a positive correlation *
#' *between these variables: as people spent more time at parks in June, this led to more cases in July.  Does the scatterplot support this?* 
#' 
#' 
#' 



#' # Problem 4 #
#  problem 4 ----
#' *We'll be discussing this part in class.*
#' 
#' *If you haven't done so yet, read Rex Douglass' blog post "How to be Curious Instead of Contrarian About COVID-19" *
#' *(<https://rexdouglass.github.io/TIGR/Douglass_2020_How_To_Be_Curious_Instead_of_Contrarian_About_Covid19.nb.html>).  *
#' *Douglass presents eight "lessons" for non-epidemiologists who are working with epidemiological data.  I assume you're not an epidemiologist.  *
#' *If you are an epidemiologist, please pretend that you're not for the sake of this exercise.*
#' 
#' *Suppose you were exploring Covid data for fun in late July or early August 2020 (cf this blog post by Kieran Healy: *
#' *<https://kieranhealy.org/blog/archives/2020/05/21/the-kitchen-counter-observatory/>), and that you found the pattern that we identified in Problem 3.  *
#' *You're contemplating writing a blog post, op-ed, or even short research letter to share this finding.  How can Douglass' 8 lessons help us determine *
#' *whether and how to share this finding?* 
#' 
#' 
#' 