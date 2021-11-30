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


###AV *Create separate DF for each county and process new DFs*


###AV   BUTTE COUNTY DF & PROCESSING    (Note: setDT uses package 'data.table')

df_butte <- filter(mob_df, sub_region_2 == 'Butte County') 

df_butte2 <- group_by(df_butte, type)

df_butte3 <- filter(df_butte2, type %in% c('parks', 'residential', 'retail'))

df_butte4 <- setDT(df_butte3)[between(date, '2020-03-25', '2020-09-05', incbounds=FALSE)]


###AV   MERCED COUNTY DF & PROCESSING

df_merced <- filter(mob_df, sub_region_2 == 'Merced County')

df_merced2 <- group_by(df_merced, type)

df_merced3 <- filter(df_merced2, type %in% c('parks', 'residential', 'retail'))

df_merced4 <- setDT(df_merced3)[between(date, '2020-03-25', '2020-09-05', incbounds=FALSE)]


###AV   SACRAMENTO COUNTY DF & PROCESSING

df_sacramento <- filter(mob_df, sub_region_2 == 'Sacramento County')

df_sacramento2 <- group_by(df_sacramento, type)

df_sacramento3 <- filter(df_sacramento2, type %in% c('parks', 'residential', 'retail'))

df_sacramento4 <- setDT(df_sacramento3)[between(date, '2020-03-25', '2020-09-05', incbounds=FALSE)]

###AV   SANTA CLARA COUNTY DF & PROCESSING

df_santaclara <- filter(mob_df, sub_region_2 == 'Santa Clara County')

df_santaclara2 <- group_by(df_santaclara, type)

df_santaclara3 <- filter(df_santaclara2, type %in% c('parks', 'residential', 'retail'))

df_santaclara4 <- setDT(df_santaclara3)[between(date, '2020-03-25', '2020-09-05', incbounds=FALSE)]

###AV   PLOTS FOR EACH COUNTY

plot_butte <- ggplot(data = df_butte4, mapping = aes(x = date, y = pct_diff, group = type)) +
    geom_line(aes(color=type)) +
    geom_hline(yintercept = 0, alpha = .5)

plot_merced <- ggplot(data = df_merced4, mapping = aes(x = date, y = pct_diff, group = type)) +
    geom_line(aes(color=type)) +
    geom_hline(yintercept = 0, alpha = .5)

plot_sacramento <- ggplot(data = df_sacramento4, mapping = aes(x = date, y = pct_diff, group = type)) +
    geom_line(aes(color=type)) +
    geom_hline(yintercept = 0, alpha = .5)

plot_santaclara <- ggplot(data = df_santaclara4, mapping = aes(x = date, y = pct_diff, group = type)) +
    geom_line(aes(color=type)) +
    geom_hline(yintercept = 0, alpha = .5)

###AV Huh, should have created a function for these huh? This is terribly inefficient. Will redo if time permits

###AV   COMBINE PLOTS (Using GGPUBR)

finalplot <- ggarrange(plot_butte, plot_merced, plot_sacramento, plot_santaclara,
                    labels = c("Butte", "Merced", "Sacramento", "Santa Clara"),
                    ncol = 2, nrow = 2)
###AV Display Plot


finalplot



###AV Is it pretty? not at all, not even a little bit, but it is a plot, that looks mildly similar. If this were a project I would
###AV ideally find out why data keeps getting removed at nearly every step :(



#' 5. *Again, the standard narrative of Covid-19 in California says that people were staying home in the spring, then going out more in May-June as stay-at-home orders were lifted.  *
#'    *Do this data support that narrative?*
#'   There does seem to be an uptick in retail pct differences as opposed to april.
#'   
#'   

#' 6. *What other potentially interesting patterns do you see in these mobility data?* 
#' I suppose the residential weekend bit with the weekly oscillations is due not to people going out or staying in more, bur rather due to mon-fri
#' typically being times where the average time at those places is lower. So even if a population stayed home the whole time, they would still demonstrate this pattern,
#' because compared to the baseline week (pre-US acknowledgement of pandemic) on those days people would be working. It is also very strange to compare delta in time 
#' with pct_diff over time itself with this line chart, we are tracking differences over differences, which doesnt seem like the optimal way to go about visualizing this.
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
#' test places which implemented and enforced stay at home against places which didn't (and have similar demographics/environmental bits) or we could model spread, or
#' we could generally check other places/countries which had stay at home orders which were lifted and track differences that way. 
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

###AV step by step filtering, renaming, etc.

parks_june9 <- filter(mob_df, date >= '2020-06-01')
parks_june8 <- filter(parks_june9, date <= '2020-06-30')  
parks_june7 <- group_by(parks_june8, sub_region_2)
parks_june6 <- mutate(parks_june7, parks = mean(pct_diff, na.rm=TRUE))
parks_june5 <- rename(parks_june6, county = sub_region_2)
parks_june4 <- rename(parks_june5, fips = census_fips_code)
parks_june3 <- parks_june4[c('parks', 'fips', 'county')]
parks_june2 <- distinct(parks_june3)
parks_june  <- parks_june2


#' 3. *Construct a dataframe `cases_july` that reports the total level of new cases per 1 million residents of each county in July 2020.  (Don't worry about negative values.  *
#' *I'm just asking you to do `sum(cases_per_pop)`.)  This dataframe should have three columns and one row for each county in the Covid-19 data.*
#' 



cases_july9 <- filter(covid_df, date >= '2020-07-01')
cases_july8 <- filter(cases_july9, date <= '2020-07-30')  
cases_july7 <- group_by(cases_july8, county)
cases_july6 <- cases_july7
cases_july5 <- (summarize(cases_july6, variablesum = sum(cases_new, na.rm = TRUE)))
cases_july4 <- (summarize(cases_july6, variablepop = sum(population, na.rm = TRUE)))
cases_july3 <- inner_join(cases_july5, cases_july4, by = "county")
cases_july2 <- mutate(cases_july3, new_cases_per_mil = ((variablesum/variablepop)*1000000))
cases_july1 <- cases_july6[c('county','fips')]
cases_july0 <- distinct(cases_july1)
cases_julya <- inner_join(cases_july0, cases_july2, by = "county")
cases_july  <- cases_julya[c('county', 'new_cases_per_mil', 'fips')]

#' 4. *Combine `parks_june` with `cases_july` using an inner join and appropriate matching columns.  Assign the result to `summer_df`.  *
#' *(Note that the automatic checks will be looking at the `county` column.)* 
#' 
#' 
###AV note, parks_june had 57 variables before including NA, and the joining removed NA and left us with 56, the removal was due to google not providing that data

summer_df9 <- inner_join(cases_july, parks_june, by = 'fips')
summer_df8 <- rename(summer_df9, county = county.x)
summer_df7 <- summer_df8[c('county', 'fips', 'new_cases_per_mil', 'parks')]
summer_df <- summer_df7


#' 5. *Construct a scatterplot of July cases against June "parks."  The standard narrative suggests that there should be a positive correlation *
#' *between these variables: as people spent more time at parks in June, this led to more cases in July.  Does the scatterplot support this?* 
#' 

otherfinalplot <- ggplot(data = summer_df, mapping = aes(x = new_cases_per_mil, y = parks)) +
    geom_point()

otherfinalplot

skim(summer_df)

###AV this bottom part is stolen from your site under the visual eda section, i didnt write this part at all
statsidk <- summer_df %>% 
    summarize(n = n(), 
              across(.cols = c(new_cases_per_mil, parks), 
                     .fns = lst(mean, sd)), 
              cor_xy = cor(new_cases_per_mil, parks), 
              p_value = cor.test(new_cases_per_mil, parks)$p.value)

statsidk

#' ###AV *The plot supports nothing, nor does the cursory stats analysis.*





#' # Problem 4 #
#  problem 4 ----
#' *We'll be discussing this part in class.*
#' 
#' *If you haven't done so yet, read Rex Douglass' blog post "How to be Curious Instead of Contrarian About COVID-19" *
#' *(<https://rexdouglass.github.io/TIGR/Douglass_2020_How_To_Be_Curious_Instead_of_Contrarian_About_Covid19.nb.html>).  *
#' *Douglass presents eight "lessons" for non-epidemiologists who are working with epidemiological data.  I assume you're not an epidemiologist.  *
#' *If you are an epidemiologist, please pretend that you're not for the sake of this exercise.* Lol
#' 
#' *Suppose you were exploring Covid data for fun in late July or early August 2020 (cf this blog post by Kieran Healy: *
#' *<https://kieranhealy.org/blog/archives/2020/05/21/the-kitchen-counter-observatory/>), and that you found the pattern that we identified in Problem 3.  *
#' *You're contemplating writing a blog post, op-ed, or even short research letter to share this finding.  How can Douglass' 8 lessons help us determine *
#' *whether and how to share this finding?* 
#' 
#' 
#' In the beginning of the pandemic I knew it would get bad on account of the consensus from other nations. By late December 2019, in some southeast Asian nations like Korea and Japan,
#' travelers were fully masked when on international flights, and China of course already demonstrated a rapid and somewhat unchecked spread was on the horizon.
#' When I got off my flight, my fellow passengers didn't seem to know what to do, everyone from the flight was masked, everyone at SFO wasn't.
#' 
#' As the disease progressed I saw the absurd commentary noting that it would die down after a while, in a bell curve sort of manner. I saw the absurdity of our response.
#' Given this context, and 
#' given the blatant misrepresentation of the facts, the lies originating from seemingly everyone with a voice, it would seem prudent to publish in such a manner that 
#' makes the findings irreducible without context. Ought the paper be published? Yes. However one must take the precautions that someone like Richard A. Epstein may take 
#' one's findings and misconstrue them. The paper should be clear and unambiguous in its scope and ought to take preventative measure against its co-opting. Measures that do not
#' obfuscate the findings but rather ensure that their clarity remains unaltered by propagandist outlets like the Hoover Institution at Stanford. Now I will not outline 
#' any measures here, however it is clear to me that as a matter of principle one ought not lend their name to ideas or assertions that they are not willing to defend and own.
#' If I were to write an article including these findings, I would note that the data I sourced this from is trash, that the data shows literally nothing on this front, and that 
#' to call this data inconclusive would be an understatement. 
#' 
#' The data says nothing, and to an extent that ought to be said. Given the many confounding variables involved, (including the notion that
#' aggregate google data isn't liable to capture any relevant information, and perhaps how being coughed on in the face outdoors by someone with the omega variant is 
#' strikingly similar to being coughed on in the face by someone with the omega variant indoors regardless of this sort of finding) one may be better suited writing a paper on the 
#' inadequacy of Covid tracing and reporting in the US. To be honest I am not entirely clear on why google released this data like this. 
#' 
#' I suppose that a preemptive article could be
#' warranted to ensure that a intellectually or otherwise morally deficient Hooverite does not get the advantage of being first to publish this. 
#'
#' The Douglass article, above all else, serves as a vignette of a morally or capacity deficient pseudo-intellectual (albeit one with real-world credentials), and how they go 
#' about bullshitting a captive audience. While  I take the recommendations of Douglass  for granted, this article reminds me that persons like Epstein exist and are liable 
#' to co-opt any research that one may produce, with little consequence to themselves.
#' These people will undoubtedly continue to fabricate articles serving their own purposes, and one must therefore go about frustrating and otherwise vociferously demonstrating that
#' they are disapointingly and pathetically wrong. To that end, I believe that one ought to publish the data, the limitations, and one must decisively preempt co-option. The 
#' medium article used by Epstein to diminish the perception of covid's legality and spread is pro-stay at home and its thesis is quite opposed to Epstein.
#' 
#' *To summarize, be clear about what the data says, recognize that people will misconstrue everything, mitigate against co-option, and know that one's very identity can be used *
#' *against the findings, thereby damaging the discourse regardless of the correctness of the findings themselves.* As for the medium article, the author actually did a good job at
#' mitigation, he remained active and defended the work as it was, while acting against those who would have kept things open to the detriment of many human lives.
#' https://www.youtube.com/watch?v=C98FmoZVbjs.It goes without saying that the work by Douglass is fantastic, and one ought to strive for that sort of clarity in ones work.
#' 
#' The video is worth a watch. Hindsight is 20/20, however the calls for herd immunity were especially jarring, even then.







# Misc Code, Please Ignore
#
#test1 <- group_by('census_fips_code')

#count(mob_df, variable_name='census_fips_code')
#count(mob_df[c('census_fips_code')])


###AV create plots per county (I know this isn't efficient and will not be the same, however I think it may be more instructive for me to go this route than with a 
###AV facet wrap that I don't quite get yet)

#ggplot(data = df_butte2, mapping = aes(x = date, y = pct_diff)) +
#  geom_hline(yintercept = 0, alpha = .5) +
#  geom_point() +
#  geom_line(color = variable)

#summarize(df_butte2)

# df_bmss <- filter(mob_df, cut == 'Merced County', 'Butte County', 'Sacramento County', 'Santa Clara County')

#regionsUsed <- c('Merced County', 'Butte County', 'Sacramento County', 'Santa Clara County')
#df_bmss <- filter(mob_df, sub_region_2 %in% regionsUsed)

#ggplot(data = df_bmss, mapping = aes(x = date, y = pct_diff)) +
#  geom_point() +
#  geom_line(color = "blue")




#ggplot(data = df_butte3, mapping = aes(x = date, y = pct_diff, group = type)) +
#   geom_line(aes(color=type)) +
#   geom_point()

###AV the data is in the wrong format for me to be able to use this bit, they need ot be seperate y axies, but that isnt what I have done. I will check.
#ggplot(data = df_butte, mapping = aes(x = date, y = pct_diff)) +
#    geom_hline(yintercept = 0, alpha = .5) +
#    geom_line(aes(y = parks), color = "red") + 
#    geom_line(aes(y = residential), color="green", linetype="twodash") 

# groupeddf<- group_by(df_sacramento, type)

#(remove al except rows with given value in columns)
#df_new <- filter(df_santaclara2, type %in% c('parks', 'residential', 'retail'))


#cases_july3 <- list(cases_july5, cases_july4) %/% bind_rows(.id = "county")




#cases_july6 <- mutate(cases_july7, cases_over_pop = sum())



#cases_july5 <- rename(cases_july6, county = sub_region_2)
#cases_july4 <- rename(cases_july5, fips = census_fips_code)
#cases_july3 <- parks_june4[c('parks', 'fips', 'county')]

