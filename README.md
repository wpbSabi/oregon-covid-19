
# Oregon COVID-19 

The R script and R markdown in this repository are equivalent methods for the following visualizations of the Coronavirus (COVID-19) spread in Oregon.
1. Positive Test Results by County (by day) on an Oregon county map
2. Cumulative Positive Test Results on a line chart to compare counties

The data source is the Oregon Health Authority (OHA) website, and the data is scraped using the rvest package in R. 


## Why/History

These visualizations are helpful to understand how COVID-19 is spreading in Oregon. Whereas statewide data of daily positive test results has been readily available, I was interested to learn more about how the coronavirus pandemic was spreading closer to home.  Therefore, I started utilizing the public data by county to create visualizations and track trends.  At first, I scraped the data daily.  Currently, I scrape the data monthly for the GIF but no longer run the R script daily.  In addition to developing more reporting systems, after a few months the OHA limited updates to weekdays, and created a weekly report that also included positive tests by zipcode (for those zipcodes with a minimum number of cases).




Links to data:
* [OHA Main Response page](https://govstatus.egov.com/OR-OHA-COVID-19)
* [OHA's New Dashboard](https://public.tableau.com/profile/oregon.health.authority.covid.19#!/vizhome/OregonHealthAuthorityCOVID-19DataDashboard/COVID-19EPICases)
* [OHA Daily Reports and Weekly Updates](https://www.oregon.gov/oha/erd/pages/covid-19-news.aspx)
* [Oregonian by Zipcode based on OHA Weekly Updates](https://projects.oregonlive.com/coronavirus/cases-by-zip)


## Oregon Health Authority Covid-19 Statewide Dashboard 

The OHA's dashboard now shows the daily positive test results among other factors. This first visual is directly from the OHA dashboard and has become the standard way of reporting the cases of coronavirus.

![OHA Data](https://github.com/wpbSabi/oregon-covid-19/blob/master/scripts_and_data/OHA_data.png)


## Oregon Covid by County Visualizations

For reviewing the data by county, let's first look at the county population density and the total positive tests per million people.  About 70% of Oregon's population resides in the Willamette River Valley near the northwestern part of the state.  The Willamette Valley is bounded by the coastal mountain range on the west, the Cascade Mountain Range on the east, and the Columbia River- which forms the state border- on the north.

![OHA](https://github.com/wpbSabi/oregon-covid-19/blob/master/images/Populalation%20and%20Positivity%20Densities.png)

The cumulative cases by county provides insight to the trends by county.  The similarity of the trends among counties shows that each county's cornonavirus infection rate is correlated to the rest of the state. 

![Line](https://github.com/wpbSabi/oregon-covid-19/blob/master/images/Cases%20by%20County%20Line%20Chart.png)

The following GIF shows the cumulative monthly COVID-19 positive test results by county.  This visualization is able to surface specific shorter-term trends in the spread of the coronavirus.

![GIF](https://github.com/wpbSabi/oregon-covid-19/blob/master/images/Oregon%20Covid%20GIF.gif)


## How do I run the code?

If you have cloned the repository, run 'covid19.R'

Alternatively, you may run 'Overview with code.Rmd' if you prefer to knit any modifications directly into a PDF afterwards.

In addition to creating the visualizations, the R script and/or RMarkdown will also update the historical repository of scrapes in the CSV file.  Don't worry if you run the script multiple times in the same day, as duplicate data will not be written as there is a validation check to ensure that only one webscrape is conducted each day.  The OHA website is only updated with data once per day, but the time of day is not consistent and the website is no longer updated on Saturdays and Sundays.
