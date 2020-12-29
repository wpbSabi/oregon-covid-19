
# What is oregon-covid-19? 

This program creates visualizations of the Coronavirus (COVID-19) spread in Oregon.
The data source is from the Oregon Health Authority (OHA) website, and the data is scraped using the rvest package in R. 

# Why/History

These visualizations are helpful to understand how COVID-19 is spreading in Oregon. 

## Want to know more or see a quick summary?

Check out 'Overview oregon-covid-19.pdf'

# How do I run the code?

If you have cloned the repository, run 'covid19.R'

Alternatively, you may run 'Overview with code.Rmd' if you prefer to knit any modifications directly into a PDF afterwards.

In addition to creating the visualizations, the R script and/or RMarkdown will also update the historical repository of scrapes in the CSV file.  Don't worry if you run the script multiple times in the same day, as duplicate data will not be written as there is a validation check to ensure that only one webscrape is conducted each day.  The OHA website is only updated with data once per day, but the time of day is not consistent and the website is no longer updated on Saturdays and Sundays.