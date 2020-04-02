# Created by: Sabi Horvat, March 2020 - April 2020
# Starting on 2020-03-20
# Displays a map of Oregon Covid19 cases by county
# Displays an analysis line chart (not a presentation chart) 

library(tidyverse) # data
library(ggrepel) # helps with labels on plots
library(rvest) # for webscraping
library(sf) # st_read for shape file


# set working directory to your folder, create variable first
setwd(working_directory)

# scrape data from https://govstatus.egov.com/OR-OHA-COVID-19
oregon_covid19 <- html_nodes(read_html("https://govstatus.egov.com/OR-OHA-COVID-19"),  
                             xpath='//*[@id="collapseOne"]/div/table[1]')
oregon_covid19_df <- rvest::html_table(oregon_covid19)[[1]] %>%
  mutate(Snapshot = as.Date(Sys.Date())) %>%
  filter(County != 'Total')

# output the daily data in a unique file
# csvFileName <- paste("daily_covid_",Sys.time(), ".csv",sep="")
# write.csv(oregon_covid19_df, file=csvFileName, row.names = FALSE)

write.csv(oregon_covid19_df, 'covid-19-data-daily - today_map.csv', row.names = FALSE)
# validate that this is actually new from yesterday's data before merge

# import historical data
# all_data <- read_csv('covid-19-data-daily - all.csv',
#                      col_type = cols(County = col_character(),
#                                      `Positive†` = col_integer(),
#                                      `Deaths*` = col_integer(),
#                                      Negative = col_integer(),
#                                      Snapshot = col_date(format = "%Y-%m-%d")))

all_data <- read_csv('covid-19-data-daily - all.csv',
                     col_type = cols(County = col_character(),
                                     `Positive†` = col_integer(),
                                     `Deaths*` = col_integer(),
                                     Negative = col_integer(),
                                     Snapshot = col_date(format = "%m/%d/%y")))

# merge the new data to the historical data
all_data_today_added <- rbind(all_data, oregon_covid19_df)

csvFileName <- paste("covid-19-data-daily - all.csv")
write.csv(all_data_today_added, file=csvFileName, row.names = FALSE)


######## import and wrangle data ############
# county/city geo for labels/points
counties_ll <- read_csv('oregon_counties_lat_long.csv')
cities <- data.frame(city = c('Portland', 'Salem', 'Eugene'),
                     population = c(653115, 173442, 171245),
                     latitude = c(45.54, 44.92, 44.06),
                     longitude = c(-122.65, -123.02, -123.12))

# import counties shapefile http://geog.uoregon.edu/bartlein/courses/geog495/lec06.html 
oregon_shape <- st_read('orcounty.shp') %>% select(NAME)

# covid19 data source: https://govstatus.egov.com/OR-OHA-COVID-19
oregon_cases <- read_csv('covid-19-data-daily - today_map.csv')

# merge data frames for the county map 
merge1 <- merge(oregon_shape, oregon_cases, by.x = 'NAME', by.y = 'County')
merge2 <- merge(merge1, counties_ll, by.x = 'NAME', by.y = 'County')
str(merge2)
# import data for the chart used only for analysis
data_chart <- read_csv('covid-19-data-daily - all.csv') %>%
  arrange(desc(Snapshot))
##############################################

# controlled color-coding
# https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=3
custom_color_scale <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb',
                        '#74a9cf','#3690c0','#0570b0','#045a8d',
                        '#023858')
merge2$Cases <- cut(merge2$`Positive†`, 
                   breaks=c(-1,0,10,20,30,50,75,100,150,200),
                   labels=c("0","1 - 10","11 - 20","21 - 30",
                            "31 - 50","51 - 75","76 - 100",
                            "101 - 150","151 - 200"))

p3 <- ggplot() + 
  geom_sf(data = merge2, aes(fill = Cases), size = 0.3) + 
  geom_label_repel(data = merge2, aes(Long, Lat, label = NAME), 
                   force = 0.2,
                   nudge_x = .1, 
                   nudge_y = .1,
                   size = 3
  ) +
  geom_point(data = cities, 
             aes(y = latitude, x =  longitude, size = population/2),
             show.legend = FALSE) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -2,
                  nudge_y = 1,
                  force = 0.5) +
  #           shape = 21, size = 4, color = 'black')) +
  coord_sf() +
  ggtitle("Oregon COVID-19 Cases by County: March 20, 2020") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +  
  ylab("") +
  scale_fill_manual(values = custom_color_scale)
p3


# LINE CHART 
ggplot(data = data_chart, aes(x = Snapshot, y = `Positive†`, 
                          color = County, label = County)) + 
  geom_line() +
  geom_label() + 
  xlab("Date") +
  ggtitle("Oregon COVID-19 Positive Tests by Day by County") 
 
