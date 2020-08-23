# Created by: Sabi Horvat, March 2020 - April 2020
# (1) map of total Oregon Covid19 positive test by county
# (2) line chart of cumulative Oregon Covid19 positives tests 
# (3) multnomah county new positive tests by day (w/i run)
# (4) density maps for comparison (commented out)
# to do (line 30 in updated format):
# # instead of these mutates and select, could merge without the field names

library(tidyverse)  # data wrangling and ggplot2
library(ggrepel)    # helps with labels on plots
library(rvest)      # webscraping
library(sf)         # st_read for shape file
library(ggthemes)   # plotting background

### data processing

# scrape data from https://govstatus.egov.com/OR-OHA-COVID-19
oregon_covid19 <- html_nodes(read_html("https://govstatus.egov.com/OR-OHA-COVID-19"),  
                  # xpath='//*[@id="collapseOne"]/div/table[1]') # til 5/4/2020
                    xpath='//*[@id="collapseDemographics"]/div/table[1]') # 5/5 on
oregon_covid19_df <- rvest::html_table(oregon_covid19)[[1]] %>%
  mutate(Snapshot = as.Date(Sys.Date())) %>%
  #mutate(Snapshot = as.Date('2020-06-29')) %>% # if scraping next morning
  filter(County != 'Total') %>% # also new column names on 5/5/2020
  mutate(`Positive†` = Cases1) %>%
  mutate(`Deaths*` = Deaths2) %>%
  mutate(`Negative` = Negatives3) %>%
  select(County,`Positive†`,`Deaths*`,`Negative`, Snapshot )
  
# instead of these mutates and select, could merge without the field names
  
# import historical data
# this version works on some computers
all_data <- read_csv('covid-19-data-daily - all.csv',
                     col_type = cols(County = col_character(),
                                     `Positive†` = col_integer(),
                                     `Deaths*` = col_integer(),
                                     Negative = col_integer(),
                                     Snapshot = col_date(format = "%Y-%m-%d")))
# this version works on some other computers or if the CSV is edited
# all_data <- read_csv('covid-19-data-daily - all.csv',
#                      col_type = cols(County = col_character(),
#                                      `Positive†` = col_integer(),
#                                      `Deaths*` = col_integer(),
#                                      Negative = col_integer(),
#                                      Snapshot = col_date(format = "%m/%d/%y")))

# to validate data is new vs. yesterday's data before merge
head(all_data %>% 
  select(`Positive†`,Snapshot) %>% 
  group_by(Snapshot) %>%
  tally(`Positive†`) %>%
  arrange(desc(Snapshot)))
oregon_covid19_df %>% tally(`Positive†`)

# merge the new data to the historical data
# all_data_today_added <- all_data # if the data has already been updated
all_data_today_added <- rbind(all_data, oregon_covid19_df)

# multnomah county by day 
head(all_data_today_added %>%
  filter(County == 'Multnomah') %>%
  select(County, `Positive†`,Snapshot) %>% 
  group_by(County, Snapshot) %>%
  tally(`Positive†`) %>%
  arrange(desc(Snapshot)))

# output the cumulative daily data by day
csvFileName <- paste("covid-19-data-daily - all.csv")
write.csv(all_data_today_added, file=csvFileName, row.names = FALSE)

### visualization preparation

# county lat long for lables and city lat long for labels/points
counties_ll <- read_csv('oregon_counties_lat_long.csv')
cities <- data.frame(city = c('Portland', 'Salem', 'Eugene'),
                     population = c(653115, 173442, 171245),
                     latitude = c(45.54, 44.92, 44.06),
                     longitude = c(-122.65, -123.02, -123.12))

# import counties shapefile and merge w/ today's data for map
# http://geog.uoregon.edu/bartlein/courses/geog495/lec06.html 
oregon_shape <- st_read('orcounty.shp') %>% select(NAME)
merge1 <- merge(oregon_shape, oregon_covid19_df, by.x = 'NAME', by.y = 'County')
merge2 <- merge(merge1, counties_ll, by.x = 'NAME', by.y = 'County')

# controlled color-coding
# https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=3
custom_color_scale <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb',
                        '#74a9cf','#3690c0','#0570b0','#045a8d',
                        '#023858')

# set breakpoints for map shading
merge2$Cases <- cut(merge2$`Positive†`, 
                   breaks=c(-1,0,10,20,50,100,500,1000,5000,10000),
                   labels=c("0","1 - 10","11 - 20","21 - 50",
                            "51 - 100","101 - 500","501 - 1000",
                            "1001 - 5000","5001 - 10000"))

# map of shaded counties by positive tests
plot_map <- ggplot() + 
  geom_sf(data = merge2, aes(fill = Cases), size = 0.3) + 
  geom_label_repel(data = merge2, aes(Long, Lat, label = NAME), 
                   force = 0.2, nudge_x = .1, nudge_y = .1,
                   size = 3) + 
  geom_point(data = cities, 
             aes(y = latitude, x =  longitude, size = population/2),
             show.legend = FALSE) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -2,
                  nudge_y = 1,
                  force = 0.5) +
  coord_sf() +
  ggtitle("Oregon COVID-19 Cases by County: ", Sys.Date()) + 
  # 2020-05-02 added Sys.Date() rather than hardcoding the date
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + # clears default label, not wanted for the map
  ylab("") + # clears default label, not wanted for the map
  scale_fill_manual(values = custom_color_scale)
plot_map
# line chart:  counties with most positive tests 
# as of 2020-07-15, at least 300 cases
more_cases <- all_data_today_added %>%
  filter(County == 'Washington' | County == 'Multnomah' |
           County == 'Marion' | County == 'Clackamas' | 
           County == 'Lincoln' | County == 'Lane' | 
           County == 'Umatilla' | County == 'Malheur' |
           County == 'Union') %>%
  mutate(n = `Positive†`) %>%
  select(County, Snapshot, n)
less_cases <- all_data_today_added %>%
  filter(County != 'Washington' & County != 'Multnomah' &
           County != 'Marion' & County != 'Clackamas' &
           County != 'Lincoln' & County != 'Lane' & 
           County != 'Umatilla' & County != 'Malheur' &
           County != 'Union') %>%
  select(Snapshot, `Positive†`) %>%
  mutate(County = 'The Other 27 Counties') %>%
  group_by(County,Snapshot) %>%
  tally(sum(`Positive†`)) %>%
  arrange(desc(n))
cases <- bind_rows(more_cases,less_cases)

plot_line_chart <- ggplot(data = cases, aes(x = Snapshot, y = n, 
                                    color = County,
                                    label = County))+
  geom_line(size = 2) +
  xlab("Date") +
  ggtitle("Oregon COVID-19 Positive Tests by County") +
  theme_bw() + 
  scale_color_manual(values = c('#a6cee3','#1f78b4','#e78ac3',
                                '#33a02c','#7570b3','#7fcdbb',
                                '#de2d26','#636363','#bdbdbd',
                                '#fdbb84'))
plot_line_chart
all_counties_chart <- ggplot(data = all_data_today_added, aes(x = Snapshot, y = `Positive†`, 
                                            color = County,
                                            label = County))+
  geom_line(size = 2) +
  xlab("Date") +
  ggtitle("Oregon COVID-19 Positive Tests by County") +
  theme_bw() 
# # density map
# # 2018 population by county for density map 
 county_pop <- read_csv('oregon_population_by_county.csv')
density1 <- merge(oregon_shape, county_pop, by.x = 'NAME', by.y = 'County')
density1$pop <- cut(density1$'2018',
                    breaks=c(-1,100000,200000,300000,400000,500000,600000,
                             700000,800000,900000),
                    labels=c("0+","100000+","200000+","300000+","400000+",
                             "500000+","600000+",
                             "700000+","800000+"))
density1 %>% ggplot() +
  geom_sf(aes(fill = pop)) +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                   nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Population by Counties") +
  coord_sf()  +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = custom_color_scale)
# positive tests per 1,000 people
density2 <- merge(merge2, county_pop, by.x = 'NAME', by.y = 'County') %>%
  mutate(positive_per_million =  round(`Positive†`*1000000 / `2018`, 0))
density2$pop <- cut(density2$positive_per_million,
                    breaks=c(-1,100,500,1000,2000,5000,10000,20000,30000,40000),
                    labels=c("0+","500+","1,000+","2,000+","5,000+","10,000+","20,000+",
                             "30,000+","40,000+"))

# # map density of covid19 by county
density2 %>% ggplot() +
  geom_sf(aes(fill = pop)) + #, size = 0.3) +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Positive Tests per Million People") +
  coord_sf()  +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = custom_color_scale)

