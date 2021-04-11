# Created by: Sabi Horvat, March 2020 - April 2020
# Updated February 2020 for scaling and gif
# This script includes webscraping from the Oregon Health Authority website
#   the scrape pulls cumulative Covid19 positive tests by county
# The following plots are different than the plots which are easily available:
# (1) Map of total Oregon Covid19 positive test by county
# (2) Line chart of cumulative Oregon Covid19 positives tests by county
# (3) Density maps for comparison

library(tidyverse)  # data wrangling and ggplot2
library(ggrepel)    # helps with labels on plots
library(rvest)      # webscraping
library(sf)         # st_read for shape file
library(ggthemes)   # plotting background
library(cowplot)    # plot multiple plots in a grid
library(magick)     # create a gif


### data processing

# scrape data from https://govstatus.egov.com/OR-OHA-COVID-19
new_data <- html_nodes(read_html("https://govstatus.egov.com/OR-OHA-COVID-19"),  
                  # xpath='//*[@id="collapseOne"]/div/table[1]') # til 2020-05-05
                    xpath='//*[@id="collapseDemographics"]/div/table[1]') # 2020-05-05
new_data_df <- rvest::html_table(new_data)[[1]] %>%
  mutate(Snapshot = as.Date(Sys.Date())) %>%
  #mutate(Snapshot = as.Date('2020-06-29')) %>% # if scraping next morning
  filter(County != 'Total') %>% # also new column names on 2020-05-05
  mutate(`Positive†` = Cases1) %>%
  mutate(`Deaths*` = Deaths2) %>%
#  mutate(`Negative` = Negatives3) %>% # 2020-12-29 Negatives is no longer in the OHA table 
  select(County,`Positive†`,`Deaths*`, Snapshot )
  
# import historical data
# this import statement works on most computers
all_data <- read_csv('covid-19-data-daily - all.csv',
                     col_type = cols(County = col_character(),
                                     `Positive†` = col_integer(),
                                     `Deaths*` = col_integer(),
                                   #  Negative = col_integer(), # removed 2020-12-29
                                     Snapshot = col_date(format = "%Y-%m-%d")))
# this import statement alternatively works on some other computers or if the CSV is edited
# all_data <- read_csv('covid-19-data-daily - all.csv',
#                      col_type = cols(County = col_character(),
#                                      `Positive†` = col_integer(),
#                                      `Deaths*` = col_integer(),
#                                     # Negative = col_integer(),
#                                      Snapshot = col_date(format = "%m/%d/%y")))

# to validate data is new vs. yesterday's data before merge, automated on 2020-12-29
# if_else is more strict, checking the type also, while ifelse will promote types as necessary
duplicate_validation <- ifelse(max(new_data_df$Snapshot) == max(all_data$Snapshot), 
            'Validation: Error - This data has already been loaded',
            'Validation: Proceed')
duplicate_validation
# the old manual version to validate, while I was looking at the daily increase closely
# head(all_data %>% 
#   select(`Positive†`,Snapshot) %>% 
#   group_by(Snapshot) %>%
#   tally(`Positive†`) %>%
#   arrange(desc(Snapshot)))
# new_data_df %>% tally(`Positive†`)

# if there is new data, merge that new data to the historical data
ifelse(duplicate_validation == 'Validation: Proceed', 
       all_data_today_added <- rbind(all_data, new_data_df),
       all_data_today_added <- all_data)

# output the cumulative daily data by day
csvFileName <- paste("covid-19-data-daily - all.csv")
write.csv(all_data_today_added, file=csvFileName, row.names = FALSE)


### visualization preparation

# county latitude and longitude for lables and city lat long for labels & points
counties_ll <- read_csv('oregon_counties_lat_long.csv')
cities <- data.frame(city = c('Portland', 'Salem', 'Eugene'),
                     population = c(653115, 173442, 171245),
                     latitude = c(45.54, 44.92, 44.06),
                     longitude = c(-122.65, -123.02, -123.12))

# import counties shapefile and merge w/ covid data for the map
# http://geog.uoregon.edu/bartlein/courses/geog495/lec06.html 
oregon_shape <- st_read('orcounty.shp') %>% select(NAME)
merge1 <- merge(oregon_shape, new_data_df, by.x = 'NAME', by.y = 'County')
merge2 <- merge(merge1, counties_ll, by.x = 'NAME', by.y = 'County')

# controlled color-coding
# https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=3
custom_color_scale <- c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb',
                        '#74a9cf','#3690c0','#0570b0','#045a8d',
                        '#023858')

# set breakpoints for map shading
merge2$Cases <- cut(merge2$`Positive†`, 
                   breaks=c(-1,0,50,100,500,1000,5000,10000,20000,40000),
                   labels=c("0","1 - 50","51 - 100","101 - 500",
                            "501 - 1000","1001 - 5000","5001 - 10000",
                            "10001 - 20000","20001 - 40000"))

# map of shaded counties by positive tests
plot_covid_positives <- ggplot() + 
  geom_sf(data = merge2, aes(fill = Cases), size = 0.3) + 
  geom_label_repel(data = merge2, aes(Long, Lat, label = NAME), 
                   force = 0.2, nudge_x = .1, nudge_y = .1,
                   size = 3) + 
  geom_point(data = cities, 
             aes(y = latitude, x =  longitude, size = population/2),
             show.legend = FALSE) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -2, nudge_y = 1, force = 0.5) +
  coord_sf() +
  ggtitle(paste("Oregon COVID-19 Cases by County: ", Sys.Date(), sep = " ")) + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") + # clears default label, not wanted for the map
  ylab("") + # clears default label, not wanted for the map
  scale_fill_manual(values = custom_color_scale)
plot_covid_positives

# data prep for line chart:  25% counties with most positive tests 
#    the other 75% are grouped into an "other" column
more_cases <- all_data_today_added %>%
  filter(County == 'Multnomah' | County == 'Washington' |
           County == 'Marion' | County == 'Clackamas' | 
           County == 'Lane' | County == 'Jackson' | 
           County == 'Umatilla' | County == 'Malheur' |
           County == 'Deschutes') %>%
  mutate(n = `Positive†`) %>%
  select(County, Snapshot, n)
less_cases <- all_data_today_added %>%
  filter(County != 'Washington' & County != 'Multnomah' &
           County != 'Marion' & County != 'Clackamas' &
           County != 'Lane' & County != 'Jackson' & 
           County != 'Umatilla' & County != 'Malheur' &
           County != 'Deschutes') %>%
  select(Snapshot, `Positive†`) %>%
  mutate(County = 'The Other 27 Counties') %>%
  group_by(County,Snapshot) %>%
  tally(sum(`Positive†`)) %>%
  arrange(desc(n))
cases <- bind_rows(more_cases,less_cases)

plot_line_chart <- ggplot(data = cases, aes(x = Snapshot, y = n, 
                                    color = County))+
  geom_line(size = 2) +
  xlab("Date") +
  ggtitle("Oregon COVID-19 Positive Tests by County") +
  theme_bw() + 
  scale_color_manual(values = c('#a6cee3','#1f78b4','#e78ac3',
                                '#33a02c','#7570b3','#7fcdbb',
                                '#de2d26','#636363','#bdbdbd',
                                '#fdbb84')) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
plot_line_chart

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
plot_population_density <- density1 %>% ggplot() +
  geom_sf(aes(fill = pop)) +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                   nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Population by Counties") +
  coord_sf()  +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0, 20, 0, 20)) +
  scale_fill_manual(values = custom_color_scale)
plot_population_density

# positive tests per 1,000 people
density2 <- merge(merge2, county_pop, by.x = 'NAME', by.y = 'County') %>%
  mutate(positive_per_million =  round(`Positive†`*1000000 / `2018`, 0))
density2$pop <- cut(density2$positive_per_million,
                    breaks=c(-1,10000,20000,30000,40000,50000,60000,70000,100000,150000),
                    labels=c("0+","10,000+","20,000+","30,000+","40,000+","50,000+","60,000+",
                             "70,000+","100,000+"))
# map density of covid19 by county
plot_positive_density <- density2 %>% ggplot() +
  geom_sf(aes(fill = pop)) + #, size = 0.3) +
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Positive Tests per Million People") +
  coord_sf()  +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0, 20, 0, 20)) +
  scale_fill_manual(values = custom_color_scale)
plot_positive_density

plot_grid(plot_population_density, plot_positive_density,
          labels="Population and Positivity Densities", hjust=-0.5, vjust=10) 

# generate a GIF from monthly snapshots of the map of shaded counties by positive tests
gif_df <- data.frame(file = c('oregon_covid_for_gif/Rplot01.png',
                              'oregon_covid_for_gif/Rplot02.png',
                              'oregon_covid_for_gif/Rplot03.png',
                              'oregon_covid_for_gif/Rplot04.png',
                              'oregon_covid_for_gif/Rplot05.png',
                              'oregon_covid_for_gif/Rplot06.png',
                              'oregon_covid_for_gif/Rplot07.png',
                              'oregon_covid_for_gif/Rplot08.png',
                              'oregon_covid_for_gif/Rplot09.png',
                              'oregon_covid_for_gif/Rplot10.png',
                              'oregon_covid_for_gif/Rplot11.png',
                              'oregon_covid_for_gif/Rplot12.png',
                              'oregon_covid_for_gif/Rplot13.png'))

for(i in 1:length(gif_df$file)) {
  images <- map(gif_df$file, image_read)
  images <- image_join(images)
  animation <- image_animate(images, fps = 0.5)
  image_write(animation, 'Oregon Covid 2020 March to December.gif')
}

