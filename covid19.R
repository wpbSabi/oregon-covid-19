# Created by: Sabi Horvat, March 2020 - April 2020
# Starting on 2020-03-20
# Displays a map of Oregon Covid19 cases by county
# Displays an analysis line chart (not a presentation chart) 

library(tidyverse) # data
library(ggrepel) # helps with labels on plots
#library(ggpubr)
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


# import historical data

# this version works on home computer
all_data <- read_csv('covid-19-data-daily - all.csv',
                     col_type = cols(County = col_character(),
                                     `Positive†` = col_integer(),
                                     `Deaths*` = col_integer(),
                                     Negative = col_integer(),
                                     Snapshot = col_date(format = "%Y-%m-%d")))

# this version works on macbook pro
# all_data <- read_csv('covid-19-data-daily - all.csv',
#                      col_type = cols(County = col_character(),
#                                      `Positive†` = col_integer(),
#                                      `Deaths*` = col_integer(),
#                                      Negative = col_integer(),
#                                      Snapshot = col_date(format = "%m/%d/%y")))

# validate that this is actually new from yesterday's data before merge
oregon_covid19_df %>% tally(`Positive†`)
all_data %>% 
  select(`Positive†`,Snapshot) %>% 
  group_by(Snapshot) %>%
  tally(`Positive†`) %>%
  arrange(desc(Snapshot)) 

# merge the new data to the historical data
all_data_today_added <- all_data # if the data has already been updated
all_data_today_added <- rbind(all_data, oregon_covid19_df)

multnomah <- all_data_today_added %>%
  filter(County == 'Multnomah') %>%
  select(County, `Positive†`,Snapshot) %>% 
  group_by(County, Snapshot) %>%
  tally(`Positive†`) %>%
  arrange(desc(Snapshot)) 

csvFileName <- paste("covid-19-data-daily - all.csv")
write.csv(all_data_today_added, file=csvFileName, row.names = FALSE)


######## import and wrangle data ############
# county/city geo for labels/points
counties_ll <- read_csv('oregon_counties_lat_long.csv')
cities <- data.frame(city = c('Portland', 'Salem', 'Eugene'),
                     population = c(653115, 173442, 171245),
                     latitude = c(45.54, 44.92, 44.06),
                     longitude = c(-122.65, -123.02, -123.12))

# 2018 population by county for density map
county_pop <- read_csv('oregon_population_by_county.csv')

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
                   breaks=c(-1,0,10,20,50,100,150,200,250,300),
                   labels=c("0","1 - 10","11 - 20","21 - 50",
                            "51 - 100","101 - 150","151 - 200",
                            "201 - 250","251 - 300"))

p1 <- ggplot() + 
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
  ggtitle("Oregon COVID-19 Cases by County: April 2, 2020") + 
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +  
  ylab("") +
  scale_fill_manual(values = custom_color_scale)


# LINE CHART 
p2 <- ggplot(data = data_chart, aes(x = Snapshot, y = `Positive†`, 
                          color = County, label = County)) + 
  geom_line() +
  geom_label() + 
  xlab("Date") +
  ggtitle("Oregon COVID-19 Positive Tests by Day by County") 


# density map
density1 <- merge(oregon_shape, county_pop, by.x = 'NAME', by.y = 'County')
density1$pop <- cut(density1$'2018', 
                    breaks=c(-1,100000,200000,300000,400000,500000,600000,700000,800000,900000),
                    labels=c("0+","100000+","200000+","300000+","400000+","500000+","600000+",
                             "700000+","800000+"))

density1 %>% ggplot() + 
  geom_sf(aes(fill = pop)) + #, size = 0.3) + 
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                   nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Counties") +
  coord_sf()  +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  #xlab("") +  
  #ylab("") 
  scale_fill_manual(values = custom_color_scale)

# positive tests per 1,000 people 
density2 <- merge(merge2, county_pop, by.x = 'NAME', by.y = 'County') %>%
  mutate(positive_per_million =  round(`Positive†`*1000000 / `2018`, 0))
density2$pop <- cut(density2$positive_per_million, 
                    breaks=c(-1,100,200,300,400,500,600,700,800,900),
                    labels=c("0+","100+","200+","300+","400+","500+","600+",
                             "700+","800+"))


density2 %>% ggplot() + 
  geom_sf(aes(fill = pop)) + #, size = 0.3) + 
  geom_point(data = cities,aes(y = latitude, x =  longitude)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -4,nudge_y = 1,force = 0.5) +
  ggtitle("Positive Tests per Million People") +
  coord_sf()  +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  #xlab("") +  
  #ylab("") 
  scale_fill_manual(values = custom_color_scale)
# grid map
 #ggarrange(p1, density1)
          # , bp + rremove("x.text"), 
          # labels = c("A", "B", "C"),
          # ncol = 2, nrow = 2)
