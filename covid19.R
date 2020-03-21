# Created by: Sabi Horvat
# Starting on 2020-03-20
# Display a map of Oregon Covid19 cases by county

library(ggrepel)
library(sf) # st_read
library(tidyverse)

######## import and wrangle data ############
# import counties latitude and longitude
counties_ll <- read_csv('oregon_counties_lat_long.csv')
# import counties shapefile http://geog.uoregon.edu/bartlein/courses/geog495/lec06.html 
setwd("/Users/horvasab/Documents/github/coronavirus")
oregon_shape <- st_read('orcounty.shp') # requires sf package
# oregon_shape$NAME <- toupper(oregon_shape$NAME) # if merging with data source
# TO OPTIMIZE UPDATE TO  select only the fields that are needed
# data source: https://govstatus.egov.com/OR-OHA-COVID-19
oregon_cases <- read_csv('2020_03_20.csv')
merge1 <- merge(oregon_shape, oregon_cases, by.x = 'NAME', by.y = 'County')
merge2 <- merge(merge1, counties_ll, by.x = 'NAME', by.y = 'County')
cities <- data.frame(city = c('Portland', 'Salem', 'Eugene'),
                     population = c(653115, 173442, 171245),
                     latitude = c(45.54, 44.92, 44.06),
                     longitude = c(-122.65, -123.02, -123.12))
##############################################

# controlled color-coding
# https://colorbrewer2.org/#type=sequential&scheme=PuBu&n=3
custom_color_scale <- c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d')
merge2$Cases <- cut(merge2$`Number of cases`, 
                   breaks=c(-1, 0, 5, 10, 20, 50), 
                   labels=c("0", "1 - 5", "5 - 10", 
                            "10 - 20", "20 - 50"))

p3 <- ggplot() + 
  geom_sf(data = merge2, aes(fill = Cases), size = 0.3) + 
  geom_label_repel(data = merge2, aes(Long, Lat, label = NAME), 
                   force = 0.2,
                   nudge_x = .1, 
                   nudge_y = .1,
                   size = 3
  ) +
  geom_point(data = cities, 
             aes(y = latitude, x =  longitude, size = population)) +
  geom_text_repel(data = cities, aes(longitude, latitude, label = city),
                  nudge_x = -2,
                  nudge_y = 1,
                  force = 0.5) +
  #           shape = 21, size = 4, color = 'black')) +
  coord_sf() +
  ggtitle("Oregon counties with COVID-19 cases") + 
  scale_fill_manual(values = custom_color_scale)
p3

# here's each data layer to test out new features in shorter time
# p1 <- merge2 %>% ggplot() + 
#   geom_sf(aes(fill = Cases), size = 0.3) + 
#   ggtitle("Oregon counties with COVID-19 cases") + 
#   scale_fill_manual(values = custom_color_scale) +
#   geom_label_repel(aes(Long, Lat, label = NAME), 
#                    force = 0.2,
#                    nudge_x = .1, 
#                    nudge_y = .1,
#                    size = 3
#                    ) +
#   coord_sf() 
# 
# p2 <- cities %>% ggplot(aes(y = latitude, x =  longitude)) + 
#   geom_point(aes(colour=factor(population), fill = factor(population),
#                  size = population))
#            #  shape = 21, size = 4, color = 'black')
# p2