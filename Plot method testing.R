# More packages than required but nevertheless

library(data.table)
library(ggplot2)
library(ggfortify)
library(lubridate)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(gridExtra)
library(grid)
library(viridis)
library(ggthemes)
library(raster)
library(tidyverse)
library(leaflet)
library(shiny)

theme_set(theme_tufte())

###### main dataframes to use below, just random aggregates now #####


temp = all %>% 
  group_by(ST_NM) %>%
  mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,ST_NM) %>%
  summarize(freq = n()/max(lists))

temp1 = temp %>%
  filter(COMMON.NAME == "Asian Fairy-bluebird")


###### select appropriate fortified data frame below ######


temp2 = fortify_state


###############


plotdf = na.omit(left_join(temp2,temp1, by = c('id' = "ST_NM"))) # SPDF to plot


mask = fortify(indiamask27)
border = fortify(indiamap)

map1 = plotindiamap +
  geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq)) +
  geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white')+
  geom_path(data = border, aes(x = long, y = lat, group = group), col = 'black')

map1 +
  scale_fill_viridis() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 8))
