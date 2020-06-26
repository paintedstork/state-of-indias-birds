load("maps.RData")
load("data.RData")

require(tidyverse)
require(ggfortify)
require(rgdal)
require(sp)
require(sf)
require(rgeos)
require(raster)

a = spsample(indiamap, n = 200000, "stratified")
b = data.frame(a@coords)
names(b) = c("LONGITUDE","LATITUDE")
l = length(b[,1])
b$sl = as.character(1:l)

temp = b

rownames(temp) = 1:l # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,gridmapg1) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$sl = rownames(temp) # add column to join with the main data
c = left_join(temp,b)

temp = b

rownames(temp) = 1:l # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,gridmapg2) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$sl = rownames(temp) # add column to join with the main data
d = left_join(temp,b)

temp = b

rownames(temp) = 1:l # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,gridmapg3) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$sl = rownames(temp) # add column to join with the main data
e = left_join(temp,b)

temp = b

rownames(temp) = 1:l # only to setup adding the group.id column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
#proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,gridmapg4) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$sl = rownames(temp) # add column to join with the main data
f = left_join(temp,b)

emptyg1 = as.character(setdiff(unique(c$id),unique(data$gridg1)))
emptyg2 = as.character(setdiff(unique(d$id),unique(data$gridg2)))
emptyg3 = as.character(setdiff(unique(e$id),unique(data$gridg3)))
emptyg4 = as.character(setdiff(unique(f$id),unique(data$gridg4)))

save(emptyg1,emptyg2,emptyg3,emptyg4,file = "emptycells.RData")


