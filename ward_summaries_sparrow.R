require(tidyverse)
require(rgdal)
require(sp)
require(rgeos)

rawpath = "ebd_IN_relMay-2019.txt"

preimp = c("CATEGORY","COMMON.NAME",
           "LOCALITY.ID","LOCALITY.TYPE",
           "LATITUDE","LONGITUDE",
           "DURATION.MINUTES",
           "SAMPLING.EVENT.IDENTIFIER",
           "CATEGORY")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))

#data = data %>%
#  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

####################



nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", 
                 stringsAsFactors = F, na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% c("LOCALITY","LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE"))] = "NULL"
nms[nms %in% c("LOCALITY","LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE")] = NA

locs = read.delim("ebd_IN_relMay-2019.txt", colClasses = nms, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
locs = locs %>%
  distinct(LOCALITY,LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE)

load("data.RData")
toadd = data %>%
  distinct(group.id,LONGITUDE,LATITUDE)

load("dataforanalyses.RData")
data = left_join(data,toadd)

temp1 = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 

shp1 = readOGR("Chennai","Zones")
shp2 = readOGR("Bangalore","Bangalore Divisions") 
shp3 = readOGR("Hyderabad","Hyderabad Circles") 
shp4 = readOGR("Kolkata","Kolkata AC") 
shp5 = readOGR("Mumbai","Mumbai AC") 
shp6 = readOGR("New Delhi","Delhi AC") 


temp = temp1

rownames(temp) = temp$group.id # only to setup adding the SEI column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,shp1) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
temp = left_join(temp,data)

tempa = temp %>%
  filter(!is.na(Zone_Name))

temp = temp1

rownames(temp) = temp$group.id # only to setup adding the SEI column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,shp2) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
temp = left_join(temp,data)

tempb = temp %>%
  filter(!is.na(Division))

temp = temp1

rownames(temp) = temp$group.id # only to setup adding the SEI column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,shp3) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
temp = left_join(temp,data)

tempc = temp %>%
  filter(!is.na(Ward.No))

temp = temp1

rownames(temp) = temp$group.id # only to setup adding the SEI column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,shp4) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
temp = left_join(temp,data)

tempd = temp %>%
  filter(!is.na(AC_NAME))

temp = temp1

rownames(temp) = temp$group.id # only to setup adding the SEI column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,shp5) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
temp = left_join(temp,data)

tempe = temp %>%
  filter(!is.na(AC_NAME))

temp = temp1

rownames(temp) = temp$group.id # only to setup adding the SEI column for the future left_join
coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
temp = over(temp,shp6) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
temp = data.frame(temp) # convert into data frame for left_join
temp$group.id = rownames(temp) # add column to join with the main data
temp = left_join(temp,data)

tempf = temp %>%
  filter(!is.na(AC_NAME))

tempa = tempa %>%
  select(names(data)) %>% mutate(city = "Chennai")
tempb = tempb %>%
  select(names(data)) %>% mutate(city = "Bangalore")
tempc = tempc %>%
  select(names(data)) %>% mutate(city = "Hyderabad")
tempd = tempd %>%
  select(names(data)) %>% mutate(city = "Kolkata")
tempe = tempe %>%
  select(names(data)) %>% mutate(city = "Mumbai")
tempf = tempf %>%
  select(names(data)) %>% mutate(city = "Delhi")

metros = rbind(tempa,tempb,tempc,tempd,tempe,tempf)

rm(list=setdiff(ls(envir = .GlobalEnv), c("metros")), pos = ".GlobalEnv")

c1 = temp %>%
  group_by(Zone_Name) %>%
  summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER),richness = n_distinct(COMMON.NAME)) %>% 
  ungroup

c11 = temp %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup %>%
  group_by(Zone_Name) %>%
  summarize(hours = sum(na.omit(DURATION.MINUTES))/60) %>% ungroup

c1 = left_join(c1,c11)

#c2 = temp %>%
#  group_by(AC_NAME) %>% mutate(checklists = n_distinct(group.id)) %>% ungroup %>%
#  group_by(AC_NAME,COMMON.NAME) %>%
#  summarize(freq = n_distinct(group.id)/max(checklists)) %>%
#  arrange(desc(freq), .by_group = TRUE) %>% slice(1:5) %>% ungroup

c3 = temp %>%
  filter(LOCALITY.TYPE == "H") %>%
  group_by(Zone_Name,LOCALITY.ID) %>%
  summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER),richness = n_distinct(COMMON.NAME)) %>% 
  ungroup

c33 = temp %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup %>%
  group_by(Zone_Name,LOCALITY.ID) %>%
  summarize(hours = sum(na.omit(DURATION.MINUTES))/60) %>% ungroup

c3 = left_join(c3,c33)

c3 = c3 %>%
  group_by(Zone_Name) %>%
  arrange(desc(richness), .by_group = TRUE) %>% slice(1:5) %>% ungroup

c4 = temp %>%
  filter(LOCALITY.TYPE == "H") %>%
  group_by(LOCALITY.ID) %>%
  summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER),richness = n_distinct(COMMON.NAME)) %>% 
  ungroup

c44 = temp %>%
  group_by(SAMPLING.EVENT.IDENTIFIER) %>% slice(1) %>% ungroup %>%
  group_by(LOCALITY.ID) %>%
  summarize(hours = sum(na.omit(DURATION.MINUTES))/60) %>% ungroup

c4 = left_join(c4,c44)

c4 = c4 %>%
  arrange(desc(richness)) %>% slice(1:10)


c3 = left_join(c3,locs)
c4 = left_join(c4,locs)

write.csv(c1,"Chennai_summaries.csv")
#write.csv(c2,"Delhi_topspecies_zones.csv")
write.csv(c3,"Chennai_locations_zones.csv")
write.csv(c4,"Chennai_locations.csv")

