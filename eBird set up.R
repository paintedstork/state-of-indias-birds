setwd("/home/suhel/Documents/eBird/IndiaData")
load("All-eBird-India-data-2018-07-06.RData")

## we should have a detailed description of what this code is supposed to achieve.

library(tidyverse)
library(lubridate)
library(rgdal)
library(gridExtra)
library(grid)
library(data.table)
library(ggplot2)
library(sp)

## set date, add month, year and day columns

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

all$OBSERVATION.DATE = as.Date(all$OBSERVATION.DATE)
all$month = month(all$OBSERVATION.DATE)
all$year = year(all$OBSERVATION.DATE)
all$day = day(all$OBSERVATION.DATE) + cdays[all$month]
all$week = week(all$OBSERVATION.DATE)
all$fort = ceiling(all$day/14)

## use group by and summarize to get temporal and spatial statistics

## remove KL atlas lists

all = all %>% 
  group_by(group.id) %>% filter(SUBNATIONAL1_CODE != "IN-KL" | !grepl("atlas", FULL.NAME)) %>%
  group_by(group.id) %>% filter(SUBNATIONAL1_CODE != "IN-KL" | !grepl("Atlas", FULL.NAME)) %>%
  group_by(group.id) %>% filter(SUBNATIONAL1_CODE != "IN-KL" | !grepl("ATLAS", FULL.NAME))

## filter KL

all = all %>%
  filter(SUBNATIONAL1_CODE == "IN-KL")

## filter approved observations, complete lists, species, slice by single group ID

all = all %>% 
  filter(ALL.SPECIES.REPORTED == 1, APPROVED == 1, CATEGORY == "species") %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>%
  ungroup


## select only species that appear in more than 648 lists in the state, arbitrary

#all = all %>%
#  group_by(COMMON.NAME) %>% filter(n() > 648) %>%     
#  ungroup

## select 200 most common species from the atlas

all = all %>% filter(COMMON.NAME %in% atlascommon200)



## move personal locations to nearest hotspots

allhotspots = all %>% filter(LOCALITY.TYPE == "H") %>%
  group_by(LOCALITY.ID) %>% summarize(max(LATITUDE),max(LONGITUDE)) %>%
  ungroup

names(allhotspots)[c(2,3)] = c("LATITUDE","LONGITUDE")

setDT(all)

hotspots = as.matrix(allhotspots[, 3:2])

all[, LOCALITY.HOTSPOT := allhotspots[which.min(spDists(x = hotspots, y = cbind(LONGITUDE, LATITUDE))),]$LOCALITY.ID, by=.(LONGITUDE, LATITUDE)] 

rm(hotspots,allhotspots)

all = as.data.frame(all)



## expand data frame to include all bird species in every list

allexpanded = expand.grid(unique(all$COMMON.NAME), unique(all$group.id))
names(allexpanded) = c("COMMON.NAME", "group.id")

## choosing important variables

unimp = c(1,5,12,13,14,16,20,22,23,24,27,28,30,34,36,37,38,39,40,41,42)
all = all[,-unimp]
listlevelvars = c("COUNTRY_CODE","SUBNATIONAL1_CODE","SUBNATIONAL2_CODE","COUNTY",
                  "LOCALITY","LOCALITY.ID","LOCALITY.TYPE","LATITUDE","LONGITUDE",
                  "OBSERVATION.DATE","OBSERVER.ID","FULL.NAME","EMAIL.ADDRESS",
                  "SAMPLING.EVENT.IDENTIFIER","PROTOCOL.TYPE","PROJECT.CODE",
                  "TIME.OBSERVATIONS.STARTED","DURATION.MINUTES","EFFORT.DISTANCE.KM",
                  "EFFORT.AREA.HA","NUMBER.OBSERVERS","ALL.SPECIES.REPORTED",
                  "TRIP.COMMENTS","GROUP.IDENTIFIER","group.id","no.sp","year.month",
                  "upload.month","CATEGORY","month","year","day","week","fort","LOCALITY.HOTSPOT","season")
specieslevelvars = c("TAXONOMIC.ORDER","COMMON.NAME","SPECIES.NAME","SCIENTIFIC.NAME",
                     "SUBSPECIES.SCIENTIFIC.NAME","BREEDING.BIRD.ATLAS.CODE")
nms = names(all)

## join the two, deal with NAs next

allexpanded = left_join(allexpanded,all)

## fill up list level values

allexpanded = allexpanded %>% group_by(group.id) %>%
  fill(intersect(nms,listlevelvars), .direction = "down") %>%
  ungroup

allexpanded = allexpanded %>% group_by(group.id) %>%
  fill(intersect(nms,listlevelvars), .direction = "up") %>% 
  ungroup

allexpanded = allexpanded %>% mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, is.na(OBSERVATION.COUNT), 0))

## fill in species specific values from temp into the expanded data frame

temp = all %>%
  group_by(COMMON.NAME) %>% slice(1) %>% ungroup %>%
  select(intersect(nms,specieslevelvars))

allexpanded = allexpanded %>% select(setdiff(nms,specieslevelvars),COMMON.NAME)

allexpanded = merge(temp, allexpanded, by = "COMMON.NAME")




## filter group.ids with counts, change xs to 1s, and change to numeric


allexpandedb = allexpanded %>%
  mutate(OBSERVATION.COUNT=replace(OBSERVATION.COUNT, OBSERVATION.COUNT != "0", "1"))


allexpandedc = allexpanded %>%
  group_by(group.id) %>% filter(!"X" %in% OBSERVATION.COUNT) %>%
  ungroup


rm(allexpanded,temp)

allexpandedb$OBSERVATION.COUNT = as.numeric(allexpandedb$OBSERVATION.COUNT)
allexpandedc$OBSERVATION.COUNT = as.numeric(allexpandedc$OBSERVATION.COUNT)



##################################

## remove complete lists with only 1 observation

all = all %>%
  group_by(group.id) %>% filter(n() > 1) %>%     ## remove lists with only 1 observation, faster 
  ungroup


allexpanded = allexpanded %>%
  group_by(group.id) %>% filter(n() > 1) %>%     ## remove lists with only 1 observation, faster 
  ungroup


## remove lists longer than 20km

all = all %>%
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 20)   ## remove > 20 km travelling lists


allexpanded = allexpanded %>%
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 20)   ## remove > 20 km travelling lists

sumamry(allexpanded)