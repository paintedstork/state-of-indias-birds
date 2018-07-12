## run this before the eBird set up file

library(tidyverse)
library(lubridate)
library(rgdal)
library(gridExtra)
library(grid)
library(data.table)

## set date, add month, year and day columns

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

all$OBSERVATION.DATE = as.Date(all$OBSERVATION.DATE)
all$month = month(all$OBSERVATION.DATE)
all$year = year(all$OBSERVATION.DATE)
all$day = day(all$OBSERVATION.DATE) + cdays[all$month]
all$week = week(all$OBSERVATION.DATE)
all$fort = ceiling(all$day/14)

## get ATLAS data

klatlas = all %>% filter(grepl("atlas", FULL.NAME) | grepl("Atlas", FULL.NAME) | grepl("ATLAS", FULL.NAME), SUBNATIONAL1_CODE == "IN-KL")

klatlas = klatlas %>% 
  filter(ALL.SPECIES.REPORTED == 1, APPROVED == 1, CATEGORY == "species") %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>%
  ungroup

klatlas_dry = klatlas %>% filter(month %in% c(1,2,3))
klatlas_wet = klatlas %>% filter(month %in% c(7,8,9))

## all months

## district wise

klatlas_speciesbydist = klatlas %>% 
  group_by(SUBNATIONAL2_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(prop = n_distinct(group.id)/max(lists)) %>%
  ungroup

klatlas_listsbydist = klatlas %>% 
  group_by(SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(lists = n_distinct(group.id)) %>%
  ungroup

## state average from districts (therefore only where present)

klatlas_speciesbydiststat = klatlas_speciesbydist %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(prop = mean(prop)) %>%
  ungroup

klatlas_speciesbydiststat = klatlas_speciesbydiststat %>% arrange(desc(prop))

atlascommon200 = klatlas_speciesbydiststat$COMMON.NAME[1:200]

## state wise

klatlas_speciesbystat = klatlas %>% 
  group_by(SUBNATIONAL1_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(prop = n_distinct(group.id)/max(lists)) %>%
  ungroup

klatlas_speciesbystat = klatlas_speciesbystat %>% arrange(desc(prop))

atlascommon200s = klatlas_speciesbystat$COMMON.NAME[1:200] ## course 200 without averaging over range


#########################################



## dry months

## district wise

klatlas_speciesbydist_dry = klatlas_dry %>% 
  group_by(SUBNATIONAL2_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(prop = n_distinct(group.id)/max(lists)) %>%
  ungroup

klatlas_listsbydist_dry = klatlas_dry %>% 
  group_by(SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(lists = n_distinct(group.id)) %>%
  ungroup

## state average from districts (therefore only where present)

klatlas_speciesbydiststat_dry = klatlas_speciesbydist_dry %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(prop = mean(prop)) %>%
  ungroup

klatlas_speciesbydiststat_dry = klatlas_speciesbydiststat_dry %>% arrange(desc(prop))

atlascommon200_dry = klatlas_speciesbydiststat_dry$COMMON.NAME[1:200]


## state wise

klatlas_speciesbystat_dry = klatlas_dry %>% 
  group_by(SUBNATIONAL1_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(prop = n_distinct(group.id)/max(lists)) %>%
  ungroup



#############################################



## wet months

## district wise

klatlas_speciesbydist_wet = klatlas_wet %>% 
  group_by(SUBNATIONAL2_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(prop = n_distinct(group.id)/max(lists)) %>%
  ungroup

klatlas_listsbydist_wet = klatlas_wet %>% 
  group_by(SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(lists = n_distinct(group.id)) %>%
  ungroup

## state average from districts (therefore only where present)

klatlas_speciesbydiststat_wet = klatlas_speciesbydist_wet %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(prop = mean(prop)) %>%
  ungroup

klatlas_speciesbydiststat_wet = klatlas_speciesbydiststat_wet %>% arrange(desc(prop))

atlascommon200_wet = klatlas_speciesbydiststat_wet$COMMON.NAME[1:200]


## state wise

klatlas_speciesbystat_wet = klatlas_wet %>% 
  group_by(SUBNATIONAL1_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(prop = n_distinct(group.id)/max(lists)) %>%
  ungroup


## only important variables

unimp = c(1,5,12,13,14,16,20,22,23,24,27,28,30,34,36,37,38,39,40,41,42)

klatlas = klatlas[,-unimp]
klatlas_dry = klatlas_dry[,-unimp]
klatlas_wet = klatlas_wet[,-unimp]


rm(klatlas_listsbydist,klatlas_listsbydist_dry,klatlas_listsbydist_wet,klatlas_speciesbydist,
   klatlas_speciesbydist_dry,klatlas_speciesbydist_wet,klatlas_speciesbydiststat,klatlas_speciesbydiststat_dry,
   klatlas_speciesbydiststat_wet,klatlas_speciesbystat)

rm(atlascommon200s)
