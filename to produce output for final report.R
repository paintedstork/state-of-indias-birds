#rm(list = ls(all.names = TRUE))
###########################################################################
#############################################################################
## All the composite graphs
# Raptors

load("AllTrends.RData")
load("dataforanalyses.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("sortedspecieslist3.RData")
library(tidyverse)

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  select(eBird.English.Name.2018,eBird.English.Name.2019)

lists = read.csv("compositespecieslist.csv")
lists = left_join(lists,map,by = c("species" = "eBird.English.Name.2019"))
lists = lists %>% select(-species) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018) %>% filter(!species %in% srt)

### composite-raptors

temp = lists %>%
  filter(composite == "composite-raptors")
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])

list3 = as.character(temp$species[temp$groups == gps[3]])

list4 = as.character(temp$species[temp$groups == gps[4]])


plotcompositetrends(trends, specieslist = specieslist, 
                    g1 = list1, 
                    g2 = list2,
                    g3 = list3, 
                    g4 = list4, 
                    n1 = "Commensal\nRaptors (1)",
                    n2 = "Forest\nRaptors (2)",
                    n3 = "Open Habitat\nRaptors (3)",
                    n4 = "Scavengers (4)"
)

