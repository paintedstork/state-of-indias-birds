require(tidyverse)

load("data.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("modelcomparison.RData")
rm(bef2000,lossbygroup)

data$gridg1 = as.character(data$gridg1)
data$gridg2 = as.character(data$gridg2)
data$gridg3 = as.character(data$gridg3)
data$gridg4 = as.character(data$gridg4)
areag1$id = as.character(areag1$id)
areag2$id = as.character(areag2$id)
areag3$id = as.character(areag3$id)
areag4$id = as.character(areag4$id)

## exclude pelagic lists
data = data %>%
  filter(!gridg1 %in% setdiff(data$gridg1, intersect(areag1$id,unique(data$gridg1))) &
           !gridg2 %in% setdiff(data$gridg2, intersect(areag2$id,unique(data$gridg2))) &
           !gridg3 %in% setdiff(data$gridg3, intersect(areag3$id,unique(data$gridg3))) &
           !gridg4 %in% setdiff(data$gridg4, intersect(areag4$id,unique(data$gridg4))) &
           !is.na(gridg1) & !is.na(gridg2) & !is.na(gridg3) & !is.na(gridg4))

data = data %>%
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  filter(year != 2019)

data = data %>%
  mutate(timegroups = as.character(year)) %>%
  mutate(timegroups = ifelse(year <= 1999, "before 2000", timegroups)) %>%
  #mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
  mutate(timegroups = ifelse(year > 1999 & year <= 2006, "2000-2006", timegroups)) %>%
  mutate(timegroups = ifelse(year > 2006 & year <= 2010, "2007-2010", timegroups)) %>%
  mutate(timegroups = ifelse(year > 2010 & year <= 2012, "2011-2012", timegroups)) %>%
  mutate(timegroups = ifelse(year == 2013, "2013", timegroups)) %>%
  mutate(timegroups = ifelse(year == 2014, "2014", timegroups)) %>%
  mutate(timegroups = ifelse(year == 2015, "2015", timegroups)) %>%
  mutate(timegroups = ifelse(year == 2016, "2016", timegroups)) %>%
  mutate(timegroups = ifelse(year == 2017, "2017", timegroups)) %>%
  mutate(timegroups = ifelse(year == 2018, "2018", timegroups))

data = removevagrants(data)
data = completelistcheck(data)

dataall = data

durdis = data %>%
  filter(!is.na(EFFORT.DISTANCE.KM) & !is.na(DURATION.MINUTES))

dur = data %>%
  filter(!is.na(DURATION.MINUTES))

lla = data

a1 = durdis %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% summarize(lists = n_distinct(group.id))

a2 = dur %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% summarize(lists = n_distinct(group.id))

a3 = lla %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% summarize(lists = n_distinct(group.id))


a = a3
a$listsdurdis = a1$lists
a$listsdur = a2$lists

a$rat1 = a$listsdur/a$lists
a$rat2 = a$listsdurdis/a$lists


a$timegroups = factor(a$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                 "2011-2012","2013","2014","2015","2016","2017","2018"))

lossbygroup = na.omit(a)
lossbygroup = lossbygroup %>%
  arrange(timegroups) %>%
  select(timegroups,lists,listsdur,listsdurdis,rat1,rat2)

bef2000lla = lla %>%
  filter(timegroups == "before 2000") %>%
  select(-timegroups) %>%
  group_by(region) %>% summarize(lists = n_distinct(group.id))

bef2000dur = dur %>%
  filter(timegroups == "before 2000") %>%
  select(-timegroups) %>%
  group_by(region) %>% summarize(listsdur = n_distinct(group.id))

bef2000durdis = durdis %>%
  filter(timegroups == "before 2000") %>%
  select(-timegroups) %>%
  group_by(region) %>% summarize(listsdurdis = n_distinct(group.id))

bef2000 = left_join(bef2000lla,bef2000dur)
bef2000 = left_join(bef2000,bef2000durdis)
  
bef2000[is.na(bef2000)] = 0

lossbygroup$rat1 = round((1-lossbygroup$rat1)*100,1)
lossbygroup$rat2 = round((1-lossbygroup$rat2)*100,1)

temp1 = lossbygroup[,1]
temp1$type = "duration"
temp1$rat = lossbygroup$rat1

temp2 = lossbygroup[,1]
temp2$type = "distance"
temp2$rat = lossbygroup$rat2

temp3 = lossbygroup[,1]
temp3$type = "list length"
temp3$rat = 0

temp = rbind(temp1,temp2,temp3)
temp$rat = 100-temp$rat
temp$lists = c(lossbygroup$listsdur,lossbygroup$listsdurdis,lossbygroup$lists)
temp$type = factor(temp$type, levels = c("list length","duration","distance"))

temp = temp %>%
  arrange(type)

lossbygroup = temp

bef2000$rat1 = bef2000$listsdur/bef2000$lists
bef2000$rat2 = bef2000$listsdurdis/bef2000$lists

bef2000$rat1 = round((1-bef2000$rat1)*100,1)
bef2000$rat2 = round((1-bef2000$rat2)*100,1)

temp1 = bef2000[,1]
temp1$type = "duration"
temp1$rat = bef2000$rat1

temp2 = bef2000[,1]
temp2$type = "distance"
temp2$rat = bef2000$rat2

temp3 = bef2000[,1]
temp3$type = "list length"
temp3$rat = 0

temp = rbind(temp1,temp2,temp3)
temp$rat = 100-temp$rat
temp$lists = c(bef2000$listsdur,bef2000$listsdurdis,bef2000$lists)
temp$type = factor(temp$type, levels = c("list length","duration","distance"))

temp = temp %>%
  arrange(type)

bef2000 = temp

rm(list=setdiff(ls(envir = .GlobalEnv), c("lossbygroup","bef2000","comp")), pos = ".GlobalEnv")





  
## clear environment

rm(list=setdiff(ls(envir = .GlobalEnv), c("dataall")), pos = ".GlobalEnv")
data = dataall
rm(dataall)
source('~/GitHub/state-of-indias-birds/SoIB functions.R')

diu = read.csv("Activity - Activity.csv")
end = read.csv("Endemicity - Endemicity.csv")
ess = read.csv("Select Species from List - Select Species from List.csv")

## Select species

locationlimit = 15
gridlimit = 4

datah = data %>%
  filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(COMMON.NAME,timegroups) %>% summarize(locs = n_distinct(LOCALITY.ID), 
                                                 cells = n_distinct(gridg4)) %>%
  filter(locs > locationlimit, cells > gridlimit) %>%
  group_by(COMMON.NAME) %>% summarize(years = n()) %>%
  filter(years == 10) %>%
  mutate(ht = 1) %>% select (COMMON.NAME,ht)

datar = data %>%
  filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2013) %>%
  group_by(COMMON.NAME,year) %>% summarize(locs = n_distinct(LOCALITY.ID), 
                                           cells = n_distinct(gridg4)) %>%
  filter(locs > locationlimit, cells > gridlimit) %>%
  group_by(COMMON.NAME) %>% summarize(years = n()) %>%
  filter(years == 5) %>%
  mutate(rt = 1) %>% select(COMMON.NAME,rt)

dataf = data.frame(COMMON.NAME = as.character(ess$species))

dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
dataf = left_join(dataf,diu,by = c("COMMON.NAME" = "eBird.English.Name"))
dataf = left_join(dataf,end,by = c("COMMON.NAME" = "eBird.English.Name"))
dataf = left_join(dataf,ess,by = c("COMMON.NAME" = "species"))

specieslist = dataf %>%
  filter((essential == 1 | Subcontinent == 1 | Himalayas == 1 | 
            ht == 1 | rt == 1) & (!is.na(B.Diurnal) | !is.na(NB.Diurnal))) %>%
  select(COMMON.NAME,ht,rt)

species = specieslist %>% filter(!is.na(ht)) %>% select(COMMON.NAME)
species = species$COMMON.NAME

#################


# Compare parameter estimates



data = data %>% select(-CATEGORY,-LOCALITY.ID,-ST_NM,-DISTRICT,-REVIEWED,-APPROVED,
                         -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                         -day,-cyear)

require(tidyverse)
require(lme4)
require(VGAM)
require(parallel)

data$gridg1 = as.factor(data$gridg1)
data$gridg2 = as.factor(data$gridg2)
data$gridg3 = as.factor(data$gridg3)
data$gridg4 = as.factor(data$gridg4)
data$region = as.factor(data$region)

data = data %>%
  filter(ALL.SPECIES.REPORTED == 1)

data$month = as.factor(data$month)

#data$timegroups = as.factor(data$timegroups)
#data$gridg = data$gridg3

comp = data.frame(type = rep(c("lla","dur","dis"),length(species)),
                  species = rep(species,each = 3))

comp$p = comp$se = comp$est = 0

c = 0

for (i in 1:length(species))
{
  temp = data %>%
    filter(COMMON.NAME == species[i]) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  #datay = data1 %>%
  #  group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
  #  group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
  #  group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
  #  summarize(medianlla = round(mean(medianlla)))
  
  #medianlla = datay$medianlla
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data1,species[i])
  #tm = unique(data1$timegroups)
  
  ed = ed %>%
    filter(DURATION.MINUTES != 0 & EFFORT.DISTANCE.KM != 0 & !is.na(DURATION.MINUTES) &
             !is.na(EFFORT.DISTANCE.KM))
  
  m1 = glmer(OBSERVATION.COUNT ~ month + log(no.sp) + log(DURATION.MINUTES) + log(EFFORT.DISTANCE.KM) +
               (1|gridg3/gridg1), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  a = summary(m1)
  b = a$coefficients
  l = length(a$coefficients[,1])
  
  c = c + 1
  comp$est[c] = b[l-2,1]
  comp$se[c] = b[l-2,2]
  comp$p[c] = b[l-2,4]
  
  c = c + 1
  comp$est[c] = b[l-1,1]
  comp$se[c] = b[l-1,2]
  comp$p[c] = b[l-1,4]
  
  c = c + 1
  comp$est[c] = b[l,1]
  comp$se[c] = b[l,2]
  comp$p[c] = b[l,4]
}

## save model comparison dataframe

rm(list=setdiff(ls(envir = .GlobalEnv), c("comp")), pos = ".GlobalEnv")







############### Check for site bias

library(tidyverse)
load("data.RData")


locs = data %>%
  filter(!is.na(gridg1)) %>%
  group_by(gridg1,LOCALITY.ID) %>% summarize(lists = n_distinct(group.id))

locs1 = data %>%
  filter(!is.na(gridg1)) %>%
  group_by(gridg1) %>% summarize(lists = n_distinct(LOCALITY.ID))

singleloc = locs1 %>% filter(lists <= 1)
locs2 = locs %>% filter(!gridg1 %in% singleloc$gridg1) %>%
  group_by(gridg1) %>% summarize(var = sd(lists))

numloc = locs1 %>% filter(lists <= 1)
length(numloc$lists)/length(locs1$lists)

x = c(locs2$var,rep(0,length(singleloc$lists)))

length(x[x<=4])/length(locs1$lists)

locs3 = locs %>% 
  filter(lists <= 2) %>% 
  group_by(gridg1) %>% summarize(ls = n_distinct(LOCALITY.ID)) %>% ungroup

locs4 = left_join(locs1,locs3)
locs4[is.na(locs4$ls),]$ls = 0
locs4$perc = locs4$ls/locs4$lists

y = locs4$perc

length(y[y>=0.8])/length(y)









