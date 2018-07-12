library(tidyverse)
library(lubridate)
library(rgdal)
library(gridExtra)
library(grid)
library(data.table)
library(ggplot2)
library(lme4)

all = all %>% filter(month %in% c(1:3,7:9))
allexpandedb = allexpandedb %>% filter(month %in% c(1:3,7:9))
allexpandedc = allexpandedc %>% filter(month %in% c(1:3,7:9))

## Trivial statewide averages for 200 species

klatlas_1 = klatlas %>% 
  group_by(SUBNATIONAL1_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(atlas = n_distinct(group.id)/max(lists)) %>%
  ungroup

all_1 = all %>% 
  group_by(SUBNATIONAL1_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(eBird = n_distinct(group.id)/max(lists)) %>%
  ungroup

bystate = left_join(klatlas_1,all_1)

with(bystate, plot(eBird ~ atlas))
abline(0,1)     
a = with(bystate, summary(lm(eBird ~ atlas)))





## Statewide averages over districts where present for 200 species

klatlas_2a = klatlas %>% 
  group_by(SUBNATIONAL2_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(atlas = n_distinct(group.id)/max(lists)) %>%
  ungroup

klatlas_2 = klatlas_2a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(atlas = mean(atlas)) %>%
  ungroup

all_2a = all %>% 
  group_by(SUBNATIONAL2_CODE) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(eBird = n_distinct(group.id)/max(lists)) %>%
  ungroup

all_2 = all_2a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(eBird = mean(eBird)) %>%
  ungroup

bystate_dist = left_join(klatlas_2,all_2)

with(bystate_dist, plot(eBird ~ atlas))
abline(0,1)     
b = with(bystate_dist, summary(lm(eBird ~ atlas)))





## After taking temporal averages

season = c("dry","dry","dry","-","-","-","wet","wet","wet")

klatlas = klatlas %>%
  group_by(month) %>% mutate(season = season[month]) %>%
  ungroup

all = all %>%
  group_by(month) %>% mutate(season = season[month]) %>%
  ungroup

allexpandedb = allexpandedb %>%
  group_by(month) %>% mutate(season = season[month]) %>%
  ungroup

allexpandedc = allexpandedc %>%
  group_by(month) %>% mutate(season = season[month]) %>%
  ungroup



klatlas_3b = klatlas %>% 
  group_by(SUBNATIONAL2_CODE,season) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE,season) %>% summarize(atlas = n_distinct(group.id)/max(lists)) %>%
  ungroup

klatlas_3a = klatlas_3b %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(atlas = mean(atlas)) %>%
  ungroup

klatlas_3 = klatlas_3a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(atlas = mean(atlas)) %>%
  ungroup



all_3b = all %>% 
  group_by(SUBNATIONAL2_CODE,season) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE,season) %>% summarize(eBird = n_distinct(group.id)/max(lists)) %>%
  ungroup

all_3a = all_3b %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(eBird = mean(eBird)) %>%
  ungroup

all_3 = all_3a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(eBird = mean(eBird)) %>%
  ungroup


bystate_season = left_join(klatlas_3,all_3)

with(bystate_season, plot(eBird ~ atlas))
abline(0,1)     
c = with(bystate_season, summary(lm(eBird ~ atlas)))

names(bystate)[3:4] = c("atlas_state", "eBird_state")
names(bystate_dist)[3:4] = c("atlas_dist", "eBird_dist")
names(bystate_season)[3:4] = c("atlas_season", "eBird_season")

atlaseBird = left_join(bystate,bystate_dist,by = c("COMMON.NAME","SUBNATIONAL1_CODE"))
atlaseBird = left_join(atlaseBird,bystate_season,by = c("COMMON.NAME","SUBNATIONAL1_CODE"))

allexpandedb = allexpandedb %>% mutate(EFFORT.DISTANCE.KM = replace(EFFORT.DISTANCE.KM,PROTOCOL.TYPE == "Stationary",0))
allexpandedb = allexpandedb %>% mutate(EFFORT.DISTANCE.KM = replace(EFFORT.DISTANCE.KM,PROTOCOL.TYPE == "eBird - Stationary Count",0))
allexpandedb$season = as.factor(allexpandedb$season)
allexpandedb$LOCALITY.HOTSPOT = as.factor(allexpandedb$LOCALITY.HOTSPOT)
allexpandedb$OBSERVER.ID = as.factor(allexpandedb$OBSERVER.ID)
allexpandedb$SUBNATIONAL2_CODE = as.factor(allexpandedb$SUBNATIONAL2_CODE)


glmall = all_3b[,1:4]
glmall$cov = NA
glmall$lla = NA
dur = allexpandedb %>%
  group_by(SUBNATIONAL2_CODE,season)  %>% summarize(DURATION.MINUTES = median(!is.na(DURATION.MINUTES))) %>%
  ungroup
#dur$DURATION.MINUTES = 15
glmall = left_join(glmall,dur)
dis = allexpandedb %>%
  group_by(SUBNATIONAL2_CODE,season)  %>% summarize(EFFORT.DISTANCE.KM = median(!is.na(EFFORT.DISTANCE.KM))) %>%
  ungroup
#dis$EFFORT.DISTANCE.KM = 0.25
glmall = left_join(glmall,dis)
nosp = allexpandedb %>%
  group_by(SUBNATIONAL2_CODE,season)  %>% summarize(no.sp = median(no.sp)) %>%
  ungroup
#nosp$no.sp = 15
glmall = left_join(glmall,nosp)

glmall2 = glmall
glmall2 = glmall2[0,]



for (i in atlascommon200)
{
  temp = allexpandedb[allexpandedb$COMMON.NAME == i,]
  m1 = glmer(OBSERVATION.COUNT ~ SUBNATIONAL2_CODE + season + DURATION.MINUTES + EFFORT.DISTANCE.KM +
              no.sp + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = temp, family="binomial", nAGQ = 0)
  m2 = glmer(OBSERVATION.COUNT ~ SUBNATIONAL2_CODE + season +
               no.sp + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = temp, family="binomial", nAGQ = 0)
  
  #hist(ranef(m1)[[2]][,1], xlab="Intercept", main="", breaks = 20)
  
  glmall1 = glmall %>%
    filter(COMMON.NAME == i) %>%
    mutate(cov = predict(m1, data.frame(SUBNATIONAL2_CODE = SUBNATIONAL2_CODE, season = season,
                         DURATION.MINUTES = DURATION.MINUTES,
                         EFFORT.DISTANCE.KM = EFFORT.DISTANCE.KM,
                         no.sp = no.sp),
                         type="response", re.form = NA)) %>%
    mutate(lla = predict(m2, data.frame(SUBNATIONAL2_CODE = SUBNATIONAL2_CODE, season = season,
                       no.sp = no.sp),
                       type="response", re.form = NA))
  
  glmall2 = rbind(glmall2,glmall1)
  
  print(i)

}

all_4a = glmall2 %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(cov = mean(cov)) %>%
  ungroup

all_4 = all_4a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(cov = mean(cov)) %>%
  ungroup


all_5a = glmall2 %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(lla = mean(lla)) %>%
  ungroup

all_5 = all_5a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(lla = mean(lla)) %>%
  ungroup


atlaseBird = left_join(atlaseBird,all_4)
atlaseBird = left_join(atlaseBird,all_5)

with(atlaseBird, plot(cov ~ atlas_season))
abline(0,1)     
d = with(atlaseBird, summary(lm(cov ~ atlas_season)))

with(atlaseBird, plot(lla ~ atlas_season))
abline(0,1)     
e = with(atlaseBird, summary(lm(lla ~ atlas_season)))

with(atlaseBird, plot(lla ~ cov))
abline(0,1)     
f = with(atlaseBird, summary(lm(lla ~ cov)))

a[[9]]
b[[9]]
c[[9]]

d[[9]]
e[[9]]
f[[9]]

## models with seasons but atlas numbers

glmall = all_3b[,1:4]
glmall$cov = NA
glmall$lla = NA
dur = allexpandedb %>%
  group_by(SUBNATIONAL2_CODE,season)  %>% summarize(DURATION.MINUTES = median(!is.na(DURATION.MINUTES))) %>%
  ungroup
dur$DURATION.MINUTES = 15
glmall = left_join(glmall,dur)
dis = allexpandedb %>%
  group_by(SUBNATIONAL2_CODE,season)  %>% summarize(EFFORT.DISTANCE.KM = median(!is.na(EFFORT.DISTANCE.KM))) %>%
  ungroup
dis$EFFORT.DISTANCE.KM = 0.25
glmall = left_join(glmall,dis)
nosp = allexpandedb %>%
  group_by(SUBNATIONAL2_CODE,season)  %>% summarize(no.sp = median(no.sp)) %>%
  ungroup
nosp$no.sp = 15
glmall = left_join(glmall,nosp)

glmall2 = glmall
glmall2 = glmall2[0,]

for (i in atlascommon200)
{
  temp = allexpandedb[allexpandedb$COMMON.NAME == i,]
  m1 = glmer(OBSERVATION.COUNT ~ SUBNATIONAL2_CODE + season + DURATION.MINUTES + EFFORT.DISTANCE.KM +
               no.sp + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = temp, family="binomial", nAGQ = 0)
  m2 = glmer(OBSERVATION.COUNT ~ SUBNATIONAL2_CODE + season +
               no.sp + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = temp, family="binomial", nAGQ = 0)
  
  #hist(ranef(m1)[[2]][,1], xlab="Intercept", main="", breaks = 20)
  
  glmall1 = glmall %>%
    filter(COMMON.NAME == i) %>%
    mutate(cov1 = predict(m1, data.frame(SUBNATIONAL2_CODE = SUBNATIONAL2_CODE, season = season,
                                        DURATION.MINUTES = DURATION.MINUTES,
                                        EFFORT.DISTANCE.KM = EFFORT.DISTANCE.KM,
                                        no.sp = no.sp),
                         type="response", re.form = NA)) %>%
    mutate(lla1 = predict(m2, data.frame(SUBNATIONAL2_CODE = SUBNATIONAL2_CODE, season = season,
                                        no.sp = no.sp),
                         type="response", re.form = NA))
  
  glmall2 = rbind(glmall2,glmall1)
  
  print(i)
  
}


all_6a = glmall2 %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(cov1 = mean(cov1)) %>%
  ungroup

all_6 = all_6a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(cov1 = mean(cov1)) %>%
  ungroup


all_7a = glmall2 %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE,SUBNATIONAL2_CODE) %>% summarize(lla1 = mean(lla1)) %>%
  ungroup

all_7 = all_7a %>%
  group_by(COMMON.NAME,SUBNATIONAL1_CODE) %>% summarize(lla1 = mean(lla1)) %>%
  ungroup


atlaseBird = left_join(atlaseBird,all_6)
atlaseBird = left_join(atlaseBird,all_7)

with(atlaseBird, plot(cov1 ~ atlas_season))
abline(0,1)     
g = with(atlaseBird, summary(lm(cov1 ~ atlas_season)))

with(atlaseBird, plot(lla1 ~ atlas_season))
abline(0,1)     
h = with(atlaseBird, summary(lm(lla1 ~ atlas_season)))

with(atlaseBird, plot(lla1 ~ cov1))
abline(0,1)     
i = with(atlaseBird, summary(lm(lla1 ~ cov1)))
