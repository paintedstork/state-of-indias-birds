# Sparrows in cities
## House Sparrows in cities

require(tidyverse)
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(lme4)
library(parallel)
require(VGAM)


#load("dataforanalyses.RData")

load("metros.RData")
data = metros
#[metros$city != "Hyderabad",]

data = data %>%
  mutate(timegroups=replace(timegroups, timegroups == "before 2000", "before 2006")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2000-2006", "before 2006")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2007-2010", "2007-2013")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2011-2012", "2007-2013")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2013", "2007-2013")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2014", "2014-2018")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2015", "2014-2018")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2016", "2014-2018")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2017", "2014-2018")) %>%
  mutate(timegroups=replace(timegroups, timegroups == "2018", "2014-2018"))

databins = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% summarize(lists = n_distinct(group.id), year = round(median(year))) %>%
  arrange(year)
databins = databins$year

data2 = data

datay = data2 %>%
  group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
  group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
  group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
  summarize(medianlla = round(mean(medianlla)))

medianlla = datay$medianlla

ed = expandbyspecies(data2,"House Sparrow")
ed$timegroups = factor(ed$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))

ed = ed %>%
  select(-gridg1,-gridg3,month,region,LONGITUDE,LATITUDE,OBSERVATION.NUMBER)


####################################################### for glms

m = glm(OBSERVATION.COUNT ~ log(no.sp) + timegroups, data = ed[ed$city == "Delhi",], 
        family=binomial(link = 'cloglog'))

f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

fx = predict(m, ltemp, se.fit = T, "response")
f2$freq = fx$fit
f2$se = fx$se.fit

f1 = left_join(f1,f2)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "Delhi"

delsparrow = f1




####################################################### for glms

m = glm(OBSERVATION.COUNT ~ log(no.sp) + timegroups, data = ed[ed$city == "Bangalore",], 
        family=binomial(link = 'cloglog'))

f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

fx = predict(m, ltemp, se.fit = T, "response")
f2$freq = fx$fit
f2$se = fx$se.fit

f1 = left_join(f1,f2)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "Bangalore"

bngsparrow = f1



####################################################### for glms

m = glm(OBSERVATION.COUNT ~ log(no.sp) + timegroups, data = ed[ed$city == "Chennai",], 
        family=binomial(link = 'cloglog'))

f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

fx = predict(m, ltemp, se.fit = T, "response")
f2$freq = fx$fit
f2$se = fx$se.fit

f1 = left_join(f1,f2)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "Chennai"

massparrow = f1



####################################################### for glms

m = glm(OBSERVATION.COUNT ~ log(no.sp) + timegroups, data = ed[ed$city == "Mumbai",], 
        family=binomial(link = 'cloglog'))

f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

fx = predict(m, ltemp, se.fit = T, "response")
f2$freq = fx$fit
f2$se = fx$se.fit

f1 = left_join(f1,f2)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "Mumbai"

bomsparrow = f1



####################################################### for glms

m = glm(OBSERVATION.COUNT ~ log(no.sp) + timegroups, data = ed[ed$city == "Kolkata",], 
        family=binomial(link = 'cloglog'))

f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

fx = predict(m, ltemp, se.fit = T, "response")
f2$freq = fx$fit
f2$se = fx$se.fit

f1 = left_join(f1,f2)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "Kolkata"

kolsparrow = f1



####################################################### for glms

m = glm(OBSERVATION.COUNT ~ log(no.sp) + timegroups, data = ed[ed$city == "Hyderabad",], 
        family=binomial(link = 'cloglog'))

f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

fx = predict(m, ltemp, se.fit = T, "response")
f2$freq = fx$fit
f2$se = fx$se.fit

f1 = left_join(f1,f2)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "Hyderabad"

hydsparrow = f1



####################################################### for glmer



########################################################

m = glmer(OBSERVATION.COUNT ~ log(no.sp) + timegroups + (1|city), data = ed, 
          family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))


f = data.frame(unique(ed$timegroups))
names(f) = "timegroups"
ltemp = data.frame(timegroups = f$timegroups,
                   no.sp = medianlla)

f1 = data.frame(timegroups = unique(ed$timegroups))

f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
f2$se = numeric(length(ltemp$no.sp))
f2$timegroups = ltemp$timegroups

## bootstrap to get errors


predFun = function(m) {
  predict(m,ltemp, re.form = NA, allow.new.levels=TRUE)
}

pred = bootMer(m, nsim = 1000, FUN = predFun)


for (i in 1:length(ltemp$no.sp))
{
  f2$freq[i] = median(pred$t[,i])
  f2$se[i] = sd(pred$t[,i])
}

f2$freqt = cloglog(f2$freq,inverse = T)
f2$cl = cloglog((f2$freq-f2$se),inverse = T)
f2$set = f2$freqt-f2$cl

fx = f2 %>%
  filter(!is.na(freqt) & !is.na(set)) %>%
  group_by(timegroups) %>% mutate(freq = freqt, se = set)  %>%
  select(timegroups,freq,se)

f1 = left_join(f1,fx)

f1$timegroups = factor(f1$timegroups, levels = c("before 2006","2007-2013",
                                                 "2014-2018"))
f1 = f1[order(f1$timegroups),]
names(f1)[1] = "timegroupsf"
#databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018)
mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                "2014-2018"), 
                timegroups = as.numeric(databins))
f1 = left_join(f1,mp)
f1$species = "House Sparrow"
f1$city = "All metros"

citysparrow = f1


######################################

sparrow = rbind(citysparrow,bngsparrow,massparrow,kolsparrow,delsparrow,bomsparrow,hydsparrow)

sparrow$species = sparrow$city
sparrow = sparrow %>%
  select(-city)

rm(list=setdiff(ls(envir = .GlobalEnv), c("sparrow","metros")), pos = ".GlobalEnv")

plottrends = function(trends,selectspecies,lim,off,colour)
{
  require(tidyverse)
  require(ggthemes)
  
  theme_set(theme_tufte())
  
  l1 = c(0,50,75,90,100,150,200,300,400,500,600,700)
  l1 = l1[lim]
  l2 = c("-100%","-50%","-25%","-10%","0%","+50%",
         "+100%","+200%","+300%","+400%","+500%","+600%")
  l2 = l2[lim]
  
  recenttrends = trends %>%
    filter(species %in% selectspecies)
  
  #names(recenttrends)[2]  = "nmfreqbyspec"
  #names(recenttrends)[3]  = "nmsebyspec"
  
  recenttrends = stdtrends(recenttrends)
  
  cols = c("#D55E00", "#E69F00", "#56B4E9", "#CC79A7", "#999999", "#F0E442", "#009E73", "#0072B2",
           "#CC6666", "#9999CC", "#000000", "#66CC99")
  
  cols = cols[colour]
  
  ns = length(selectspecies)
  
  
  cols1 = cols[c(1:ns)]
  bks1 = selectspecies
  lbs1 = selectspecies
  
  
  recenttrends$species = factor(recenttrends$species, levels = selectspecies)
  
  temp = recenttrends
  xbreaks = temp$timegroups[temp$timegroupsf %in% c("before 2006","2007-2013")]
  lbreaks = temp$timegroupsf[temp$timegroupsf %in% c("before 2006","2007-2013")]
  
  require(extrafont)
  #loadfonts(device = "win")
  
  #pd = position_dodge(0.2)
  
  temp$fct = 1
  temp$fct[temp$species %in% c("Kolkata","Mumbai")] = 2
  temp$fct[temp$species %in% c("Hyderabad","Chennai")] = 3
  
  temp$timegroupsf = factor(temp$timegroupsf, levels = c("before 2006","2007-2013","2014-2018"))
  
  ggp = ggplot(temp, aes(x=timegroupsf, y=nmfreqbyspec, group = species)) + 
    #facet_wrap(~species, nrow = 2, ncol = 3, scales = "free") +
    geom_point(aes(col = species),
               #position = pd,
               size = 3) +
    geom_line(aes(col = species),
              #position = pd,
              size = 1) +
    {if(12 %in% lim)geom_hline(yintercept = 700, linetype = "dotted", size = 0.5)} +
    {if(11 %in% lim)geom_hline(yintercept = 600, linetype = "dotted", size = 0.5)} +
    {if(10 %in% lim)geom_hline(yintercept = 500, linetype = "dotted", size = 0.5)} +
    {if(9 %in% lim)geom_hline(yintercept = 400, linetype = "dotted", size = 0.5)} +
    {if(8 %in% lim)geom_hline(yintercept = 300, linetype = "dotted", size = 0.5)} +
    {if(7 %in% lim)geom_hline(yintercept = 200, linetype = "dotted", size = 0.5)} +
    {if(6 %in% lim)geom_hline(yintercept = 150, linetype = "dotted", size = 0.5)} +
    {if(5 %in% lim)geom_hline(yintercept = 100, linetype = "dotted", size = 0.5)} +
    {if(4 %in% lim)geom_hline(yintercept = 90, linetype = "dotted", size = 0.5)} +
    {if(3 %in% lim)geom_hline(yintercept = 75, linetype = "dotted", size = 0.5)} +
    {if(2 %in% lim)geom_hline(yintercept = 50, linetype = "dotted", size = 0.5)} +
    {if(1 %in% lim)geom_hline(yintercept = 0, linetype = "dotted", size = 0.5)} +
    #geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
    #                ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.3) +
    geom_errorbar(aes(x = timegroupsf, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                      ymax = (nmfreqbyspec + nmsebyspec*1.96), col = species), width = 0.2,
                  #position = pd,
                  size = 0.5) +
    xlab("years") +
    ylab("frequency of reporting") +
    ggtitle(selectspecies[1])
  
  ggp1 = ggp +
    theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10),
          axis.title.y = element_blank(), axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 14, face = 'bold')) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 10)) +
    theme(strip.text.x = element_blank()) +
    theme(text=element_text(family="Gill Sans MT")) +
    scale_colour_manual(breaks = bks1, 
                        labels = lbs1,
                        values = cols1) +
    scale_fill_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
    #scale_x_continuous(breaks = xbreaks,
    #limits = c(1993,2018),
    #labels = lbreaks) +
    #scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1), 
    #limits = c(liml,limu),
    #labels = c(0,0.2,0.4,0.6,0.8,1)
    #)
    theme(legend.position = "none") +
    scale_y_continuous(breaks = l1, 
                       limits = c(min(l1)-off,max(l1)+off),
                       labels = l2)
  
  return(ggp1)
}
