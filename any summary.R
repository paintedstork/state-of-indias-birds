library(lubridate)
library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)
#loadfonts(device = "win")

preimp = c("COMMON.NAME","LAST.EDITED.DATE","OBSERVATION.DATE","OBSERVER.ID","SAMPLING.EVENT.IDENTIFIER",
           "GROUP.IDENTIFIER","REVIEWED","APPROVED","STATE","COUNTY")
rawpath = "ebd_IN_relMay-2019.txt"

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

ebirdindia = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                        stringsAsFactors = F, na.strings = c(""," ",NA))

nms = nms[-47]
sesp = read.csv("Sensitive_India_may 2019.csv", colClasses = nms)

stdformat = data.frame(date = as.character(sesp$OBSERVATION.DATE))
stdformat = stdformat %>%
  separate(date, c("month","day","year"), "/")
stdformat$year = as.numeric(stdformat$year)
sesp$OBSERVATION.DATE = paste(stdformat$year,"-",stdformat$month,"-",stdformat$day, sep = "")


stdformat = data.frame(date = as.character(sesp$LAST.EDITED.DATE))
stdformat = stdformat %>%
  separate(date, c("date","time"), " ") %>%
  select(-time)
stdformat = stdformat %>%
  separate(date, c("month","day","year"), "/")
stdformat$year = as.numeric(stdformat$year)
sesp$LAST.EDITED.DATE = paste(stdformat$year,"-",stdformat$month,"-",stdformat$day, sep = "")

ebirdindia = rbind(ebirdindia,sesp)

ebirdindia = ebirdindia %>%
  filter(!COMMON.NAME %in% c("Western Orphean Warbler"))

ebirdindia = ebirdindia[ebirdindia$STATE == "Maharashtra",]

ebirdindia = ebirdindia %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

ebirdindia = ebirdindia %>%
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         LAST.EDITED.DATE = as.Date(LAST.EDITED.DATE),
         month = month(OBSERVATION.DATE),
         emonth = month(LAST.EDITED.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         eday = day(LAST.EDITED.DATE) + cdays[emonth], 
         cyear = year(OBSERVATION.DATE), ecyear = year(LAST.EDITED.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE","LAST.EDITED.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear))
#mutate(eyear = ifelse(eday <= 151, ecyear-1, ecyear))

ebirdindia = ebirdindia %>%
  dplyr::select(-c("GROUP.IDENTIFIER","month","day","emonth","eday"))

in1 = ebirdindia %>%
  group_by(COUNTY) %>% summarize(observations = n(),
                                 species = n_distinct(COMMON.NAME),
                                 checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
                                 observers = n_distinct(OBSERVER.ID)
                                 )

temp = ebirdindia %>%
  filter(ecyear <= 2014) %>%
  summarize(observations = n(),
            species = n_distinct(COMMON.NAME),
            checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
            observers = n_distinct(OBSERVER.ID)
  )

for (i in 2015:2019)
{
  temp1 = ebirdindia %>%
    filter(ecyear <= i) %>%
    summarize(observations = n(),
              species = n_distinct(COMMON.NAME),
              checklists = n_distinct(SAMPLING.EVENT.IDENTIFIER),
              observers = n_distinct(OBSERVER.ID)
    )
  
  temp = rbind(temp,temp1)
}

temp$ecyear = 2014:2019

cumin1 = temp

write.csv(in1, "Maharashtradata.csv")
