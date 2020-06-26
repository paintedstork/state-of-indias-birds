require(tidyverse)
require(lubridate)

rawpath = "ebd_IN_relMay-2019.txt"
sensitivepath = "Sensitive_India_may 2019.csv"

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))

preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# read sensitive species data
nms = nms[-47]
sesp = read.csv(sensitivepath, colClasses = nms, stringsAsFactors = F)
stdformat = data.frame(date = as.character(sesp$OBSERVATION.DATE))
stdformat = stdformat %>%
  separate(date, c("month","day","year"), "/")
stdformat$year = as.numeric(stdformat$year)
sesp$OBSERVATION.DATE = paste(stdformat$year,"-",stdformat$month,"-",stdformat$day, sep = "")

# merge both data frames
data = rbind(data,sesp)

days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear)) %>%
  ungroup

data1 = data %>% filter(year < 2000)
data1 = data1 %>% select(CATEGORY,COMMON.NAME,SCIENTIFIC.NAME,OBSERVATION.COUNT,STATE,COUNTY,LOCALITY.ID,
                         LOCALITY.TYPE,LATITUDE,LONGITUDE,TIME.OBSERVATIONS.STARTED,OBSERVER.ID,
                         SAMPLING.EVENT.IDENTIFIER,PROTOCOL.TYPE,DURATION.MINUTES,EFFORT.DISTANCE.KM,
                         NUMBER.OBSERVERS,ALL.SPECIES.REPORTED,GROUP.IDENTIFIER,month,day,cyear,year)

data2000a = data %>%
  filter(year<2000) %>%
  group_by(OBSERVER.ID) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data2000b = data %>%
  filter(year<2000, ALL.SPECIES.REPORTED == 1) %>%
  group_by(OBSERVER.ID) %>% summarize(clists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data2000c = data %>%
  filter(year<2000, CATEGORY == "species" | CATEGORY == "ISSF") %>%
  group_by(OBSERVER.ID) %>% summarize(species = n_distinct(COMMON.NAME))

data2000 = left_join(data2000a,data2000b)
data2000 = left_join(data2000,data2000c)
data2000a = data2000 %>% arrange(desc(lists)) %>% slice(1:50)
data2000b = data2000 %>% arrange(desc(clists)) %>% slice(1:50)
data2000c = data2000 %>% arrange(desc(species)) %>% slice(1:50)

data2011a = data %>%
  filter(year<2011) %>%
  group_by(OBSERVER.ID) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data2011b = data %>%
  filter(year<2011, ALL.SPECIES.REPORTED == 1) %>%
  group_by(OBSERVER.ID) %>% summarize(clists = n_distinct(SAMPLING.EVENT.IDENTIFIER))

data2011c = data %>%
  filter(year<2011, CATEGORY == "species" | CATEGORY == "ISSF") %>%
  group_by(OBSERVER.ID) %>% summarize(species = n_distinct(COMMON.NAME))

data2011 = left_join(data2011a,data2011b)
data2011 = left_join(data2011,data2011c)
data2011a = data2011 %>% arrange(desc(lists)) %>% slice(1:50)
data2011b = data2011 %>% arrange(desc(clists)) %>% slice(1:50)
data2011c = data2011 %>% arrange(desc(species)) %>% slice(1:50)


write.csv(data1,"data_before_2000.csv",row.names=F)
write.csv(data2000a,"before2000_top50_arranged_completelists.csv",row.names=F)
write.csv(data2000a,"before2000_top50_arranged_species.csv",row.names=F)

write.csv(data2011a,"before2011_top50_arranged_alllists.csv",row.names=F)
write.csv(data2011a,"before2011_top50_arranged_completelists.csv",row.names=F)
write.csv(data2011a,"before2011_top50_arranged_species.csv",row.names=F)