require(lubridate)
require(tidyverse)
rawpath = "ebd_IN_relApr-2019.txt"

# select only necessary columns
preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT","LOCALITY","STATE","COUNTY",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","OBSERVER.ID",
           "PROTOCOL.TYPE","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

# create a column "group.id" which can help remove duplicate checklists
data = data %>%
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))


data = data %>%
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  filter(COUNTY == "Bangalore" | COUNTY == "Bangalore Rural") %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(day <= 151, cyear-1, cyear)) %>%
  ungroup

a1 = data %>%
  filter(cyear == 2018) %>%
  group_by(COUNTY,LOCALITY.TYPE,LOCALITY) %>% summarize(lists = n_distinct(SAMPLING.EVENT.IDENTIFIER)) %>%
  ungroup %>%
  group_by(COUNTY,LOCALITY.TYPE) %>%
  arrange(desc(lists), .by_group = TRUE) %>% slice(1:5) %>% ungroup

a2 = data %>%
  filter(cyear == 2018) %>%
  group_by(COUNTY,LOCALITY.TYPE,LOCALITY) %>% summarize(lists = n_distinct(group.id)) %>%
  ungroup %>%
  group_by(COUNTY,LOCALITY.TYPE) %>%
  arrange(desc(lists), .by_group = TRUE) %>% slice(1:5) %>% ungroup


b1 = data %>%
  filter(cyear == 2018, LOCALITY.TYPE == "H") %>%
  group_by(COUNTY,LOCALITY) %>% summarize(species = n_distinct(COMMON.NAME)) %>%
  ungroup %>%
  group_by(COUNTY) %>%
  arrange(desc(species), .by_group = TRUE) %>% slice(1:5) %>% ungroup

c1 = data %>%
  filter(cyear == 2018) %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(COUNTY) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  group_by(COUNTY,COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  ungroup %>%
  group_by(COUNTY) %>%
  arrange(desc(freq), .by_group = TRUE) %>% slice(1:10) %>% ungroup

write.csv(a1,"Bangalore Checklists.csv", row.names=FALSE)
write.csv(b1,"Bangalore Species.csv", row.names=FALSE)
write.csv(c1,"Bangalore Common Species.csv", row.names=FALSE)
