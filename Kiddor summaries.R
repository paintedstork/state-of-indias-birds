library(tidyverse)

data$season = ""
data$season[data$month %in% c(11,12,1,2)] = "Win"
data$season[data$month %in% c(3,4,5,6)] = "Sum"
data$season[data$month %in% c(7,8,9,10)] = "Mon"

freqoverall = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  mutate(lists = n_distinct(group.id)) %>%
  group_by(COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  arrange(desc(freq))

freqbymonth = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(month) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(month,COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  group_by(month) %>% arrange(desc(freq), .by_group = TRUE)

freqbyseason = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(season) %>% mutate(lists = n_distinct(group.id)) %>%
  group_by(season,COMMON.NAME) %>% summarize(freq = n()/max(lists)) %>%
  group_by(season) %>% arrange(desc(freq), .by_group = TRUE)

data1 = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf")

length(unique(data1$COMMON.NAME))
length(unique(data1$OBSERVER.ID))
length(unique(data1$SAMPLING.EVENT.IDENTIFIER))
length(unique(data1$group.id))

monthsummary = data1 %>%
  group_by(month) %>% summarize(species = n_distinct(COMMON.NAME), uniquelists = n_distinct(group.id))

seasonsummary = data1 %>%
  group_by(season) %>% summarize(species = n_distinct(COMMON.NAME), uniquelists = n_distinct(group.id))


write.csv(freqoverall, "kidoorfrequency.csv",row.names=FALSE)
write.csv(freqbymonth, "kidoorfrequencymonth.csv",row.names=FALSE)
write.csv(freqbyseason, "kidoorfrequencyseason.csv",row.names=FALSE)
write.csv(monthsummary, "monthsummary.csv",row.names=FALSE)
write.csv(seasonsummary, "seasonsummary.csv",row.names=FALSE)

