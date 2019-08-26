library(tidyverse)
load("data.RData")

data = data %>%
  filter(cyear < 2019)
data = data %>%
  mutate(timegroups = as.character(cyear)) %>%
  mutate(timegroups = ifelse(cyear <= 1999, "before 1999", timegroups)) %>%
  #mutate(timegroups = ifelse(cyear >= 1990 & cyear <= 1999, "1990-1999", timegroups)) %>%
  mutate(timegroups = ifelse(cyear > 1999 & cyear <= 2006, "2000-2006", timegroups)) %>%
  mutate(timegroups = ifelse(cyear > 2006 & cyear <= 2010, "2007-2010", timegroups)) %>%
  mutate(timegroups = ifelse(cyear > 2010 & cyear <= 2012, "2011-2012", timegroups)) %>%
  mutate(timegroups = ifelse(cyear == 2013, "2013", timegroups)) %>%
  mutate(timegroups = ifelse(cyear == 2014, "2014", timegroups)) %>%
  mutate(timegroups = ifelse(cyear == 2015, "2015", timegroups)) %>%
  mutate(timegroups = ifelse(cyear == 2016, "2016", timegroups)) %>%
  mutate(timegroups = ifelse(cyear == 2017, "2017", timegroups)) %>%
  mutate(timegroups = ifelse(cyear == 2018, "2018", timegroups))

data$timegroups = factor(data$timegroups, levels = c("before 1999","2000-2006","2007-2010",
                                                 "2011-2012","2013","2014","2015","2016","2017","2018"))
x1 = data %>%
  filter(ST_NM == "Kerala") %>%
  group_by(timegroups) %>% summarize(ls = n_distinct(group.id), ob = n_distinct(OBSERVER.ID))
x1$region = "Kerala"
x2 = data %>%
  group_by(timegroups) %>% summarize(ls = n_distinct(group.id), ob = n_distinct(OBSERVER.ID))
x2$region = "India"

x = rbind(x1,x2)

library(ggthemes)
theme_set(theme_tufte())

temp = x

ggp = ggplot(temp, aes(x=timegroups, y=ls, col=region, group = region)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  geom_text(aes(label=ls),hjust=0, vjust=1.5) +
  xlab("years") +
  ylab("unique checklists")

ggp +
  theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(angle = 90, size = 18), axis.text.y = element_text(size = 16)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 16))
