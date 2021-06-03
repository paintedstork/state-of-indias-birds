## statistics and growth - eBird in India - Figure 1 and 2

library(lubridate)
library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)
#loadfonts(device = "win")

preimp = c("COMMON.NAME","LAST.EDITED.DATE","OBSERVATION.DATE","OBSERVER.ID","SAMPLING.EVENT.IDENTIFIER",
               "GROUP.IDENTIFIER","REVIEWED","APPROVED")
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
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))

nrow(ebirdindia)

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


tr2 = ebirdindia %>%
  group_by(ecyear) %>% summarize(rec2 = n(),obs2 = n_distinct(OBSERVER.ID),
                                 uqli2 = n_distinct(group.id))


editdate1 = data.frame(year = tr2$ecyear)
editdate1$type = "observations"
editdate1$met = tr2$rec2
editdate2 = data.frame(year = tr2$ecyear)
editdate2$type = "observers"
editdate2$met = tr2$obs2
editdate3 = data.frame(year = tr2$ecyear)
editdate3$type = "checklists"
editdate3$met = tr2$uqli2
editdate = rbind(editdate1,editdate2,editdate3)

editdate$type = factor(editdate$type, levels = c("observers","checklists","observations"))


library(scales)

ymeta = ggplot(editdate[editdate$year<2019 & editdate$type=="observers",], aes(year, met))+
  facet_wrap(. ~ type, scale="free_y", ncol = 1)+
  geom_point()+
  geom_line() +
  xlab("year of upload")+
  ylab("")+
  geom_text(aes(label=met),hjust=0, vjust=1.5)+
  theme_bw()
ymet2a = ymeta +
  theme(axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.3, size = 30), 
        axis.text.x = element_text(size = 14),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.position = "none")+
  expand_limits(y=0)+
  scale_x_continuous(expand = c(0,0.5))+
  scale_y_continuous(expand = c(0.12,0), breaks = 0, labels = 0)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_log10()+
  #scale_y_continuous(trans = log10_trans(),
  #                   breaks = trans_breaks("log10", function(x) 10^x),
  #                   labels = trans_format("log10", math_format(10^.x)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.x = element_text(size = 20))
  #theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

ymeta = ggplot(editdate[editdate$year<2019 & editdate$type!="observers",], aes(year, met, group = type))+
  facet_wrap(. ~ type, scale="free_y", ncol = 1)+
  geom_point()+
  geom_line() +
  xlab("year of upload")+
  ylab("")+
  geom_text(aes(label=met),hjust=0, vjust=1.5)+
  theme_bw()
ymet2b = ymeta +
  theme(axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.3, size = 30), 
        axis.text.x = element_text(size = 14),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.position = "none")+
  expand_limits(y=0)+
  scale_x_continuous(expand = c(0,0.8))+
  scale_y_continuous(expand = c(0.12,0), breaks = 0, labels = 0)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_log10()+
  #scale_y_continuous(trans = log10_trans(),
  #                   breaks = trans_breaks("log10", function(x) 10^x),
  #                   labels = trans_format("log10", math_format(10^.x)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.x = element_text(size = 20))
  #theme(plot.margin = unit(c(0.1,0.5,0.5,0.1), "cm"))

require(cowplot)
ymet2 = plot_grid(ymet2a,ymet2b,nrow=1,ncol=2,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

x.grob = textGrob("year of upload", gp = gpar(fontface = "bold", fontsize = 20,
                                              fontfamily = "Gill Sans MT"))

ymet2f = grid.arrange(arrangeGrob(ymet2, bottom = x.grob))

png('Fig. 1.png', units="in", width=10, height=7, res=1000)
grid::grid.draw(ymet2f)
dev.off()



## figure 2 ## load dataforanalyses

library(lubridate)
library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)
load("dataforanalyses.RData")

data1 = data %>%
  filter(ALL.SPECIES.REPORTED == 1)

tr1 = data1 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(timegroups) %>% summarize(obs1 = n_distinct(OBSERVER.ID),
                                     uqli1 = n_distinct(group.id))
tr1a = data1 %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  distinct(timegroups,group.id,COMMON.NAME) %>%
  group_by(timegroups) %>% summarize(rec1 = n())

tr1 = left_join(tr1,tr1a)

realdate1 = data.frame(year = tr1$timegroups)
realdate1$type = "observations"
realdate1$met = tr1$rec1
realdate2 = data.frame(year = tr1$timegroups)
realdate2$type = "observers"
realdate2$met = tr1$obs1
realdate3 = data.frame(year = tr1$timegroups)
realdate3$type = "checklists"
realdate3$met = tr1$uqli1
realdate = rbind(realdate1,realdate2,realdate3)

#realdate$year = as.character(realdate$year)
#realdate[realdate$year == "before 1999",]$year = "before 2000"

realdate$year = factor(realdate$year, levels = c("before 2000","2000-2006","2007-2010",
                                                 "2011-2012","2013","2014","2015","2016","2017","2018"))
brks = sort(unique(realdate$year))
realdate$type = factor(realdate$type, levels = c("observers","checklists","observations"))


library(scales)

ymetb = ggplot(realdate[realdate$type=="observers",], aes(year, met, group = type))+
  facet_wrap(. ~ type, scale="free_y", ncol = 1)+
  geom_point()+
  geom_line() +
  xlab("year of observation")+
  ylab("")+
  geom_text(aes(label=met),hjust=0, vjust=1.5, size = 3)+
  theme_bw()
ymet1a = ymetb +
  theme(axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.3, size = 30), 
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.position = "none")+
  expand_limits(y=0)+
  scale_x_discrete(breaks = brks[c(1,3,5,6,7,8,9,10)],expand = c(0,0.6))+
  scale_y_continuous(expand = c(0.12,0), breaks = 0, labels = 0)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_log10()+
  #scale_y_continuous(trans = log10_trans(),
  #                   breaks = trans_breaks("log10", function(x) 10^x),
  #                   labels = trans_format("log10", math_format(10^.x)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.x = element_text(size = 20))
#theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"))

ymetb = ggplot(realdate[realdate$type!="observers",], aes(year, met, group = type))+
  facet_wrap(. ~ type, scale="free_y", ncol = 1)+
  geom_point()+
  geom_line() +
  xlab("year of observation")+
  ylab("")+
  geom_text(aes(label=met),hjust=0, vjust=1.5, size = 3)+
  theme_bw()
ymet1b = ymetb +
  theme(axis.title.x = element_blank(),
        #axis.title.x = element_text(vjust = 0.3, size = 30), 
        axis.text.x = element_text(size = 12),
        axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), 
        axis.text.y = element_text(size = 14)) +
  theme(legend.position = "none")+
  expand_limits(y=0)+
  scale_x_discrete(breaks = brks[c(1,3,5,6,7,8,9,10)],expand = c(0,1.1))+
  scale_y_continuous(expand = c(0.2,0), breaks = 0, labels = 0)+
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  theme(text=element_text(family="Gill Sans MT")) +
  #scale_y_log10()+
  #scale_y_continuous(trans = log10_trans(),
  #                   breaks = trans_breaks("log10", function(x) 10^x),
  #                   labels = trans_format("log10", math_format(10^.x)))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.x = element_text(size = 20))
#theme(plot.margin = unit(c(0.1,0.5,0.5,0.1), "cm"))

require(cowplot)
ymet1 = plot_grid(ymet1a,ymet1b,nrow=1,ncol=2,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

x.grob = textGrob("year of observation", gp = gpar(fontface = "bold", fontsize = 20,
                                              fontfamily = "Gill Sans MT"))

ymet1f = grid.arrange(arrangeGrob(ymet1, bottom = x.grob))

png('Fig. 7.png', units="in", width=10, height=7, res=1000)
grid::grid.draw(ymet1f)
dev.off()


## misc. statistics

# records in 2014 to records in 2018

editdate[editdate$year == 2014,]
editdate[editdate$year == 2018,]

# observers before 2014
length(unique(ebirdindia[ebirdindia$cyear < 2014,]$OBSERVER.ID))
# checklists before 2014
length(unique(ebirdindia[ebirdindia$cyear < 2014,]$SAMPLING.EVENT.IDENTIFIER))
# records before 2014
length(ebirdindia[ebirdindia$cyear < 2014,]$OBSERVER.ID)


## figure 5 ## load dataforanalyses

library(tidyverse)
load("listforfig3.RData")

list1 = list
list1$freq = list1$freq1
list1$type = "long-term"
list2 = list
list2$freq = list1$freq2
list2$type = "current change"
list3 = rbind(list1,list2)

require(ggthemes)
theme_set(theme_tufte())
require(extrafont)

fhist = ggplot(list3, aes(freq, fill = type))+
  geom_histogram(binwidth = c(0.02), col = "black")+
  facet_grid(type ~ ., scale="free_y")+
  geom_vline(xintercept = 0.5, linetype = "dotted", size = 1) +
  xlab("frequency of reporting in the base Time-period")+
  ylab("number of species")+
  theme_bw()
fhist1 = fhist+
  theme(axis.title.x = element_text(vjust = 0.3, size = 20), axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(vjust = 0.3, angle = 90, size = 20), axis.text.y = element_text(size = 14)) +
  scale_x_continuous(limits = c(0,1))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.y = element_text(size = 20, vjust = 1.7, angle = -90))

png('Fig. 7.png', units="in", width=10, height=7, res=1000)
grid::grid.draw(fhist1)
dev.off()



##############################################################3
## Sparrows

rm(list = ls(all.names = TRUE))
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("metros.RData")
specieslist = data.frame(COMMON.NAME = unique(sparrow$species),
                         ht = 1, rt = 1)

a1 = plottrends(sparrow, c("Bangalore"), lim = c(1,2,3,5), 10, 1)
a2 = plottrends(sparrow, c("Delhi"), lim = c(2,3,5), 5, 2)
a3 = plottrends(sparrow, c("Mumbai"), lim = c(2,4,5,6), 10, 3)
a4 = plottrends(sparrow, c("Kolkata"), lim = c(2,4,5), 20, 4)
a5 = plottrends(sparrow, c("Chennai"), lim = c(5,6,7,8), 20, 5)
a6 = plottrends(sparrow, c("Hyderabad"), lim = c(1,5,8,11), 10, 6)

library(cowplot)
g = plot_grid(a1,a2,a3,a4,a5,a6,align = "v",nrow=2,ncol=3,rel_heights = c(1/2, 1/2))

require(gridExtra)
require(grid)

x.grob = textGrob("time period", gp = gpar(fontface = "bold", fontsize = 14,
                                              fontfamily = "Gill Sans MT"))

y.grob = textGrob("frequency of reporting", gp = gpar(fontface = "bold", fontsize = 14,
                                           fontfamily = "Gill Sans MT"), rot = 90)

g1 = grid.arrange(arrangeGrob(g, bottom = x.grob, left = y.grob))

png('plot1.png', units="in", width=10, height=7, res=1000)
grid::grid.draw(g1)
dev.off()




################################################################
############## diagnostics
# model comparison with complete data


rm(list = ls(all.names = TRUE))
library(tidyverse)
load("datacomparisonyears.RData")
require(ggthemes)
theme_set(theme_tufte())
require(extrafont)

ggp = ggplot(lossbygroup, aes(timegroups,rat,col = type, fill = type))+
  geom_bar(stat="identity",position="dodge", width = 0.7, alpha = 0.6)+
  geom_hline(yintercept = 50, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 75, linetype = "dotted", size = 1) +
  xlab("time period")+
  ylab("usable data by time period (%)")+
  theme_bw()

ggp1 = ggp+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 14), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                    breaks = c("list length","duration","distance"),
                    labels = c("list length\n(no. of species)","duration\n(min)","distance\n(km)")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                    breaks = c("list length","duration","distance"),
                    labels = c("list length\n(no. of species)","duration\n(min)","distance\n(km)")) +
  theme(text=element_text(family="Gill Sans MT")) +
  #theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

# before 2000 graphs

ggp = ggplot(bef2000, aes(region,rat,col = type, fill = type))+
  geom_bar(stat="identity",position="dodge", width = 0.7, alpha = 0.6)+
  geom_hline(yintercept = 50, linetype = "dotted", size = 1) +
  geom_hline(yintercept = 25, linetype = "dotted", size = 1) +
  xlab("ecological region")+
  ylab("usable data before 2000 (%)")+
  theme_bw()

ggp2 = ggp+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 14), axis.text.y = element_text(size = 14)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                    breaks = c("list length","duration","distance"),
                    labels = c("list length\n(no. of species)","duration\n(min)","distance\n(km)")) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#CC6666"),
                      breaks = c("list length","duration","distance"),
                      labels = c("list length\n(no. of species)","duration\n(min)","distance\n(km)")) +
  theme(text=element_text(family="Gill Sans MT")) +
  #theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

require(gridExtra)
require(grid)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 2))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

png('Fig. 5.png', units="in", width=10, height=7, res=1000)
grid_arrange_shared_legend(ggp1,ggp2)
dev.off()



#################################

# median list length over time

load("dataforanalyses.RData")
llaovertime = data %>%
  group_by(group.id) %>% slice(1) %>% ungroup %>%
  group_by(timegroups,gridg1) %>% summarize(med = median(no.sp)) %>% ungroup %>%
  group_by(timegroups) %>% summarize(med = round(mean(med)))

#llaovertime$timegroups = factor(llaovertime$timegroups, levels = c("before 2000","2000-2006","2007-2010",
#                                                                   "2011-2012","2013","2014","2015","2016","2017","2018"))


