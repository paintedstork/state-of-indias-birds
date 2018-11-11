library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())

source('~/GitHub/state-of-indias-birds/functions.R')

load("dataforspatialanalyses.RData")

data1 = data %>% filter(month %in% c(10,11,12,1,2,3))
plotfreqmap(data1, "Lesser Sand-Plover", "g4")
plotfreqmap(data1, "Lesser Flamingo", "g4")
plotfreqmap(data1, "Greater Flamingo", "g4")
plotfreqmap(data1, "Indian Skimmer", "g4")
plotfreqmap(data1, "Black-bellied Tern", "g4")
plotfreqmap(data1, "Common Pochard", "g4")
plotfreqmap(data1, "Ferruginous Duck", "g4")
plotfreqmap(data1, "Black-tailed Godwit", "g4")
plotfreqmap(data1, "Bar-tailed Godwit", "g4")
plotfreqmap(data1, "Crab-Plover", "g4")
plotfreqmap(data1, "Little Stint", "g4")
plotfreqmap(data1, "Eurasian Curlew", "g4")

data1 = data %>% filter(month %in% c(4,5,6))
plotfreqmap(data1, "Indian Skimmer", "g4")
data1 = data %>% filter(month %in% c(4,5))

family = c("Great Hornbill","Rufous-necked Hornbill","Malabar Gray Hornbill","Indian Gray Hornbill",
           "Malabar Pied-Hornbill","Narcondam Hornbill","Oriental Pied-Hornbill","Wreathed Hornbill",
           "Brown Hornbill")
datat = data[data$COMMON.NAME %in% family,]

plotfreqmap(data, "Indian Paradise-Flycatcher", "g4", level = "species", season = "winter", smooth = T, 
            rich = F, h = 1.5, cutoff = 10, baseyear = 1980, showempty = F)


########################### run frequency trends function #########################################

freqtrends(data, "Indian Paradise-Flycatcher", politicalunit="state", unitname="Kerala", analysis="count1",
           tempres="month", spaceres="g4", trends=T, minobs=100, baseyear=2010, zinf=0)




########################## plot trends function ###############################################

list1 = c("White-rumped Vulture","Indian Vulture","Egyptian Vulture","Tawny Eagle","Common Myna","Black Kite","Red-vented Bulbul","Ashy Prinia")
list2 = c("Indian Vulture","Egyptian Vulture","Steppe Eagle","Common Myna","Large-billed Crow","Red-whiskered Bulbul","Jungle Myna","Ashy Prinia")

plottrends(trends = trends, recent = T, type = "species", selectspecies = list2, smethod = "g5")
plottrends(trends = trends, recent = T, singlespecies = "Tawny Eagle")








## pull data from the cloud

species = "House Crow"
rawpath = 'aug2018'
mappath = 'maps.RData'

require(lubridate)
require(tidyverse)
require(bigrquery)
require(DBI)
require(stringr)

bq_projects() 

con = dbConnect(
  bigrquery::bigquery(),
  project = "stateofindiasbirds",
  dataset = "ebird",
  billing = "stateofindiasbirds"
)

data = 
  tbl(con, rawpath) %>% 
  mutate(group_id = ifelse(GROUP_IDENTIFIER == "NA", SAMPLING_EVENT_IDENTIFIER, GROUP_IDENTIFIER)) %>%
  filter(COMMON_NAME %in% species) %>%
  collect()

data = data %>%
  group_by(group_id) %>% slice(1) %>% ungroup

nms = names(data)
nms = str_replace_all(nms,"_",".")
names(data) = nms

require(ggfortify)

if(!exists("indiamap")) {
  load(mappath)
}

plotindiamap = ggplot() +
  geom_path(data = fortify(indiamap), aes(x=long, y=lat, group=group), colour = 'black')+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank())+
  coord_map()

plotindiamap +
  geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE), size = 0.5)
