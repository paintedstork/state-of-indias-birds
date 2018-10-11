library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())

source('~/GitHub/state-of-indias-birds/functions.R')

load("dataforspatialanalyses.RData")

#ggp = plotfreqmap(data, "Black-headed Ibis", "g4")

WrVu = expandbyspecies(data,"White-rumped Vulture")
RrPa = expandbyspecies(data,"Rose-ringed Parakeet")
HoSp = expandbyspecies(data,"House Sparrow")
EgVu = expandbyspecies(data,"Egyptian Vulture")
InVu = expandbyspecies(data,"Indian Vulture")
HoCr = expandbyspecies(data,"House Crow")
LbCr = expandbyspecies(data,"Large-billed Crow")
StEa = expandbyspecies(data,"Steppe Eagle")
TaEa = expandbyspecies(data,"Tawny Eagle")
BlKi = expandbyspecies(data,"Black Kite")
CoMy = expandbyspecies(data,"Common Myna")
JuMy = expandbyspecies(data,"JuMy")
GrCo = expandbyspecies(data,"Great Cormorant")

CoCu = expandbyspecies(data,"Common Cuckoo")
AmFa = expandbyspecies(data,"Amur Falcon")

f = freqtrends(data,"House Sparrow",tempres="none",spaceres="g4",trends=T,exd=NA)









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
