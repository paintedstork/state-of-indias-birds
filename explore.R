library(tidyverse)
library(ggthemes)

source('~/GitHub/state-of-indias-birds/functions.R')
readcleanrawdata("ebd_IN_relApr-2019.txt")
source('~/GitHub/state-of-indias-birds/functions.R')
createmaps()
source('~/GitHub/state-of-indias-birds/functions.R')
addmapvars()

source('~/GitHub/state-of-indias-birds/functions.R')
readcleanrawdata(KL=T)
source('~/GitHub/state-of-indias-birds/functions.R')
addmapvars("KL.RData", KL=T)




theme_set(theme_tufte())

source('~/GitHub/state-of-indias-birds/functions.R')

load("dataforspatialanalyses.RData")
dlist = dataspeciesfilter(data,15,4,"ebd_IN_relApr-2019.txt")

data = dlist$data
selectedspecies = dlist$specieslist
appendix1 = dlist$fulllist
databins = dlist$binneddata
meadianlla = dlist$medianlla  

write.csv(appendix1, "appendix1.csv")



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

species = c("Jungle Myna","House Crow","Large-billed Crow","Brahminy Kite","Black Kite",
            "Asian Fairy-bluebird","Indian Paradise-Flycatcher","Indian Pitta","Hooded Pitta",
            "Blue-naped Pitta","Indian Scimitar-Babbler","Puff-throated Babbler",
            "Common Myna","Velvet-fronted Nuthatch","Chestnut-bellied Nuthatch","Brown-cheeked Fulvetta")


for(i in c("trivial","null","nosp","nosptime","nb","nosptimenb"))
{
  start = Sys.time()
  if (i %in% c("nb","nosptimenb"))
  {
    temp1 = occufreq(data, species = species, c("g2","g3","g4"), type = i, nb = 4, cutoff = 0)
    temp2 = occufreq(data, species = species, c("g2","g3","g4"), type = i, nb = 8, cutoff = 0)
    temp = rbind(temp1,temp2)
  }
  if (!i %in% c("nb","nosptimenb"))
  {
    temp = occufreq(data, species = species, c("g2","g3","g4"), type = i, nb = 4, cutoff = 0)
  }
  
  if (i == "trivial")
  {
    occ = temp
  }else{
    occ = rbind(occ,temp)
  }
  
  end = Sys.time()
  print(end-start)
}

plotfreqmap(data, "White-cheeked Barbet", "district", level = "species", season = "year round", smooth = F, 
            rich = T, add = "species", h = 1.2, cutoff = 5, baseyear = 1900, endyear = 2018, 
            showempty = F, states = "Karnataka")

#, states = c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh","Telangana")


########################### run frequency trends function #########################################

start = Sys.time()
trends = freqtrends(data, species = "Rosy Starling", politicalunit="state", unitname="Kerala",
                    analysis="pa2", tempres="month", spaceres="g4", trends=T, minobs=100, 
                    baseyear=2013, zinf=0)
end = Sys.time()
end-start

trends = freqtrends(data, species = "Tickell's Blue Flycatcher", politicalunit="country", unitname="Karnataka",
           analysis="pa3", tempres="month", spaceres="g4", trends=T, minobs=100, 
           baseyear=2010, zinf=0)




########################## plot trends function ###############################################

list1 = c("Indian Robin","Cinereous Tit","House Sparrow","Baya Weaver",
          "Ashy Prinia","Crimson-backed Sunbird","White-cheeked Barbet","Asian Palm-Swift")
list2 = c("Indian Peafowl","Red-whiskered Bulbul","Blyth's Reed Warbler",
          "Indian Paradise-Flycatcher","White-browed Bulbul","Red-vented Bulbul","Yellow-browed Bulbul")
list3 = c("Black Kite","Greater Spotted Eagle","Lesser Whistling-Duck","Indian Spot-billed Duck",
          "Bronze-winged Jacana","Painted Stork","Woolly-necked Stork")
list4 = c("Curlew Sandpiper","Little Ringed Plover","Red-wattled Lapwing","Temminck's Stint",
          "Black-tailed Godwit","Glossy Ibis","River Tern","Lesser Black-backed Gull")

list = c(list1,list2,list3,list4,list5)

list5 = c("Rosy Starling","Chestnut-tailed Starling","Common Myna","Jungle Myna","Malabar Starling")

KLatlas = KLatlas %>% filter(month %in% 1:3)
KLnonatlas = KLnonatlas %>% filter(month %in% 1:3)

atlasfreq = KLatlas %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
  group_by(COMMON.NAME) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>% ungroup() %>%
  arrange(desc(freq))

list = sample(atlasfreq$COMMON.NAME[1:200],60)
list = list[-c(5,52:60)]

for (i in 1:50)
{
  start = Sys.time()
  trends1 = freqtrends(KLatlas, species = list[i], politicalunit="state", unitname="Kerala",
                       analysis="trivial pa", tempres="month", spaceres="g4", trends=F, minobs=10, 
                       baseyear=2013, zinf=0)
  end = Sys.time()
  print(end-start)
  if (i == 1)
    trends = trends1
  if (i > 1)
    trends = rbind(trends,trends1)
}


trends1 = freqtrends(KLnonatlas, species = "White-cheeked Barbet", politicalunit="state", 
                     unitname="Kerala",
                     analysis="pa2", tempres="month", spaceres="g4", trends=F, minobs=10, 
                     baseyear=2013, zinf=0, KL = F)

for (i in 11:15)
{
  start = Sys.time()
  abund1 = freqtrends(data, species = list[i], politicalunit="state", unitname="Kerala",
                      analysis="pa4", tempres="month", spaceres="g4", trends=F, minobs=100, 
                      baseyear=2013, zinf=0)
  end = Sys.time()
  print(end-start)
  if (i == 11)
    abund = abund1
  if (i > 11)
    abund = rbind(abund,abund1)
}

a = unique(trends$species)


comp1 = composite(trends[trends$species %in% listf[c(5,15,16,20,21)],], stdby = 1, recent = F)
comp1$species = "wetland species"
comp2 = composite(trends[trends$species %in% listf[c(13,14,17,18,19,22,23)],], stdby = 1, recent = F)
comp2$species = "generalists"
comp = rbind(comp1,comp2)
plottrends(trends = comp, selectspecies = c("wetland species","generalists"))

trends1 = stdtrends(trends[trends$species == "Red-necked Falcon",], recent = F, stdby = 1)
plottrends(trends = trends1, selectspecies = "Red-necked Falcon")

trends1 = stdtrends(trends, recent = F, stdby = 1)
trends1 = trends1[trends1$timegroupsf == 2018,]
trends1$max = trends1$nmfreqbyspec + 1.96*trends1$nmsebyspec
trends1$min = trends1$nmfreqbyspec - 1.96*trends1$nmsebyspec

trends1$trend = "stable"
trends1[trends1$max<50,]$trend = "strong decline"
trends1[trends1$max<75 & trends1$max > 50,]$trend = "moderate decline"
trends1[trends1$min>200,]$trend = "strong increase"
trends1[trends1$min>133 & trends1$min < 200,]$trend = "moderate increase"

trends1 = trends1[,c(4,9)]
write.csv(trends1,"ht.csv")

slopes = calculateslope(trends, species = listf[1], recent = T, composite = F)

for (i in 2:23)
{
  slopes1 = calculateslope(trends, species = listf[i], recent = T, composite = F)
  slopes = rbind(slopes,slopes1)
}

slopes$max = slopes$slope + slopes$ci
slopes$min = slopes$slope - slopes$ci

slopes$trend = "stable"
slopes[slopes$max< -5,]$trend = "strong decline"
slopes[slopes$max< -2 & slopes$max > -5,]$trend = "moderate decline"
slopes[slopes$min>5,]$trend = "strong increase"
slopes[slopes$min>2 & slopes$min < 5,]$trend = "moderate increase"

slopes = slopes[,c(1,7)]
write.csv(slopes, "rt.csv")



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