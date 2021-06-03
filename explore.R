##################### Add g2clip - to be run once after creating the main data frames

require(tidyverse)
require(data.table)
require(sp)
require(rgeos)

load("clips.RData")

load("data.RData")

temp = data %>% group_by(group.id) %>% slice(1)

rownames(temp) = temp$group.id
coordinates(temp) = ~LONGITUDE + LATITUDE
temp = over(temp,g2clip)
temp = data.frame(temp)
temp$group.id = rownames(temp)
data = left_join(temp,data)
names(data)[1] = "g2clip"

rm(g2clip,temp)
save.image("data.RData")

clipjoin = data %>% distinct(g2clip,group.id)
save(clipjoin,file = "clipjoin.RData")
rm(list = ls())


load("clipjoin.RData")

load("dataforanalyses.RData")

data = left_join(data,clipjoin)

rm(clipjoin)
save.image("dataforanalyses.RData")
rm(list = ls())





###############################################3

require(tidyverse)

source('~/GitHub/state-of-indias-birds/plot all trends and maps.R')
map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  dplyr::select(eBird.English.Name.2018,India.Checklist.Name)

lists = read.csv("stateofindiasbirds.csv")
lists = left_join(lists,map,by = c("Common.Name" = "India.Checklist.Name"))
lists = lists %>% mutate(India.Checklist.Name = Common.Name) %>% dplyr::select(-Common.Name) %>% mutate(Common.Name = eBird.English.Name.2018) %>% 
  dplyr::select(-eBird.English.Name.2018) %>% filter(!is.na(Common.Name))

selectspecies = as.character(lists$Common.Name[1:length(lists$Common.Name)])
selectspecies = selectspecies[!selectspecies %in% c("Manipur Bush-Quail","Himalayan Quail","Jerdon's Courser")]

selectspecies = c("Yellow-throated Bulbul","Brown-breasted Flycatcher","Rufous Sibia","Rusty-tailed Flycatcher",
                  "Indian Pitta","Indian Paradise-Flycatcher","Black-headed Cuckooshrike","Malabar Whistling-Thrush",
                  "Indian Blackbird","Common Cuckoo","Blue-cheeked Bee-eater","Steppe Eagle",
                  "Large-billed Leaf Warbler")

selectspecies = c("Yellow-throated Bulbul","Brown-breasted Flycatcher","Indian Paradise-Flycatcher","Common Cuckoo",
                  "Indian Pitta","Rufous Sibia")

selectspecies = c("Yellow-breasted Bunting","Large Blue Flycatcher","White-throated Bushchat")

selectspecies = c("House Crow","Indian Golden Oriole","Black-headed Cuckooshrike","White-bellied Sholakili",
                  "Nilgiri Sholakili","Nilgiri Pipit","Ashambu Laughingthrush","Banasura Laughingthrush")
selectspecies = c("Indian Golden Oriole","Nilgiri Sholakili")

selectspecies = c("Black-capped Kingfisher")

plotspeciesmaps(type = "terrain", listofbirds = selectspecies, back = "transparent")
#plotspeciesmaps(type = "blank", listofbirds = selectspecies, back = "transparent")
plotspeciestrends(listofbirds = selectspecies, scol = "#869B27")



############################################################


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

source('~/GitHub/state-of-indias-birds/functions.R')

load("data.RData")
data1 = data
data1[data1$ST_NM == "Andhra Pradesh" & data1$DISTRICT == "Nizamabad" & !is.na(data1$ST_NM),]$ST_NM = "Telangana"
unique(na.omit(data1[data1$ST_NM == "Telangana",]$DISTRICT))

plotfreqmap(data1[data1$CATEGORY == "species" & !is.na(data1$group.id),], "Pied Cuckoo", "g2",
            level = "species", 
            season = "year round",
            smooth = F, 
            rich = T, add = "species", h = 1.2, cutoff = 5, baseyear = 1900, endyear = 2018, 
            showempty = F, states = "Tamil Nadu")

plotfreqmap(data[data$CATEGORY == "species" & !is.na(data$group.id),], "Purple Heron", "g3", 
            level = "species", 
            season = "year round",
            smooth = F, 
            rich = T, add = "unique locations", h = 2.2, cutoff = 5, baseyear = 1900, endyear = 2018, 
            showempty = F, states = "Tamil Nadu")

load("data.RData")
source('~/GitHub/state-of-indias-birds/functions.R')
library(tidyverse)

ggp1 = plotfreqmap(data[data$CATEGORY == "species" & !is.na(data$group.id),], "Rufous Babbler", "g1", 
                   level = "species", 
                   season = "year round",
                   smooth = F, 
                   rich = F, add = "frequency", h = 2.2, cutoff = 0, baseyear = 1900, endyear = 2018, 
                   showempty = F, states = c("none"))


temp = data %>% 
  filter(COMMON.NAME == "Curlew Sandpiper")


ggp2 = plotfreqmap(temp, "Curlew Sandpiper", "g3", 
                   level = "species", 
                   season = "year round",
                   smooth = F, 
                   rich = T, add = "unique lists", h = 2.2, cutoff = 0, baseyear = 1900, endyear = 2018, 
                   showempty = F, states = c("none"))

temp = data %>%
  filter(COMMON.NAME == "Curlew Sandpiper") %>%
  distinct(gridg3)
data1 = temp %>% left_join(data)

ggp3 = plotfreqmap(data1, "Curlew Sandpiper", "g3", 
                   level = "species", 
                   season = "year round",
                   smooth = F, 
                   rich = T, add = "unique lists", h = 2.2, cutoff = 0, baseyear = 1900, endyear = 2018, 
                   showempty = F, states = c("none"))

png("Curlew Sandpiper frequencies.png", units="in", width=10, height=7, res=1000)
print(ggp1)
dev.off()

png("Curlew Sandpiper observations.png", units="in", width=10, height=7, res=1000)
print(ggp2)
dev.off()

png("Curlew Sandpiper sampling.png", units="in", width=10, height=7, res=1000)
print(ggp3)
dev.off()

png("Rufous Babbler frequencies.png", units="in", width=10, height=7, res=1000)
print(ggp1)
dev.off()

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


trends = b
plottrends(trends, selectspecies = c("House Sparrow","Red-necked Falcon","Ashy Prinia"))

list1 = c("Shikra","Booted Eagle",
          "Black Kite","Brahminy Kite")

list2 = c("Bonelli's Eagle",
          "Crested Hawk-Eagle",
          "Black Eagle","Crested Goshawk","Crested Serpent-Eagle",
          "Gray-headed Fish-Eagle","Pallas's Fish-Eagle",
          "Rufous-bellied Eagle","Oriental Honey-buzzard","White-eyed Buzzard")

list3 = c("Tawny Eagle","Red-necked Falcon",
             "Short-toed Snake-Eagle",
             "Eurasian Kestrel","Black-winged Kite","Pallid Harrier","Montagu's Harrier",
             "Peregrine Falcon",
             "Eurasian Marsh-Harrier")

list4 = c("Eurasian Griffon","Egyptian Vulture","Bearded Vulture","White-rumped Vulture",
             "Indian Vulture","Red-headed Vulture")



plotcompositetrends(trends, specieslist = specieslist, g1 = list1, 
                    g2 = list2,
                    g3 = list3, 
                    g4 = list4, 
                    n1 = "Commensal\nRaptors (1)",
                    n2 = "Forest\nRaptors (2)",
                    n3 = "Open Habitat\nRaptors (3)",
                    n4 = "Vultures (4)"
                    )

c = rbind(c4,c5)
plottrends(trends, selectspecies = list4[c(2,4)])


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