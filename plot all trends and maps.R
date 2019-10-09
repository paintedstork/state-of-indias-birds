##################### Plot all trend graphs


load("AllTrends.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("specieslists.RData")
library(tidyverse)

map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  select(eBird.English.Name.2018,eBird.English.Name.2019)

lists = read.csv("stateofindiasbirds.csv")
lists = left_join(lists,map,by = c("eBird.English.Name" = "eBird.English.Name.2019"))
lists = lists %>% select(-eBird.English.Name) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018) %>% filter(!is.na(species))

for (i in 1:length(lists$species))
{
  if (lists$Long.Term.Status[i] %in% c("Data Deficient","Uncertain") &
      lists$Current.Status[i] %in% c("Data Deficient","Uncertain"))
  {next}
  plottrends(trends,lists$species[i],rem=T)
}


##################### Plot all maps


require(tidyverse)
load("data.RData")
latlong = data %>% distinct(group.id,LONGITUDE,LATITUDE)
load("dataforanalyses.RData")
data = left_join(data,latlong)
load("maps.RData")
load("neighbours.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
pg = fortify(gridmapg1, region = c("id"))
lgrid = fortify(gridmapg3, region = c("id"))

data = data %>% filter(year>2013)
datac = data %>%
  distinct(group.id,LONGITUDE,LATITUDE)

datac = datac[duplicated(datac[,2:3]),]

load("vagrantdata.RData")
vaglist = unique(d$COMMON.NAME)



map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  select(eBird.English.Name.2018,eBird.English.Name.2019)

lists = read.csv("stateofindiasbirds.csv")
lists = left_join(lists,map,by = c("eBird.English.Name" = "eBird.English.Name.2019"))
lists = lists %>% select(-eBird.English.Name) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018) %>% filter(!is.na(species))

filtercountry = fortify(indiamap)

plotindiamap = ggplot() +
  geom_polygon(data = filtercountry, aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
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

spss = vaglist

lists = lists %>% filter(species %in% spss)

switch = F
for (i in 1:length(lists$species))
{
  if (is.na(lists$Range.Status[i]) | lists$Range.Status[i] == "Data Deficient")
  {next}
  
  t3 = data %>%
    filter(COMMON.NAME == lists$species[i]) %>%
    select(gridg1)
  
  t = as.numeric(t3$gridg1)
  
  t2 = unique(nb8g1[[t[1]]])
  for (j in 2:length(t))
  {
    t1 = unique(nb8g1[[t[j]]])
    t2 = union(t2,t1)
  }
  
  if (lists$species[i] %in% vaglist)
  {
    switch = T
    dv = d %>% filter(COMMON.NAME == lists$species[i]) %>%
      filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
  }
  
  n = setdiff(t2,t)
  n = intersect(n,unique(data$gridg1))
  
  pg1 = pg %>%
    filter(id %in% t)
  pg2 = pg %>%
    filter(id %in% n)
  
  lgrid1 = lgrid %>%
    filter(id %in% unique(data$gridg3))
  
  ggp = plotindiamap +
    #geom_path(data = lgrid1, aes(x = long, y = lat, group = group), col = "black") +
    #geom_point(data = datac, aes(x = LONGITUDE, y = LATITUDE),col = "red", alpha = 0.3, size = 2, stroke = 0)
    {if(switch)geom_point(data = dv, aes(x = LONGITUDE, y = LATITUDE), col = "red", shape = 4, 
                          size = 0.5, alpha = 0.5)} +
    geom_polygon(data = pg1, aes(x = long, y = lat, group = group), col = "red", fill = "red") +
    geom_polygon(data = pg2, aes(x = long, y = lat, group = group), col = NA, fill = "red", alpha = 0.2)
    
  
  sps = as.character(lists$species[i])
  name = paste(sps,".png",sep="")
  
  png(name, units="in", width=10, height=7, res=1000)
  print(ggp)
  dev.off()
}