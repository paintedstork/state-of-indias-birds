#rm(list = ls(all.names = TRUE))
###########################################################################
#############################################################################
## All the composite graphs
# Raptors

load("AllTrends.RData")
load("specieslists.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  select(eBird.English.Name.2018,eBird.English.Name.2019)

lists = read.csv("compositespecieslist.csv")
lists = left_join(lists,map,by = c("species" = "eBird.English.Name.2019"))
lists = lists %>% select(-species) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018)

cgps = unique(lists$composite)

### composite 1

temp = lists %>%
  filter(composite == cgps[1])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])

list3 = as.character(temp$species[temp$groups == gps[3]])

list4 = as.character(temp$species[temp$groups == gps[4]])

list5 = as.character(temp$species[temp$groups == gps[5]])


plotcompositetrends(trends, specieslist = specieslist, name = cgps[1],
                    g1 = list4, 
                    g2 = list3,
                    g3 = list1, 
                    g4 = list2,
                    g5 = list5, 
                    n1 = gps[4],
                    n2 = gps[3],
                    n3 = gps[1],
                    n4 = gps[2],
                    n5 = gps[5]
)


### composite 2

temp = lists %>%
  filter(composite == cgps[2])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])


plotcompositetrends(trends, specieslist = specieslist, name = cgps[2],
                    g1 = list2, 
                    g2 = list1,
                    n1 = gps[2],
                    n2 = gps[1]
)


### composite 3

temp = lists %>%
  filter(composite == cgps[3])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])

list3 = as.character(temp$species[temp$groups == gps[3]])

list4 = as.character(temp$species[temp$groups == gps[4]])


plotcompositetrends(trends, specieslist = specieslist, name = cgps[3],
                    g1 = list4, 
                    g2 = list3,
                    g3 = list2, 
                    g4 = list1,
                    n1 = gps[4],
                    n2 = gps[3],
                    n3 = gps[2],
                    n4 = gps[1]
)


### composite 4

temp = lists %>%
  filter(composite == cgps[4])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])

list3 = as.character(temp$species[temp$groups == gps[3]])


plotcompositetrends(trends, specieslist = specieslist, name = cgps[4],
                    g1 = list1, 
                    g2 = list2,
                    g3 = list3, 
                    n1 = gps[1],
                    n2 = gps[2],
                    n3 = gps[3]
)


### composite 5

temp = lists %>%
  filter(composite == cgps[5])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])

list3 = as.character(temp$species[temp$groups == gps[3]])

list4 = as.character(temp$species[temp$groups == gps[4]])


plotcompositetrends(trends, specieslist = specieslist, name = cgps[5],
                    g1 = list2, 
                    g2 = list1,
                    g3 = list3, 
                    g4 = list4,
                    n1 = gps[2],
                    n2 = gps[1],
                    n3 = gps[3],
                    n4 = gps[4]
)


### composite 6

temp = lists %>%
  filter(composite == cgps[6])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

list2 = as.character(temp$species[temp$groups == gps[2]])

list3 = as.character(temp$species[temp$groups == gps[3]])

list4 = as.character(temp$species[temp$groups == gps[4]])


plotcompositetrends(trends, specieslist = specieslist, name = cgps[6],
                    g1 = list4, 
                    g2 = list1,
                    g3 = list2, 
                    g4 = list3, 
                    n1 = gps[4],
                    n2 = gps[1],
                    n3 = gps[2],
                    n4 = gps[3]
)




########################################################
## Vultures

load("AllTrends.RData")
load("specieslists.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

list = c("Egyptian Vulture","Eurasian Griffon","Red-headed Vulture",
         "Indian Vulture","White-rumped Vulture")

vuls = plottrends(trends, list, leg = F)
n1 = "vultures.svg"
n2 = "vulturelegends.png"

print(vuls[[1]])
ggsave(file=n1, units="in", width=11, height=7)

png(n2, units="in", width=10, height=2, res=1000)
grid::grid.draw(vuls[[2]])
dev.off()




########################################################
## House Sparrow

load("AllTrends.RData")
load("specieslists.RData")
load("metros.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

bng = sparrow %>% filter(species == "Bangalore")
che = sparrow %>% filter(species == "Chennai")
kol = sparrow %>% filter(species == "Kolkata")
del = sparrow %>% filter(species == "Delhi")
mum = sparrow %>% filter(species == "Mumbai")
hyd = sparrow %>% filter(species == "Hyderabad")
spar = trends %>% filter(species == "House Sparrow")

cityplot = rbind(bng,kol,del,mum)
cities = composite(cityplot, "Big Cities")
cities$timegroups = c(2006,2013,2018)

spar1 = cities
spar1$species = "House Sparrow"
names(spar1)[2:3] = c("freq","se")
spar1$freq[1] = mean(spar$freq[1:2])
spar1$se[1] = erroradd(spar$se[1:2])/sqrt(2)
spar1$freq[2] = mean(spar$freq[3:5])
spar1$se[2] = erroradd(spar$se[3:5])/sqrt(2)
spar1$freq[3] = mean(spar$freq[6:10])
spar1$se[3] = erroradd(spar$se[6:10])/sqrt(2)


spars = composite(spar1, "India")

spartoplot = rbind(cities,spars)
n = unique(spartoplot$species)

require(ggthemes)

theme_set(theme_tufte())

recenttrends = spartoplot %>%
  filter(species %in% unique(spartoplot$species))

cols = c("#E49B36", "#869B27", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = length(n)


cols1 = cols[c(1:ns)]
bks1 = n
lbs1 = n


recenttrends$species = factor(recenttrends$species, levels = n)

temp = recenttrends

require(extrafont)

ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  #geom_line(aes(group = species),size = 1.5) +
  #geom_hline(yintercept = 150, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 125, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 75, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 50, linetype = "dotted", size = 0.5) +
  geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                  ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.1) +
  #geom_errorbar(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
  #ymax = (nmfreqbyspec + nmsebyspec*1.96)), width = 0.1, size = 0.1, position = pd) +
  xlab("years") +
  ylab("change in frequency of reporting")

xbreaks1 = unique(temp$timegroups)

ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14, colour = "#56697B", face = "italic"),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(breaks = xbreaks1) +
  scale_y_continuous(breaks = c(50,75,100,125), 
                     #limits = c(liml,limu),
                     labels = c("-50%","-25%","0%",
                                "+25%")
  ) +
  theme(legend.position = "bottom")

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14, colour = "#56697B", face = "italic"),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(breaks = xbreaks1) +
  scale_y_continuous(breaks = c(50,75,100,125), 
                     #limits = c(liml,limu),
                     labels = c("-50%","-25%","0%",
                                "+25%")
  ) +
  theme(legend.position = "none")

p1 = ggp1
require(cowplot)
sepleg = get_legend(p1)

gg = list(ggpx,sepleg)

n1 = "sparrow.svg"

print(gg[[1]])
ggsave(file=n1, units="in", width=11, height=7)

#n2 = "sparrow.png"

#png(n2, units="in", width=10, height=8, res=1000)
#grid::grid.draw(gg[[1]])
#dev.off()



#######################################################################
## Indian Peafowl

load("AllTrends.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())

recenttrends = trends %>%
  filter(species %in% "Indian Peafowl")
n = "Indian Peafowl"
recenttrends = stdtrends(recenttrends)

cols = c("#E49B36", "#869B27", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = length(n)


cols1 = cols[2]
bks1 = n
lbs1 = n


recenttrends$species = factor(recenttrends$species, levels = n)

temp = recenttrends

require(extrafont)

ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  #geom_line(aes(group = species),size = 1.5) +
  #geom_hline(yintercept = 150, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 200, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 150, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dotted", size = 0.5) +
  geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                  ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.1) +
  #geom_errorbar(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
  #ymax = (nmfreqbyspec + nmsebyspec*1.96)), width = 0.1, size = 0.1, position = pd) +
  xlab("years") +
  ylab("change in frequency of reporting")

xbreaks1 = unique(temp$timegroups)

ggpx = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(size = 14, colour = "#56697B", face = "italic"),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(breaks = xbreaks1) +
  scale_y_continuous(breaks = c(100,150,200), 
                     #limits = c(liml,limu),
                     labels = c("0%","+50%","+100%")
  ) +
  theme(legend.position = "none")

n1 = "peafowl.svg"

print(ggpx)
ggsave(file=n1, units="in", width=11, height=7)
