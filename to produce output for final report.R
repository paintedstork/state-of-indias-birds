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
                    n2 = "Ducks and Geese",
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
n3 = "vultures.png"

print(vuls[[1]])
ggsave(file=n1, units="in", width=11, height=7)

png(n2, units="in", width=10, height=2, res=1000)
grid::grid.draw(vuls[[2]])
dev.off()

png(n3, units="in", width=10, height=7, res=1000)
grid::grid.draw(vuls[[3]])
dev.off()






########################################################
## House Sparrow

load("SparrowTrends.RData")
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
spar = trends

cityplot = rbind(bng,kol,del,mum)
cities = composite(cityplot, "Metropolitan Cities")
cities$timegroups = c(1999,2012,2017)

spar1 = cities
spar1$species = "House Sparrow"
names(spar1)[2:3] = c("freq","se")
spar1$freq = spar$freq
spar1$se = spar$se


spars = composite(spar1, "Across India")

spartoplot = rbind(cities,spars)

spartoplot$species = factor(spartoplot$species, levels = c("Across India","Metropolitan Cities"))
n = sort(unique(spartoplot$species))

require(ggthemes)

theme_set(theme_tufte())

recenttrends = spartoplot

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
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
  geom_hline(yintercept = 25, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                  ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.1) +
  #geom_errorbar(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
  #ymax = (nmfreqbyspec + nmsebyspec*1.96)), width = 0.1, size = 0.1, position = pd) +
  xlab("years") +
  ylab("change in frequency of reporting")

xbreaks1 = unique(temp$timegroups)

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), 
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
  scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                     #limits = c(liml,limu),
                     labels = c("-100%","-75%","-50%","-25%","0%",
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
  scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                     #limits = c(liml,limu),
                     labels = c("-100%","-75%","-50%","-25%","0%",
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

n2 = "sparrowlegends.png"

png(n2, units="in", width=10, height=2, res=1000)
grid::grid.draw(gg[[2]])
dev.off()

n3 = "sparrow.png"

png(n3, units="in", width=10, height=7, res=1000)
ggp1
dev.off()



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
xbreaks = temp$timegroups[c(1:4,6,8,10)]
lbreaks = temp$timegroupsf[c(1:4,6,8,10)]

xbreaksl = temp$timegroups[c(1:3,6,8,10)]
lbreaksl = temp$timegroupsf[c(1:3,6,8,10)]

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

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), 
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
  scale_x_continuous(breaks = xbreaksl, labels = lbreaksl) +
  scale_y_continuous(breaks = c(100,150,200), 
                     #limits = c(liml,limu),
                     labels = c("0%","+50%","+100%")
  ) +
  theme(legend.position = "none")

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

n2 = "peafowl.png"
png(n2, units="in", width=10, height=7, res=1000)
ggp1
dev.off()



###################################################################
## State summaries

require(tidyverse)
load("data.RData")
datax = data
datax = datax %>% filter(!is.na(ST_NM))
datax$ST_NM = as.character(datax$ST_NM)
datax = datax %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Arunanchal Pradesh", "Arunachal Pradesh"))
load("dataforanalyses.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
data = data %>% filter(year>2013, !is.na(ST_NM))
data$ST_NM = as.character(data$ST_NM)
data = data %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Arunanchal Pradesh", "Arunachal Pradesh"))


map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  select(eBird.English.Name.2018,eBird.English.Name.2019)

lists = read.csv("highconcern.csv")
lists = left_join(lists,map,by = c("eBird.English.Name" = "eBird.English.Name.2019"))
lists = lists %>% select(-eBird.English.Name) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018) %>% filter(!is.na(species))

temp = data %>%
  distinct(ST_NM,COMMON.NAME,gridg1) %>%
  group_by(COMMON.NAME,ST_NM) %>% summarize(totalgrids = n_distinct(gridg1)) %>% ungroup %>%
  group_by(COMMON.NAME) %>% summarize(totalgrids = sum(totalgrids))

data1 = left_join(data,temp)
data1 = data1 %>% distinct(ST_NM,COMMON.NAME,gridg1,totalgrids)

tempx = datax %>%
  distinct(ST_NM,COMMON.NAME,gridg1) %>%
  group_by(COMMON.NAME,ST_NM) %>% summarize(totalgrids = n_distinct(gridg1)) %>% ungroup %>%
  filter(!is.na(totalgrids)) %>%
  group_by(COMMON.NAME) %>% summarize(totalgrids = sum(totalgrids))

datax = left_join(datax,tempx)
datax = datax %>% distinct(ST_NM,COMMON.NAME,gridg1,totalgrids)

t1 = data1 %>%
  group_by(ST_NM,COMMON.NAME) %>% summarize(perc = 100*n_distinct(gridg1)/max(totalgrids)) %>% ungroup %>%
  filter(COMMON.NAME %in% lists$species)

spec0 = setdiff(lists$species,t1$COMMON.NAME)

tx = datax %>%
  group_by(ST_NM,COMMON.NAME) %>% summarize(perc = 100*n_distinct(gridg1)/max(totalgrids)) %>% ungroup %>%
  filter(COMMON.NAME %in% spec0) %>%
  group_by(COMMON.NAME) %>% arrange(desc(perc), .by_group = T) %>% ungroup %>%
  group_by(COMMON.NAME) %>% slice(1) %>% ungroup

t2 = t1 %>%
  group_by(ST_NM) %>% arrange(desc(perc), .by_group = T) %>% ungroup %>%
  group_by(ST_NM) %>% slice(1:5) %>% ungroup

## subset species found in 3 or less states

t3 = data1 %>%
  filter(COMMON.NAME %in% lists$species) %>%
  group_by(COMMON.NAME) %>% summarize(st = n_distinct(ST_NM)) %>% ungroup %>%
  filter(st <= 2 | (COMMON.NAME %in% lists$species[lists$IUCN == "Critically Endangered"] & 
                      !COMMON.NAME %in% t2$COMMON.NAME))

t2 = t2 %>%
  filter(!COMMON.NAME %in% t3$COMMON.NAME)

t4 = t1 %>%
  filter(COMMON.NAME %in% t3$COMMON.NAME)

tx$perc = 0
## added Cachar Bulbul to the Tripura list as it had one less

ta = data.frame(ST_NM = "Tripura", COMMON.NAME = "Cachar Bulbul", perc = 0)

t = rbind(t2,t4,tx,ta)


# include Critically Endangered species
# include species with 0 range in the last 5 years based on overall range

t = t %>%
  group_by(ST_NM) %>% arrange(desc(perc), .by_group = T) %>% ungroup

names(t) = c("state","eBird.English.Name","percentage")

map = read.csv("Map to Other Lists - map.csv")
tf = left_join(t,map,by = c("eBird.English.Name" = "eBird.English.Name.2018"))
tf = tf %>% select(state,eBird.English.Name.2019,percentage)

write.csv(t,"statesummaries.csv",row.names = F)

