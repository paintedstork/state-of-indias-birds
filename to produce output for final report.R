#rm(list = ls(all.names = TRUE))



########## All the composite graphs #################
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
  select(eBird.English.Name.2018,India.Checklist.Name)

lists = read.csv("compositespecieslist.csv")
lists = left_join(lists,map,by = c("species" = "India.Checklist.Name"))
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

list3 = as.character(temp$species[temp$groups == gps[3]])

plotcompositetrends(trends, specieslist = specieslist, name = cgps[2],
                    g1 = list1,
                    g2 = list3, 
                    g3 = list2,
                    n1 = gps[1],
                    n2 = gps[3],
                    n3 = gps[2]
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





############# Vultures ########################

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

vuls = plottrends(trends, list, leg = F, al = 0.2)
n1 = "vultures.svg"
n2 = "vulturelegends.png"
n3 = "vultures.png"

print(vuls[[1]])
ggsave(file=n1, units="in", width=11, height=8)

png(n2, units="in", width=10, height=2, res=1000)
grid::grid.draw(vuls[[2]])
dev.off()

png(n3, units="in", width=10, height=7, res=1000)
grid::grid.draw(vuls[[3]])
dev.off()






#################### House Sparrow ########################

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
                  ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.3) +
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
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 15, colour = "#56697B", vjust = -4, 
                                   margin = margin(0, 0, 0.8, 0, 'cm')),
        axis.title.y = element_blank(), axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
        axis.ticks.length=unit(.4, "cm"),
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(breaks = xbreaks1, labels = c("before 2006","2007-13","2014-18")) +
  scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                     #limits = c(-12.5,limu),
                     labels = c("-100%","-75%","-50%","-25%","0%",
                                "+25%")
  ) +
  expand_limits(y=-12.5) +
  theme(legend.position = "none")

p1 = ggp1
require(cowplot)
sepleg = get_legend(p1)

gg = list(ggpx,sepleg)

n1 = "sparrow.svg"

print(gg[[1]])
ggsave(file=n1, units="in", width=11, height=8)

n2 = "sparrowlegends.png"

png(n2, units="in", width=10, height=2, res=1000)
grid::grid.draw(gg[[2]])
dev.off()

n3 = "sparrow.png"

png(n3, units="in", width=10, height=7, res=1000)
ggp1
dev.off()

n4 = "sparrowfull.svg"

print(ggp1)
ggsave(file=n4, units="in", width=10, height=7)




########## Indian Peafowl ############################

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
                  ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.3) +
  #geom_errorbar(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
  #ymax = (nmfreqbyspec + nmsebyspec*1.96)), width = 0.1, size = 0.1, position = pd) +
  xlab("years") +
  ylab("change in frequency of reporting")

xbreaks1 = unique(temp$timegroups)
lbreaks1 = temp$timegroupsf[1:10]
lbreaks1[c(5,7,9)] = ""

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
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 15, colour = "#56697B", vjust = -4, 
                                   margin = margin(0, 0, 0.8, 0, 'cm')),
        axis.title.y = element_blank(), axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
        axis.ticks.length=unit(.4, "cm"),
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(breaks = xbreaks1, labels = lbreaks1) +
  scale_y_continuous(breaks = c(100,150,200), 
                     #limits = c(80,limu),
                     labels = c("0%","+50%","+100%")
  ) +
  expand_limits(y=80) +
  theme(legend.position = "none")

n1 = "peafowl.svg"

print(ggpx)
ggsave(file=n1, units="in", width=11, height=8)

n2 = "peafowl.png"
png(n2, units="in", width=10, height=7, res=1000)
ggp1
dev.off()




##################### State summaries ######################

require(tidyverse)
load("data.RData")
datax = data
datax = datax %>% filter(!is.na(ST_NM))
datax$ST_NM = as.character(datax$ST_NM)
datax = datax %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Arunanchal Pradesh", "Arunachal Pradesh"))
datax = datax %>%
  mutate(ST_NM = replace(ST_NM, DISTRICT == "Leh (ladakh)", "Ladakh"))
datax = datax %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Dadara & Nagar Havelli", "Dadara & Nagar Havelli/Daman & Diu"))
datax = datax %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Daman & Diu", "Dadara & Nagar Havelli/Daman & Diu"))
load("dataforanalyses.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
data = data %>% filter(year>2013, !is.na(ST_NM))
data$ST_NM = as.character(data$ST_NM)
data = data %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Arunanchal Pradesh", "Arunachal Pradesh"))
data = data %>%
  mutate(ST_NM = replace(ST_NM, DISTRICT == "Leh (ladakh)", "Ladakh"))
data = data %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Dadara & Nagar Havelli", "Dadara & Nagar Havelli/Daman & Diu"))
data = data %>%
  mutate(ST_NM = replace(ST_NM, ST_NM == "Daman & Diu", "Dadara & Nagar Havelli/Daman & Diu"))


map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  select(eBird.English.Name.2018,India.Checklist.Name)

lists = read.csv("highconcern.csv")
lists = left_join(lists,map,by = c("Common.Name" = "India.Checklist.Name"))
lists = lists %>% select(-Common.Name) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018) %>% filter(!is.na(species))

lists1 = read.csv("stateofindiasbirds.csv")
lists1 = lists1 %>% filter(IUCN %in% c("Vulnerable","Endangered","Critically Endangered"))
lists1 = left_join(lists1,map,by = c("Common.Name" = "India.Checklist.Name"))
lists1 = lists1 %>% select(-Common.Name) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018) %>% filter(!is.na(species))

lists = union(as.character(lists$species),as.character(lists1$species))

specs = c("White-naped Tit","White-rumped Vulture","Indian Vulture","Red-headed Vulture",
          "Indian Bustard","Lesser Florican","Jerdon's Babbler","Yellow Weaver",
          "Bengal Florican","Broad-tailed Grassbird")
lists = lists[!lists %in% specs]

data = data %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[1] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Andhra Pradesh"), 
                   "White-naped Tit (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[1], "White-naped Tit (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[2] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh"),
                   "White-rumped Vulture (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[2], "White-rumped Vulture (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[3] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh"), 
                   "Indian Vulture (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[3], "Indian Vulture (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[4] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh"), 
                   "Red-headed Vulture (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[4], "Red-headed Vulture (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[5] & 
                     ST_NM %in% c("Karnataka","Telangana","Andhra Pradesh"), 
                   "Indian Bustard (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[5], "Indian Bustard (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[6] & 
                     ST_NM %in% c("Karnataka","Telangana","Andhra Pradesh"), 
                   "Lesser Florican (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[6], "Lesser Florican (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[7] & 
                     ST_NM %in% c("Punjab"), 
                   "Jerdon's Babbler (W)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[7], "Jerdon's Babbler (E)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[8] & 
                     ST_NM %in% c("Uttar Pradesh","Uttarakhand"), 
                   "Yellow Weaver (W)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[8], "Yellow Weaver (E)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[9] & 
                     ST_NM %in% c("Assam","Arunachal Pradesh"), 
                   "Bengal Florican (E)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[9], "Bengal Florican (W)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[10] & 
                     ST_NM %in% c("Maharashtra"), 
                   "Broad-tailed Grassbird (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[10], "Broad-tailed Grassbird (S)"))

datax = datax %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[1] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Andhra Pradesh"), 
                   "White-naped Tit (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[1], "White-naped Tit (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[2] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh"),
                   "White-rumped Vulture (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[2], "White-rumped Vulture (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[3] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh"), 
                   "Indian Vulture (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[3], "Indian Vulture (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[4] & 
                     ST_NM %in% c("Karnataka","Tamil Nadu","Kerala","Andhra Pradesh"), 
                   "Red-headed Vulture (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[4], "Red-headed Vulture (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[5] & 
                     ST_NM %in% c("Karnataka","Telangana","Andhra Pradesh"), 
                   "Indian Bustard (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[5], "Indian Bustard (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[6] & 
                     ST_NM %in% c("Karnataka","Telangana","Andhra Pradesh"), 
                   "Lesser Florican (S)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[6], "Lesser Florican (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[7] & 
                     ST_NM %in% c("Punjab"), 
                   "Jerdon's Babbler (W)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[7], "Jerdon's Babbler (E)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[8] & 
                     ST_NM %in% c("Uttar Pradesh","Uttarakhand"), 
                   "Yellow Weaver (W)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[8], "Yellow Weaver (E)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[9] & 
                     ST_NM %in% c("Assam","Arunachal Pradesh"), 
                   "Bengal Florican (E)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[9], "Bengal Florican (W)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[10] & 
                     ST_NM %in% c("Maharashtra"), 
                   "Broad-tailed Grassbird (N)")) %>%
  mutate(COMMON.NAME = 
           replace(COMMON.NAME, COMMON.NAME == specs[10], "Broad-tailed Grassbird (S)"))


map = read.csv("Map to Other Lists - map.csv")
tadd1 = c(rep("(S)",1),rep("(W)",1),
          rep("(S)",1),
          rep("(S)",5),rep("(W)",2))
tadd2 = c(rep("(N)",1),rep("(E)",1),
          rep("(N)",1),
          rep("(N)",5),rep("(E)",2))

map$eBird.English.Name.2018 = as.character(map$eBird.English.Name.2018)
map$India.Checklist.Name = as.character(map$India.Checklist.Name)

mapa = map %>% filter(eBird.English.Name.2018 %in% specs)
mapb = map %>% filter(eBird.English.Name.2018 %in% specs)
mapa$eBird.English.Name.2018 = paste(mapa$eBird.English.Name.2018,tadd1,sep=" ")
mapa$India.Checklist.Name = paste(mapa$India.Checklist.Name,tadd1,sep=" ")
mapb$eBird.English.Name.2018 = paste(mapb$eBird.English.Name.2018,tadd2,sep=" ")
mapb$India.Checklist.Name = paste(mapb$India.Checklist.Name,tadd2,sep=" ")

lists = c(lists,mapa$eBird.English.Name.2018,mapb$eBird.English.Name.2018)


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
  filter(COMMON.NAME %in% lists)

spec0 = setdiff(lists,t1$COMMON.NAME)

tx = datax %>%
  group_by(ST_NM,COMMON.NAME) %>% summarize(perc = 100*n_distinct(gridg1)/max(totalgrids)) %>% ungroup %>%
  filter(COMMON.NAME %in% spec0) %>%
  group_by(COMMON.NAME) %>% arrange(desc(perc), .by_group = T) %>% ungroup %>%
  group_by(COMMON.NAME) %>% slice(1) %>% ungroup



## subset species with more than 9% found in a state, or top 5 above 3%, 
## or the single top species when none above 3%

t2 = t1 %>%
  group_by(ST_NM) %>% arrange(desc(perc), .by_group = T) %>% ungroup %>%
  group_by(ST_NM) %>% slice(1:5) %>% ungroup %>%
  filter(perc >= 6)

t3 = t1 %>%
  group_by(ST_NM) %>% arrange(desc(perc), .by_group = T) %>% ungroup %>%
  group_by(ST_NM) %>% slice(1) %>% ungroup %>%
  filter(perc < 6)

t4 = t1 %>%
  group_by(ST_NM) %>% arrange(desc(perc), .by_group = T) %>% ungroup %>%
  filter(perc >= 15)

tx$perc = 0
## added Cachar Bulbul to the Tripura list as it had one less

ta = data.frame(ST_NM = "Tripura", COMMON.NAME = "Cachar Bulbul", perc = 0)
tb = data.frame(ST_NM = "Andhra Pradesh", COMMON.NAME = "Indian Skimmer", perc = 0)
tc = data.frame(ST_NM = "Odisha", COMMON.NAME = "Indian Skimmer", perc = 0)
td = data.frame(ST_NM = "Assam", COMMON.NAME = "Manipur Bush-Quail", perc = 0)
te = data.frame(ST_NM = "Andhra Pradesh", COMMON.NAME = "Great Knot", perc = 0)



t = rbind(t2,t3,t4,tx,ta,tb,tc,td,te)
t = unique(t)


# include Critically Endangered species
# include species with 0 range in the last 5 years based on overall range

t = t %>%
  group_by(ST_NM) %>% arrange(desc(perc), .by_group = T) %>% ungroup

names(t) = c("state","eBird.English.Name","percentage")

map = rbind(map,mapa,mapb)

tf = left_join(t,map,by = c("eBird.English.Name" = "eBird.English.Name.2018"))
tf = tf %>% select(state,India.Checklist.Name,percentage)

mp = read.csv("stateofindiasbirds.csv")
mp1 = as.character(mp$Common.Name[mp$Concern.Status == "High"])
mp2 = as.character(mp$Common.Name[mp$Concern.Status == "Moderate"])
mp3 = as.character(mp$Common.Name[mp$Concern.Status == "Low"])

# - Red : #A01000
# - Amber : #E5AE37
# - Green : #089148


library(openxlsx)
wb = createWorkbook() # create a workbook
addWorksheet(wb, "Sheet", gridLines = F)
writeData(wb, "Sheet", tf)


Style1 = createStyle(fontColour = "#A01000", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)
Style2 = createStyle(fontColour = "#E5AE37", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)
Style3 = createStyle(fontColour = "#089148", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)

for (i in 1:length(mp1))
{
  conditionalFormatting(wb, "Sheet", cols = 2,
                        rows = 2:255, rule = mp1[i], style = Style1,
                        type = "contains")
}

for (i in 1:length(mp2))
{
  conditionalFormatting(wb, "Sheet", cols = 2,
                        rows = 2:255, rule = mp2[i], style = Style2,
                        type = "contains")
}

for (i in 1:length(mp3))
{
  conditionalFormatting(wb, "Sheet", cols = 2,
                        rows = 2:255, rule = mp3[i], style = Style3,
                        type = "contains")
}


saveWorkbook(wb, "statesummaries.xlsx", overwrite = TRUE)

write.csv(tf,"statesummaries.csv",row.names = F)



################# Finn's Weaver #############################

require(tidyverse)
finn = read.csv("finns_weaver.csv")
library(ggthemes)

theme_set(theme_tufte())


recenttrends = finn
recenttrends = recenttrends %>% group_by(type) %>% mutate(denom = number[1])
recenttrends$number = (recenttrends$number/recenttrends$denom)*100

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 2


cols1 = cols[c(1:ns)]
bks1 = c("individuals","nests")
lbs1 = c("individuals","nests")


recenttrends$type = factor(recenttrends$type)

temp = recenttrends

require(extrafont)


ggp = ggplot(temp, aes(x=year, y=number, colour=type)) + 
  geom_point(size = 3) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = 350, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 300, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 250, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 200, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 150, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 50, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  xlab("years") +
  ylab("change in count")

xbreaksl = unique(temp$year)
xbreaks1 = temp$year[c(1:4,6,8)]

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
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350), 
                     #limits = c(liml,limu),
                     labels = c("-100%","-50%","0%","+50%","+100%","+150%","+200%","+250%")
  ) +
  theme(legend.position = "bottom")

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 15, colour = "#56697B", vjust = -4, 
                                   margin = margin(0, 0, 0.8, 0, 'cm')),
        axis.title.y = element_blank(), axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
        axis.ticks.length=unit(.4, "cm"),
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_colour_manual(breaks = bks1, 
                      labels = lbs1,
                      values = cols1) +
  scale_fill_manual(breaks = bks1, 
                    labels = lbs1,
                    values = cols1) +
  scale_x_continuous(breaks = xbreaksl) +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350), 
                     #limits = c(-35,limu),
                     labels = c("-100%","-50%","0%","+50%","+100%","+150%","+200%","+250%")
  ) +
  expand_limits(y=-30) +
  theme(legend.position = "none")

p1 = ggp1
require(cowplot)
sepleg = get_legend(p1)

gg = list(ggpx,sepleg)

n1 = "finns.svg"

print(gg[[1]])
ggsave(file=n1, units="in", width=11, height=8)

#n2 = "finnslegends.png"

#png(n2, units="in", width=10, height=2, res=1000)
#grid::grid.draw(gg[[2]])
#dev.off()

n3 = "finns.png"

png(n3, units="in", width=10, height=7, res=1000)
ggp1
dev.off()


################### IUCN cross tabulation ##############################

require(tidyverse)
a = read.csv("stateofindiasbirds.csv")
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
a$Concern.Status = as.character(a$Concern.Status)
b = a[a$Concern.Status != "",]
b$Concern.Status = factor(b$Concern.Status, levels = c("High","Moderate","Low"))
b$IUCN = factor(b$IUCN, levels = c("Critically Endangered","Endangered","Vulnerable","Near Threatened","Least Concern"))
c = crosstab(b, row.vars = "IUCN", col.vars = "Concern.Status", type = "f")
write.csv(c$crosstab,"IUCN-SoIB-crosstabulation.csv",
          row.names = T)



################# India map with grids #############################

require(tidyverse)
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("data.RData")
load("maps.RData")
load("mask.RData")

require(ggthemes)
theme_set(theme_tufte())

require(raster)



#diff = gridmapg3 - indiamap
#diff1 = gridmapg3 - diff

data = data %>%
  group_by(LOCALITY.ID) %>% slice(1)

filtercountry = fortify(indiamap)

plotindiamap = ggplot() +
  geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE), col = "#dcc343", size = 1, alpha = 0.5) +
  geom_polygon(data = gridmapg3, aes(x = long, y = lat, group = group), col = "black", fill = NA, size = 1) +
  geom_polygon(data = filtercountry, aes(x=long, y=lat, group=group), colour = 'black', fill = NA)+  
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
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

ggp = plotindiamap +
  geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white') +
  geom_polygon(data = filtercountry, aes(x=long, y=lat, group=group), colour = 'black', fill = NA) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
  )

n1 = "griddedindiamap.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11)



############ India map with points ##########################

require(tidyverse)

load("data.RData")
mappath = "maps.RData"
load(mappath)

data = data %>% distinct(LOCALITY.ID,LATITUDE,LONGITUDE)

ggp = ggplot() +
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = 'white', fill = "transparent")+  
  geom_point(data = data, aes(x=LONGITUDE,y=LATITUDE), colour = "#dcc343", size = 0.2) +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        #panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA))+
  coord_map()

n1 = "pointindiamap.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11, bg = "transparent")


############ AWC map with points ##########################

require(tidyverse)
require(sf)

AWCmap = st_read("Imp_AWC_Sites.shp")
AWCmap$centroids = st_transform(AWCmap, 7755) %>% 
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

AWCcoords = do.call(rbind, st_geometry(AWCmap)) %>% 
  as_tibble() %>% setNames(c("long","lat"))

AWCmap$long = AWCcoords$long
AWCmap$lat = AWCcoords$lat

mappath = "maps.RData"
load(mappath)

ggp = ggplot() +
  geom_polygon(data = indiamap, aes(x=long, y=lat, group=group), colour = 'white', fill = "transparent")+  
  geom_point(data = AWCmap, aes(x=long,y=lat,size=Population), colour = "#dcc343") +
  #scale_x_continuous(expand = c(0,0)) +
  #scale_y_continuous(expand = c(0,0)) +
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin=unit(c(0,0,0,0), "cm"),
        #panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "transparent",colour = NA),
        panel.background = element_rect(fill = "transparent",colour = NA))+
  coord_map() +
  theme(legend.position = "none")

n1 = "AWCindiamap.png"

print(ggp)
ggsave(file=n1, units="in", width=8, height=11, bg = "transparent")



################# Great Indian Bustard #############################

require(tidyverse)
bust = read.csv("bustards.csv")
bust$count1 = (bust$count/bust$count[1])*100

library(ggthemes)

theme_set(theme_tufte())


recenttrends = bust

cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
         "#CC6666", "#9999CC", "#000000", "#66CC99")

ns = 1


cols1 = cols[c(1:ns)]


temp = recenttrends

require(extrafont)

ggp = ggplot(temp, aes(x=year, y=count1)) + 
  geom_point(size = 3, col = cols1) +
  geom_line(size = 1.5, col = cols1) +
  geom_hline(yintercept = 125, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 100, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 75, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 50, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 25, linetype = "dotted", size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
  geom_text(aes(label=count),hjust=-0, vjust=-1.5, size=5)+
  xlab("years") +
  ylab("count")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), 
        axis.text.y = element_text(size = 14, colour = "#56697B", face = "italic"),
        axis.ticks.y = element_blank()) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(1969,1978,2001,2006,2018)) +
  scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                     #limits = c(liml,limu),
                     labels = c("-100%","-75%","-50%","-25%","0%",
                                "+25%")
  ) +
  theme(legend.position = "bottom")

ggpx = ggp +
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 15, colour = "#56697B", vjust = -4, 
                                   margin = margin(0, 0, 0.8, 0, 'cm')),
        axis.title.y = element_blank(), axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
        axis.ticks.length=unit(.4, "cm"),
        axis.text.y = element_text(size = 20, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                   margin = margin(0, -0.8, 0, 0, 'cm')),
        axis.ticks.y = element_blank(), 
        axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = c(1969,1978,2001,2006,2018)) +
  scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                     #limits = c(-12.5,limu),
                     labels = c("-100%","-75%","-50%","-25%","0%",
                                "+25%")
  ) +
  expand_limits(y=-12.5) +
  theme(legend.position = "none")

n1 = "bustard.svg"

print(ggpx)
ggsave(file=n1, units="in", width=11, height=8)

#n2 = "finnslegends.png"

#png(n2, units="in", width=10, height=2, res=1000)
#grid::grid.draw(gg[[2]])
#dev.off()

n3 = "bustard.png"

png(n3, units="in", width=10, height=7, res=1000)
ggp1
dev.off()



############# common species of concern table ############

require(tidyverse)
com = read.csv("stateofindiasbirdsfull.csv")

com = com %>% filter(Long.Term.Status == "Strong Decline") %>% select(Common.Name,Range.Size.Mean)

com = com %>% arrange(desc(Range.Size.Mean))

mp = read.csv("stateofindiasbirds.csv")
mp1 = as.character(mp$Common.Name[mp$Concern.Status == "High"])
mp2 = as.character(mp$Common.Name[mp$Concern.Status == "Moderate"])
mp3 = as.character(mp$Common.Name[mp$Concern.Status == "Low"])

# - Red : #A01000
# - Amber : #E5AE37
# - Green : #089148


library(openxlsx)
wb = createWorkbook() # create a workbook
addWorksheet(wb, "Sheet", gridLines = F)
writeData(wb, "Sheet", com)


Style1 = createStyle(fontColour = "#A01000", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)
Style2 = createStyle(fontColour = "#E5AE37", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)
Style3 = createStyle(fontColour = "#089148", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)

for (i in 1:length(mp1))
{
  conditionalFormatting(wb, "Sheet", cols = 1,
                        rows = 2:59, rule = mp1[i], style = Style1,
                        type = "contains")
}

for (i in 1:length(mp2))
{
  conditionalFormatting(wb, "Sheet", cols = 1,
                        rows = 2:59, rule = mp2[i], style = Style2,
                        type = "contains")
}

for (i in 1:length(mp3))
{
  conditionalFormatting(wb, "Sheet", cols = 1,
                        rows = 2:59, rule = mp3[i], style = Style3,
                        type = "contains")
}


saveWorkbook(wb, "commonspecies.xlsx", overwrite = TRUE)

write.csv(com, "commonspecies.csv", row.names = F)



############ results explanation template #########

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
  select(eBird.English.Name.2018,India.Checklist.Name)

lists = read.csv("compositespecieslist.csv")
lists = left_join(lists,map,by = c("species" = "India.Checklist.Name"))
lists = lists %>% select(-species) %>% mutate(species = eBird.English.Name.2018) %>% 
  select(-eBird.English.Name.2018)

cgps = unique(lists$composite)

temp = lists %>%
  filter(composite == cgps[2])
temp$groups = as.character(temp$groups)
gps = unique(temp$groups)

list1 = as.character(temp$species[temp$groups == gps[1]])

a = sample(1:length(list1),50)
b = list1[a]

plotcompositetrends(trends, specieslist = specieslist, name = "results-interpretation-template",
                    g1 = b,
                    n1 = gps[1],
)




################# species with main wintering ground  #############

load("AllTrends.RData")
load("specieslists.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

list = c("Rosy Starling","Blyth's Reed Warbler","Greenish Warbler")

migind = plottrends(trends, list, leg = F, al = 0.3, deft = F)
n1 = "migrants-only-india.svg"
n2 = "migrants-only-india-legends.png"
n3 = "migrants-only-india.png"

print(migind[[1]])
ggsave(file=n1, units="in", width=11, height=8)

png(n2, units="in", width=10, height=2, res=1000)
grid::grid.draw(migind[[2]])
dev.off()

png(n3, units="in", width=10, height=7, res=1000)
grid::grid.draw(migind[[3]])
dev.off()