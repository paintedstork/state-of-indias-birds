require(tidyverse)
load("dataforanalyses.RData")

spec = "Andaman Green-Pigeon"
#spec = "Nilgiri Pipit"
stt = c("Andaman & Nicobar Island")
stt1 = c("ANDAMAN & NICOBAR")
#stt = c("Tamil Nadu","Kerala")
#stt1 = c("TAMIL NADU","KERALA")

data1 = data %>% filter(year %in% c(2014:2018)) %>% filter(ALL.SPECIES.REPORTED == 1)

data1 = data1 %>% filter(ST_NM %in% stt)

agpi = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,DISTRICT,year) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(gridg1,DISTRICT,year) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
  ungroup()

total = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,DISTRICT,year) %>% summarize(lists = n_distinct(group.id)) %>% ungroup
t1 = total %>% distinct(gridg1,DISTRICT)

fq = left_join(total,agpi)

fq$freq[is.na(fq$freq)] = 0

tot = expand.grid(gridg1 = unique(data1$gridg1), year = unique(data1$year))
tot = left_join(tot,t1)

temp = fq[,1:3]
tot = anti_join(tot,temp)
tot$lists = 0
tot$freq = NA
tot$gridg1 = as.character(tot$gridg1)

fq1 = rbind(tot,fq)
fq1$gridg = fq1$gridg1

comp = fq1 %>% filter(year %in% c(2014,2018))
length(unique(agpi$gridg1))

c2014 = comp %>% filter(year == 2014) %>% select(gridg1,DISTRICT,freq)
names(c2014)[3] = "freq2014"
c2018 = comp %>% filter(year == 2018) %>% select(gridg1,DISTRICT,freq)
names(c2018)[3] = "freq2018"

sub = left_join(c2014,c2018)
sub1 = sub %>% filter(!is.na(freq2014) | !is.na(freq2018)) %>% arrange(desc(DISTRICT))
sub = sub %>% filter(!is.na(freq2014),!is.na(freq2018))
sub = sub %>% filter(freq2014 != 0 | freq2018 != 0)


tempz = comp %>% group_by(year) %>% summarize(freq = mean(na.omit(freq)))


load("maps.RData")
load("latest_state_map.RData")
require(rgeos)


statemap1 = gSimplify(smap, tol=0.01, topologyPreserve=TRUE)
s1 = smap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)


source('~/GitHub/state-of-indias-birds/SoIB functions.R')


pg = fortify(gridmapg1, region = c("id"))

filterstate = fortify(statemap1[statemap1@data$stname %in% stt1,])


plotdis = na.omit(left_join(pg, comp, by = c('id' = "gridg1"))) # SPDF to plot

require(viridis)
require(extrafont)
require(ggthemes)
theme_set(theme_tufte())

brks = cut(c(0,max(na.omit(comp$freq))),breaks = 7)
x = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", levels(brks))),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(brks))))
brks = c(0,x[,2])
require(scales)

fort = fortify(gridmapg1)
fort = fort %>% filter(order == 1) %>% select(c(1,2,6))
require(stringr)
fort$gridg1 = unlist(str_split_fixed(fort$id,"g",2))[,2]
fort = fort[,-3]
sub1 = left_join(sub1,fort)
write.csv(sub1, "AGPIlatlong.csv",row.names = F)

plotdismap = ggplot(data=comp) +
  facet_grid(.~year) +
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = freq), colour = "black")+  
  geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+  
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
        plot.margin=unit(c(0.5,1,0,1), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                       breaks = brks) +
  theme(strip.text.x = element_text(size = 20))+
  guides(fill = guide_legend(title = "", reverse = TRUE, override.aes = list(size=10))) +
  theme(legend.position = "none") +
  coord_map()

print(plotdismap)
ggsave(file="compmapAGPI.jpeg", units="in", width=6.6, height=7)
dev.off()  


#spec = "Andaman Green-Pigeon"
spec = "Nilgiri Pipit"
#stt = c("Andaman & Nicobar Island")
#stt1 = c("ANDAMAN & NICOBAR")
stt = c("Tamil Nadu","Kerala")
stt1 = c("TAMIL NADU","KERALA")

data1 = data %>% filter(year %in% c(2014:2018)) %>% filter(ALL.SPECIES.REPORTED == 1)

data1 = data1 %>% filter(ST_NM %in% stt)

agpi = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,year) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(gridg1,year) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
  ungroup()

total = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,year) %>% summarize(lists = n_distinct(group.id)) %>% ungroup

fq = left_join(total,agpi)

fq$freq[is.na(fq$freq)] = 0

tot = expand.grid(gridg1 = unique(data1$gridg1), year = unique(data1$year))
temp = fq[,1:2]
tot = anti_join(tot,temp)
tot$lists = 0
tot$freq = NA
tot$gridg1 = as.character(tot$gridg1)

fq1 = rbind(tot,fq)
fq1$gridg = fq1$gridg1

comp = fq1 %>% filter(year %in% c(2014,2018))
length(unique(agpi$gridg1))

c2014 = comp %>% filter(year == 2014) %>% select(gridg1,freq)
names(c2014)[2] = "freq2014"
c2018 = comp %>% filter(year == 2018) %>% select(gridg1,freq)
names(c2018)[2] = "freq2018"

sub = left_join(c2014,c2018)
sub = sub %>% filter(!is.na(freq2014),!is.na(freq2018))
sub = sub %>% filter(freq2014 != 0 | freq2018 != 0)


tempz = comp %>% group_by(year) %>% summarize(freq = mean(na.omit(freq)))


load("maps.RData")
load("latest_state_map.RData")
require(rgeos)


statemap1 = gSimplify(smap, tol=0.01, topologyPreserve=TRUE)
s1 = smap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)


source('~/GitHub/state-of-indias-birds/SoIB functions.R')


pg = fortify(gridmapg1, region = c("id"))

filterstate = fortify(statemap1[statemap1@data$stname %in% stt1,])


plotdis = na.omit(left_join(pg, comp, by = c('id' = "gridg1"))) # SPDF to plot

require(viridis)
require(extrafont)
require(ggthemes)
theme_set(theme_tufte())

brks = cut(c(0,max(na.omit(comp$freq))),breaks = 7)
x = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", levels(brks))),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(brks))))
brks = c(0,x[,2])
require(scales)

plotdismap = ggplot(data=comp) +
  facet_grid(.~year) +
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = freq), colour = "black")+  
  geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+  
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
        plot.margin=unit(c(0.5,1,0,1), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                       breaks = brks) +
  theme(strip.text.x = element_text(size = 20))+
  guides(fill = guide_legend(title = "", reverse = TRUE, override.aes = list(size=10))) +
  theme(legend.position = "none") +
  coord_map()

print(plotdismap)
ggsave(file="compmapNIPI.jpeg", units="in", width=6.6, height=7)
dev.off()  


#spec = "Andaman Green-Pigeon"
spec = "Palani Laughingthrush"
#stt = c("Andaman & Nicobar Island")
#stt1 = c("ANDAMAN & NICOBAR")
stt = c("Tamil Nadu","Kerala")
stt1 = c("TAMIL NADU","KERALA")

data1 = data %>% filter(year %in% c(2014:2018)) %>% filter(ALL.SPECIES.REPORTED == 1)

data1 = data1 %>% filter(ST_NM %in% stt)

agpi = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,year) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(gridg1,year) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
  ungroup()

total = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,year) %>% summarize(lists = n_distinct(group.id)) %>% ungroup

fq = left_join(total,agpi)

fq$freq[is.na(fq$freq)] = 0

tot = expand.grid(gridg1 = unique(data1$gridg1), year = unique(data1$year))
temp = fq[,1:2]
tot = anti_join(tot,temp)
tot$lists = 0
tot$freq = NA
tot$gridg1 = as.character(tot$gridg1)

fq1 = rbind(tot,fq)
fq1$gridg = fq1$gridg1

comp = fq1 %>% filter(year %in% c(2014,2018))
length(unique(agpi$gridg1))

c2014 = comp %>% filter(year == 2014) %>% select(gridg1,freq)
names(c2014)[2] = "freq2014"
c2018 = comp %>% filter(year == 2018) %>% select(gridg1,freq)
names(c2018)[2] = "freq2018"

sub = left_join(c2014,c2018)
sub = sub %>% filter(!is.na(freq2014),!is.na(freq2018))
sub = sub %>% filter(freq2014 != 0 | freq2018 != 0)

tempz = comp %>% group_by(year) %>% summarize(freq = mean(na.omit(freq)))


load("maps.RData")
load("latest_state_map.RData")
require(rgeos)


statemap1 = gSimplify(smap, tol=0.01, topologyPreserve=TRUE)
s1 = smap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)


source('~/GitHub/state-of-indias-birds/SoIB functions.R')


pg = fortify(gridmapg1, region = c("id"))

filterstate = fortify(statemap1[statemap1@data$stname %in% stt1,])


plotdis = na.omit(left_join(pg, comp, by = c('id' = "gridg1"))) # SPDF to plot

require(viridis)
require(extrafont)
require(ggthemes)
theme_set(theme_tufte())

brks = cut(c(0,max(na.omit(comp$freq))),breaks = 7)
x = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", levels(brks))),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(brks))))
brks = c(0,x[,2])
require(scales)

plotdismap = ggplot(data=comp) +
  facet_grid(.~year) +
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = freq), colour = "black")+  
  geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+  
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
        plot.margin=unit(c(0.5,1,0,1), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                       breaks = brks) +
  theme(strip.text.x = element_text(size = 20))+
  guides(fill = guide_legend(title = "", reverse = TRUE, override.aes = list(size=10))) +
  theme(legend.position = "none") +
  coord_map()

print(plotdismap)
ggsave(file="compmapPALA.jpeg", units="in", width=6.6, height=7)
dev.off()  


#spec = "Andaman Green-Pigeon"
spec = "Forest Wagtail"
#stt = c("Andaman & Nicobar Island")
#stt1 = c("ANDAMAN & NICOBAR")
stt = c("Tamil Nadu","Kerala")
stt1 = c("TAMIL NADU","KERALA")

data1 = data %>% filter(year %in% c(2014:2018)) %>% filter(ALL.SPECIES.REPORTED == 1)

#data1 = data1 %>% filter(ST_NM %in% stt)

agpi = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg3,gridg1,year) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(gridg3,gridg1,year) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%  ungroup()

total = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg3,gridg1,year) %>% summarize(lists = n_distinct(group.id)) %>% ungroup

fq = left_join(total,agpi)

fq$freq[is.na(fq$freq)] = 0

tot = expand.grid(gridg3 = unique(data1$gridg3), gridg1 = unique(data1$gridg1), year = unique(data1$year))
temp = fq[,1:2]
tot = anti_join(tot,temp)
tot$lists = 0
tot$freq = NA
tot$gridg1 = as.character(tot$gridg1)

fq1 = rbind(tot,fq)
fq1$gridg = fq1$gridg1
fq1 = fq1 %>% group_by(gridg3,year) %>% summarize(freq = mean(na.omit(freq)))

comp = fq1 %>% filter(year %in% c(2014,2018))
comp$freq[comp$freq>0.5] = 0
length(unique(agpi$gridg3))

c2014 = comp %>% filter(year == 2014) %>% select(gridg3,freq)
names(c2014)[2] = "freq2014"
c2018 = comp %>% filter(year == 2018) %>% select(gridg3,freq)
names(c2018)[2] = "freq2018"

sub = left_join(c2014,c2018)
sub = sub %>% filter(!is.na(freq2014),!is.na(freq2018))
sub = sub %>% filter(freq2014 != 0 | freq2018 != 0)

tempz = comp %>% group_by(year) %>% summarize(freq = mean(na.omit(freq)))


load("maps.RData")
load("latest_state_map.RData")
require(rgeos)


statemap1 = gSimplify(smap, tol=0.01, topologyPreserve=TRUE)
s1 = smap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)


source('~/GitHub/state-of-indias-birds/SoIB functions.R')


pg = fortify(gridmapg3, region = c("id"))

filterstate = fortify(statemap1)


plotdis = na.omit(left_join(pg, comp, by = c('id' = "gridg3"))) # SPDF to plot

require(viridis)
require(extrafont)
require(ggthemes)
theme_set(theme_tufte())

brks = cut(c(0,max(na.omit(comp$freq))),breaks = 7)
x = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", levels(brks))),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(brks))))
brks = c(0,x[,2])
require(scales)

plotdismap = ggplot(data=comp) +
  facet_grid(.~year) +
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = freq), colour = NA)+  
  geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+  
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
        plot.margin=unit(c(0.5,1,0,1), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                       breaks = brks) +
  theme(strip.text.x = element_text(size = 20))+
  guides(fill = guide_legend(title = "", reverse = TRUE, override.aes = list(size=10))) +
  theme(legend.position = "none") +
  coord_map()


print(plotdismap)
ggsave(file="compmapFOWA.jpeg", units="in", width=6.6, height=7)
dev.off()  



########################### Andaman Green Pigeon test with unfiltered data

require(tidyverse)
load("data.RData")

spec = "Andaman Green-Pigeon"
#spec = "Nilgiri Pipit"
stt = c("Andaman & Nicobar Island")
stt1 = c("ANDAMAN & NICOBAR")
#stt = c("Tamil Nadu","Kerala")
#stt1 = c("TAMIL NADU","KERALA")

data1 = data %>% filter(year %in% c(2014:2018)) %>% filter(ALL.SPECIES.REPORTED == 1)

data1 = data1 %>% filter(ST_NM %in% stt)

agpi = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,year) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(gridg1,year) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
  ungroup()

total = data1 %>%
  group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
  group_by(gridg1,year) %>% summarize(lists = n_distinct(group.id)) %>% ungroup

fq = left_join(total,agpi)

fq$freq[is.na(fq$freq)] = 0

tot = expand.grid(gridg1 = unique(data1$gridg1), year = unique(data1$year))
temp = fq[,1:2]
tot = anti_join(tot,temp)
tot$lists = 0
tot$freq = NA
tot$gridg1 = as.character(tot$gridg1)

fq1 = rbind(tot,fq)
fq1$gridg = fq1$gridg1

comp = fq1 %>% filter(year %in% c(2014,2018))
length(unique(agpi$gridg1))

c2014 = comp %>% filter(year == 2014) %>% select(gridg1,freq)
names(c2014)[2] = "freq2014"
c2018 = comp %>% filter(year == 2018) %>% select(gridg1,freq)
names(c2018)[2] = "freq2018"

sub = left_join(c2014,c2018)
sub = sub %>% filter(!is.na(freq2014),!is.na(freq2018))
sub = sub %>% filter(freq2014 != 0 | freq2018 != 0)


tempz = comp %>% group_by(year) %>% summarize(freq = mean(na.omit(freq)))


load("maps.RData")
load("latest_state_map.RData")
require(rgeos)


statemap1 = gSimplify(smap, tol=0.01, topologyPreserve=TRUE)
s1 = smap@data
statemap1 = sp::SpatialPolygonsDataFrame(statemap1, s1)


source('~/GitHub/state-of-indias-birds/SoIB functions.R')


pg = fortify(gridmapg1, region = c("id"))

filterstate = fortify(statemap1[statemap1@data$stname %in% stt1,])


plotdis = na.omit(left_join(pg, comp, by = c('id' = "gridg1"))) # SPDF to plot

require(viridis)
require(extrafont)
require(ggthemes)
theme_set(theme_tufte())

brks = cut(c(0,max(na.omit(comp$freq))),breaks = 7)
x = cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", levels(brks))),
          upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", levels(brks))))
brks = c(0,x[,2])
require(scales)

plotdismap = ggplot(data=comp) +
  facet_grid(.~year) +
  geom_polygon(data = plotdis, aes(x=long, y=lat, group=group,fill = freq), colour = "black")+  
  geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = "black", fill = NA)+  
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
        plot.margin=unit(c(0.5,1,0,1), "cm"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = NA, colour = NA),
        plot.background = element_rect(fill = NA, colour = NA))+
  theme(text=element_text(family="Gill Sans MT"), legend.position = "bottom", legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_gradient2(low = muted("blue"),
                       high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                       breaks = brks) +
  theme(strip.text.x = element_text(size = 20))+
  guides(fill = guide_legend(title = "", reverse = TRUE, override.aes = list(size=10))) +
  theme(legend.position = "none") +
  coord_map()

print(plotdismap)
ggsave(file="compmapAGPIunfiltered.jpeg", units="in", width=6.6, height=7)
dev.off()  
