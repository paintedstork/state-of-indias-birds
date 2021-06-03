## prep the eBird data

########### Relationship between covariates, and between frequency of reporting and covariates

rm(list = ls(all.names = TRUE))
require(tidyverse)
load("data.RData")
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)
library(cowplot)

data1 = data %>% filter(!is.na(region)) %>% group_by(group.id) %>% slice(1) %>% ungroup %>%
  select(region,no.sp,DURATION.MINUTES)

data1$no.sp = as.numeric(data1$no.sp)
data1$DURATION.MINUTES = as.numeric(data1$DURATION.MINUTES)/60

data1$dur = ceiling(data1$DURATION.MINUTES*10)/10
data1 = data1 %>%
  group_by(region,dur) %>% summarize(lla = round(mean(no.sp)))

## add sample size for regions

ss = data1 %>% group_by(region) %>% summarize(n = n())
data1[data1$region == "Coast",]$region = "Coast (N=169,026)"
data1[data1$region == "Desert",]$region = "Desert (N=5,758)"
data1[data1$region == "Himalaya",]$region = "Himalaya (N=9,253)"
data1[data1$region == "Island",]$region = "Island (N=3,050)"
data1[data1$region == "North-east",]$region = "North-east (N=57,403)"
data1[data1$region == "Plains",]$region = "Plains (N=246,133)"
data1[data1$region == "Trans-Himalaya",]$region = "Trans-Himalaya (N=3,605)"
data1[data1$region == "WGhats",]$region = "WGhats (N=116,548)"

##

ggp = ggplot(data1, aes(x=dur, y=lla)) + 
  facet_wrap(. ~ region, scale="free", ncol = 4) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point(size = 0.5) +
  #geom_smooth(method = "loess") +
  xlab("Duration (hours)") +
  ylab("List Length (number of species)")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 12, colour = "black", face = 'italic')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

data2 = data %>% filter(!is.na(region)) %>% group_by(group.id) %>% slice(1) %>% ungroup %>%
  select(region,no.sp,EFFORT.DISTANCE.KM)

data2$no.sp = as.numeric(data2$no.sp)
data2$EFFORT.DISTANCE.KM = as.numeric(data2$EFFORT.DISTANCE.KM)
data2$dis = ceiling(data2$EFFORT.DISTANCE.KM*5)/5
data2 = data2 %>%
  group_by(region,dis) %>% summarize(lla = round(mean(no.sp)))


data2[data2$region == "Coast",]$region = "Coast (N=169,026)"
data2[data2$region == "Desert",]$region = "Desert (N=5,758)"
data2[data2$region == "Himalaya",]$region = "Himalaya (N=9,253)"
data2[data2$region == "Island",]$region = "Island (N=3,050)"
data2[data2$region == "North-east",]$region = "North-east (N=57,403)"
data2[data2$region == "Plains",]$region = "Plains (N=246,133)"
data2[data2$region == "Trans-Himalaya",]$region = "Trans-Himalaya (N=3,605)"
data2[data2$region == "WGhats",]$region = "WGhats (N=116,548)"

ggp = ggplot(data2, aes(x=dis, y=lla)) + 
  facet_wrap(. ~ region, scale="free", ncol = 4) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point(size = 0.5) +
  #geom_smooth(method = "loess") +
  xlab("Distance (km)") +
  ylab("List Length (number of species)")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 12, colour = "black", face = 'italic')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ggpp = plot_grid(ggp1,ggp2,nrow=2,ncol=1,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

y.grob = textGrob("List Length (number of species)", gp = gpar(fontface = "bold", fontsize = 16,
                                                               fontfamily = "Gill Sans MT"), rot = 90)

ggpp1 = grid.arrange(arrangeGrob(ggpp, left = y.grob))

tiff('Fig. 1.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggpp1)
dev.off()


########### tightness of relationship between detectability and effort


load("data.RData")
library(tidyverse)
library(ggthemes)
library(cowplot)

theme_set(theme_tufte())
require(extrafont)

spec = "Jungle Babbler"

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

data1$DURATION.MINUTES = data1$DURATION.MINUTES/60

data1$dur = ceiling(data1$DURATION.MINUTES*10)/10

data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*5)/5


# lla

datal = data1 %>%
  group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(no.sp) %>% summarize(perc = n()/max(lists))
names(datal)[1] = "effort"
datal$type = "List Length"

datal = datal %>% filter(effort <= 100)

# dur

datar = data1 %>%
  group_by(dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dur) %>% summarize(perc = n()/max(lists))
names(datar)[1] = "effort"
datar$type = "Duration (hours)"

datar = datar %>% filter(effort <= 10)

# distance

datad = data1 %>%
  group_by(dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dis) %>% summarize(perc = n()/max(lists))
names(datad)[1] = "effort"
datad$type = "Distance (kms)"

datad = datad %>% filter(effort <= 20)

datat = rbind(datal,datar,datad)

datat$type = factor(datat$type, levels = c("List Length","Duration (hours)","Distance (kms)"))

ggp = ggplot(datat, aes(x=effort, y = perc)) + 
  facet_wrap(. ~ type, scale="free_x", nrow = 3, strip.position="right") +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("Effort") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  ggtitle("Jungle Babbler") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.y = element_text(size = 14, colour = "white", face = 'italic')) +
  #scale_x_continuous(limits = c(0,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )




spec = "Black-winged Kite"

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

data1$DURATION.MINUTES = data1$DURATION.MINUTES/60

data1$dur = ceiling(data1$DURATION.MINUTES*10)/10

data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*10)/10


# lla

datal = data1 %>%
  group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(no.sp) %>% summarize(perc = n()/max(lists))
names(datal)[1] = "effort"
datal$type = "List Length"

datal = datal %>% filter(effort <= 100)

# dur

datar = data1 %>%
  group_by(dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dur) %>% summarize(perc = n()/max(lists))
names(datar)[1] = "effort"
datar$type = "Duration (hours)"

datar = datar %>% filter(effort <= 10)

# distance

datad = data1 %>%
  group_by(dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dis) %>% summarize(perc = n()/max(lists))
names(datad)[1] = "effort"
datad$type = "Distance (kms)"

datad = datad %>% filter(effort <= 20)

datat = rbind(datal,datar,datad)

datat$type = factor(datat$type, levels = c("List Length","Duration (hours)","Distance (kms)"))

ggp = ggplot(datat, aes(x=effort, y = perc)) + 
  facet_wrap(. ~ type, scale="free_x", nrow = 3, strip.position="right") +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("Effort") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  ggtitle("Black-winged Kite") +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.y = element_text(size = 14, colour = "black", face = 'italic')) +
  #scale_x_continuous(limits = c(0,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ggpp = plot_grid(ggp1,ggp2,nrow=1,ncol=2,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

x.grob = textGrob("Effort", gp = gpar(fontface = "bold", fontsize = 16,
                                      fontfamily = "Gill Sans MT"))

y.grob = textGrob("Frequency of reporting", gp = gpar(fontface = "bold", fontsize = 16,
                                                      fontfamily = "Gill Sans MT"), rot = 90)

ggpp1 = grid.arrange(arrangeGrob(ggpp, bottom = x.grob, left = y.grob))

tiff('Fig. 2.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggpp1)
dev.off()




########### relationship between detectability and list length - Jungle Babbler and Black-winged Kite


load("dataforanalyses.RData")
library(tidyverse)
library(ggthemes)
library(cowplot)

theme_set(theme_tufte())
require(extrafont)

spec = "Jungle Babbler"

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)
data1 = data1 %>%
  group_by(region,no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec, region %in% c("Coast","Plains","WGhats")) %>%
  group_by(region,no.sp) %>% summarize(perc = n()/max(lists))

ss = data %>% filter(!is.na(region)) %>% group_by(group.id) %>% slice(1) %>% ungroup %>%
  group_by(region) %>% summarize(n = n())
data1[data1$region == "Coast",]$region = "Coast (N=168,463)"
data1[data1$region == "Plains",]$region = "Plains (N=244,699)"
data1[data1$region == "WGhats",]$region = "WGhats (N=116,288)"


ggp = ggplot(data1, aes(x=no.sp, y = perc)) + 
  ggtitle(spec) +
  facet_wrap(. ~ region, nrow = 3) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("List Length (number of species)") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_x_continuous(limits = c(0,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

spec = "Black-winged Kite"

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)
data1 = data1 %>%
  group_by(region,no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec, region %in% c("Coast","Plains","WGhats")) %>%
  group_by(region,no.sp) %>% summarize(perc = n()/max(lists))

data1[data1$region == "Coast",]$region = "Coast (N=168,463)"
data1[data1$region == "Plains",]$region = "Plains (N=244,699)"
data1[data1$region == "WGhats",]$region = "WGhats (N=116,288)"


ggp = ggplot(data1, aes(x=no.sp, y = perc)) + 
  ggtitle(spec) +
  facet_wrap(. ~ region, nrow = 3) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point() +
  xlab("List Length (number of species)") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_x_continuous(limits = c(0,100)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ggpp = plot_grid(ggp1,ggp2,nrow=1,ncol=2,rel_widths = c(1/2, 1/2))

require(grid)
require(gridExtra)

x.grob = textGrob("List Length (number of species)", gp = gpar(fontface = "bold", fontsize = 16,
                                                               fontfamily = "Gill Sans MT"))

y.grob = textGrob("Frequency of reporting", gp = gpar(fontface = "bold", fontsize = 16,
                                                      fontfamily = "Gill Sans MT"), rot = 90)

ggpp1 = grid.arrange(arrangeGrob(ggpp, bottom = x.grob, left = y.grob))

tiff('Fig. 3.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggpp1)
dev.off()






#### comparison of 50 species - supplementary material

load("dataforanalyses.RData")
set.seed(50)
species = specieslist$COMMON.NAME[!is.na(specieslist$ht)]
select_specs = sample(species,50)

load("data.RData")
library(tidyverse)
library(ggthemes)
library(cowplot)

theme_set(theme_tufte())
require(extrafont)

## lla

spec = select_specs[1]

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

# lla

datal = data1 %>%
  group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(no.sp) %>% summarize(perc = n()/max(lists))
names(datal)[1] = "effort"
datal$type = "List Length"

datal = datal %>% filter(effort <= 100)
datal$species = spec
dataf = datal



for (i in 2:50)
{
  spec = select_specs[i]
  
  temp = data %>%
    filter(COMMON.NAME == spec) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  # lla
  
  datal = data1 %>%
    group_by(no.sp) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
    filter(COMMON.NAME == spec) %>%
    group_by(no.sp) %>% summarize(perc = n()/max(lists))
  names(datal)[1] = "effort"
  datal$type = "List Length"
  
  datal = datal %>% filter(effort <= 100)
  datal$species = spec
  dataf = rbind(dataf,datal)
}


ggp = ggplot(dataf[dataf$species %in% select_specs[1:25],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("List Length") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

tiff('Fig. S1.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp1)
dev.off()


ggp = ggplot(dataf[dataf$species %in% select_specs[26:50],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("List Length") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

tiff('Fig. S2.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp2)
dev.off()


###############################################################
## dur

spec = select_specs[1]

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

data1$DURATION.MINUTES = data1$DURATION.MINUTES/60

data1$dur = ceiling(data1$DURATION.MINUTES*10)/10

# dur

datar = data1 %>%
  group_by(dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dur) %>% summarize(perc = n()/max(lists))
names(datar)[1] = "effort"
datar$type = "Duration (hours)"

datar = datar %>% filter(effort <= 10)
datar$species = spec
datag = datar



for (i in 2:50)
{
  spec = select_specs[i]
  
  temp = data %>%
    filter(COMMON.NAME == spec) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  data1$DURATION.MINUTES = data1$DURATION.MINUTES/60
  
  data1$dur = ceiling(data1$DURATION.MINUTES*10)/10
  
  # dur
  
  datar = data1 %>%
    group_by(dur) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
    filter(COMMON.NAME == spec) %>%
    group_by(dur) %>% summarize(perc = n()/max(lists))
  names(datar)[1] = "effort"
  datar$type = "Duration (hours)"
  
  datar = datar %>% filter(effort <= 10)
  datar$species = spec
  datag = rbind(datag,datar)
}


ggp = ggplot(datag[datag$species %in% select_specs[1:25],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Duration") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

tiff('Fig. S3.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp1)
dev.off()


ggp = ggplot(datag[datag$species %in% select_specs[26:50],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Duration") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

tiff('Fig. S4.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp2)
dev.off()



###############################################################
## dis

spec = select_specs[1]

temp = data %>%
  filter(COMMON.NAME == spec) %>%
  distinct(gridg3,month)
data1 = temp %>% left_join(data)

data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*10)/10

# dis

datad = data1 %>%
  group_by(dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
  filter(COMMON.NAME == spec) %>%
  group_by(dis) %>% summarize(perc = n()/max(lists))
names(datad)[1] = "effort"
datad$type = "Distance (kms)"

datad = datad %>% filter(effort <= 20)
datad$species = spec
datah = datad



for (i in 2:50)
{
  spec = select_specs[i]
  
  temp = data %>%
    filter(COMMON.NAME == spec) %>%
    distinct(gridg3,month)
  data1 = temp %>% left_join(data)
  
  data1$dis = ceiling(data1$EFFORT.DISTANCE.KM*10)/10
  
  # dis
  
  datad = data1 %>%
    group_by(dis) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
    filter(COMMON.NAME == spec) %>%
    group_by(dis) %>% summarize(perc = n()/max(lists))
  names(datad)[1] = "effort"
  datad$type = "Distance (kms)"
  
  datad = datad %>% filter(effort <= 20)
  datad$species = spec
  datah = rbind(datah,datad)
}


ggp = ggplot(datah[datah$species %in% select_specs[1:25],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Distance") +
  ylab("Frequency of reporting")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

tiff('Fig. S5.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp1)
dev.off()


ggp = ggplot(datah[datah$species %in% select_specs[26:50],], aes(x=effort, y = perc)) + 
  facet_wrap(. ~ species, scale="free_y", nrow = 5, ncol = 5, strip.position="top") +
  geom_point() +
  xlab("Distance") +
  ylab("Frequency of reporting")

ggp2 = ggp +
  theme(axis.title.x = element_text(size = 14), axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 14, angle = 90), axis.text.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.x = element_text(size = 10, colour = "black", face = 'italic')) +
  scale_y_continuous(breaks = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

tiff('Fig. S6.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp2)
dev.off()