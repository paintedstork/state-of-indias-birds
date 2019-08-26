require(lubridate)
require(tidyverse)
require(data.table)
require(viridis)

gaps = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  group_by(ST_NM,DISTRICT) %>% summarize(lists = n_distinct(group.id)) %>%
  arrange(lists, .by_group = T) %>% ungroup %>%
  #group_by(ST_NM) %>% filter(lists <= 5) %>% ungroup
  group_by(ST_NM) %>% slice(1:5) %>% ungroup

alldis = data.frame(ST_NM = as.character(districtmap$ST_NM), DISTRICT = as.character(districtmap$DISTRICT))

dis = data %>%
  filter(ALL.SPECIES.REPORTED == 1) %>%
  distinct(ST_NM,DISTRICT)
dis$ST_NM = as.character(dis$ST_NM)
dis$DISTRICT = as.character(dis$DISTRICT)

extra = setdiff(alldis,dis)
names(extra)[1] = "t"
extra$DISTRICT = extra$t
extra = extra[,-1]
extra$lists = 0
extra$ST_NM = as.character(extra$ST_NM)
extra$DISTRICT = as.character(extra$DISTRICT)

gaps = as.data.frame(gaps)
gaps$ST_NM = as.character(gaps$ST_NM)
gaps$DISTRICT = as.character(gaps$DISTRICT)

gaps = rbind(gaps,extra)
gaps = gaps %>%
  group_by(ST_NM) %>%
  arrange(lists, .by_group = T)

write.csv(gaps, "gaps.csv")

filterdistrict = fortify(districtmap[districtmap@data$ST_NM %in% gaps$ST_NM & districtmap@data$DISTRICT %in% gaps$DISTRICT,], region = c("DISTRICT"))
filterstate = fortify(statemap)

plotindiamap = ggplot() +
  geom_polygon(data = filterstate, aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
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

plotdf = na.omit(left_join(filterdistrict,gaps, by = c('id' = "DISTRICT")))

# for log scale
plotdf$lists = plotdf$lists + 1

plotindiamap +
  geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = lists), size = 0.5)+
  geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 1)+
  scale_colour_viridis_c(aesthetics = "fill", direction = -1, trans = 'log', breaks = c(1,10,100,1000),
                         labels = c(0,10,100,1000))
