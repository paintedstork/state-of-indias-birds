library(tidyverse)

t3 = data %>%
  filter(COMMON.NAME == "Red-necked Falcon") %>%
  select(gridg1)

t = as.numeric(t3$gridg1)

t2 = unique(nb8g1[[t[1]]])
for (i in 2:length(t))
{
  t1 = unique(nb8g1[[t[i]]])
  t2 = union(t2,t1)
}

n = setdiff(t2,t)
n = intersect(n,unique(data$gridg1))

filtercountry = fortify(indiamap)
pg = fortify(gridmapg1, region = c("id"))
pg1 = pg %>%
  filter(id %in% t)
pg2 = pg %>%
  filter(id %in% n)

lgrid = fortify(gridmapg3, region = c("id"))
lgrid = lgrid %>%
  filter(id %in% unique(data$gridg3))

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

datac = data %>%
  distinct(group.id,LONGITUDE,LATITUDE)

datac = datac[duplicated(datac[,2:3]),]

plotindiamap +
  #geom_path(data = lgrid, aes(x = long, y = lat, group = group), col = "black") +
  #geom_point(data = datac, aes(x = LONGITUDE, y = LATITUDE),col = "red", alpha = 0.3, size = 2, stroke = 0)
  geom_polygon(data = pg1, aes(x = long, y = lat, group = group), col = "red", fill = "red") +
  geom_polygon(data = pg2, aes(x = long, y = lat, group = group), col = "red", fill = "red", alpha = 0.3)
  
