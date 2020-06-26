t_col = function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val = col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col = rgb(rgb.val[1], rgb.val[2], rgb.val[3],
              max = 255,
              alpha = (100 - percent) * 255 / 100,
              names = name)
  
  ## Save the color
  invisible(t.col)
}

##################### Plot all maps

plotspeciesmaps = function(type = "terrain", listofbirds, back = "black")
{
  require(tidyverse)
  require(raster)
  require(ggsci)
  load("data.RData")
  data = data %>% filter(!LONGITUDE > 88.39 | !COMMON.NAME %in% c("Indian Paradise-Flycatcher",
                                                                  "Thick-billed Flowerpecker",
                                                                  "Indian Golden-Oriole",
                                                                  "Booted Warbler",
                                                                  "Western Crowned Warbler",
                                                                  "Black-headed Cuckooshrike",
                                                                  "Jungle Owlet",
                                                                  "Indian Cormorant",
                                                                  "Fork-tailed Drongo-Cuckoo"))
  data = data %>% filter(ST_NM != "Kerala" | !COMMON.NAME %in% c("White-tailed Iora"))
  latlong = data %>% distinct(group.id,LONGITUDE,LATITUDE)
  days = data %>% distinct(group.id,day)
  load("dataforanalyses.RData")
  data = left_join(data,latlong)
  data = data %>% filter(!LONGITUDE > 88.39 | !COMMON.NAME %in% c("Indian Paradise-Flycatcher",
                                                                  "Thick-billed Flowerpecker",
                                                                  "Indian Golden-Oriole",
                                                                  "Booted Warbler",
                                                                  "Western Crowned Warbler",
                                                                  "Black-headed Cuckooshrike",
                                                                  "Jungle Owlet",
                                                                  "Indian Cormorant",
                                                                  "Fork-tailed Drongo-Cuckoo"))
  data = data %>% filter(ST_NM != "Kerala" | !COMMON.NAME %in% c("White-tailed Iora"))
  data = left_join(data,days)
  load("maps.RData")
  load("clips.RData")
  load("neighbours.RData")
  #load("emptycells.RData")
  load("vagrantdata.RData")
  d = d %>% filter(!LONGITUDE > 88.39 | !COMMON.NAME %in% c("Indian Paradise-Flycatcher",
                                                            "Thick-billed Flowerpecker",
                                                            "Indian Golden-Oriole",
                                                            "Booted Warbler",
                                                            "Western Crowned Warbler",
                                                            "Black-headed Cuckooshrike",
                                                            "Jungle Owlet",
                                                            "Indian Cormorant",
                                                            "Fork-tailed Drongo-Cuckoo"))
  d = d %>% filter(ST_NM != "Kerala" | !COMMON.NAME %in% c("White-tailed Iora"))
  vaglist = unique(d$COMMON.NAME)
  
  source('~/GitHub/state-of-indias-birds/SoIB functions.R')
  data$gridg = data$g2clip
  nb8g = nb8g2
  #emp = emptyg2
  d$gridg = d$g2clip
  
  pg = fortify(g2clip, region = c("id"))
  #pge = pg %>%
  #  filter(id %in% as.numeric(emp))
  #lgrid = fortify(gridmapg3, region = c("id"))
  
  data = data %>% filter(year>2013)
  datac = data %>%
    distinct(group.id,LONGITUDE,LATITUDE)
  
  datac = datac[duplicated(datac[,2:3]),]
  
  
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
  
  
  filtercountry = fortify(indiamap)
  
  
  if (type == "terrain")
  {
    indiatif = brick("IndiaDEM-Colour.tif")
    indiatif = as.data.frame(indiatif, xy = TRUE)
    indiatif$IndiaDEM.Colour.1 = indiatif$IndiaDEM.Colour.1/255
    indiatif$IndiaDEM.Colour.2 = indiatif$IndiaDEM.Colour.2/255
    indiatif$IndiaDEM.Colour.3 = indiatif$IndiaDEM.Colour.3/255
    names(indiatif)[3:5] = c("r","g","b")
    indiatif$codes = rgb(indiatif$r,indiatif$g,indiatif$b)
    indiatif = indiatif %>% mutate(codes = replace(codes, codes == "#000000", NA))
  }
  
  #pgec = pge %>%
  #  group_by(group) %>% nest() 
  #lines = map_df(pgec$data, draw.crosshatch, width = .13, pattern= "vertical")
  #lines = lines %>% filter(!is.na(x) & !is.na(y) & !is.na(x) & !is.na(y))
  
  datas = data %>% filter(day > 145 & day <= 215)
  dataw = data %>% filter(day <= 60 | day > 325)
  datap = data %>% filter((day > 60 & day <= 145) | (day > 215 & day <= 325))
  
  ds = d %>% filter(day > 145 & day <= 215)
  dw = d %>% filter(day <= 60 | day > 325)
  dp = d %>% filter((day > 60 & day <= 145) | (day > 215 & day <= 325))
  
  tw = 1
  ts = 1
  
  #spss = vaglist
  
  #lists = lists %>% filter(Common.Name %in% spss)
  
  lists = lists %>% filter(Common.Name %in% listofbirds)
  
  for (i in 1:length(lists$Common.Name))
  {
    switchr = F
    switchm = F
    
    if (is.na(lists$Range.Status[i]))
    {next}
    
    status = as.character(lists$Migratory.Status[i])
    if (lists$Common.Name[i] %in% c("House Sparrow","Black Kite"))
    {status = "R"}
    
    if (status == "R")
    {
      t3 = data %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      t = as.numeric(t3$gridg)
      
      t2 = unique(nb8g[[t[1]]])
      
      for (j in 2:length(t))
      {
        t1 = unique(nb8g[[t[j]]])
        t2 = union(t2,t1)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchr = T
        dv = d %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      n = setdiff(t2,t)
      n = intersect(n,unique(data$gridg))
      
      pg1 = pg %>%
        filter(id %in% t)
      pg2 = pg %>%
        filter(id %in% n)
    }
    
    if (status == "LM")
    {
      t3 = data %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      t = as.numeric(t3$gridg)
      
      t2 = unique(nb8g[[t[1]]])
      
      if (length(t) > 1)
      {
        for (j in 2:length(t))
        {
          t1 = unique(nb8g[[t[j]]])
          t2 = union(t2,t1)
        }
      }
      
      ## summer
      
      t3s = datas %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      ts = as.numeric(t3s$gridg)
      
      t2s = unique(nb8g[[ts[1]]])
      
      if (length(ts) > 1)
      {
        for (j in 2:length(ts))
        {
          t1s = unique(nb8g[[ts[j]]])
          t2s = union(t2s,t1s)
        }
      }
      
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvs = ds %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      ns = setdiff(t2s,t)
      ns = intersect(ns,unique(data$gridg))
      
      pg1s = pg %>%
        filter(id %in% ts)
      pg2s = pg %>%
        filter(id %in% ns)
      
      ## winter
      
      t3w = dataw %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      tw = as.numeric(t3w$gridg)
      
      t2w = unique(nb8g[[tw[1]]])
      
      if(length(tw > 1))
      {
        for (j in 2:length(tw))
        {
          t1w = unique(nb8g[[tw[j]]])
          t2w = union(t2w,t1w)
        }
      }
      
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvw = dw %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      nw = setdiff(t2w,t)
      nw = intersect(nw,unique(data$gridg))
      
      pg1w = pg %>%
        filter(id %in% tw)
      pg2w = pg %>%
        filter(id %in% nw)
      
      ## passage
      
      t3p = datap %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      tp = as.numeric(t3p$gridg)
      
      t2p = unique(nb8g[[tp[1]]])
      
      if (length(tp) > 1)
      {
        for (j in 2:length(tp))
        {
          t1p = unique(nb8g[[tp[j]]])
          t2p = union(t2p,t1p)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvp = dp %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      np = setdiff(t2p,t)
      np = intersect(np,unique(data$gridg))
      
      pg1p = pg %>%
        filter(id %in% tp)
      pg2p = pg %>%
        filter(id %in% np)
      
      pg1p = setdiff(pg1p,pg1s)
      pg1p = setdiff(pg1p,pg1w)
      
      pg2p = setdiff(pg2p,pg2s)
      pg2p = setdiff(pg2p,pg2w)
      
      pg1w = setdiff(pg1w,pg1s)
      pg2w = setdiff(pg2w,pg2s)
      
      dvw = setdiff(dvw,dvs)
      dvp = setdiff(dvp,dvs)
      dvp = setdiff(dvp,dvw)
    }
    
    if (status == "S")
    {
      t3 = data %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      t = as.numeric(t3$gridg)
      
      t2 = unique(nb8g[[t[1]]])
      
      if (length(t) > 1)
      {
        for (j in 2:length(t))
        {
          t1 = unique(nb8g[[t[j]]])
          t2 = union(t2,t1)
        }
      }
      
      ## summer
      
      t3s = datas %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      ts = as.numeric(t3s$gridg)
      
      t2s = unique(nb8g[[ts[1]]])
      
      if (length(ts) > 1)
      {
        for (j in 2:length(ts))
        {
          t1s = unique(nb8g[[ts[j]]])
          t2s = union(t2s,t1s)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvs = ds %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      ns = setdiff(t2s,t)
      ns = intersect(ns,unique(data$gridg))
      
      pg1s = pg %>%
        filter(id %in% ts)
      pg2s = pg %>%
        filter(id %in% ns)
      
      ## winter
      
      t3w = dataw %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      tw = as.numeric(t3w$gridg)
      
      
      t2w = unique(nb8g[[tw[1]]])
      
      if (length(tw) > 1)
      {
        for (j in 2:length(tw))
        {
          t1w = unique(nb8g[[tw[j]]])
          t2w = union(t2w,t1w)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvw = dw %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      nw = setdiff(t2w,t)
      nw = intersect(nw,unique(data$gridg))
      
      pg1w = pg %>%
        filter(id %in% tw)
      pg2w = pg %>%
        filter(id %in% nw)
      
      
      ## passage
      
      t3p = datap %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      tp = as.numeric(t3p$gridg)
      
      t2p = unique(nb8g[[tp[1]]])
      
      if (length(tp) > 1)
      {
        for (j in 2:length(tp))
        {
          t1p = unique(nb8g[[tp[j]]])
          t2p = union(t2p,t1p)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvp = dp %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      np = setdiff(t2p,t)
      np = intersect(np,unique(data$gridg))
      
      pg1p = pg %>%
        filter(id %in% tp)
      pg2p = pg %>%
        filter(id %in% np)
      
      pg1w = setdiff(pg1w,pg1s)
      pg1w = setdiff(pg1w,pg1p)
      
      pg2w = setdiff(pg2w,pg2s)
      pg2w = setdiff(pg2w,pg2p)
      
      pg1p = setdiff(pg1p,pg1s)
      pg2p = setdiff(pg2p,pg2s)
      
      dvp = setdiff(dvp,dvs)
      dvw = setdiff(dvw,dvs)
      dvw = setdiff(dvw,dvp)
    }
    
    if (status == "W/P")
    {
      t3 = data %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      t = as.numeric(t3$gridg)
      
      t2 = unique(nb8g[[t[1]]])
      
      if (length(t) > 1)
      {
        for (j in 2:length(t))
        {
          t1 = unique(nb8g[[t[j]]])
          t2 = union(t2,t1)
        }
      }
      
      ## summer
      
      t3s = datas %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      ts = as.numeric(t3s$gridg)
      
      t2s = unique(nb8g[[ts[1]]])
      
      if (length(ts) > 1)
      {
        for (j in 2:length(ts))
        {
          t1s = unique(nb8g[[ts[j]]])
          t2s = union(t2s,t1s)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvs = ds %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      ns = setdiff(t2s,t)
      ns = intersect(ns,unique(data$gridg))
      
      pg1s = pg %>%
        filter(id %in% ts)
      pg2s = pg %>%
        filter(id %in% ns)
      
      
      
      
      ## winter
      
      t3w = dataw %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      tw = as.numeric(t3w$gridg)
      
      t2w = unique(nb8g[[tw[1]]])
      
      if (length(tw) > 1)
      {
        for (j in 2:length(tw))
        {
          t1w = unique(nb8g[[tw[j]]])
          t2w = union(t2w,t1w)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvw = dw %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      nw = setdiff(t2w,t)
      nw = intersect(nw,unique(data$gridg))
      
      pg1w = pg %>%
        filter(id %in% tw)
      pg2w = pg %>%
        filter(id %in% nw)
      
      ## passage
      
      t3p = datap %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg)
      
      tp = as.numeric(t3p$gridg)
      
      t2p = unique(nb8g[[tp[1]]])
      
      if (length(tp) > 1)
      {
        for (j in 2:length(tp))
        {
          t1p = unique(nb8g[[tp[j]]])
          t2p = union(t2p,t1p)
        }
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvp = dp %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      
      
      np = setdiff(t2p,t)
      np = intersect(np,unique(data$gridg))
      
      pg1p = pg %>%
        filter(id %in% tp)
      pg2p = pg %>%
        filter(id %in% np)
      
      pg1s = setdiff(pg1s,pg1w)
      pg1s = setdiff(pg1s,pg1p)
      
      pg2s = setdiff(pg2s,pg2w)
      pg2s = setdiff(pg2s,pg2p)
      
      pg1p = setdiff(pg1p,pg1w)
      pg2p = setdiff(pg2p,pg2w)
      
      dvp = setdiff(dvp,dvw)
      dvs = setdiff(dvs,dvw)
      dvs = setdiff(dvs,dvp)
    }
    
    
    #lgrid1 = lgrid %>%
    #  filter(id %in% unique(data$gridg3))
    
    if (type == "terrain")
    {
      resident = "#5c557e"
      summer = "#791842"
      passage = "#c47755"
      winter = "#006079"
    }
    
    if (type == "blank")
    {
      resident = "#807093"
      summer = "#a8596b"
      passage = "#ca8357"
      winter = "#3e8e97"
    }
    
    require(extrafont)
    
    #sm1 = t_col(summer,perc=60,name="lt.sm")
    #ps1 = t_col(passage,perc=60,name="lt.ps")
    #wt1 = t_col(winter,perc=60,name="lt.wt")
    
    ggp = ggplot() +
      {if(type == "blank")geom_polygon(data = filtercountry, aes(x=long, y=lat, group=group), 
                                       colour = "black", fill = "white")}+  
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      {if(type == "terrain")geom_raster(data = indiatif , aes(x = x, y = y, fill = codes),
                                        alpha = 0.4)} +
      {if(type == "terrain")scale_fill_identity(na.value = back)} +
      #{if(type == "terrain")geom_polygon(data = filtercountry, aes(x=long, y=lat, group=group), 
      #colour = "black", fill = NA)}+
      {if(switchr)geom_point(data = dv, aes(x = LONGITUDE, y = LATITUDE), col = resident, shape = 4, 
                             size = 1, alpha = 1)}+
      {if(switchm)geom_point(data = dvp, aes(x = LONGITUDE, y = LATITUDE), col = passage, shape = 4, 
                             size = 1, alpha = 1)}+
      {if(switchm)geom_point(data = dvw, aes(x = LONGITUDE, y = LATITUDE), col = winter, shape = 4, 
                             size = 1, alpha = 1)}+
      {if(switchm)geom_point(data = dvs, aes(x = LONGITUDE, y = LATITUDE), col = summer, shape = 4, 
                             size = 1, alpha = 1)}+
      #geom_polygon(data = pge, aes(x = long, y = lat, group = group), 
      #             col = NA, fill = "transparent", alpha = 0.6)+
      {if(status == "R")geom_polygon(data = pg1, aes(x = long, y = lat, group = group), 
                                     col = NA, fill = resident, alpha = 1)}+
      {if(status == "R")geom_polygon(data = pg2, aes(x = long, y = lat, group = group), 
                                     col = NA, fill = resident, alpha = 0.6)}+
      {if(status != "R")geom_polygon(data = pg1p, aes(x = long, y = lat, group = group), 
                                     col = NA, fill = passage, alpha = 1)}+
      {if(status != "R")geom_polygon(data = pg2p, aes(x = long, y = lat, group = group), 
                                     col = NA, fill = passage, alpha = 0.6)}+
      {if(status != "R" & length(tw) > 1)geom_polygon(data = pg1w, aes(x = long, y = lat, group = group), 
                                                      col = NA, fill = winter, alpha = 1)}+
      {if(status != "R" & length(tw) > 1)geom_polygon(data = pg2w, aes(x = long, y = lat, group = group), 
                                                      col = NA, fill = winter, alpha = 0.6)}+
      {if(status != "R" & length(ts) > 1)geom_polygon(data = pg1s, aes(x = long, y = lat, group = group), 
                                                      col = NA, fill = summer, alpha = 1)}+
      {if(status != "R" & length(ts) > 1)geom_polygon(data = pg2s, aes(x = long, y = lat, group = group), 
                                                      col = NA, fill = summer, alpha = 0.6)}+
      {if(switchm)annotate("rect", xmin=c(82.2,82.2,82.2), xmax=c(83.7,83.7,83.7), ymin=c(12,10,8), 
                           ymax=c(13.5,11.5,9.5), alpha=0.6, fill = c(summer,passage,winter))}+
      {if(switchm)annotate("rect", xmin=c(82.4,82.4,82.4), xmax=c(83.5,83.5,83.5), ymin=c(12.2,10.2,8.2), 
                           ymax=c(13.3,11.3,9.3), alpha=1, fill=c(summer,passage,winter))}+
      {if(switchm)annotate("text", x = c(87.47,87.2,87.2), y = c(12.75,10.75,8.75), 
                           label = c("Summer","Passage","Winter") , color="#56697B", size=8, family="Gill Sans MT")}+
      {if(!switchm)annotate("rect", xmin=c(82.2), xmax=c(83.7), ymin=c(10), 
                            ymax=c(11.5), alpha=0.6, fill = c(resident))}+
      {if(!switchm)annotate("rect", xmin=c(82.4), xmax=c(83.5), ymin=c(10.2), 
                            ymax=c(11.3), alpha=1, fill=c(resident))}+
      {if(!switchm)annotate("text", x = c(87.56), y = c(10.75), 
                            label = c("Resident") , color="#56697B", size=8, family="Gill Sans MT")}+
      #geom_segment(data=lines, aes(x= x, y = y , xend = xend, yend = yend), inherit.aes = F, col = "black")+
      theme(text=element_text(family="Gill Sans MT")) +
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.margin=unit(c(0.5,0,0.5,0), "cm"),
            plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
      theme(legend.position = "none")+
      coord_quickmap()
    
    
    sps = as.character(lists$India.Checklist.Name[i])
    name = paste(sps,"_",type,".jpeg",sep="")
    
    print(ggp)
    ggsave(file=name, units="in", width=6.3, height=7, bg = back)
    dev.off()
  }
}



#####################################################


plotspeciestrends = function(listofbirds, scol = "#869B27")
{
  load("AllTrends.RData")
  source('~/GitHub/state-of-indias-birds/SoIB functions.R')
  library(tidyverse)
  library(plyr)
  
  map = read.csv("Map to Other Lists - map.csv")
  map = map %>%
    filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                           "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                           "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
    dplyr::select(eBird.English.Name.2018,India.Checklist.Name)
  
  lists = read.csv("stateofindiasbirds.csv")
  lists = left_join(lists,map,by = c("Common.Name" = "India.Checklist.Name"))
  lists = lists %>% mutate(India.Checklist.Name = Common.Name) %>% dplyr::select(-Common.Name) %>% 
    mutate(Common.Name = eBird.English.Name.2018) %>% 
    dplyr::select(-eBird.English.Name.2018) %>% filter(!is.na(Common.Name))
  
  lists = lists %>% filter(Common.Name %in% listofbirds)
  
  for (i in 1:length(lists$Common.Name))
  {
    if (lists$Long.Term.Status[i] %in% c("Data Deficient") &
        lists$Current.Status[i] %in% c("Data Deficient"))
    {next}
    
    recenttrends = trends %>%
      filter(species %in% lists$Common.Name[i])
    
    recenttrends = stdtrends(recenttrends)
    
    temp = recenttrends
    
    require(extrafont)
    #loadfonts(device = "win")
    
    maxci = temp$nmfreqbyspec + temp$nmsebyspec*1.96
    minci = temp$nmfreqbyspec - temp$nmsebyspec*1.96
    
    liml = min(minci)
    liml = round_any(liml,50,floor)
    
    limu = max(maxci)
    limu = round_any(limu,50,ceiling)
    
    if ((limu-liml) < 100 & liml < 0)
      liml = liml - 50
    if ((limu-liml) < 100 & limu > 0)
      limu = limu + 50
    
    ybreaks = seq(liml,limu,length.out=5)
    
    if (any(ybreaks != 100))
    {
      tmpx = sort((abs(ybreaks-100)))
      tmp = tmpx[1]
      tmp1 = ybreaks - tmp
      tmp2 = ybreaks + tmp
      if (any(tmp1 == 100) & min(tmp1) >= 0)
      {
        ybreaks = tmp1
        liml = round_any(ybreaks[1],50,floor)
      }
      if (min(tmp1) < 0 & any(tmp1 == 100))
      {
        ybreaks = ybreaks + tmpx[2]
        limu = round_any(ybreaks[5],50,ceiling)
        limu = limu + round(0.01*(limu-liml))
      }
      if (any(tmp2 == 100))
      {
        ybreaks = tmp2
        limu = round_any(ybreaks[5],50,ceiling)
        limu = limu + round(0.01*(limu-liml))
      }
      
      ybreaks = round_any(ybreaks,10,round)
      print(as.character(lists$Common.Name[i]))
      print(ybreaks)
    }
    
    ybreaksl = rep("",5)
    
    for (j in 1:5)
    {
      ybreaksl[j] = paste("+",(ybreaks[j]-100),"%",sep="")
      if (ybreaks[j] <= 100)
        ybreaksl[j] = paste((ybreaks[j]-100),"%",sep="")
    }
    
    
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec)) + 
      geom_point(size = 3, colour = scol) +
      geom_line(size = 1.5, colour = scol) +
      geom_hline(yintercept = ybreaks[1], linetype = "dotted", size = 0.7) +
      geom_hline(yintercept = ybreaks[2], linetype = "dotted", size = 0.7) +
      geom_hline(yintercept = ybreaks[3], linetype = "dotted", size = 0.7) +
      geom_hline(yintercept = ybreaks[4], linetype = "dotted", size = 0.7) +
      geom_hline(yintercept = ybreaks[5], linetype = "dotted", size = 0.7) +
      geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                      ymax = (nmfreqbyspec + nmsebyspec*1.96)), fill = scol, colour = NA, alpha = 0.3) +
      xlab("years") +
      ylab("change in abundance index")
    
    
    if (!lists$Long.Term.Status[i] %in% c("Data Deficient"))
    { 
      xbreaks1 = temp$timegroups[1:10]
      lbreaks1 = temp$timegroupsf[1:10]
      lbreaks1[1:4] = c(paste(sprintf('\u2190')," before 2000"),"2000-06","2007-10","2012")
      lbreaks1[c(5,7,9)] = ""
    }
    
    if (lists$Long.Term.Status[i] %in% c("Data Deficient"))
    {
      xbreaks1 = temp$timegroups[1:5]
      lbreaks1 = temp$timegroupsf[1:5]
    }
    
    ggpx = ggp +
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 20, colour = "#56697B", vjust = -4, 
                                       margin = margin(0, 0, 0.8, 0, 'cm')),
            axis.title.y = element_text(size = 22, colour = "#56697B",margin = margin(0, 0.8, 0, 0, 'cm')), 
            axis.ticks.x = element_line(size = 0.7, colour = "#56697B"), 
            axis.ticks.length=unit(.4, "cm"),
            axis.text.y = element_text(size = 25, colour = "#56697B", vjust = -0.4, hjust = 1, 
                                       margin = margin(0, -0.8, 0, 0, 'cm')),
            axis.ticks.y = element_blank(), 
            axis.line.x = element_line(size = 0.7, colour = "#56697B")) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
      theme(text=element_text(family="Gill Sans MT")) +
      scale_x_continuous(breaks = xbreaks1, labels = lbreaks1) +
      scale_y_continuous(breaks = c(ybreaks[1],ybreaks[2],ybreaks[3],ybreaks[4],ybreaks[5]), 
                         limits = c(liml,limu),
                         labels = c(ybreaksl[1],ybreaksl[2],ybreaksl[3],
                                    ybreaksl[4],ybreaksl[5])) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            plot.margin=unit(c(0,0,0,0), "cm"),
            plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
      theme(legend.position = "none")
    
    sps = as.character(lists$India.Checklist.Name[i])
    name = paste(sps,"_","trend.svg",sep="")
    name1 = paste(sps,"_","trend.jpeg",sep="")
    
    ggpx1 = ggpx +
      {if(!lists$Long.Term.Status[i] %in% c("Data Deficient"))labs(tag = paste(sprintf('\u25A0'),
                                                                               " ",sprintf('\u25A0'),
                                                                               " ",sprintf('\u25A0')))}+
      {if(!lists$Long.Term.Status[i] %in% c("Data Deficient"))theme(plot.tag.position = c(0.865, 0.072), 
                                                                    plot.tag = element_text(size = 30, colour = 'white', face = 'bold'))}
    
    ggpx2 = ggpx +
      {if(!lists$Long.Term.Status[i] %in% c("Data Deficient"))labs(tag = paste(sprintf('\u25A0'),
                                                                               " ",sprintf('\u25A0'),
                                                                               " ",sprintf('\u25A0')))}+
      {if(!lists$Long.Term.Status[i] %in% c("Data Deficient"))theme(plot.tag.position = c(0.865, 0.085), 
                                                                    plot.tag = element_text(size = 30, colour = 'white', face = 'bold'))}
    
    print(ggpx2)
    ggsave(file=name, units="in", width=11, height=7, bg = "transparent")
    dev.off()
    
    print(ggpx1)
    ggsave(file=name1, units="in", width=11, height=7, bg = "transparent")
    dev.off()
  }
}


draw.crosshatch = function(data, width, pattern=c("vertical", "horizontal", "crosshatch")) {
  if (!pattern %in% c("vertical", "horizontal", "crosshatch") ) {
    stop(print("Please specify vertical, horizontal or crosshatch pattern"))
    
  }
  # Our function is going to first find the leftmost and rightmost points
  # of the shapefile piece
  rank_long_right <- row_number(-data$long)
  rank_long_left  <- row_number(data$long)
  leftmostpoint   <- row_number(rank_long_left)      # Pretty sure rows 9 and 10 just duplicate; might want to change this to the actual row number
  rightmostpoint  <- row_number(rank_long_right)
  
  # Now it's going to create an index of longitudes "width" apart.  
  # if that's not possible, it returns a 1x3 dataframe of NAs:
  if ((data$long[leftmostpoint == 1]+width) >= (data$long[rightmostpoint == 1]- width)) {
    
    lines           <- as.data.frame(matrix(c(NA, NA, NA, NA), 
                                            ncol=4))
    colnames(lines) <- c("x", "y", "xend", "yend")
    
    return(lines)
    
  } else {
    # If it is possible, it returns a vector of longitudes. These will define
    # the horizontal placement of our vertical lines. 
    index_long <- seq(from = (data$long[leftmostpoint==1]  + width), 
                      to   = (data$long[rightmostpoint==1] - .00001), 
                      by   = width )
    
    # We then need to find the start and end points (in terms of latitude) for each
    # of those vertical lines: 
    
    # For each longitude in the vector, we want to find the points 
    # immediately to the left and right of that longitude on the 
    # top and bottom of the shape.
    
    # We do so by assuming that the "order" variable goes clockwise (which is convention in most shapefiles)
    # We find the first point that is past index_long, then take the point immediately
    # before it in the order.
    # We then use the slope equation to find latitude at the index_long.
    # (We have to do a bit of renumbering of the order to reflect the fact that there's a "start" number;
    # we renumber so that the start number is the left most point.)
    
    find.top.lat <- function(index_longitude) {
      
      past_index_long    <- data$long - index_longitude # (positive values are points to the right of the vertical line)
      past_leftmostpoint <- data$order - data$order[leftmostpoint == 1] 
      neworder           <- ifelse(past_leftmostpoint < 0, 
                                   data$order + (max(data$order) - min(data$order) + 1), 
                                   data$order) # This does the renumbering
      data$neworder                 <- neworder
      neworder[past_index_long < 0] <-NA         # We are uninterested in any points to the left of the vertical line
      data$neworderNA               <- neworder
      rightside                     <- data[which.min(data$neworderNA),]            # Find first point to the right of the vertical line
      leftside                      <- data[data$neworder == (rightside$neworder - 1),]  # Find point immediately preceding it          
      
      # Find correct top latitude using slope formula
      long_lat <- leftside$lat + ((index_longitude - leftside$long) * 
                                    ((leftside$lat    - rightside$lat) /
                                       (leftside$long    - rightside$long)))
      
    }
    
    index_top_lat <- map_dbl(index_long, find.top.lat)
    
    # For bottom line, do the same thing but backwards, measuring from right most point
    find.bottom.lat <- function(index_longitude) {
      
      past_index_long     <- data$long  - index_longitude  # (negative values are points to the left of the vertical line)
      past_rightmostpoint <- data$order - data$order[rightmostpoint == 1]
      neworder            <- ifelse(past_rightmostpoint < 0, 
                                    data$order + (max(data$order) - min(data$order) + 1), 
                                    data$order)
      data$neworder               <- neworder
      neworder[past_index_long>0] <- NA
      data$neworderNA             <- neworder
      leftside                    <- data[which.min(data$neworderNA),]
      rightside                   <- data[data$neworder==(leftside$neworder-1),]
      
      # Find correct bottom latitude using slope formula
      long_lat <- leftside$lat + ((index_longitude - leftside$long) * 
                                    ((leftside$lat    - rightside$lat) /
                                       (leftside$long    - rightside$long)))
    }
    
    index_bottom_lat <- map_dbl(index_long, find.bottom.lat) 
    
    # Output as dataset
    vertical_lines <- as.data.frame(cbind(index_long, index_top_lat, index_long, index_bottom_lat))
    colnames(vertical_lines) <- c("x", "y", "xend", "yend")
    
  } # end of else loop
  
  
  # It's now going to do the same thing to create horizontal lines. 
  # this code is much less commented, to avoid duplication with the code above. 
  rank_lat_top      <- row_number(-data$lat) 
  rank_lat_bottom   <- row_number(data$lat)
  highestpoint      <- row_number(rank_lat_top)
  lowestpoint       <- row_number(rank_lat_bottom)  
  
  # Create an index of latitudes "width" apart.  
  if ((data$lat[lowestpoint == 1]+width) >= (data$lat[highestpoint == 1] - width)) {
    
    lines           <- as.data.frame(matrix(c(NA, NA, NA, NA), 
                                            ncol=4))
    colnames(lines) <- c("x", "y", "xend", "yend")
    return(lines)
    
  } else {
    index_lat <- seq(from = (data$lat[lowestpoint  == 1] + width), 
                     to  = (data$lat[highestpoint == 1] - .000001), 
                     by  = width)  
    
    
    # Find start and end points (in terms of longitudes) for each horizontal line
    
    # Left side:
    
    find.left.long <- function(index_latitude) {
      
      past_index_lat     <- data$lat - index_latitude # (positive values are points above the horizontal line)
      past_lowestpoint   <- data$order - data$order[lowestpoint == 1] 
      neworder           <- ifelse(past_lowestpoint < 0, 
                                   data$order + (max(data$order) - min(data$order) + 1), 
                                   data$order) # This does the renumbering
      
      data$neworder                <- neworder
      neworder[past_index_lat < 0] <-NA         # We are uninterested in any points below the vertical line
      data$neworderNA               <- neworder
      topside                       <- data[which.min(data$neworderNA),]                 # Find first point above vertical line
      bottomside                    <- data[data$neworder == (topside$neworder - 1),]    # Find point immediately below it          
      
      # Find correct top longitude using slope formula
      lat_long <- topside$long +    ((index_latitude - topside$lat) * 
                                       ((topside$long    - bottomside$long) /
                                          (topside$lat    - bottomside$lat)))
      
    }
    
    
    index_left_long <- map_dbl(index_lat, find.left.long)
    
    
    find.right.long <- function(index_latitude) {
      
      past_index_lat      <- data$lat - index_latitude # (negative values are points below the horizontal line)
      past_highestpoint   <- data$order - data$order[highestpoint == 1]
      neworder            <- ifelse(past_highestpoint < 0, 
                                    data$order + (max(data$order) - min(data$order) + 1), 
                                    data$order)
      
      data$neworder              <- neworder
      neworder[past_index_lat>0] <- NA
      data$neworderNA            <- neworder
      bottomside                 <- data[which.min(data$neworderNA),]
      topside                    <- data[data$neworder==(bottomside$neworder-1),]
      
      # Find correct top longitude using slope formula
      lat_long <- topside$long +    ((index_latitude - topside$lat) * 
                                       ((topside$long    - bottomside$long) /
                                          (topside$lat    - bottomside$lat)))
    }
    
    
    index_right_long <- map_dbl(index_lat, find.right.long)
    
    # Output as dataset
    horizontal_lines           <- as.data.frame(cbind(index_left_long, index_lat, index_right_long, index_lat))
    colnames(horizontal_lines) <- c("x", "y", "xend", "yend")
    
  }
  
  if (pattern == "vertical") {
    lines <- vertical_lines 
  }
  if (pattern == "horizontal") {
    lines <- horizontal_lines 
  }
  if (pattern == "crosshatch") {
    lines <- rbind(vertical_lines, horizontal_lines)
  } 
  
  return(lines)
  
}