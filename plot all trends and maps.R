##################### Plot all maps

plotspeciesmaps = function(type = "terrain", listofbirds, back = "black")
{
  require(tidyverse)
  require(raster)
  require(ggsci)
  load("data.RData")
  latlong = data %>% distinct(group.id,LONGITUDE,LATITUDE)
  days = data %>% distinct(group.id,day)
  load("dataforanalyses.RData")
  data = left_join(data,latlong)
  data = left_join(data,days)
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
    
    if (is.na(lists$Range.Status[i]) | lists$Range.Status[i] == "Data Deficient")
    {next}
    
    status = as.character(lists$Migratory.Status[i])
    
    if (status == "R")
    {
      t3 = data %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      t = as.numeric(t3$gridg1)
      
      t2 = unique(nb8g1[[t[1]]])
      for (j in 2:length(t))
      {
        t1 = unique(nb8g1[[t[j]]])
        t2 = union(t2,t1)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchr = T
        dv = d %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      n = setdiff(t2,t)
      n = intersect(n,unique(data$gridg1))
      
      pg1 = pg %>%
        filter(id %in% t)
      pg2 = pg %>%
        filter(id %in% n)
    }
    
    if (status == "LM")
    {
      t3 = data %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      t = as.numeric(t3$gridg1)
      
      t2 = unique(nb8g1[[t[1]]])
      for (j in 2:length(t))
      {
        t1 = unique(nb8g1[[t[j]]])
        t2 = union(t2,t1)
      }
      
      ## summer
      
      t3s = datas %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      ts = as.numeric(t3s$gridg1)
      
      t2s = unique(nb8g1[[ts[1]]])
      for (j in 2:length(ts))
      {
        t1s = unique(nb8g1[[ts[j]]])
        t2s = union(t2s,t1s)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvs = ds %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      ns = setdiff(t2s,t)
      ns = intersect(ns,unique(data$gridg1))
      
      pg1s = pg %>%
        filter(id %in% ts)
      pg2s = pg %>%
        filter(id %in% ns)
      
      ## winter
      
      t3w = dataw %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      tw = as.numeric(t3w$gridg1)
      
      t2w = unique(nb8g1[[tw[1]]])
      for (j in 2:length(tw))
      {
        t1w = unique(nb8g1[[tw[j]]])
        t2w = union(t2w,t1w)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvw = dw %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      nw = setdiff(t2w,t)
      nw = intersect(nw,unique(data$gridg1))
      
      pg1w = pg %>%
        filter(id %in% tw)
      pg2w = pg %>%
        filter(id %in% nw)
      
      ## passage
      
      t3p = datap %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      tp = as.numeric(t3p$gridg1)
      
      t2p = unique(nb8g1[[tp[1]]])
      for (j in 2:length(tp))
      {
        t1p = unique(nb8g1[[tp[j]]])
        t2p = union(t2p,t1p)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvp = dp %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      np = setdiff(t2p,t)
      np = intersect(np,unique(data$gridg1))
      
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
        dplyr::select(gridg1)
      
      t = as.numeric(t3$gridg1)
      
      t2 = unique(nb8g1[[t[1]]])
      for (j in 2:length(t))
      {
        t1 = unique(nb8g1[[t[j]]])
        t2 = union(t2,t1)
      }
      
      ## summer
      
      t3s = datas %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      ts = as.numeric(t3s$gridg1)
      
      t2s = unique(nb8g1[[ts[1]]])
      for (j in 2:length(ts))
      {
        t1s = unique(nb8g1[[ts[j]]])
        t2s = union(t2s,t1s)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvs = ds %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      ns = setdiff(t2s,t)
      ns = intersect(ns,unique(data$gridg1))
      
      pg1s = pg %>%
        filter(id %in% ts)
      pg2s = pg %>%
        filter(id %in% ns)
      
      ## winter
      
      t3w = dataw %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      tw = as.numeric(t3w$gridg1)
      
      if (length(ts > 1))
      {
        t2w = unique(nb8g1[[tw[1]]])
        for (j in 2:length(tw))
        {
          t1w = unique(nb8g1[[tw[j]]])
          t2w = union(t2w,t1w)
        }
        
        if (lists$Common.Name[i] %in% vaglist)
        {
          switchm = T
          dvw = dw %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
            filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
        }
        
        nw = setdiff(t2w,t)
        nw = intersect(nw,unique(data$gridg1))
        
        pg1w = pg %>%
          filter(id %in% tw)
        pg2w = pg %>%
          filter(id %in% nw)
      }
      
      ## passage
      
      t3p = datap %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      tp = as.numeric(t3p$gridg1)
      
      t2p = unique(nb8g1[[tp[1]]])
      for (j in 2:length(tp))
      {
        t1p = unique(nb8g1[[tp[j]]])
        t2p = union(t2p,t1p)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvp = dp %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      np = setdiff(t2p,t)
      np = intersect(np,unique(data$gridg1))
      
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
        dplyr::select(gridg1)
      
      t = as.numeric(t3$gridg1)
      
      t2 = unique(nb8g1[[t[1]]])
      for (j in 2:length(t))
      {
        t1 = unique(nb8g1[[t[j]]])
        t2 = union(t2,t1)
      }
      
      ## summer
      
      t3s = datas %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      ts = as.numeric(t3s$gridg1)
      
      if (length(ts > 1))
      {
        t2s = unique(nb8g1[[ts[1]]])
        for (j in 2:length(ts))
        {
          t1s = unique(nb8g1[[ts[j]]])
          t2s = union(t2s,t1s)
        }
        
        if (lists$Common.Name[i] %in% vaglist)
        {
          switchm = T
          dvs = ds %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
            filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
        }
        
        ns = setdiff(t2s,t)
        ns = intersect(ns,unique(data$gridg1))
        
        pg1s = pg %>%
          filter(id %in% ts)
        pg2s = pg %>%
          filter(id %in% ns)
      }
      
  
      
      ## winter
      
      t3w = dataw %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      tw = as.numeric(t3w$gridg1)
      
      t2w = unique(nb8g1[[tw[1]]])
      for (j in 2:length(tw))
      {
        t1w = unique(nb8g1[[tw[j]]])
        t2w = union(t2w,t1w)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvw = dw %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      nw = setdiff(t2w,t)
      nw = intersect(nw,unique(data$gridg1))
      
      pg1w = pg %>%
        filter(id %in% tw)
      pg2w = pg %>%
        filter(id %in% nw)
      
      ## passage
      
      t3p = datap %>%
        filter(COMMON.NAME == lists$Common.Name[i]) %>%
        dplyr::select(gridg1)
      
      tp = as.numeric(t3p$gridg1)
      
      t2p = unique(nb8g1[[tp[1]]])
      for (j in 2:length(tp))
      {
        t1p = unique(nb8g1[[tp[j]]])
        t2p = union(t2p,t1p)
      }
      
      if (lists$Common.Name[i] %in% vaglist)
      {
        switchm = T
        dvp = dp %>% filter(COMMON.NAME == lists$Common.Name[i]) %>%
          filter(!gridg1 %in% unique(t2)) %>% distinct(LONGITUDE,LATITUDE)
      }
      
      np = setdiff(t2p,t)
      np = intersect(np,unique(data$gridg1))
      
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
            plot.title = element_text(hjust = 0.5),
            plot.background = element_rect(fill = "transparent",colour = NA),
            panel.background = element_rect(fill = "transparent",colour = NA))+
      theme(legend.position = "none")+
      coord_quickmap()
      
    
    sps = as.character(lists$India.Checklist.Name[i])
    name = paste(sps,"_",type,".png",sep="")
  
    print(ggp)
    ggsave(file=name, units="in", width=10, height=7, bg = back)
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
      }
      if (any(tmp2 == 100))
      {
        ybreaks = tmp2
        limu = round_any(ybreaks[5],50,ceiling)
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
      geom_hline(yintercept = ybreaks[1], linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = ybreaks[2], linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = ybreaks[3], linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = ybreaks[4], linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = ybreaks[5], linetype = "dotted", size = 0.5) +
      geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                      ymax = (nmfreqbyspec + nmsebyspec*1.96)), fill = scol, colour = NA, alpha = 0.3) +
      xlab("years") +
      ylab("change in frequency of reporting")
    
    
    if (!lists$Long.Term.Status[i] %in% c("Data Deficient"))
    { 
      xbreaks1 = temp$timegroups[1:10]
      lbreaks1 = temp$timegroupsf[1:10]
      lbreaks1[c(5,7,9)] = ""
    }
    
    if (lists$Long.Term.Status[i] %in% c("Data Deficient"))
    {
      xbreaks1 = temp$timegroups[1:5]
      lbreaks1 = temp$timegroupsf[1:5]
    }
    
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
    name = paste(sps,"_","trend","_",scol,".svg",sep="")
    
    print(ggpx)
    ggsave(file=name, units="in", width=11, height=7, bg = "transparent")
    dev.off()
  }
}
