####################################################################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "ebd_IN_relAug-2018.txt")
{
  require(lubridate)
  require(tidyverse)
  
  #library(auk)
  
  #allin = system.file("extdata/ebd_IN_relMay-2018.txt", package = "auk")
  
  #allout = tempfile()
  #auk_clean(allin,allout, sep = "\t", remove_text = FALSE)
  
  #all = allout %>%
  #  read_ebd()
  
  preimp = c("COMMON.NAME","OBSERVATION.COUNT",
             "LOCALITY.ID","LOCALITY.TYPE",
             "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
             "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER","APPROVED","CATEGORY")
  
  nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
  nms = names(nms)
  nms[!(nms %in% preimp)] = "NULL"
  nms[nms %in% preimp] = NA
  
  data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
  
  ## choosing important variables
  
  imp = c("COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID","LOCALITY.TYPE",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
          "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","group.id")
  
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  ## setup eBird data ##
  
  ## filter approved observations, species, slice by single group ID, remove repetitions
  ## remove repeats
  ## set date, add month, year and day columns using package LUBRIDATE
  ## add number of species column (no.sp)
  
  data = data %>%
    filter(APPROVED == 1) %>%
    mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
    group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
    dplyr::select(imp) %>%
    mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           month = month(OBSERVATION.DATE), year = year(OBSERVATION.DATE),
           day = day(OBSERVATION.DATE) + cdays[month], week = week(OBSERVATION.DATE),
           fort = ceiling(day/14)) %>%
    group_by(group.id) %>% mutate(no.sp = n_distinct(COMMON.NAME)) %>%
    ungroup
  
  
  temp = data %>%
    group_by(COMMON.NAME) %>% slice(1) %>% ungroup()
  
  write.csv(temp,"indiaspecieslist.csv")
  
  assign("data",data,.GlobalEnv)
  rm(readcleanrawdata, pos = ".GlobalEnv")
  
  save.image("data.RData")
}


##########################################################################################


## requires shapefiles and several packages - path1 = India; path2 = India States; path3 = India Districts
## provide path to folder and name of file within

## this can be edited for more flexibility with grid sizes; current default is 20,40,60,80,320

## current default args are c("India","India_2011","India States","IndiaStates_2011","India Districts","IndiaDistricts_2011")

## saves a workspace image called "maps.RData"

createmaps = function(g1=20,g2=40,g3=60,g4=80,g5=320,path1="India",name1="India_2011",path2="India States",name2="IndiaStates_2011",
                      path3="India Districts",name3="IndiaDistricts_2011")
{
  require(tidyverse)
  require(rgdal)
  require(sp)
  
  
  # reading maps
  
  assign("indiamap",readOGR(path1,name1),.GlobalEnv)
  assign("statemap",readOGR(path2,name2),.GlobalEnv)
  assign("districtmap",readOGR(path3,name3),.GlobalEnv)
  
  # creating SPDF grids below that can be intersected with various maps and overlaid on to data
  
  bb = bbox(indiamap) # creates a box with extents from map
  cs = c(g1*1000/111111,g1*1000/111111)  # cell size g1 km x g1 km
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd) # create required grids
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd))) # create spatial grid data frame
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame") # SGDF to SPDF
  assign("gridmapg1",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g2*1000/111111,g2*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg2",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g3*1000/111111,g3*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg3",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g4*1000/111111,g4*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg4",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g5*1000/111111,g5*1000/111111)  # cell size 320km x 320km
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("gridmapg5",sp_grd_poly,.GlobalEnv)
  
  
  #indiamap = spTransform(indiamap,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  # not required here, CRS is NA
  
  assign("gridlevels",c(g1,g2,g3,g4,g5),.GlobalEnv)
  
  rm(createmaps, pos = ".GlobalEnv")
  
  save.image("maps.RData")
}


###########################################################################################

## to create a mask for any readOGR vector without transformation
## a grid appears to be required to do this cleanly

## returns two fortified SPDFs that can easily be used in ggplot

createmask = function(grid=27,path1="India",name1="India_2011")
{
  require(tidyverse)
  require(ggfortify)
  require(sp)
  require(rgdal)
  require(raster)
  
  extentmap = readOGR(path1,name1)
  
  # creating SPDF grids below to create a mask
  
  bb = bbox(extentmap)
  cs = c((grid*1000)/111111,(grid*1000)/111111)  # cell size grid*grid
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  mask = sp_grd_poly - extentmap # SPDF that excludes India map
  
  assign("mask",fortify(mask),.GlobalEnv)
  assign("border",fortify(extentmap),.GlobalEnv)
  
  rm(createmask, pos = ".GlobalEnv")
  
  save.image("mask.RData")
}


######################################################################################


## prepare data for spatial analyses, add map variables, grids
## place the 'maps' workspace in working directory

addmapvars = function(datapath = "data.RData", mappath = "maps.RData")
{
  require(tidyverse)
  require(data.table)
  require(sp)
  require(rgeos)
  
  load(datapath)
  
  ## add map details to eBird data
  
  load(mappath)
  
  # add columns with DISTRICT and ST_NM to main data 
  
  temp = data %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 
  
  rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
  coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
  #proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
  temp = data.frame(temp) # convert into data frame for left_join
  temp$group.id = rownames(temp) # add column to join with the main data
  data = left_join(temp,data)
  
  
  # add columns with GRID ATTRIBUTES to main data
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg1)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg1"
  data$gridg1 = as.factor(data$gridg1)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg2)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg2"
  data$gridg2 = as.factor(data$gridg2)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg3)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg3"
  data$gridg3 = as.factor(data$gridg3)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg4)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg4"
  data$gridg4 = as.factor(data$gridg4)
  
  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg5)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg5"
  data$gridg5 = as.factor(data$gridg5)
  
  ## 
  
  ## move personal locations to nearest hotspots
  
  allhotspots = data %>% filter(LOCALITY.TYPE == "H") %>%
    group_by(LOCALITY.ID) %>% summarize(max(LATITUDE),max(LONGITUDE)) %>% # get lat long for each hotspot
    ungroup
  
  names(allhotspots)[c(2,3)] = c("LATITUDE","LONGITUDE")
  
  # using DATA TABLES below
  
  setDT(data) # useful for extremely large data frames as each object will then be modified in place without creating 
  # a copy, creates a 'reference' apparently; this will considerably reduce RAM usage 
  hotspots = as.matrix(allhotspots[, 3:2])
  
  # again 'refernce' based, choosing nearest hotspots to each location
  data[, LOCALITY.HOTSPOT := allhotspots[which.min(spDists(x = hotspots, y = cbind(LONGITUDE, LATITUDE))),]$LOCALITY.ID, by=.(LONGITUDE, LATITUDE)] 
  data = as.data.frame(data) # back into dataframe
  
  assign("data",data,.GlobalEnv)
  assign("gridlevels",gridlevels,.GlobalEnv)
  rm(addmapvars, pos = ".GlobalEnv")
  
  save.image("dataforspatialanalyses.RData")
}


#######################################################################################


## a script to plot frequencies on a map with relative ease
## input can be trivial frequencies or modeled output
## data is unsummarized data
## resolution can take 7 values; "state","district","g1","g2","g3","g4","g5"
## species common name
## path can be true or false; this is for boundaries

plotfreqmap = function(data, species, resolution, rich = F, smooth = F, h = 2, cutoff = 5, showempty = T,
                       mappath = "maps.RData", maskpath = "mask.RData")
{
  require(tidyverse)
  require(ggfortify)
  require(viridis)
  require(RColorBrewer)
  require(scales)
  
  load(mappath)
  
  plotindiamap = ggplot() +
    geom_polygon(data = fortify(statemap), aes(x=long, y=lat, group=group), colour = 'black', fill = "white")+  
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
  
  if (!isTRUE(rich))
  {
    data = data %>%
      filter(year >=2013, ALL.SPECIES.REPORTED == 1)
    data1 = data %>% filter(COMMON.NAME == species)
  }
  
  if (isTRUE(rich))
  {
    if (resolution == "state")
    {

      ct = datar %>%
        group_by(DISTRICT) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(ST_NM) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(ST_NM,lists)
      
      fortified = fortify(statemap, region = c("ST_NM"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$ST_NM))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "ST_NM"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "ST_NM"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "district")
    {
      ct = datar %>%
        group_by(DISTRICT) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(DISTRICT) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(DISTRICT,lists)
      
      fortified = fortify(districtmap, region = c("DISTRICT"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$DISTRICT))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "DISTRICT"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "DISTRICT"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "g1")
    {
      datar = data %>%
        group_by(gridg1,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      ct = datar %>%
        group_by(gridg1) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(gridg1) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(gridg1) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg1,lists)
      
      fortified = fortify(gridmapg1, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg1))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg1"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg1"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg1" = 'id'))
      
      datar = datar %>%
        group_by(gridg1) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g2")
    {
      datar = data %>%
        group_by(gridg2,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      ct = datar %>%
        group_by(gridg2) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(gridg2) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(gridg2) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg2,lists)
      
      fortified = fortify(gridmapg2, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg2))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg2"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg2"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg2" = 'id'))
      
      datar = datar %>%
        group_by(gridg2) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g3")
    {
      datar = data %>%
        group_by(gridg3,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      ct = datar %>%
        group_by(gridg3) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(gridg3) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(gridg3) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg3,lists)
      
      fortified = fortify(gridmapg3, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg3))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg3"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg3"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg3" = 'id'))
      
      datar = datar %>%
        group_by(gridg3) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g4")
    {
      datar = data %>%
        group_by(gridg4,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      ct = datar %>%
        group_by(gridg4) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(gridg4) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(gridg4) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg4,lists)
      
      fortified = fortify(gridmapg4, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg4))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg4"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg4"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg4" = 'id'))
      
      datar = datar %>%
        group_by(gridg4) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g5")
    {
      datar = data %>%
        group_by(gridg5,COMMON.NAME) %>% slice(1) %>% ungroup()
      
      ct = datar %>%
        group_by(gridg5) %>% summarize(rich = n_distinct(COMMON.NAME))
      min = min(ct$rich)
      max = max(ct$rich)
      
      temp = data %>% 
        group_by(gridg5) %>%
        summarize(freq = n_distinct(COMMON.NAME))
      
      filled = data %>%
        group_by(gridg5) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg5,lists)
      
      fortified = fortify(gridmapg5, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg5))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg5"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg5"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat),maxlat = max(lat)) %>% ungroup()
      
      datar = left_join(datar,mmplotdf,by = c("gridg5" = 'id'))
      
      datar = datar %>%
        group_by(gridg5) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
  }
  
  if (!isTRUE(rich))
  {
    if (resolution == "state")
    {
      temp = data %>% 
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(ST_NM) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(ST_NM,lists)
      
      fortified = fortify(statemap, region = c("ST_NM"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$ST_NM))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }

      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "ST_NM"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "ST_NM"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "district")
    {
      temp = data %>% 
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(DISTRICT) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(DISTRICT,lists)
      
      fortified = fortify(districtmap, region = c("DISTRICT"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$DISTRICT))
      
      if(length(zlists > 0))
      {
        empty = data.frame(unique(zlists),0)
        names(empty) = names(filled)
      }
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "DISTRICT"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "DISTRICT"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
    }
    
    if (resolution == "g1")
    {
      temp = data %>% 
        group_by(gridg1) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(gridg1) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg1) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg1,lists)
      
      fortified = fortify(gridmapg1, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg1))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg1"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg1"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg1) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg1" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg1) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g2")
    {
      temp = data %>% 
        group_by(gridg2) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(gridg2) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg2) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg2,lists)
      
      fortified = fortify(gridmapg2, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg2))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg2"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg2"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg2) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg2" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg2) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g3")
    {
      temp = data %>% 
        group_by(gridg3) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(gridg3) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg3) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg3,lists)
      
      fortified = fortify(gridmapg3, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg3))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg3"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg3"))) # SPDF to plot
        switch = T
      }else{
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg3) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg3" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg3) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g4")
    {
      temp = data %>% 
        group_by(gridg4) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(gridg4) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg4) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg4,lists)
      
      fortified = fortify(gridmapg4, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg4))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg4"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg4"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg4) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg4" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg4) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
    
    if (resolution == "g5")
    {
      temp = data %>% 
        group_by(gridg5) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species, lists >= cutoff) %>%
        group_by(gridg5) %>%
        summarize(freq = n()/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(gridg5) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(gridg5,lists)
      
      fortified = fortify(gridmapg5, region = c("id"))
      fortind = fortify(indiamap)
      mnlo = min(fortind$long)
      mnla = min(fortind$lat)
      mxlo = max(fortind$long)
      mxla = max(fortind$lat)
      
      fort1 = fortified %>%
        group_by(id) %>% filter(all(long >= mnlo),all(lat >= mnla),all(long <= mxlo),all(lat <= mxla))
      
      zlists = setdiff(unique(fort1$id),unique(filled$gridg5))
      empty = data.frame(unique(zlists),0)
      names(empty) = names(filled)
      
      fortified$id = as.factor(fortified$id)
      fort1$id = as.factor(fort1$id)
      
      plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg5"))) # SPDF to plot
      
      if(length(zlists > 0))
      {
        emptydf = na.omit(left_join(fort1,empty, by = c('id' = "gridg5"))) # SPDF to plot
        switch = T
      }
      else
      {
        switch = F
      }
      
      data1 = data1 %>%
        group_by(gridg5) %>% slice(1) %>% ungroup()
      
      mmplotdf = plotdf %>%
        group_by(id) %>% summarize(minlong = min(long), maxlong = max(long), minlat = min(lat), maxlat = max(lat), freq = max(freq)) %>% ungroup()
      
      data1 = left_join(data1,mmplotdf,by = c("gridg5" = 'id'))
      roundUp = function(x) 10^ceiling(log10(x))
      data1$freq = round(data1$freq*roundUp(1/median(na.omit(data1$freq)))*10)
      data1 = data1 %>% filter(freq > 0)
      
      
      data1 = data1[rep(row.names(data1), data1$freq),]
      
      
      data1 = data1 %>%
        group_by(gridg5) %>% mutate(LONGITUDE = cbind(runif(n(),max(minlong),
                                                            max(maxlong)),runif(n(),
                                                                                max(minlat),
                                                                                max(maxlat)))[,1],
                                    LATITUDE = cbind(runif(n(),max(minlong),
                                                           max(maxlong)),runif(n(),
                                                                               max(minlat),
                                                                               max(maxlat)))[,2])
    }
  }
  
  if(resolution != "state" & resolution != "district"){load(maskpath)}

  if(isTRUE(smooth) & resolution != "state" & resolution != "district")
  {
    stats = plotindiamap +
    {if(!isTRUE(rich))stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(rich)stat_density2d(data = datar, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
      scale_fill_gradient2(low = muted("blue"),
                           high = "white", space = "Lab", na.value = "grey50", trans = 'reverse')
  }

      
  if(smooth){
    minstat = min(ggplot_build(stats)$data[[2]]$level)
    maxstat = max(ggplot_build(stats)$data[[2]]$level)
  }else{
    minstat = min
    maxstat = max
  }
  
    
  for (i in c(5,4,3,2,1))
  {
    breaks = seq(minstat,maxstat,length.out=i)
    labels = round(seq(min,max,length.out=i),2)
    if(rich){labels = round(seq(min,max,length.out=i))}
    if (length(unique(labels)) == i)
      break
  }
      
  cols = "grey30"
  cl = paste(" <",cutoff,"lists")
  names(cols) = cl
  
  plot = plotindiamap +
    {if(smooth & resolution != "state" & resolution != "district" & !isTRUE(rich))stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(smooth & resolution != "state" & resolution != "district" & rich)stat_density2d(data = datar, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(!isTRUE(smooth))geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq))} +
    {if(switch & showempty)geom_polygon(data = emptydf, aes(x = long, y = lat, group = group, col = cl), fill = "grey30")} +
    {if(resolution != "state" & resolution != "district")geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white')}+
    geom_path(data = fortify(statemap), aes(x = long, y = lat, group = group), col = 'black', size = 1) +
    scale_fill_gradient2(low = muted("blue"),
                         high = "white", space = "Lab", na.value = "grey50", trans = 'reverse',
                         breaks = breaks, labels = labels) +
    {if(switch)scale_colour_manual(values = cols)} +
    theme(legend.justification=c(1,1), legend.position=c(0.99,0.99)) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    {if(!isTRUE(rich))ggtitle(species)} +
    {if(!isTRUE(rich))theme(plot.title = element_text(hjust = 0.5, vjust = 0.1, size = 20))} +
    guides(fill = guide_legend(reverse = TRUE))
  
  return(plot)
}


########################################################################################


## ensure that the working directory has list of India's birds with scientific names 
## (just a safety mechanism for the function to work for small subsets, needs to be enabled if required)
## only need to input data, the species of interest and the complete list of India's bird species
## also groupspecs if required (a dataframe with all relevant list level info), it is defaulted to data

expandbyspecies = function(data, species)
{
  require(tidyverse)
  
  data = data %>%
    mutate(timegroups = as.character(year)) %>%
    mutate(timegroups = ifelse(year < 1990, "before 1990", timegroups)) %>%
    mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
    mutate(timegroups = ifelse(year > 1999 & year <= 2005, "2000-2005", timegroups)) %>%
    mutate(timegroups = ifelse(year > 2005 & year <= 2010, "2006-2010", timegroups)) %>%
    mutate(timegroups = ifelse(year > 2010 & year <= 2013, "2011-2013", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2014, "2014", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2015, "2015", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2016, "2016", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2017, "2017", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2018, "2018", timegroups))
  
  data$timegroups = as.factor(data$timegroups)
  
  ## considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1,gridg2,gridg3,gridg4,gridg5,DISTRICT,ST_NM,
             LOCALITY.ID,LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
             OBSERVER.ID,PROTOCOL.TYPE,DURATION.MINUTES,EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,ALL.SPECIES.REPORTED,
             group.id,month,year,day,week,fort,LOCALITY.HOTSPOT,no.sp,timegroups)
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  
  ## deal with NAs
  
  expanded = expanded %>% mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, is.na(OBSERVATION.COUNT), "0"))
  
  
  expanded = expanded %>%
    mutate(OBSERVATION.NUMBER=OBSERVATION.COUNT,
           OBSERVATION.COUNT=replace(OBSERVATION.COUNT, OBSERVATION.COUNT != "0", "1"))
  
  
  
  expanded$OBSERVATION.COUNT = as.numeric(expanded$OBSERVATION.COUNT)
  expanded$OBSERVATION.NUMBER = as.numeric(expanded$OBSERVATION.NUMBER)
  
  return(expanded)
}


#######################################################################################


## to output a dataframe that compares 'abundances' from summaries and models
## tempres can be "fortnight", "month", "none"
## spaceres can be 40 and 80 km ("g2","g4","none")
## returns 4 values or 4 x 10 values for trends
## the type/scale of analyses - "all","g1","g2","g3","g4","g5","model"

freqtrends = function(data,species,tempres="none",spaceres="none",
                      trends=F,analysis="model",count=F)
{
  require(tidyverse)
  require(lme4)
  
  if (isTRUE(trends))
  {
    data = data %>%
      mutate(timegroups = as.character(year)) %>%
      mutate(timegroups = ifelse(year < 1990, "before 1990", timegroups)) %>%
      mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
      mutate(timegroups = ifelse(year > 1999 & year <= 2005, "2000-2005", timegroups)) %>%
      mutate(timegroups = ifelse(year > 2005 & year <= 2010, "2006-2010", timegroups)) %>%
      mutate(timegroups = ifelse(year > 2010 & year <= 2013, "2011-2013", timegroups)) %>%
      mutate(timegroups = ifelse(year == 2014, "2014", timegroups)) %>%
      mutate(timegroups = ifelse(year == 2015, "2015", timegroups)) %>%
      mutate(timegroups = ifelse(year == 2016, "2016", timegroups)) %>%
      mutate(timegroups = ifelse(year == 2017, "2017", timegroups)) %>%
      mutate(timegroups = ifelse(year == 2018, "2018", timegroups))
    
    data$timegroups = as.factor(data$timegroups)
  }

  ## considers only complete lists
  
  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)
  
  if (tempres == "fortnight" & spaceres == "g2")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg2,fort)
  }
  
  if (tempres == "fortnight" & spaceres == "g4")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg4,fort)
  }
  
  if (tempres == "fortnight" & spaceres == "none")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(fort)
  }
  
  if (tempres == "month" & spaceres == "g2")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg2,month)
  }
  
  if (tempres == "month" & spaceres == "g4")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg4,month)
  }
  
  if (tempres == "month" & spaceres == "none")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(month)
  }
  
  if (tempres == "none" & spaceres == "g2")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg2)
  }
  
  if (tempres == "none" & spaceres == "g4")
  {
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg4)
  }
  
  fl = 0
  
  if (tempres == "none" & spaceres == "none")
  {
    fl = 1
  }
  
  ## filter only those combinations
  
  if (fl == 0)
  {
    data = temp %>% left_join(data)
  }
  
  data2 = data %>%
    filter(year>(as.integer(format(Sys.Date(), "%Y"))-5))
  
  ## overall for country
  
  if (analysis == "all")
  {
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        f = data2 %>% 
          mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          summarize(freq = n()/max(lists))
      }
      if (isTRUE(count))
      {
        f = data2 %>%
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          summarize(freq = sum(OBSERVATION.COUNT)/max(lists))
      }
    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        f = data %>% 
          group_by(timegroups) %>%
          mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups) %>% summarize(freq = n()/max(lists)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        f = data %>%
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(timegroups) %>%
          mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups) %>%   summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
    }
  }
  
  
  ## averaged across g1
  
  if (analysis == "g1")
  {
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data2 %>% 
          group_by(gridg1) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg1) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
      if (isTRUE(count))
      {
        temp = data2 %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(gridg1) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg1) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }

    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data %>% 
          group_by(timegroups,gridg1) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg1) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        temp = data %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(timegroups,gridg1) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg1) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
    }
  }
  
  ## averaged across g2
  
  if (analysis == "g2")
  {
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data2 %>% 
          group_by(gridg2) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg2) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
      if (isTRUE(count))
      {
        temp = data2 %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(gridg2) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg2) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
      
    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data %>% 
          group_by(timegroups,gridg2) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg2) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        temp = data %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(timegroups,gridg2) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg2) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
    }
  }
  
  ## averaged across g3
  
  if (analysis == "g3")
  {
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data2 %>% 
          group_by(gridg3) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg3) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
      if (isTRUE(count))
      {
        temp = data2 %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(gridg3) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg3) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data %>% 
          group_by(timegroups,gridg3) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg3) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        temp = data %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(timegroups,gridg3) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg3) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
    }
  }
  
  ## averaged across g4
  
  if (analysis == "g4")
  {
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data2 %>% 
          group_by(gridg4) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg4) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
      if (isTRUE(count))
      {
        temp = data2 %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(gridg4) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg4) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data %>% 
          group_by(timegroups,gridg4) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg4) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        temp = data %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(timegroups,gridg4) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg4) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
    }
  }
  
  ## averaged across g5
  
  if (analysis == "g5")
  {
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data2 %>% 
          group_by(gridg5) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg5) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
      if (isTRUE(count))
      {
        temp = data2 %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(gridg5) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(gridg5) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          summarize(freq = mean(freq))
      }
    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        temp = data %>% 
          group_by(timegroups,gridg5) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg5) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        temp = data %>% 
          mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
          group_by(timegroups,gridg5) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
          filter(COMMON.NAME == species) %>%
          group_by(timegroups,gridg5) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
          ungroup()
        
        f = temp %>%
          group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
        
        f$timegroups = as.character(f$timegroups)
      }
    }
  }
  
  if (analysis == "model")
  {
    if (!isTRUE(trends))
    {
      data1 = data
    }
    
    if (isTRUE(trends))
    {
      data1 = data %>% select(-timegroups)
    }
    

    ## expand data for models
    
    #if (is.na(exd))
    #{
    ed = expandbyspecies(data1,species)
    #}
    
    #if (!is.na(exd))
    #{
    #  ed = exd
    #}
    
    ## model 1
    
    if (!isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        ed = ed %>%
          filter(year>(as.integer(format(Sys.Date(), "%Y"))-5))
        
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|ST_NM/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, family=binomial(link = 'cloglog'), nAGQ = 0)
        
        f = predict(m1, data.frame(
          no.sp = 20),
          type="response", re.form = NA)
      }
      if (isTRUE(count))
      {
        require(glmmTMB)
        
        ed = ed %>%
          filter(year>(as.integer(format(Sys.Date(), "%Y"))-5)) %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
          
        m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                     log(no.sp) + (1|ST_NM/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                     family=poisson)
        
        newdata = ed %>%
          distinct(ST_NM,LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        
        fd = predict(m1, newdata = newdata,
          type="response", allow.new.levels = T)
        newdata$f = fd
        
        fx = newdata %>%
          group_by(ST_NM) %>% summarize(f = mean(f)) %>% ungroup() %>%
          summarize(f = mean(f))
        
        f = fx$f
      }
    }
    
    if (isTRUE(trends))
    {
      if (!isTRUE(count))
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|ST_NM/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, family=binomial(link = 'cloglog'), nAGQ = 0)
        
        f = data.frame(unique(data$timegroups))
        names(f) = "timegroups"
        f$freq = predict(m1, data.frame(timegroups = f$timegroups,
                                        no.sp = 20),
                         type="response", re.form = NA)
        f$timegroups = as.character(f$timegroups)
      }
      if (isTRUE(count))
      {
        ed = ed %>%
          group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
          
        m1 = glmer(OBSERVATION.NUMBER ~ 
                     log(no.sp) + timegroups + (1|ST_NM/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, family=poisson(link = 'log'), nAGQ = 0)
        
        f = data.frame(unique(data$timegroups))
        names(f) = "timegroups"
        f$freq = predict(m1, data.frame(timegroups = f$timegroups,
                                        no.sp = 20),
                         type="response", re.form = NA)
        f$timegroups = as.character(f$timegroups)
      }
    }
    
    ## model 2
    
    #gc()
    
    #if (!isTRUE(trends))
    #{
    #  m2 = glmer(OBSERVATION.COUNT ~ 
    #               log(no.sp) + (1|gridg5/gridg3/gridg1) + (1|OBSERVER.ID), data = ed, family=binomial(link = 'cloglog'), nAGQ = 0)
    
    #  f6 = predict(m2, data.frame(
    #    no.sp = 20),
    #    type="response", re.form = NA)
    #}
    
    #if (isTRUE(trends))
    #{
    #  m2 = glmer(OBSERVATION.COUNT ~ 
    #               log(no.sp) + timegroups + (1|gridg5/gridg3/gridg1) + (1|OBSERVER.ID), data = ed, family=binomial(link = 'cloglog'), nAGQ = 0)
    #  f6 = data.frame(unique(data$timegroups))
    #  names(f6) = "timegroups"
    #  f6$freq = predict(m2, data.frame(timegroups = f5$timegroups,
    #                                   no.sp = 20),
    #                    type="response", re.form = NA)
    #  f6$timegroups = as.character(f6$timegroups)
    #}
  }
  
  
  
  if (!isTRUE(trends))
  {
    f1 = data.frame(method = analysis)
    
    if (analysis != "model")
    {
      f1$freq = f$freq
    }
    
    if (analysis == "model")
    {
      f1$freq = f
    }
    
  }
  
  if (isTRUE(trends))
  {
    l = length(unique(data$timegroups))
    f1 = data.frame(method = rep(analysis,l)
                    #,rep("modelgrids",l)
    )
    f1$timegroups = f$timegroups
    
    f1$freq = f$freq
    
    f1$timegroups = factor(f1$timegroups, levels = c("before 1990","1990-1999","2000-2005","2006-2010",
                                                     "2011-2013","2014","2015","2016","2017","2018"))
    f1 = f1[order(f1$method,f1$timegroups),]
    f1$species = species
  }
  
  return(f1)
}




########################################################################################

################## to plot either a comparison of species trends or methods #############
## trends can be recent or historical
## input a list of species or a single species
## input a method
## returns a ggplot object
## MAXIMUM of 8 species



plottrends = function(trends,type = "method",singlespecies = NA,selectspecies = NA,smethod = "model1",recent = F)
{
  modtrends = trends
  
  cols = c("#009E73", "#0072B2", "#D55E00", "#E69F00", "#56B4E9", "#CC79A7", "#999999", "#F0E442")
  
  bksm = c("overall","g5","model1","model2","model3","model4")
  lbsm = c("country average","320 km grid average","model 1","model 2","model 3","model 4")
  nm = length(unique(trends$method))
  
  if (isTRUE(recent))
  {
    tg = c("2014","2015","2016","2017","2018")
  }else
  {
    tg = unique(trends$timegroups)
  }
  
  
  
  ################# by species ########################
  
  if (type == "species")
  {
    modtrends = trends %>%
      filter(method == smethod)
    
    cols1 = cols[1:ns]
    bks1 = selectspecies
    lbs1 = selectspecies
    
    recenttrends = modtrends %>%
      filter(timegroups %in% tg) %>%
      filter(species %in% selectspecies) %>%
      group_by(species) %>% mutate(freq1 = freq[1]) %>% ungroup() %>%
      group_by(species) %>% mutate(nmfreqbyspec = freq/freq1) %>% ungroup() %>%
      mutate(nmfreq = freq/max(freq1))
    
    recenttrends$species = factor(recenttrends$species, levels = selectspecies)
    
    temp = recenttrends
    
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
      geom_point(size = 3) +
      geom_line(aes(group = species),size = 1.5) +
      xlab("years") +
      ylab("frequency of reporting")
    
    ggp1 = ggp +
      theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
            axis.title.y = element_text(angle = 90, size = 18), axis.text.y = element_text(size = 16)) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 16)) +
      scale_colour_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_y_continuous(limits = c(round(min(temp$nmfreqbyspec),1)-0.1,round(max(temp$nmfreqbyspec),1)+0.1))
  }
  
  
  ################# by method ########################
  
  
  if (type == "method")
  {
    cols1 = cols[1:nm]
    bks1 = bksm[1:nm]
    lbs1 = lbsm[1:nm]
    
    recenttrends = modtrends %>%
      filter(timegroups %in% tg) %>%
      group_by(method,species) %>% mutate(freq1 = freq[1]) %>% ungroup() %>%
      group_by(method,species) %>% mutate(nmfreqbyspecmeth = freq/freq1) %>% ungroup() %>%
      group_by(species) %>% mutate(freq2 = freq[1]) %>% ungroup() %>%
      group_by(species) %>% mutate(nmfreqbyspec = freq/freq2) %>% ungroup() %>%
      mutate(nmfreq = freq/max(freq1))
    
    recenttrends$method = factor(recenttrends$method, levels = bks1)
    
    temp = recenttrends[recenttrends$species %in% singlespecies,]
    
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspecmeth, colour=method)) + 
      geom_point(size = 3) +
      geom_line(aes(group = method),size = 1.5) +
      ggtitle(singlespecies) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.1, size = 18)) +
      xlab("years") +
      ylab("frequency of reporting")
    
    ggp1 = ggp +
      theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
            axis.title.y = element_text(angle = 90, size = 18), axis.text.y = element_text(size = 16)) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 16)) +
      scale_colour_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_y_continuous(limits = c(round(min(temp$nmfreqbyspecmeth),1)-0.1,round(max(temp$nmfreqbyspecmeth),1)+0.1))
  }
  
  return(ggp1)
}







######################################################################################
######################################################################################
######################################################################################



## FUNCTIONS CLOUD


## expandcloud - function used inside data from cloud
## only need to input data, the species of interest and a dataframe with
## checklist level info for every unique checklist
## input data needs to only have group.id and OBSERVATION.COUNT
## returns an expanded dataframe

expandcloud = function(data, species, checklistinfo)
{
  require(tidyverse)
  
  ## considers only complete lists
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  
  ## deal with NAs
  
  expanded = expanded %>% mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, is.na(OBSERVATION.COUNT), "0"))
  
  
  expanded = expanded %>%
    mutate(OBSERVATION.NUMBER=OBSERVATION.COUNT,
           OBSERVATION.COUNT=replace(OBSERVATION.COUNT, OBSERVATION.COUNT != "0", "1"))
  
  
  
  expanded$OBSERVATION.COUNT = as.numeric(expanded$OBSERVATION.COUNT)
  expanded$OBSERVATION.NUMBER = as.numeric(expanded$OBSERVATION.NUMBER)
  
  return(expanded)
}


######################################################################################


## prepare data imported from CLOUD for spatial analyses, add map variables, grids
## place the 'maps' workspace in working directory
## resolution can take 7 values; "state","district","g1","g2","g3","g4","g5"
## returns an expanded dataframe with only the required resolution
## calls 'expandcloud'

datafromcloud = function(species, resolution = "none", rawpath = "aug2018", mappath = "maps.RData")
{
  require(lubridate)
  require(tidyverse)
  require(bigrquery)
  require(DBI)
  require(stringr)
  
  
  bq_projects() 
  
  con = dbConnect(
    bigrquery::bigquery(),
    project = "stateofindiasbirds",
    dataset = "ebird",
    billing = "stateofindiasbirds"
  )
  
  ## choosing important variables
  
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  imp = c("COMMON_NAME","OBSERVATION_COUNT",
          "LOCALITY_ID","LOCALITY_TYPE",
          "LATITUDE","LONGITUDE","OBSERVATION_DATE","TIME_OBSERVATIONS_STARTED","OBSERVER_ID",
          "PROTOCOL_TYPE","DURATION_MINUTES","EFFORT_DISTANCE_KM",
          "NUMBER_OBSERVERS","ALL_SPECIES_REPORTED","group_id")
  
  listlevelvars = c("gridg1","gridg2","gridg3","gridg4","gridg5","DISTRICT","ST_NM","GLOBAL_UNIQUE_IDENTIFIER","LAST_EDITED_DATE","COUNTRY","COUNTRY_CODE","STATE",
                    "STATE_CODE","COUNTY","COUNTY_CODE","IBA_CODE","BCR_CODE","USFWS_CODE","ATLAS_BLOCK","LOCALITY",
                    "LOCALITY_ID","LOCALITY_TYPE","LATITUDE","LONGITUDE","OBSERVATION_DATE","TIME_OBSERVATIONS_STARTED",
                    "OBSERVER_ID","SAMPLING_EVENT_IDENTIFIER","PROTOCOL_TYPE","PROTOCOL_CODE","PROJECT_CODE",
                    "DURATION_MINUTES","EFFORT_DISTANCE_KM","EFFORT_AREA_HA","NUMBER_OBSERVERS","ALL_SPECIES_REPORTED",
                    "GROUP_IDENTIFIER","group_id","month","year","day","week","fort","LOCALITY_HOTSPOT","no_sp")
  
  specieslevelvars = c("TAXONOMIC_ORDER","COMMON_NAME","SCIENTIFIC_NAME","SUBSPECIES_COMMON_NAME",
                       "SUBSPECIES_SCIENTIFIC_NAME","BREEDING_BIRD_ATLAS_CODE",
                       "BREEDING_BIRD_ATLAS_CODE","BREEDING_BIRD_ATLAS_CATEGORY","CATEGORY","AGE_SEX")
  
  implistlevelvars = intersect(imp,listlevelvars)
  
  ## filter approved observations, species, only include group id and observation count
  
  
  data = 
    tbl(con, rawpath) %>% 
    filter(APPROVED == 1) %>%
    mutate(group_id = ifelse(GROUP_IDENTIFIER == "NA", SAMPLING_EVENT_IDENTIFIER, GROUP_IDENTIFIER)) %>%
    filter(COMMON_NAME %in% species) %>%
    dplyr::select(group_id,OBSERVATION_COUNT) %>%
    collect()
  
  ###############################
  
  checklistinfo1 = 
    tbl(con, rawpath) %>% 
    filter(APPROVED == 1) %>%
    mutate(group_id = ifelse(GROUP_IDENTIFIER == "NA", SAMPLING_EVENT_IDENTIFIER, GROUP_IDENTIFIER)) %>%
    distinct(group_id,COMMON_NAME) %>%
    group_by(group_id) %>% mutate(no_sp = length(COMMON_NAME)) %>%
    ungroup() %>%
    distinct(group_id,no_sp) %>%
    collect()
  
  checklistinfo2 = 
    tbl(con, rawpath) %>% 
    filter(APPROVED == 1) %>%
    mutate(group_id = ifelse(GROUP_IDENTIFIER == "NA", SAMPLING_EVENT_IDENTIFIER, GROUP_IDENTIFIER)) %>%
    distinct(LOCALITY_ID,LOCALITY_TYPE,
             LATITUDE,LONGITUDE,OBSERVATION_DATE,TIME_OBSERVATIONS_STARTED,OBSERVER_ID,
             PROTOCOL_TYPE,DURATION_MINUTES,EFFORT_DISTANCE_KM,
             NUMBER_OBSERVERS,ALL_SPECIES_REPORTED,group_id) %>%
    collect()
  
  checklistinfo = left_join(checklistinfo1,checklistinfo2)
  
  
  
  ## setup eBird data ##
  
  data = data %>%
    group_by(group_id) %>% slice(1) %>% ungroup
  
  nms = names(data)
  nms = str_replace_all(nms,"_",".")
  names(data) = nms
  
  checklistinfo = checklistinfo %>%
    group_by(group_id) %>% slice(1) %>% ungroup %>%
    mutate(OBSERVATION_DATE = as.Date(OBSERVATION_DATE), 
           month = month(OBSERVATION_DATE), year = year(OBSERVATION_DATE),
           day = day(OBSERVATION_DATE) + cdays[month], week = week(OBSERVATION_DATE),
           fort = ceiling(day/14))
  
  
  nms = names(checklistinfo)
  nms = str_replace_all(nms,"_",".")
  names(checklistinfo) = nms
  
  require(data.table)
  require(sp)
  require(rgeos)
  
  if(!exists("indiamap")) {
    load(mappath)
  }
  
  if (resolution != "none")
  {
    # add columns with DISTRICT and ST_NM to checklist info
    
    if (resolution == "state" | resolution == "district")
    {
      temp = checklistinfo %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state 
      rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
      coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
      #proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
      temp = data.frame(temp) # convert into data frame for left_join
      temp$group.id = rownames(temp) # add column to join with the main data
      checklistinfo = left_join(temp,checklistinfo)
    }
    
    # add columns with GRID ATTRIBUTES to checklist info
    
    if (resolution == "g1")
    {
      temp = checklistinfo %>% group_by(group.id) %>% slice(1)
      rownames(temp) = temp$group.id
      coordinates(temp) = ~LONGITUDE + LATITUDE
      temp = over(temp,gridmapg1)
      temp = data.frame(temp)
      temp$group.id = rownames(temp)
      checklistinfo = left_join(temp,checklistinfo)
      names(checklistinfo)[1] = "gridg1"
      checklistinfo$gridg1 = as.factor(checklistinfo$gridg1)
    }
    
    if (resolution == "g2")
    {
      temp = checklistinfo %>% group_by(group.id) %>% slice(1)
      rownames(temp) = temp$group.id
      coordinates(temp) = ~LONGITUDE + LATITUDE
      temp = over(temp,gridmapg2)
      temp = data.frame(temp)
      temp$group.id = rownames(temp)
      checklistinfo = left_join(temp,checklistinfo)
      names(checklistinfo)[1] = "gridg2"
      checklistinfo$gridg2 = as.factor(checklistinfo$gridg2)
    }
    
    if (resolution == "g3")
    {
      temp = checklistinfo %>% group_by(group.id) %>% slice(1)
      rownames(temp) = temp$group.id
      coordinates(temp) = ~LONGITUDE + LATITUDE
      temp = over(temp,gridmapg3)
      temp = data.frame(temp)
      temp$group.id = rownames(temp)
      checklistinfo = left_join(temp,checklistinfo)
      names(checklistinfo)[1] = "gridg3"
      checklistinfo$gridg3 = as.factor(checklistinfo$gridg3)
    }
    
    if (resolution == "g4")
    {
      temp = checklistinfo %>% group_by(group.id) %>% slice(1)
      rownames(temp) = temp$group.id
      coordinates(temp) = ~LONGITUDE + LATITUDE
      temp = over(temp,gridmapg4)
      temp = data.frame(temp)
      temp$group.id = rownames(temp)
      checklistinfo = left_join(temp,checklistinfo)
      names(checklistinfo)[1] = "gridg4"
      checklistinfo$gridg4 = as.factor(checklistinfo$gridg4)
    }
    
    if (resolution == "g5")
    {
      temp = checklistinfo %>% group_by(group.id) %>% slice(1)
      rownames(temp) = temp$group.id
      coordinates(temp) = ~LONGITUDE + LATITUDE
      temp = over(temp,gridmapg5)
      temp = data.frame(temp)
      temp$group.id = rownames(temp)
      checklistinfo = left_join(temp,checklistinfo)
      names(checklistinfo)[1] = "gridg5"
      checklistinfo$gridg5 = as.factor(checklistinfo$gridg5)
    }
  }
  
  if (resolution == "none")
  {
    # add columns with DISTRICT and ST_NM to checklist info
    
    temp = checklistinfo %>% group_by(group.id) %>% slice(1) # same group ID, same grid/district/state
    rownames(temp) = temp$group.id # only to setup adding the group.id column for the future left_join
    coordinates(temp) = ~LONGITUDE + LATITUDE # convert to SPDF?
    #proj4string(temp) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    temp = over(temp,districtmap) # returns only ATTRIBUTES of districtmap (DISTRICT and ST_NM)
    temp = data.frame(temp) # convert into data frame for left_join
    temp$group.id = rownames(temp) # add column to join with the main data
    checklistinfo = left_join(temp,checklistinfo)
    
    # add columns with GRID ATTRIBUTES to checklist info
    
    temp = checklistinfo %>% group_by(group.id) %>% slice(1)
    rownames(temp) = temp$group.id
    coordinates(temp) = ~LONGITUDE + LATITUDE
    temp = over(temp,gridmapg1)
    temp = data.frame(temp)
    temp$group.id = rownames(temp)
    checklistinfo = left_join(temp,checklistinfo)
    names(checklistinfo)[1] = "gridg1"
    checklistinfo$gridg1 = as.factor(checklistinfo$gridg1)
    
    temp = checklistinfo %>% group_by(group.id) %>% slice(1)
    rownames(temp) = temp$group.id
    coordinates(temp) = ~LONGITUDE + LATITUDE
    temp = over(temp,gridmapg2)
    temp = data.frame(temp)
    temp$group.id = rownames(temp)
    checklistinfo = left_join(temp,checklistinfo)
    names(checklistinfo)[1] = "gridg2"
    checklistinfo$gridg2 = as.factor(checklistinfo$gridg2)
    
    temp = checklistinfo %>% group_by(group.id) %>% slice(1)
    rownames(temp) = temp$group.id
    coordinates(temp) = ~LONGITUDE + LATITUDE
    temp = over(temp,gridmapg3)
    temp = data.frame(temp)
    temp$group.id = rownames(temp)
    checklistinfo = left_join(temp,checklistinfo)
    names(checklistinfo)[1] = "gridg3"
    checklistinfo$gridg3 = as.factor(checklistinfo$gridg3)
    
    temp = checklistinfo %>% group_by(group.id) %>% slice(1)
    rownames(temp) = temp$group.id
    coordinates(temp) = ~LONGITUDE + LATITUDE
    temp = over(temp,gridmapg4)
    temp = data.frame(temp)
    temp$group.id = rownames(temp)
    checklistinfo = left_join(temp,checklistinfo)
    names(checklistinfo)[1] = "gridg4"
    checklistinfo$gridg4 = as.factor(checklistinfo$gridg4)
    
    temp = checklistinfo %>% group_by(group.id) %>% slice(1)
    rownames(temp) = temp$group.id
    coordinates(temp) = ~LONGITUDE + LATITUDE
    temp = over(temp,gridmapg5)
    temp = data.frame(temp)
    temp$group.id = rownames(temp)
    checklistinfo = left_join(temp,checklistinfo)
    names(checklistinfo)[1] = "gridg5"
    checklistinfo$gridg5 = as.factor(checklistinfo$gridg5)
    
    ## move personal locations to nearest hotspots
    
    allhotspots = checklistinfo %>% filter(LOCALITY.TYPE == "H") %>%
      group_by(LOCALITY.ID) %>% summarize(max(LATITUDE),max(LONGITUDE)) %>% # get lat long for each hotspot
      ungroup
    
    names(allhotspots)[c(2,3)] = c("LATITUDE","LONGITUDE")
    
    # using DATA TABLES below
    
    setDT(checklistinfo) # useful for extremely large data frames as each object will then be modified in place without creating 
    # a copy, creates a 'reference' apparently; this will considerably reduce RAM usage 
    hotspots = as.matrix(allhotspots[, 3:2])
    
    # again 'refernce' based, choosing nearest hotspots to each location
    checklistinfo[, LOCALITY.HOTSPOT := allhotspots[which.min(spDists(x = hotspots, y = cbind(LONGITUDE, LATITUDE))),]$LOCALITY.ID, by=.(LONGITUDE, LATITUDE)] 
    checklistinfo = as.data.frame(checklistinfo) # back into dataframe
  }
  
  data = expandcloud(data, species, checklistinfo)
  
  return(data)
}


## a script to plot frequencies on a map from the cloud
## resolution can take 7 values; "state","district","g1","g2","g3","g4","g5"
## species common name, calls 'datafromcloud' and 'expandcloud', returns a ggplot object

plotfreqmapcloud = function(species, resolution, mappath = "maps.RData", maskpath = "mask.RData")
{
  require(tidyverse)
  require(ggfortify)
  require(viridis)
  
  if(!exists("indiamap")) {
    load(mappath)
  }
  
  plotindiamap = ggplot() +
    geom_path(data = fortify(indiamap), aes(x=long, y=lat, group=group), colour = 'black')+  
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
  
  data = datafromcloud(species, resolution)  
  
  if (resolution == "state")
  {
    temp = data %>% 
      group_by(ST_NM) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(statemap, region = c("ST_NM"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "ST_NM"))) # SPDF to plot
  }
  
  if (resolution == "district")
  {
    temp = data %>% 
      group_by(DISTRICT) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(districtmap, region = c("DISTRICT"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "DISTRICT"))) # SPDF to plot
  }
  
  if (resolution == "g1")
  {
    temp = data %>% 
      group_by(gridg1) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(gridmapg1, region = c("id"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg1"))) # SPDF to plot
  }
  
  if (resolution == "g2")
  {
    temp = data %>% 
      group_by(gridg2) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(gridmapg2, region = c("id"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg2"))) # SPDF to plot
  }
  
  if (resolution == "g3")
  {
    temp = data %>% 
      group_by(gridg3) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(gridmapg3, region = c("id"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg3"))) # SPDF to plot
  }
  
  if (resolution == "g4")
  {
    temp = data %>% 
      group_by(gridg4) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(gridmapg4, region = c("id"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg4"))) # SPDF to plot
  }
  
  if (resolution == "g5")
  {
    temp = data %>% 
      group_by(gridg5) %>%
      summarize(freq = sum(OBSERVATION.COUNT)/n())
    
    fortified = fortify(gridmapg5, region = c("id"))
    fortified$id = as.factor(fortified$id)
    
    plotdf = na.omit(left_join(fortified,temp, by = c('id' = "gridg5"))) # SPDF to plot
  }
  
  plotdf = plotdf %>%
    filter(freq>0)
  
  if (resolution == "state" | resolution == "district")
  {
    plot = plotindiamap +
      geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq)) +
      scale_fill_viridis() +
      theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  }
  else
  {
    load(maskpath)
    plot = plotindiamap +
      geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq)) +
      geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white')+
      geom_path(data = border, aes(x = long, y = lat, group = group), col = 'black') +
      scale_fill_viridis() +
      theme(legend.title = element_blank(), legend.text = element_text(size = 8))
  }
  
  return(plot)
}


###############################################################


## to output a dataframe that compares 'abundances' from summaries and models
## tempres can be "fortnight", "month", "none"
## spaceres can be 40 and 80 km ("g2","g4","none")
## returns 6 values
## calls 'datafromcloud'

freqcomparecloud = function(species, tempres = "none",spaceres = "none")
{
  require(tidyverse)
  require(lme4)
  
  data = datafromcloud(species)
  
  if (tempres == "fortnight" & spaceres == "g2")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(gridg2,fort)
  }
  
  if (tempres == "fortnight" & spaceres == "g4")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(gridg4,fort)
  }
  
  if (tempres == "fortnight" & spaceres == "none")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(fort)
  }
  
  if (tempres == "month" & spaceres == "g2")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(gridg2,month)
  }
  
  if (tempres == "month" & spaceres == "g4")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(gridg4,month)
  }
  
  if (tempres == "month" & spaceres == "none")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(month)
  }
  
  if (tempres == "none" & spaceres == "g2")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(gridg2)
  }
  
  if (tempres == "none" & spaceres == "g4")
  {
    temp = data %>%
      filter(OBSERVATION.COUNT == 1) %>%
      distinct(gridg4)
  }
  
  if (tempres == "none" & spaceres == "none")
  {
    temp = data %>%
      dplyr::select(gridg2,fort)
  }
  
  ## filter only those combinations
  
  data = temp %>% left_join(data)
  
  ## overall for country
  
  f1 = data %>% 
    summarize(freq = sum(OBSERVATION.COUNT)/n())
  
  ## averaged across g5
  
  temp = data %>% 
    group_by(gridg5) %>%
    summarize(freq = sum(OBSERVATION.COUNT)/n()) %>%
    ungroup()
  
  f2 = temp %>%
    summarize(freq = mean(freq))
  
  ## averaged across g3 and g5
  
  temp = data %>% 
    group_by(gridg5,gridg3) %>% summarize(freq = sum(OBSERVATION.COUNT)/n()) %>%
    ungroup()
  
  temp = temp %>% 
    group_by(gridg5) %>% summarize(freq = mean(freq)) %>%
    ungroup()
  
  f3 = temp %>%
    summarize(freq = mean(freq))
  
  ## averaged across g1, g3 and g5
  
  temp = data %>% 
    group_by(gridg5,gridg3,gridg1) %>% summarize(freq = sum(OBSERVATION.COUNT)/n()) %>% 
    ungroup
  
  temp = temp %>% 
    group_by(gridg5,gridg3) %>% summarize(freq = mean(freq)) %>%
    ungroup()
  
  temp = temp %>% 
    group_by(gridg5) %>% summarize(freq = mean(freq)) %>%
    ungroup()
  
  f4 = temp %>%
    summarize(freq = mean(freq))
  
  
  ## model 1
  
  m1 = glmer(OBSERVATION.COUNT ~ 
               log(no.sp) + (1|ST_NM/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = data, family=binomial(link = 'cloglog'), nAGQ = 0)
  
  f5 = predict(m1, data.frame(
    no.sp = 20),
    type="response", re.form = NA)
  
  ## model 2
  
  m2 = glmer(OBSERVATION.COUNT ~ 
               log(no.sp) + (1|gridg5/gridg3/gridg1) + (1|OBSERVER.ID), data = data, family=binomial(link = 'cloglog'), nAGQ = 0)
  
  f6 = predict(m2, data.frame(
    no.sp = 20),
    type="response", re.form = NA)
  
  f = data.frame(method = c("overall","g5","g5/g3","g5/g3/g1","modelhotspots","modelgrids"))
  f$freq = c(f1,f2,f3,f4,f5,f6)
  
  return(f)
}



######################################################################################
