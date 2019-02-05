####################################################################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "ebd_IN_relAug-2018.txt")
{
  require(lubridate)
  require(tidyverse)
  
  #library(auk)
  
  #allin = system.file("extdata/ebd_IN_relAug-2018.txt", package = "auk")
  
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

createmaps = function(g1=20,g2=50,g3=80,g4=100,g5=320,path1="India",name1="India_2011",path2="India States",name2="IndiaStates_2011",
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
  rm(addmapvars, districtmap, gridmapg1, gridmapg2, gridmapg3, gridmapg4, gridmapg5,
     indiamap, statemap, calculateslope, createmask, errordiv,
     expandbyspecies, freqtrends, plotfreqmap, plottrends, readcleanrawdata, pos = ".GlobalEnv")
  
  save.image("dataforspatialanalyses.RData")
}


#######################################################################################


## a script to plot frequencies on a map with relative ease
## input can be trivial frequencies or modeled output
## data is unsummarized data
## resolution can take 7 values; "state","district","g1","g2","g3","g4","g5"
## species common name
## path can be true or false; this is for boundaries
## add can take the values "species", "unique lists","observers","unique locations"

plotfreqmap = function(data, taxonname, resolution, level = "species", season = "year round", rich = F,
                       add = "species", smooth = F, h = 2, cutoff = 5, baseyear = 2010, endyear = 2018,
                       showempty = T, states = "none",
                       mappath = "maps.RData", maskpath = "mask.RData")
{
  ## errors for wrong parameter values
  
  if (!level %in% c("species","genus","family","order"))
    return(paste("the taxonomic level",level,"doesn't exist, select any one from species, genus, family and order (in quotes)"))

  if (!resolution %in% c("state","district","g1","g2","g3","g4","g5"))
    return(paste("the resolution",resolution,"doesn't exist, select any one from state, district, g1, g2, g3, g4 and g5 (in quotes)"))
  
  if (!season %in% c("year round","summer","winter","passage"))
    return(paste("the season",season,"doesn't exist, select any one from year round, summer, winter and passage (in quotes)"))
  
  if (!is.na(as.logical(rich)))
  {
    if (rich != as.logical(rich))
      return("rich must be a logical operator")
  }else{
    return("rich must be a logical operator")
  }

  if (!is.na(as.logical(smooth)))
  {
    if (smooth != as.logical(smooth))
      return("smooth must be a logical operator")
  }else{
    return("smooth must be a logical operator")
  }
  
  if (!is.na(as.logical(showempty)))
  {
    if (showempty != as.logical(showempty))
      return("showempty must be a logical operator")
  }else{
    return("showempty must be a logical operator")
  }
  
  if (cutoff & (cutoff < 0 | cutoff != as.integer(cutoff)))
    return("cutoff (minimum no. of lists) must be a non-negative integer")
  
  if (baseyear & (baseyear < 0 | baseyear > as.integer(format(Sys.Date(), "%Y")) | baseyear != as.integer(baseyear)))
    return("baseyear (minimum year to consider) must be a non-negative integer before the current year")

  if (endyear & (endyear < 0 | endyear > as.integer(format(Sys.Date(), "%Y")) | endyear != as.integer(endyear)))
    return("endyear (maximum year to consider) must be a non-negative integer before the current year")
    
  if (smooth & (h < 0 | h != as.numeric(h)))
    return("h (smoothing bandwidth) must be positive")
    
  
  require(tidyverse)
  require(ggfortify)
  require(viridis)
  require(RColorBrewer)
  require(scales)
  
  load(mappath)
  
  filterstate = fortify(statemap)
  
  if (states == "none")
  {
    states = as.character(na.omit(unique(data$ST_NM)))
  }
  
  if (states[1] != "none")
  {
    if (resolution == "district" | resolution == "state")
    {
      filterstate = fortify(statemap[statemap@data$ST_NM %in% states,], region = c("ST_NM"))
      
      data = data %>%
        filter(ST_NM %in% states)
      
      filterdistrict = fortify(districtmap[districtmap@data$ST_NM %in% states,], region = c("DISTRICT"))
    }
  }
  
  
  
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
  
  if (level == "species")
  {
    if (!taxonname %in% unique(data$COMMON.NAME))
      return(paste(taxonname,"is not a valid",level,"name"))
  }
  
  if (level != "species")
  {
    taxlevels = read.csv("speciestaxonomiclevels.csv")
    taxlevels = taxlevels[,c("COMMON.NAME",level)]
    
    if (level == "genus")
    {
      if (!taxonname %in% unique(taxlevels$genus))
        return(paste(taxonname,"is not a valid",level,"name"))
    }
    
    if (level == "family")
    {
      if (!taxonname %in% unique(taxlevels$family))
        return(paste(taxonname,"is not a valid",level,"name"))
    }
    
    if (level == "order")
    {
      if (!taxonname %in% unique(taxlevels$order))
        return(paste(taxonname,"is not a valid",level,"name"))
    }
    
    data = left_join(data,taxlevels)
    l = length(names(data))
    names(data)[l] = "TAXON.NAME"
    
    if (rich)
    {
      data = data %>% filter(TAXON.NAME == taxonname)
    }else{
      data = data %>% distinct(gridg5,group.id,gridg4,gridg3,gridg2,gridg1,DISTRICT,ST_NM,LOCALITY.ID,
                               LOCALITY.TYPE,LATITUDE,LONGITUDE,OBSERVATION.DATE,TIME.OBSERVATIONS.STARTED,
                               OBSERVER.ID,PROTOCOL.TYPE,DURATION.MINUTES,EFFORT.DISTANCE.KM,NUMBER.OBSERVERS,
                               ALL.SPECIES.REPORTED,month,year,day,week,fort,no.sp,LOCALITY.HOTSPOT,TAXON.NAME)
    }
  }else{
    data$TAXON.NAME = data$COMMON.NAME
  }
  
  if (season == "summer")
  {
    data = data %>% 
      filter(day >= 135 & day <= 225)
  }
  
  if (season == "winter")
  {
    data = data %>% 
      filter(day < 60 | day > 300)
  }
  
  if (season == "passage")
  {
    data = data %>% 
      filter((day >= 120 & day < 135) | (day > 240 & day <= 300))
  }
  
  if (!isTRUE(rich))
  {
    data = data %>%
      filter(year >= baseyear, year <= endyear, ALL.SPECIES.REPORTED == 1)
    data1 = data %>% filter(TAXON.NAME == taxonname)
  }
  
  if (isTRUE(rich))
  {
    if (resolution == "state")
    {
      if (add == "species"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(ST_NM) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(ST_NM,lists)
      
      fortified = filterstate
      fortind = filterstate
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
      if (add == "species"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(DISTRICT) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(DISTRICT,lists)
      
      fortified = filterdistrict
      fortind = filterstate
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
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg1) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
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
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg2) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
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
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg3) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
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
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg4) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
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
      }else{
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
      
      if (add == "species"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(COMMON.NAME))
      }
      if (add == "unique lists"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(group.id))
      }
      if (add == "observers"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(OBSERVER.ID))
      }
      if (add == "unique locations"){
        temp = data %>% 
          group_by(gridg5) %>%
          summarize(freq = n_distinct(LOCALITY.ID))
      }
      
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(ST_NM) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(ST_NM) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(ST_NM,lists)
      
      fortified = filterstate
      fortind = filterstate
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(DISTRICT) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
      min = min(temp$freq)
      max = max(temp$freq)
      
      filled = data %>%
        group_by(DISTRICT) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(lists >= cutoff) %>%
        distinct(DISTRICT,lists)
      
      fortified = filterdistrict
      fortind = filterstate
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg1) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg2) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg4) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
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
        filter(TAXON.NAME == taxonname, lists >= cutoff) %>%
        group_by(gridg5) %>%
        summarize(freq = n_distinct(group.id)/max(lists))
      
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
  
  
  if(smooth & resolution != "state" & resolution != "district"){
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
  
  require(mltools)
  
  plotdf$freq1 = mltools::bin_data(plotdf$freq, bins=4, binType = "quantile")
  
  sm = plotdf %>%
    group_by(freq1) %>% summarize(min = round(min(freq),2),max = round(max(freq),2))
  
  l = length(sm$freq1)
  vals = c("#99CCFF","#6699CC","#336699","#003399")
  
  data = data %>%
    filter(COMMON.NAME == taxonname) %>%
    group_by(LOCALITY.ID) %>% slice(1)
  
  plot = plotindiamap +
    {if(smooth & resolution != "state" & resolution != "district" & !isTRUE(rich))stat_density2d(data = data1, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(smooth & resolution != "state" & resolution != "district" & rich)stat_density2d(data = datar, aes(x = LONGITUDE, y = LATITUDE, fill = stat(level)), h = h, n = 100, geom = "polygon")} +
    {if(!isTRUE(smooth) | resolution == "state" | resolution == "district")geom_polygon(data = plotdf, aes(x = long, y = lat, group = group, fill = freq1))} +
    {if(switch & showempty)geom_polygon(data = emptydf, aes(x = long, y = lat, group = group, col = cl), fill = "grey30")} +
    {if(resolution != "state" & resolution != "district")geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'white', fill = 'white')}+
    {if(resolution == "state" | resolution == "district")geom_path(data = filterdistrict, aes(x = long, y = lat, group = group), col = 'darkolivegreen', size = 0.5)}+
    geom_path(data = filterstate, aes(x = long, y = lat, group = group), col = 'black', size = 1) +
    {if(l <= 2)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = sm$freq1)} +
    {if(l == 3)scale_fill_manual(values = vals[1:l],
                                 breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                 paste(">",sm$min[3])))} +
    {if(l > 3)scale_fill_manual(values = vals,breaks = sm$freq1, labels = c(paste("<=",sm$max[1]),paste(sm$min[2]," - ",sm$max[2]),
                                 paste(sm$min[3]," - ",sm$max[3]),paste(">",sm$min[4])))} +
    {if(switch)scale_colour_manual(values = cols)} +
    #theme(legend.justification=c(1,1), legend.position=c(0.99,0.99)) +
    theme(legend.text = element_text(size = 12)) +
    {if(!isTRUE(rich) | level != "species")ggtitle(taxonname)} +
    {if(!isTRUE(rich) | level != "species")theme(plot.title = element_text(hjust = 0.5, vjust = 0.1, size = 20))} +
    guides(fill = guide_legend(title = add, reverse = TRUE, override.aes = list(size=10)))
    #geom_point(data = data, aes(x = LONGITUDE, y = LATITUDE), col = 'red', size = 2)+
    #theme(legend.position = "none")+
    #ggtitle(endyear)
  
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
    mutate(timegroups = ifelse(year <= 1999, "before 1999", timegroups)) %>%
    #mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
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


## to output a frequency based on the type of summary
## tempres can be "fortnight", "month", "none"
## spaceres can be 40 and 80 km ("g2","g4","none")
## returns a single value or 10 values for trends
## the type/scale of analyses - "trivial pa","gridded pa","trivial count","gridded count","pa1","pa2",
## "pa3","pa4","count1","count2","count3"
## only last 5 years for non-trend analyses

freqtrends = function(data,species,politicalunit="country",unitname=NA,analysis="trivial pa",
                      tempres="none",spaceres="none",trends=F,minobs=100,baseyear=2010,zinf=0)
{
  require(tidyverse)
  require(lme4)
  require(VGAM)
  
  ## errors for wrong parameter values
  
  if (!politicalunit %in% c("country","state","district"))
    return(paste("the filtering level",politicalunit,"doesn't exist. Select any one from country, state and district (in quotes)"))
  
  if (politicalunit == "state")
  {
    if (is.na(unitname) | !unitname %in% unique(data$ST_NM))
      return(paste(unitname,"is not a valid",politicalunit,"name"))
  }
  
  if (politicalunit == "district")
  {
    if (is.na(unitname) | !unitname %in% unique(data$DISTRICT))
      return(paste(unitname,"is not a valid",politicalunit,"name"))
  }
  
  if (!analysis %in% c("trivial pa","gridded pa","trivial count","gridded count","pa1","pa2","pa3","pa4","count1","count2","count3"))
    return(paste("the analysis type",analysis,"doesn't exist, select any one from trivial pa, gridded pa, trivial count, gridded count, pa1, pa2, pa3, count1, count2 and count3 (in quotes)"))
  
  
  if (!tempres %in% c("fortnight","month","none"))
    return(paste("the temporal resolution",tempres,"doesn't exist, select any one from fortnight, month and none (in quotes)"))
  
  if (!spaceres %in% c("g2","g4","none"))
    return(paste("the spatial resolution",spaceres,"doesn't exist, select any one from g2, g4 and none (in quotes)"))
  
  if (!is.na(as.logical(trends)))
  {
    if (trends != as.logical(trends))
      return("trends must be a logical operator")
  }else{
    return("trends must be a logical operator")
  }
  
  if (minobs & (minobs <= 0 | minobs != as.integer(minobs)))
    return("minobs (minimum no. of lists) must be a positive integer")
  
  if (baseyear & (baseyear < 0 | baseyear > as.integer(format(Sys.Date(), "%Y")) | baseyear != as.integer(baseyear)))
    return("baseyear (minimum year to consider) must be a non-negative integer before the current year")
  
  ## considers only complete lists
  
  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)

  if (politicalunit == "state")
  {
    data = data %>%
      filter(ST_NM == unitname)
  }
  
  if (politicalunit == "district")
  {
    data = data %>%
      filter(DISTRICT == unitname)
  }
  
  data$fort = as.factor(data$fort)
  data$month = as.factor(data$month)
  
  if (!species %in% unique(data$COMMON.NAME))
    return(paste(species,"is not a valid species name for the region selected"))
  
  if (is.na(zinf) | (zinf != 0 & zinf != 1))
    return(paste("zero inflation value zinf must be either 0 or 1"))
  
  if (isTRUE(trends))
  {
    data = data %>%
      mutate(timegroups = as.character(year)) %>%
      mutate(timegroups = ifelse(year <= 1999, "before 1999", timegroups)) %>%
      #mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
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
  

  if (tempres == "fortnight" & spaceres == "g2")
  {
    data$gridg = data$gridg2
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg2,fort)
  }
  
  if (tempres == "fortnight" & spaceres == "g4")
  {
    data$gridg = data$gridg4
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg4,fort)
  }
  
  if (tempres == "fortnight" & spaceres == "none")
  {
    data$gridg = data$gridg4
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(fort)
  }
  
  if (tempres == "month" & spaceres == "g2")
  {
    data$gridg = data$gridg2
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg2,month)
  }
  
  if (tempres == "month" & spaceres == "g4")
  {
    data$gridg = data$gridg4
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg4,month)
  }
  
  if (tempres == "month" & spaceres == "none")
  {
    data$gridg = data$gridg4
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(month)
  }
  
  if (tempres == "none" & spaceres == "g2")
  {
    data$gridg = data$gridg2
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg2)
  }
  
  if (tempres == "none" & spaceres == "g4")
  {
    data$gridg = data$gridg4
    temp = data %>%
      filter(COMMON.NAME == species) %>%
      distinct(gridg4)
  }
  
  fl = 0
  
  if (tempres == "none" & spaceres == "none")
  {
    fl = 1
    data$gridg = data$gridg4
  }
  
  ## filter only those combinations
  
  if (fl == 0)
  {
    data = temp %>% left_join(data)
  }
  
  if (length(data[data$COMMON.NAME == species,]$OBSERVATION.COUNT) < minobs)
    return(paste(species,"has been reported in less than",minobs,"lists, please choose another species"))
  
  ## filter out data for non-trend analyses - only last 5 years
  
  data2 = data %>%
    filter(year >= baseyear)
  

  if (!isTRUE(trends))
  {
    if (analysis == "trivial pa")
    {
      f = data2 %>% 
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species) %>%
        summarize(freq = n()/max(lists))
    }
    
    if (analysis == "trivial count")
    {
      f = data2 %>%
        mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species) %>%
        summarize(freq = sum(OBSERVATION.COUNT)/max(lists))
    }
    
    if (analysis == "gridded pa")
    {
      temp = data2 %>% 
        group_by(gridg) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species) %>%
        group_by(gridg) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
        ungroup()
      
      f = temp %>%
        summarize(freq = mean(freq))
    }
    
    if (analysis == "gridded count")
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
    
    if (analysis == "pa1")
    {
      data1 = data
      ed = expandbyspecies(data1,species)
      
      ed = ed %>%
        filter(year >= baseyear)
      
      if (politicalunit == "country")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|ST_NM/DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (politicalunit == "state")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (politicalunit == "district")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      ltemp = data.frame(no.sp = 20)
    }
      
    if (analysis == "pa2")
    {
      data1 = data
      ed = expandbyspecies(data1,species)
      
      ed = ed %>%
        filter(year >= baseyear)
      
      if (spaceres == "none")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g4")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g2")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + (1|gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      ltemp = data.frame(no.sp = 20)
    }  
    
    if (analysis == "count1")
    {
      data1 = data
      ed = expandbyspecies(data1,species)
      
      require(glmmTMB)
      
      ed = ed %>%
        filter(year >= baseyear) %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
      
      if (politicalunit == "country")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|ST_NM/DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|ST_NM/DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        
        newdata = ed %>%
          distinct(ST_NM,DISTRICT,LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (politicalunit == "state")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(DISTRICT,LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (politicalunit == "district")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        
        newdata = ed %>%
          distinct(LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        

      }
    }
    
    if (analysis == "count2")
    {
      data1 = data
      ed = expandbyspecies(data1,species)
      
      require(glmmTMB)
      
      ed = ed %>%
        filter(year >= baseyear) %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
      
      if (spaceres == "none")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(gridg5,gridg4,gridg2,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (spaceres == "g4")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        
        newdata = ed %>%
          distinct(gridg4,gridg2,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (spaceres == "g2")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + (1|gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(gridg2,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
    }
    
    if (analysis == "pa3")
    {
      data1 = data
      ed = expandbyspecies(data1,species)
      
      ed = ed %>%
        filter(year >= baseyear)
      
      if (spaceres == "none")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     month + month:log(no.sp) + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g4")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     month + month:log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g2")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     month + month:log(no.sp) + (1|gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      ltemp = data.frame(no.sp = 20,month = unique(data$month))
      
    }
    
    if (analysis == "pa4")
    {
      data1 = data %>%
        group_by(gridg2) %>% mutate(lists = n_distinct(group.id)) %>%
        filter(lists >= 20)
        
      ed = expandbyspecies(data1,species)
      
      require(glmmTMB)
      
      ed = ed %>%
        filter(year >= baseyear) %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
      
      m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                     month + month:log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                   family=poisson)
      
      newdata = ed %>%
        distinct(gridg4,gridg2,OBSERVER.ID)
      newdata = do.call("rbind", replicate(length(unique(ed$month)),newdata,simplify=F))
      newdata$no.sp = 20
      newdata$month = rep(unique(ed$month), each = length(newdata$no.sp)/length(unique(ed$month)))

      fd = predict(m1, newdata = newdata,
                   type="response", allow.new.levels = T)
      newdata$f = fd
      
      fx = newdata %>%
        group_by(gridg2,month) %>% summarize(abund = mean(f))
      
      ed = left_join(ed,fx)
      
      if (spaceres == "none")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     month + month:log(no.sp) + abund + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g4")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     month + month:log(no.sp) + abund + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g2")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     month + month:log(no.sp) + abund + (1|gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      ltemp = data.frame(no.sp = 20,abund = 1,month = unique(ed$month))
      
    }
    
    if (analysis == "count3")
    {
      data1 = data
      ed = expandbyspecies(data1,species)
      
      require(glmmTMB)
      
      ed = ed %>%
        filter(year >= baseyear) %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
      
      if (spaceres == "none")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         month + month:log(no.sp) + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         month + month:log(no.sp) + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(gridg5,gridg4,gridg2,OBSERVER.ID)
        newdata = do.call("rbind", replicate(length(unique(ed$month)),newdata,simplify=F))
        newdata$no.sp = 20
        newdata$month = rep(unique(ed$month), each = length(newdata$no.sp)/length(unique(ed$month)))
        
      }
      
      if (spaceres == "g4")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         month + month:log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         month + month:log(no.sp) + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        
        newdata = ed %>%
          distinct(gridg4,gridg2,OBSERVER.ID)
        newdata = do.call("rbind", replicate(length(unique(ed$month)),newdata,simplify=F))
        newdata$no.sp = 20
        newdata$month = rep(unique(ed$month), each = length(newdata$no.sp)/length(unique(ed$month)))
        
        
      }
      
      if (spaceres == "g2")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         month + month:log(no.sp) + (1|gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         month + month:log(no.sp) + (1|gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(gridg2,OBSERVER.ID)
        newdata = do.call("rbind", replicate(length(unique(ed$month)),newdata,simplify=F))
        newdata$no.sp = 20
        newdata$month = rep(unique(ed$month), each = length(newdata$no.sp)/length(unique(ed$month)))
        
      }
    }
  }
  
  
    
  ## trends
  
  
  
  if (isTRUE(trends))
  {
    if (analysis == "trivial pa")
    {
      f = data %>% 
        group_by(timegroups) %>%
        mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species) %>%
        group_by(timegroups) %>% summarize(freq = n()/max(lists)) %>% ungroup()
      
      f$timegroups = as.character(f$timegroups)
    }
    
    if (analysis == "trivial count")
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
    
    if (analysis == "gridded pa")
    {
      temp = data %>% 
        group_by(timegroups,gridg) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species) %>%
        group_by(timegroups,gridg) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>%
        ungroup()
      
      f = temp %>%
        group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
      
      f$timegroups = as.character(f$timegroups)
    }
    
    if (analysis == "gridded count")
    {
      temp = data %>% 
        mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.COUNT))) %>% ungroup() %>%
        group_by(timegroups,gridg) %>% mutate(lists = n_distinct(group.id)) %>% ungroup() %>%
        filter(COMMON.NAME == species) %>%
        group_by(timegroups,gridg) %>% summarize(freq = sum(OBSERVATION.COUNT)/max(lists)) %>%
        ungroup()
      
      f = temp %>%
        group_by(timegroups) %>% summarize(freq = mean(freq)) %>% ungroup()
      
      f$timegroups = as.character(f$timegroups)
    }
    
    if (analysis == "pa1")
    {
      data1 = data %>% select(-timegroups)
      ed = expandbyspecies(data1,species)
      
      if (politicalunit == "country")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|ST_NM/DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (politicalunit == "state")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (politicalunit == "district")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      ltemp = data.frame(timegroups = unique(data$timegroups),
                                      no.sp = 20)
    }
    
    if (analysis == "pa2")
    {
      data1 = data %>% select(-timegroups)
      ed = expandbyspecies(data1,species)
      
      if (spaceres == "none")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g4")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g2")
      {
        m1 = glmer(OBSERVATION.COUNT ~ 
                     log(no.sp) + timegroups + (1|gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      ltemp = data.frame(timegroups = unique(data$timegroups),
                                      no.sp = 20)
    }  
    
    if (analysis == "pa3")
    {
      data1 = data %>% select(-timegroups)
      ed = expandbyspecies(data1,species)
      
      if (spaceres == "none")
      {
        m1 = glmer(OBSERVATION.COUNT ~ month +
                     month:log(no.sp) + timegroups + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g4")
      {
        m1 = glmer(OBSERVATION.COUNT ~ month +
                     month:log(no.sp) + timegroups + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      if (spaceres == "g2")
      {
        m1 = glmer(OBSERVATION.COUNT ~ month +
                     month:log(no.sp) + timegroups + (1|gridg2) + (1|OBSERVER.ID), data = ed, 
                   family=binomial(link = 'cloglog'), nAGQ = 0)
      }
      
      f = data.frame(unique(data$timegroups))
      f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
      names(f) = "timegroups"
      f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
      ltemp = data.frame(timegroups = f$timegroups,
                                      no.sp = 20, month = f$month)
      
    }
    
    if (analysis == "count1")
    {
      data1 = data %>% select(-timegroups)
      ed = expandbyspecies(data1,species)
      
      require(glmmTMB)
      
      ed = ed %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
      
      if (politicalunit == "country")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|ST_NM/DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID),
                       data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|ST_NM/DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID),
                       data = ed, ziformula=~1,
                       family=poisson)
        }
        
        
        newdata = ed %>%
          distinct(timegroups,ST_NM,DISTRICT,LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (politicalunit == "state")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed,
                       ziformula=~0, family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|DISTRICT/LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed,
                       ziformula=~1, family=poisson)
        }
        
        newdata = ed %>%
          distinct(timegroups,DISTRICT,LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (politicalunit == "district")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                       ziformula=~0, family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|LOCALITY.HOTSPOT) + (1|OBSERVER.ID), data = ed, 
                       ziformula=~1, family=poisson)
        }
        
        newdata = ed %>%
          distinct(timegroups,LOCALITY.HOTSPOT,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
    }
    
    if (analysis == "count2")
    {
      data1 = data %>% select(-timegroups)
      ed = expandbyspecies(data1,species)
      
      require(glmmTMB)
      
      ed = ed %>%
        group_by(group.id) %>% filter(!any(is.na(OBSERVATION.NUMBER))) %>% ungroup()
      
      if (spaceres == "none")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), 
                       data = ed, ziformula=~0,family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|gridg5/gridg4/gridg2) + (1|OBSERVER.ID), 
                       data = ed, ziformula=~1,family=poisson)
        }
        
        newdata = ed %>%
          distinct(timegroups,gridg5,gridg4,gridg2,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (spaceres == "g4")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|gridg4/gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(timegroups,gridg4,gridg2,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
      
      if (spaceres == "g2")
      {
        if (zinf == 0)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~0,
                       family=poisson)
        }
        if (zinf == 1)
        {
          m1 = glmmTMB(OBSERVATION.NUMBER ~ 
                         log(no.sp) + timegroups + (1|gridg2) + (1|OBSERVER.ID), data = ed, ziformula=~1,
                       family=poisson)
        }
        
        newdata = ed %>%
          distinct(timegroups,gridg2,OBSERVER.ID)
        newdata$no.sp = 20
        
      }
    }
    
  }

  
  
  if (!isTRUE(trends))
  {
    f1 = data.frame(method = analysis)
    
    if (analysis != "pa1" & analysis != "pa2" & analysis != "pa3" & analysis != "pa4" &
        analysis != "count1" & analysis != "count2" & analysis != "count3")
    {
      f1$freq = f$freq
    }else{
      
      if (analysis == "pa1" | analysis == "pa2" | analysis == "pa3" | analysis == "pa4")
      {
        f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
        f2$se = numeric(length(ltemp$no.sp))
        
        predFun = function(m1) {
          predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
        }
        
        pred = bootMer(m1, nsim=100, FUN=predFun, parallel = "snow")
        
        for (i in 1:length(ltemp$no.sp))
        {
          f2$freq[i] = median(pred$t[,i])
          f2$se[i] = sd(pred$t[,i])
        }
        
        f2$freqt = cloglog(f2$freq,inverse = T)
        f2$cl = cloglog((f2$freq-f2$se),inverse = T)
        f2$set = f2$freqt-f2$cl
        
        fx = f2 %>%
          summarize(freq = mean(freqt), se = sqrt(sum(set^2)))
        fx$se = fx$se/length(f2$set)
        
        f1$freq = fx$freq
        f1$se = fx$se
      }
      
      if (analysis == "count1" | analysis == "count2" | analysis == "count3")
      {
        fd = predict(m1, newdata = newdata,
                     type = "response", se.fit = T, allow.new.levels = T)
        newdata$freq = as.numeric(fd$fit)
        newdata$se = as.numeric(fd$se.fit)
        
        fx = newdata %>%
          summarize(freq = mean(freq), se = sqrt(sum(se^2)))
        fx$se = fx$se/length(newdata$se)
        
        f1$freq = fx$freq
        f1$se = fx$se
      }
    }
    
    f1$species = species
  }
  
  if (isTRUE(trends))
  {
    f1 = data.frame(timegroups = unique(data$timegroups), method = analysis)
    
    if (analysis != "pa1" & analysis != "pa2" & analysis != "pa3" & analysis != "pa4" &
        analysis != "count1" & analysis != "count2" & analysis != "count3")
    {
      f1 = left_join(f1,f)
    }else{
      
      if (analysis == "pa1" | analysis == "pa2" | analysis == "pa3")
      {
        f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
        f2$se = numeric(length(ltemp$no.sp))
        f2$timegroups = ltemp$timegroups
        
        predFun = function(m1) {
          predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
        }
        
        pred = bootMer(m1, nsim = 100, FUN = predFun, parallel = "snow")
        
        for (i in 1:length(ltemp$no.sp))
        {
          f2$freq[i] = median(pred$t[,i])
          f2$se[i] = sd(pred$t[,i])
        }
        
        f2$freqt = cloglog(f2$freq,inverse = T)
        f2$cl = cloglog((f2$freq-f2$se),inverse = T)
        f2$set = f2$freqt-f2$cl
        
        fx = f2 %>%
          group_by(timegroups) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n())) 
        
        f1 = left_join(f1,fx)

      }
      
      if (analysis == "count1" | analysis == "count2")
      {
        fd = predict(m1, newdata = newdata,
                     type = "response", se.fit = T, allow.new.levels = T)
        newdata$freq = as.numeric(fd$fit)
        newdata$se = as.numeric(fd$se.fit)
        
        fx = newdata %>%
          group_by(timegroups) %>% summarize(freq = mean(freq), se = sqrt(sum(se^2)/n())) 

        f1 = left_join(f1,fx)

      }
    }
    
    f1$timegroups = factor(f1$timegroups, levels = c("before 1999","2000-2005","2006-2010",
                                                     "2011-2013","2014","2015","2016","2017","2018"))
    f1 = f1[order(f1$method,f1$timegroups),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 1999","2000-2005","2006-2010",
                                    "2011-2013","2014","2015","2016","2017","2018"), 
                    timegroups = as.numeric(c(1995,2003,2008,2012,2014,2015,2016,2017,2018)))
    f1 = left_join(f1,mp)
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



plottrends = function(trends,type = "species",singlespecies = NA,selectspecies = NA,smethod = "pa2",recent = F)
{
  require(tidyverse)
  require(ggthemes)
  
  theme_set(theme_tufte())
  
  modtrends = trends

  cols = c("#D55E00", "#E69F00", "#56B4E9", "#CC79A7", "#999999", "#F0E442", "#009E73", "#0072B2")
  
  bksm = c("trivial pa","gridded pa","pa1","pa2","pa3")
  lbsm = c("country average","grided average","model 1","model 2","model 3")
  nm = length(unique(trends$method))
  ns = length(selectspecies)
  
  if (isTRUE(recent))
  {
    tg = c(2014,2015,2016,2017,2018)
  }else
  {
    tg = unique(modtrends$timegroups)
  }
  
  
  
  ################# by species ########################
  
  if (type == "species")
  {
    modtrends = modtrends %>%
      filter(method == smethod)
    
    cols1 = cols[c(1:ns)]
    bks1 = selectspecies
    lbs1 = selectspecies
    
    recenttrends = modtrends %>%
      filter(timegroups %in% tg) %>%
      filter(species %in% selectspecies) %>%
      group_by(species) %>% mutate(freq1 = freq[1]) %>% ungroup() %>%
      group_by(species) %>% mutate(se1 = se[1]) %>% ungroup() %>%
      mutate(nmfreqbyspec = as.numeric(errordiv(freq,freq1,se,se1)[,1])) %>%
      mutate(nmsebyspec = as.numeric(errordiv(freq,freq1,se,se1)[,2])) %>%
      mutate(nmfreq = freq/max(freq1))
    recenttrends$nmsebyspec[recenttrends$timegroups == min(recenttrends$timegroups)] = 0
    
    recenttrends$species = factor(recenttrends$species, levels = selectspecies)
    
    temp = recenttrends
    
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
      geom_point(size = 3) +
      geom_line(aes(group = species),size = 1.5) +
      geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec),
                      ymax = (nmfreqbyspec + nmsebyspec), fill = species), colour = NA, alpha = 0.1) +
      xlab("years") +
      ylab("frequency of reporting")
    
    ggp1 = ggp +
      theme(axis.title.x = element_text(size = 18), axis.text.x = element_text(size = 14),
            axis.title.y = element_text(angle = 90, size = 18), axis.text.y = element_text(size = 16)) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 16)) +
      scale_colour_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_fill_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_y_continuous(limits = c(0,1.8))
      #theme(legend.position = "none")
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
    
    temp = recenttrends[recenttrends$species %in% selectspecies,]
    
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspecmeth, colour=method)) + 
      geom_point(size = 3) +
      geom_line(aes(group = method),size = 1.5) +
      ggtitle(selectspecies) +
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


errordiv = function(x1,x2,se1,se2)
{
  r = x1/x2
  t = data.frame(se1/x1,se2/x2)
  ser = r*sqrt(t[,1]^2 + t[,2]^2)
  a = data.frame(freq = numeric(length(r)))
  a$freq = r
  a$se = ser
  return(a)
}


## funtion to calculate slope from trends


calculateslope = function(trends)
{
  require(tidyverse)
  
  sp = unique(trends$species)
  corr = data.frame(species = sp, recentslope = 0, recentse = 0, histslope = 0, histse = 0, recentp = 0,
                    histp = 0, recentfreqi = 0, histfreqi = 0, recentsei = 0, histsei = 0)
  for (i in 1:length(sp))
  {
    data = trends %>% filter(species == sp[i])
    samp = data.frame(timegroups = rep(data$timegroups, each = 50))
    samp$freq = 0
    for (j in 1:length(data$timegroups))
    {
      samp$freq[(50*(j-1)+1):(50*j)] = rnorm(50,data$freq[j],data$se[j])
    }
    sampsum = samp %>%
      group_by(timegroups) %>% summarize(freq2 = mean(freq), se2 = sd(freq)) %>% ungroup
    samp = left_join(samp,sampsum)
    
    recents = samp[samp$timegroups >= 2014,]
    
    samp$freq = samp$freq - samp$freq2[1]
    recents$freq = recents$freq - recents$freq2[1]
    
    samp$timegroups = samp$timegroups - min(samp$timegroups)
    recents$timegroups = recents$timegroups - min(recents$timegroups)
    hist = with(samp, summary(lm(freq~0+timegroups)))
    recent = with(recents, summary(lm(freq~0+timegroups)))
    
    corr$recentslope[i] = recent$coefficients[1,1]
    corr$recentse[i] = recent$coefficients[1,2]
    corr$histslope[i] = hist$coefficients[1,1]
    corr$histse[i] = hist$coefficients[1,2]
    corr$recentp[i] = recent$coefficients[1,4]
    corr$histp[i] = hist$coefficients[1,4]
    
    samp = samp %>%
      mutate(freq1 = freq2[1]) %>% ungroup() %>%
      mutate(se1 = se2[1]) %>% ungroup()
    
    recents = recents %>%
      mutate(freq1 = freq2[1]) %>% ungroup() %>%
      mutate(se1 = se2[1]) %>% ungroup()
    
    corr$recentfreqi[i] = recents$freq1[1]
    corr$recentsei[i] = recents$se1[1]
    corr$histfreqi[i] = samp$freq1[1]
    corr$histsei[i] = samp$se1[1]
  }
  
  corr$recentfreqp = as.numeric(errordiv(corr$recentslope,corr$recentfreqi,
                                         corr$recentse,corr$recentsei)[,1])*100
  corr$recentsep = as.numeric(errordiv(corr$recentslope,corr$recentfreqi,
                                       corr$recentse,corr$recentsei)[,2])*100
  corr$histfreqp = as.numeric(errordiv(corr$histslope,corr$histfreqi,
                                       corr$histse,corr$histsei)[,1])*100
  corr$histsep = as.numeric(errordiv(corr$histslope,corr$histfreqi,
                                     corr$histse,corr$histsei)[,2])*100
  
  corr[corr$recentp > 0.05,]$recentp = 1
  corr[corr$recentp < 0.05,]$recentp = 0
  #corr[corr$histp > 0.05,]$histp = 1
  corr[corr$histp < 0.05,]$histp = 0
  
  corr1 = corr[,c(1,6,7,12,13,14,15)]
  names(corr1)[4:7] = c("recentchange","recenterror","historicalchange","historicalerror")
  corr1 = corr1[,c(1,2,4,5)]
  
  return(corr1)
}