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