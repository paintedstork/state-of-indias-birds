####################################################################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "ebd_IN_relApr-2019.txt")
{
  require(lubridate)
  require(tidyverse)
  
  # select only necessary columns
  preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
             "LOCALITY.ID","LOCALITY.TYPE",
             "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
             "PROTOCOL.TYPE","DURATION.MINUTES","EFFORT.DISTANCE.KM",
             "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")
  
  # CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
  # SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
  # LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
  # LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
  # TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
  # PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
  # EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
  # ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
  # GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
  # SAMPLING.EVENT.IDENTIFIER - unique checlist ID
  
  nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
  nms = names(nms)
  nms[!(nms %in% preimp)] = "NULL"
  nms[nms %in% preimp] = NA
  
  # read data from certain columns only
  data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
  
  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(CATEGORY == "species") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  
  write.csv(temp,"indiaspecieslist.csv", row.names=FALSE)
  
  ## choosing important columns required for further analyses
  
  imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          #"LOCALITY.ID","LOCALITY.TYPE",
          "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
          "PROTOCOL.TYPE",
          "DURATION.MINUTES","EFFORT.DISTANCE.KM",
          "ALL.SPECIES.REPORTED","group.id")
  

  # no of days in every month, and cumulative number
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  # create a column "group.id" which can help remove duplicate checklists
  data = data %>%
    mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER))
  
  ## setup eBird data ##
  
  ## filter species, slice by single group ID, remove repetitions
  ## remove repeats by retaining only a single group.id + species combination
  ## set date, add month, year and day columns using package LUBRIDATE
  ## add number of species/list length column (no.sp), for list length analyses (lla)
  
  
  data = data %>%
    group_by(group.id,COMMON.NAME) %>% slice(1) %>% ungroup %>%
    dplyr::select(imp) %>%
    mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
           month = month(OBSERVATION.DATE),
           day = day(OBSERVATION.DATE) + cdays[month], 
           #week = week(OBSERVATION.DATE),
           #fort = ceiling(day/14),
           cyear = year(OBSERVATION.DATE)) %>%
    dplyr::select(-c("OBSERVATION.DATE")) %>%
    mutate(year = ifelse(day <= 151, cyear-1, cyear)) %>%
    group_by(group.id) %>% mutate(no.sp = n_distinct(COMMON.NAME)) %>%
    ungroup
  

  assign("data",data,.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data")), pos = ".GlobalEnv")
  
  # save workspace
  save.image("data.RData")
  rm(data, pos = ".GlobalEnv")
}

##########################################################################################


## requires shapefiles and several packages - path1 = India; path2 = India States; 
## path3 = India Districts
## provide path to folder and name of file within

## this can be edited for more flexibility with grid sizes; current default is 25,50,100,200

## current default args are c("India","India_2011","India States","IndiaStates_2011","India Districts","IndiaDistricts_2011")

## saves a workspace image called "maps.RData"

createmaps = function(g1=25,g2=50,g3=100,g4=200,path1="India",name1="India_2011",path2="India States",
                      name2="IndiaStates_2011",path3="India Districts",name3="IndiaDistricts_2011")
{
  require(tidyverse)
  require(rgdal)
  require(sp)
  require(raster)
  
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
  nb4g1 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE) # creates list of neighbours
  nb8g1 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE) # creates list of neighbours
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame") # SGDF to SPDF
  assign("nb4g1",nb4g1,.GlobalEnv)
  assign("nb8g1",nb8g1,.GlobalEnv)
  assign("gridmapg1",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g2*1000/111111,g2*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  nb4g2 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE)
  nb8g2 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE)
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("nb4g2",nb4g2,.GlobalEnv)
  assign("nb8g2",nb8g2,.GlobalEnv)
  assign("gridmapg2",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g3*1000/111111,g3*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  nb4g3 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE)
  nb8g3 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE)
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("nb4g3",nb4g3,.GlobalEnv)
  assign("nb8g3",nb8g3,.GlobalEnv)
  assign("gridmapg3",sp_grd_poly,.GlobalEnv)
  
  bb = bbox(indiamap)
  cs = c(g4*1000/111111,g4*1000/111111) 
  cc = bb[, 1] + (cs/2)  # cell offset
  cd = ceiling(diff(t(bb))/cs)  # number of cells per direction
  grd = GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
  sp_grd = SpatialGridDataFrame(grd, data=data.frame(id=1:prod(cd)))
  nb4g4 = gridIndex2nb(sp_grd, maxdist = sqrt(1), fullMat = TRUE)
  nb8g4 = gridIndex2nb(sp_grd, maxdist = sqrt(2), fullMat = TRUE)
  sp_grd_poly = as(sp_grd, "SpatialPolygonsDataFrame")
  assign("nb4g4",nb4g4,.GlobalEnv)
  assign("nb8g4",nb8g4,.GlobalEnv)
  assign("gridmapg4",sp_grd_poly,.GlobalEnv)
  
  # indiamap = spTransform(indiamap,CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  # not required here, CRS is NA
  
  # to calculate total number of grids of each size
  c1 = gridmapg1-indiamap
  c2 = gridmapg2-indiamap
  c3 = gridmapg3-indiamap
  c4 = gridmapg4-indiamap
  
  totalcells = c(length(unique(fortify(c1)$id)),length(unique(fortify(c2)$id)),
                 length(unique(fortify(c3)$id)),length(unique(fortify(c4)$id)))
  
  assign("totalcells",totalcells,.GlobalEnv)
  assign("gridlevels",c(g1,g2,g3,g4),.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", 
                                            "gridmapg2", "gridmapg3", "gridmapg4","nb4g1",
                                            "nb4g2", "nb4g3", "nb4g4", "nb8g1", "nb8g2", "nb8g3", 
                                            "nb8g4", "totalcells", "gridlevels")), pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("nb4g1", "nb4g2", "nb4g3", "nb4g4", 
                                            "nb8g1", "nb8g2", "nb8g3", "nb8g4",
                                            "totalcells","gridlevels")), pos = ".GlobalEnv")
  
  save.image("neighbours.RData")
  
  load("maps.RData", envir = globalenv())
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", "gridmapg2", 
                                            "gridmapg3", "gridmapg4",
                                            "totalcells","gridlevels")), pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(districtmap,statemap,indiamap,gridmapg1,gridmapg2, 
       gridmapg3,gridmapg4,
       totalcells,gridlevels, pos = ".GlobalEnv")
}


######################################################################################


## prepare data for analyses, add map variables, grids
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

  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg2)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg2"

  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg3)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg3"

  temp = data %>% group_by(group.id) %>% slice(1)
  
  rownames(temp) = temp$group.id
  coordinates(temp) = ~LONGITUDE + LATITUDE
  temp = over(temp,gridmapg4)
  temp = data.frame(temp)
  temp$group.id = rownames(temp)
  data = left_join(temp,data)
  names(data)[1] = "gridg4"

  ## 
  
  assign("totalcells",totalcells,.GlobalEnv)
  assign("gridlevels",gridlevels,.GlobalEnv)
  
  assign("data",data,.GlobalEnv)
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data", "totalcells", "gridlevels")), pos = ".GlobalEnv")
    
  save.image("data.RData")
  rm(data, totalcells, gridlevels, pos = ".GlobalEnv")
  
}


########################################################################################

## remove all probable errors
## type can be "trends" or "range"
## to use in dataspeciesfilter()

completelistcheck = function(data)
{
  require(tidyverse)
  require(lubridate)
  
  # create 2 columns from the "TIME.OBSERVATIONS.STARTED' column
  temp = data.frame(data$TIME.OBSERVATIONS.STARTED)
  temp = temp %>%
    separate(data.TIME.OBSERVATIONS.STARTED, c("hr","min"))
  data = cbind(data,temp)
  
  # calculate speed and species/unit time (sut)
  data = data %>%
    mutate(speed = EFFORT.DISTANCE.KM*60/DURATION.MINUTES,
           sut = no.sp*60/DURATION.MINUTES) %>%
    mutate(hr = as.numeric(hr), min = as.numeric(min)) %>%
    mutate(end = floor((hr*60+min+DURATION.MINUTES)/60)) # caluclate time checklist ended
  
  temp = data %>%
    filter(ALL.SPECIES.REPORTED == 1, PROTOCOL.TYPE != "Incidental") %>%
    group_by(group.id) %>% slice(1)
  
  # exclude any list that may in fact be incomplete
  # set threshholds for speed and sut
  
  vel = 40
  time = 2
  
  # choose checklists without info on duration with 3 or fewers species
  grp = temp %>%
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    distinct(group.id)
  
  # exclude records based on verious criteria 
  data = data %>%
    mutate(ALL.SPECIES.REPORTED = 
             case_when(ALL.SPECIES.REPORTED == 1 & (group.id %in% grp | speed > vel |
                                                      (sut < time & no.sp <= 3) | 
                                                      PROTOCOL.TYPE == "Incidental" | 
                                                      (!is.na(hr) & ((hr <= 4 & end <= 4) | 
                                                                       (hr >= 20 & end <= 28)))) ~ 0, 
                       ALL.SPECIES.REPORTED == 0 ~ 0,
                       TRUE ~ 1))
  
  data = data %>%
    select(-speed,-sut,-hr,-min,-end)
}

################################################

## remove vagrants
## to use in dataspeciesfilter()

removevagrants = function(data)
{
  names = read.csv("indiaspecieslist.csv")
  
  migstatus = read.csv("Migratory Status - Migratory Status.csv")
  migstatus = left_join(migstatus,names,by = c("eBird.Scientific.Name" = "SCIENTIFIC.NAME"))
  migspecies = migstatus %>%
    filter(Altitudinal.Migrant == 1 | Summer.Visitor == 1 | Winter.Visitor == 1 | 
             Strictly.Passage == 1) %>%
    select(COMMON.NAME)
  migspecies = as.vector(migspecies$COMMON.NAME)
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4,month,COMMON.NAME) %>% summarize (nyear = n_distinct(year)) %>% ungroup %>%
    filter(nyear <= 3) %>% select(gridg4,month,COMMON.NAME)
  
  d = left_join(d,data)
  d = d %>%
    filter(year > 2013)
  
  data = anti_join(data,d)
  return(data)
}



#################################################################

## select species for State of India's Birds, and species for historical and recent trends
## includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(datapath = "data.RData",listlimit = 15,gridlimit = 4)
{
  require(tidyverse)
  
  load(datapath)
  
  data = data %>%
    filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50)
  
  names = read.csv("indiaspecieslist.csv")
  
  diu = read.csv("Activity - Activity.csv")
  end = read.csv("Endemicity - Endemicity.csv")
  ess = read.csv("Select Species from List - Select Species from List.csv")
  
  diu = left_join(diu,names, by = c("eBird.Scientific.Name" = "SCIENTIFIC.NAME"))
  end = left_join(end,names, by = c("eBird.Scientific.Name" = "SCIENTIFIC.NAME"))
  
  data = data %>%
    mutate(timegroups = as.character(year)) %>%
    mutate(timegroups = ifelse(year <= 1999, "before 1999", timegroups)) %>%
    #mutate(timegroups = ifelse(year >= 1990 & year <= 1999, "1990-1999", timegroups)) %>%
    mutate(timegroups = ifelse(year > 1999 & year <= 2006, "2000-2006", timegroups)) %>%
    mutate(timegroups = ifelse(year > 2006 & year <= 2010, "2007-2010", timegroups)) %>%
    mutate(timegroups = ifelse(year > 2010 & year <= 2012, "2011-2012", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2013, "2013", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2014, "2014", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2015, "2015", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2016, "2016", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2017, "2017", timegroups)) %>%
    mutate(timegroups = ifelse(year == 2018, "2018", timegroups))
  
  data = completelistcheck(data)
  data = removevagrants(data)
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% summarize(lists = n_distinct(group.id), year = round(median(year)))
  
  assign("data",data,.GlobalEnv)
  assign("databins",databins,.GlobalEnv)
  
  datah = data %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species") %>%
    group_by(COMMON.NAME,timegroups) %>% summarize(lists = n(), cells = n_distinct(gridg4)) %>%
    filter(lists > listlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% summarize(years = n()) %>%
    filter(years == 10) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species", year > 2013) %>%
    group_by(COMMON.NAME,year) %>% summarize(lists = n(), cells = n_distinct(gridg4)) %>%
    filter(lists > listlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% summarize(years = n()) %>%
    filter(years == 5) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataf = data %>%
    filter(CATEGORY == "species") %>%
    distinct(COMMON.NAME)
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
  dataf = left_join(dataf,diu,by = c("COMMON.NAME"))
  dataf = left_join(dataf,end,by = c("COMMON.NAME"))
  dataf = left_join(dataf,ess,by = c("COMMON.NAME" = "species"))
  
  specieslist = dataf %>%
    filter((essential == 1 | Subcontinent == 1 | Himalayas == 1 | 
              ht == 1 | rt == 1) & (!is.na(B.Diurnal) | !is.na(NB.Diurnal))) %>%
    select(COMMON.NAME,ht,rt)
  
  sampledcells = c(length(unique(data$gridg1)),length(unique(data$gridg2)),
                   length(unique(data$gridg3)),length(unique(data$gridg4)))
    
  
  assign("sampledcells",sampledcells,.GlobalEnv)
  assign("totalcells",totalcells,.GlobalEnv)
  assign("gridlevels",gridlevels,.GlobalEnv)
  assign("specieslist",specieslist,.GlobalEnv)
  assign("fullspecieslist",dataf,.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data","specieslist","databins","fullspecieslist",
                                            "sampledcells","totalcells","gridlevels")), 
     pos = ".GlobalEnv")
  
  save.image("dataforanalyses.RData")
  
  rm(data, specieslist, databins, fullspecieslist, sampledcells, totalcells, gridlevels, 
     pos = ".GlobalEnv")
}

######################################################################################
################################################################


## ensure that the working directory has list of India's birds with scientific names 
## (just a safety mechanism for the function to work for small subsets, needs to be enabled if required)
## only need to input data, the species of interest and the complete list of India's bird species
## also groupspecs if required (a dataframe with all relevant list level info), it is defaulted to data

expandbyspecies = function(data, species)
{
  require(tidyverse)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)

  data$timegroups = as.factor(data$timegroups)
  
  ## considers only complete lists
  
  checklistinfo = data %>%
    distinct(gridg1,gridg2,gridg3,gridg4,DISTRICT,ST_NM,CATEGORY,
             LATITUDE,LONGITUDE,TIME.OBSERVATIONS.STARTED,
             OBSERVER.ID,PROTOCOL.TYPE,DURATION.MINUTES,EFFORT.DISTANCE.KM,
             ALL.SPECIES.REPORTED,
             group.id,month,year,cyear,day,no.sp,timegroups)
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  expanded = expanded %>%
    dplyr::select(-c("CATEGORY","COMMON.NAME",
                     "LATITUDE","LONGITUDE","TIME.OBSERVATIONS.STARTED",
                     "PROTOCOL.TYPE",
                     "ALL.SPECIES.REPORTED","group.id"))
  
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

freqtrends = function(data,species,specieslist,
                      databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018),
                      error=T,nsim = 1000)
{
  require(tidyverse)
  require(lme4)
  require(VGAM)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  
  specieslist = specieslist %>%
    filter(COMMON.NAME == species)
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    g1 = data.frame(timegroups = unique(data$timegroups))
    g1$freq = g1$se = NA
    g1$timegroups = factor(g1$timegroups, levels = c("before 1999","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    g1 = g1[order(g1$timegroups),]
    names(g1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 1999","2000-2006","2007-2010",
                                    "2011-2012","2013","2014","2015","2016","2017","2018"), 
                    timegroups = as.numeric(databins))
    g1 = left_join(g1,mp)
    g1$species = species
    g1 = g1 %>%
      filter(timegroups < 2014)
    
    data = data %>%
      filter(year >= 2014)
  }
  
  if (is.na(specieslist$ht) & is.na(specieslist$rt))
  {
    f1 = data.frame(timegroups = unique(data$timegroups))
    f1$freq = f1$se = NA
    f1$timegroups = factor(f1$timegroups, levels = c("before 1999","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    f1 = f1[order(f1$timegroups),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 1999","2000-2006","2007-2010",
                                    "2011-2012","2013","2014","2015","2016","2017","2018"), 
                    timegroups = as.numeric(databins))
    f1 = left_join(f1,mp)
    f1$species = species
    return(f1)
  }
  
  ## errors for wrong parameter values
  
  ## considers only complete lists
  
  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)
  
  data$month = as.factor(data$month)
  
  if (!species %in% unique(data$COMMON.NAME))
    return(paste(species,"is not a valid species name for the region selected"))
  
  
  data$timegroups = as.factor(data$timegroups)
  data$gridg = data$gridg3
  temp = data %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3,month)
  data = temp %>% left_join(data)
  
  datay = data %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
    summarize(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  
  ed = expandbyspecies(data,species)
  
  
  m1 = glmer(OBSERVATION.COUNT ~ month + 
               month:log(no.sp) + timegroups + (1|gridg3/gridg1) + (1|OBSERVER.ID), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0)
  
  
  f = data.frame(unique(data$timegroups))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month)
  
  f1 = data.frame(timegroups = unique(data$timegroups))
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  
  if (error)
  {
    predFun = function(m1) {
      predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
    }
    
    pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "snow")
    
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
  
  if(!isTRUE(error))
  {
    f2$freq = predict(m1, newdata = ltemp,
                      type="response", re.form = NA)
    f1 = f2 %>%
      group_by(timegroups) %>% summarize(freq = mean(freq))
    f1$se = NA
  }
  
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 1999","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016","2017","2018"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 1999","2000-2006","2007-2010",
                                  "2011-2012","2013","2014","2015","2016","2017","2018"), 
                  timegroups = as.numeric(databins))
  f1 = left_join(f1,mp)
  f1$species = species
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    f1 = rbind(g1,f1)
  }
  
  return(f1)
}



###########################################################

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

erroradd = function(vec)
{
  err = sqrt(sum(vec^2))
  return(err)
}




#################################################################################

## standardize trends

stdtrends = function(trends)
{
  require(tidyverse)
  
  modtrends = na.omit(trends)
  
  tg = unique(modtrends$timegroups)
  
  recenttrends = modtrends %>%
    filter(timegroups %in% tg) %>%
    group_by(species) %>% mutate(freq1 = first(freq)) %>% ungroup() %>%
    group_by(species) %>% mutate(se1 = first(se)) %>% ungroup() %>%
    mutate(nmfreqbyspec = as.numeric(errordiv(freq,freq1,se,se1)[,1])) %>%
    mutate(nmsebyspec = as.numeric(errordiv(freq,freq1,se,se1)[,2])) %>%
    mutate(nmfreq = freq/max(freq1))
  recenttrends$nmsebyspec[recenttrends$timegroups == min(recenttrends$timegroups)] = 0

  
  recenttrends$nmfreqbyspec = recenttrends$nmfreqbyspec*100
  recenttrends$nmsebyspec = recenttrends$nmsebyspec*100
  
  recenttrends = recenttrends %>%
    select(timegroupsf,timegroups,species,nmfreqbyspec,nmsebyspec)
  
  return(recenttrends)
}

###################################################################################

## calculate geometric means for composite trends

composite = function(trends, name = "unnamed group")
{
  require(tidyverse)
  
  modtrends = stdtrends(trends)
  
  compositetrends = modtrends %>%
    group_by(timegroups) %>% mutate(tempfreq = prod(nmfreqbyspec)) %>% ungroup %>%
    group_by(timegroups) %>% mutate(tempse = max(tempfreq)*sqrt(sum((nmsebyspec/nmfreqbyspec)^2))) %>% 
    ungroup %>%
    group_by(timegroups) %>% summarize(nmfreqbyspec = exp(mean(log(nmfreqbyspec))),
                                       nmsebyspec = (1/length(unique(species)))*
                                         nmfreqbyspec*max(tempse)/max(tempfreq)) %>%
    ungroup %>%
    mutate(species = name)
  
  return(compositetrends)
}



########################################################################################

################## to plot either a comparison of species trends or methods #############
## trends can be recent or historical
## input a list of species or a single species
## input a method
## returns a ggplot object
## MAXIMUM of 8 species



plottrends = function(trends,selectspecies,recent = F)
{
  require(tidyverse)
  require(ggthemes)
  
  theme_set(theme_tufte())
  
  recenttrends = trends %>%
    filter(species %in% selectspecies)
  
  cols = c("#D55E00", "#E69F00", "#56B4E9", "#CC79A7", "#999999", "#F0E442", "#009E73", "#0072B2")
  
  ns = length(selectspecies)
  
  
  cols1 = cols[c(1:ns)]
  bks1 = selectspecies
  lbs1 = selectspecies
  
  
  
  recenttrends$species = factor(recenttrends$species, levels = selectspecies)
  
  temp = recenttrends
  
  ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
    geom_point(size = 3) +
    geom_line(size = 1.5) +
    #geom_line(aes(group = species),size = 1.5) +
    #geom_hline(yintercept = 200, linetype = "dotted", size = 0.5) +
    geom_hline(yintercept = 133, linetype = "dotted", size = 0.5) +
    geom_hline(yintercept = 75, linetype = "dotted", size = 0.5) +
    geom_hline(yintercept = 50, linetype = "dotted", size = 0.5) +
    geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                    ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.1) +
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
    scale_y_continuous(breaks = c(50,75,100,133))
  #theme(legend.position = "none")
  
  
  return(ggp1)
}


## funtion to calculate slope from trends


calculatetrendslope = function(trends, species, specieslist, composite = F)
{
  require(tidyverse)
  name = species
  trends = trends %>% filter(species == name)
  
  corr = data.frame(species = name, trend = NA, trendci = NA, slope = NA, slopeci = NA)

  specieslist = specieslist %>%
    filter(COMMON.NAME == species)
  
  if (is.na(specieslist$ht) & is.na(specieslist$rt))
  {
    return(corr)
  }
  
  trends = na.omit(trends)
  if (!isTRUE(composite))
  {
    trends = stdtrends(trends)
  }
  
  if (!is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    corr$trend = tail(trends,1)$nmfreqbyspec
    corr$trendci = tail(trends,1)$nmsebyspec
  }
  
  tm = trends$timegroups
  tm = tm - min(tm)
  sl = numeric(1000)

  for (i in 1:1000)
  {
    samp = numeric(length(trends$timegroups))
    for (j in 1:length(trends$timegroups))
    {
      samp[j] = rnorm(1,trends$nmfreqbyspec[j],trends$nmsebyspec[j])
    }
    samp = samp - 100
    result = summary(lm(samp~0+tm))
    sl[i] = result$coefficients[1,1]
  }
  
  corr$slope = mean(sl)
  corr$slopeci = sd(sl)*1.96
  return(corr)
}


############################################################

## occupancy analyses for bird abundance/range
## Requires tidyverse, reshape2, data.table and unmarked####
## type = trivial, null, nosp, nosptime, nb, nosptimenb

occufreq = function(data, species, rerun = F, datatofill)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  if(rerun)
  {
    species = datatofill$species
  }
  
  load("neighbours.RData")
  
  names = read.csv("indiaspecieslist.csv")
  
  migstatus = read.csv("Migratory Status - Migratory Status.csv")
  migstatus = left_join(migstatus,names,by = c("eBird.Scientific.Name" = "SCIENTIFIC.NAME"))
  
  migstatus = migstatus %>%
    mutate(mig = 
             case_when(!is.na(Summer.Visitor) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Summer.Visitor) ~ "S",
                       !is.na(Summer.Visitor) | !is.na(Winter.Visitor) | !is.na(Strictly.Passage) ~ "W/P",
                       !is.na(Uncertain) & is.na(Resident) ~ "U",
                       TRUE ~ "R")
    ) %>%
    select(COMMON.NAME,mig)
  
  migstatus = migstatus %>%
    filter(COMMON.NAME %in% species)
  
  species = as.character(migstatus$COMMON.NAME)
  speciesf = species
  mig = migstatus$mig
  migf = mig
  
  spec = species[mig == "LM"]
  species = c(species,spec)
  mig[mig == "LM"] = "MS"
  mig = c(mig,rep("MW",length(spec)))
  
  data = data %>%
    filter(year > 2013)
  
  data = data %>%
    mutate(OBSERVATION.COUNT = replace(OBSERVATION.COUNT, !is.na(OBSERVATION.COUNT), "1"))
  
  data$OBSERVATION.COUNT = as.numeric(data$OBSERVATION.COUNT)
  
  # create dataframe to store occupancy and detection proabability 
  # estimates across species and spatial resolutions
  

  est = array(data=NA,dim=c(length(speciesf),9),
              dimnames=list(speciesf,c("detprobB","occB","occB.ci","trivB","detprobM",
                                       "occM","occM.ci","trivM","status")))
  
  if(rerun)
  {
    est[,1] = datatofill$detprobB
    est[,2] = datatofill$occB
    est[,3] = datatofill$occB.ci
    est[,4] = datatofill$trivB
    est[,5] = datatofill$detprobM
    est[,6] = datatofill$occM
    est[,7] = datatofill$occM.ci
    est[,8] = datatofill$trivM
    est[,9] = datatofill$migstatus
    
    temp = datatofill %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    
    species = species[species %in% temp$species]
    speciesf = speciesf[speciesf %in% temp$species]
    mig = mig[species %in% temp$species]
    migf = migf[speciesf %in% temp$species]
  }


  
  for(s in 1:length(species))
  {
    if (mig[s] == "S" | mig[s] == "W/P")
    {
      temp1 = data %>%
        filter(COMMON.NAME == species[s]) %>%
        distinct(month)
      
      datac = temp1 %>% left_join(data)
    }
    
    if (mig[s] == "R" | mig[s] == "U")
    {
      datac = data
    }
    
    if (mig[s] == "MS")
    {
      datac = data %>%
        filter(month %in% c(5:8))
    }
    
    if (mig[s] == "MW")
    {
      datac = data %>%
        filter(month %in% c(11:12,1:2))
    }
    
    datay = datac %>%
      group_by(gridg1,group.id) %>% slice(1) %>% ungroup %>%
      group_by(gridg1) %>% summarize(medianlla = median(no.sp)) %>%
      summarize(medianlla = round(mean(medianlla)))
    
    medianlla = datay$medianlla
    
    selexp = expandbyspecies(datac,species[s])
    
    selexp = selexp[sample(1:nrow(selexp)),]
    
    selexp$month[selexp$month %in% c(11,12,1,2)] = "Win"
    selexp$month[selexp$month %in% c(3,4,5,6)] = "Sum"
    selexp$month[selexp$month %in% c(7,8,9,10)] = "Mon"
    
    nb8g = nb8g1
    lpg = selexp %>%
      group_by(gridg1) %>% summarize(lpg = n())
    listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
    inc = datac %>%
      mutate(gridg = gridg1)
    selexp = selexp %>% 
      arrange(gridg1) %>%
      mutate(gridg = gridg1) %>%
      group_by(gridg) %>% mutate(group.id = 1:n())
      
    
    nbt = selexp %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
    nbt$nb8 = 0
      
    fil = sum(nbt$fl)
    len = length(nbt$fl)
      
    nbti = inc %>%
      filter(COMMON.NAME == species[s]) %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
      
    fil1 = sum(nbti$fl)
    len = len-fil+fil1
    fil = fil1
      
      
    setDT(selexp)
    
    det = dcast(selexp, gridg ~ group.id, value.var = "OBSERVATION.COUNT")
    cov.month = dcast(selexp, gridg ~ group.id, value.var = "month")
    cov.nosp = dcast(selexp, gridg ~ group.id, value.var = "no.sp")
    
    det = setDF(det)
    cov.month = setDF(cov.month)
    cov.nosp = setDF(cov.nosp)
    
    det = det[,1:listcutoff]
    cov.month = cov.month[,1:listcutoff]
    cov.nosp = cov.nosp[,1:listcutoff]
      
    nbt$gridg = as.character(nbt$gridg)
    nbti$gridg = as.character(nbti$gridg)

    for (i in 1:length(nbt$gridg))
    {
      temp = as.numeric(nb8g[[nbt$gridg[i]]])
      sm = sum(nbti[nbti$gridg %in% temp,]$fl)/length(temp)
      nbt$nb8[i] = sm
    }
        
    nbt$gridg = as.character(nbt$gridg)
    tp = nbt
    tp1 = nbt %>% select(-fl)
    #tp = left_join(nbti,tp1)
    nbt = nbt[,-2]
    
    nbtx = tp[tp$fl != 1,]
    
    detn = data.frame(gridg = det[,1])
    detn= left_join(detn,nbt)

    umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb8g = detn$nb8), 
                            obsCovs = list(cov1 = cov.nosp[,-1], 
                            cov2 = cov.month[,-1]))
    
    
    if (mig[s] == "R")
    {
      occ_det = tryCatch({occu(~log(cov1)*cov2 ~nb8g, data=umf, starts = c(0,0,0,0,0,0,0,0))},
                         error = function(cond){"skip"})
      
      newdat1 = data.frame(cov1=medianlla, cov2=factor(c("Mon","Win","Sum")))
      newdat2 = data.frame(nb8g=nbtx$nb8)
    }
    
    if (mig[s] != "R")
    {
      occ_det = tryCatch({occu(~log(cov1) ~nb8g, data=umf, starts = c(0,0,0,0))},
                         error = function(cond){"skip"})
      
      newdat1 = data.frame(cov1=medianlla)
      newdat2 = data.frame(nb8g=nbtx$nb8)
    }
    

    if (!is.character(occ_det))
    {
      f1 = predict(occ_det, newdata = newdat1, type = "det")
      f1 = mean(f1$Predicted)
      f2 = predict(occ_det, newdata = newdat2, type = "state")
      f2 = f2 %>% filter(!is.na(Predicted))
      f2a = (sum(f2$Predicted) + fil)/len
      f2b = erroradd(f2$SE)/sqrt(len)*1.96
      
      
      if (mig[s] == "R" | mig[s] == "MS" | mig[s] == "S")
      {
        est[species[s],"detprobB"] =  f1
        est[species[s],"occB"] = f2a
        est[species[s],"occB.ci"] = f2b
      }
      
      if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
      {
        est[species[s],"detprobM"] =  f1
        est[species[s],"occM"] = f2a
        est[species[s],"occM.ci"] = f2b
      }

    }
    
    if (mig[s] == "R" | mig[s] == "MS" | mig[s] == "S")
    {
      est[species[s],"trivB"] = fil/len
    }
    
    if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
    {
      est[species[s],"trivM"] = fil/len
    }
  }  
  estdf = data.frame(rep(rownames(est)))
  names(estdf) = "species"

  
  estdf$detprobB = round(as.numeric(est[,1]),3)
  estdf$occB = round(as.numeric(est[,2]),3)
  estdf$occB.ci = round(as.numeric(est[,3]),3)
  estdf$trivB = round(as.numeric(est[,4]),3)
  estdf$detprobM = round(as.numeric(est[,5]),3)
  estdf$occM = round(as.numeric(est[,6]),3)
  estdf$occM.ci = round(as.numeric(est[,7]),3)
  estdf$trivM = round(as.numeric(est[,8]),3)
  
  names(migstatus)[2] = "migstatus"
  estdf = left_join(estdf,migstatus,by = c("species" = "COMMON.NAME"))

  return(estdf)
}

SoIBoccupancy = function(data,species)
{
  a = occufreq(data,species)
  c = 0
  repeat
  {
    c = c + 1
    temp = a %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    if(length(temp$species) == 0)
      break
    if(c == 5)
      break
    a = occufreq(data,species,rerun=T,datatofill=a)
  }
  return(a)
}