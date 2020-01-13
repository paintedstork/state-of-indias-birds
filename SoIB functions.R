####################################################################################

## read and clean raw data and add important columns like group id, seaonality variables
## place raw txt file (India download) in working directory 

readcleanrawdata = function(rawpath = "ebd_IN_relMay-2019.txt", 
                            sensitivepath = "Sensitive_India_may 2019.csv")
{
  require(lubridate)
  require(tidyverse)
  
  # select only necessary columns
  preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
             "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
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
  
  nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                   na.strings = c(""," ",NA))
  nms = names(nms)
  nms[!(nms %in% preimp)] = "NULL"
  nms[nms %in% preimp] = NA
  
  # read data from certain columns only
  data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                    stringsAsFactors = F, na.strings = c(""," ",NA))

  # read sensitive species data
  nms = nms[-47]
  sesp = read.csv(sensitivepath, colClasses = nms, stringsAsFactors = F)
  stdformat = data.frame(date = as.character(sesp$OBSERVATION.DATE))
  stdformat = stdformat %>%
    separate(date, c("month","day","year"), "/")
  stdformat$year = as.numeric(stdformat$year)
  sesp$OBSERVATION.DATE = paste(stdformat$year,"-",stdformat$month,"-",stdformat$day, sep = "")
  sesp = sesp %>% mutate(GROUP.IDENTIFIER = ifelse(GROUP.IDENTIFIER == "", NA, GROUP.IDENTIFIER))

  # merge both data frames
  data = rbind(data,sesp)
  
  data = data %>%
    filter(!COMMON.NAME %in% c("Western Orphean Warbler"),
           (!COMMON.NAME %in% c("Jungle Prinia") | !STATE %in% c("Uttarakhand","Jammu and Kashmir",
                                                                "Chandigarh","Punjab",
                                                                "Himachal Pradesh") | 
             COUNTY %in% c("Hardwar")),
           (!COMMON.NAME %in% c("Orange Minivet") | !STATE %in% c("Gujarat") | 
              COUNTY %in% c("Narmada","Navsari","The Dangs")),
           (!COMMON.NAME %in% c("Red-breasted Flycatcher") | !STATE %in% c("West Bengal")),
           (!COMMON.NAME %in% c("Brown-cheeked Fulvetta") | !STATE %in% c("Sikkim")),
           (!COMMON.NAME %in% c("Chestnut-shouldered Petronia") | !COUNTY %in% 
              c("Darjiling","Jalpaiguri")),
           (!COMMON.NAME %in% c("Indian Robin") | 
              (!STATE %in% c("Arunachal Pradesh","Mizoram","Manipur",
                            "Sikkim","Nagaland","Tripura",
                            "Assam") & 
              !COUNTY %in% c("Darjiling","Jalpaiguri","Koch Bihar"))),
           (!COMMON.NAME %in% c("Brown-capped Woodpecker") | 
              (!STATE %in% c("Arunachal Pradesh","Mizoram","Manipur",
                             "Sikkim","Nagaland","Tripura",
                             "Assam","Jammu and Kashmir") & 
                 !COUNTY %in% c("Darjiling","Jalpaiguri","Koch Bihar","Dehradun","Almora","Shimla",
                                "Mandi","Kangra","Chamba","Kullu"))),
           (!COMMON.NAME %in% c("Rusty-throated Wren-Babbler") | !STATE %in% c("Manipur","Mizoram",
                                                                               "Nagaland")),
           (!COMMON.NAME %in% c("Gray Sibia") | !STATE %in% c("Arunachal Pradesh")),
           (!COMMON.NAME %in% c("Indian Silverbill") | 
              (!STATE %in% c("Arunachal Pradesh","Mizoram","Manipur",
                            "Sikkim","Nagaland","Tripura",
                            "Assam") & 
              !COUNTY %in% c("Darjiling","Jalpaiguri","Koch Bihar"))))
  

  # create and write a file with common names and scientific names of all Indian species
  # useful for mapping
  temp = data %>%
    filter(REVIEWED == 0 | APPROVED == 1) %>%
    filter(CATEGORY == "species" | CATEGORY == "issf") %>%
    distinct(COMMON.NAME,SCIENTIFIC.NAME)
  
  write.csv(temp,"indiaspecieslist.csv", row.names=FALSE)
  
  ## choosing important columns required for further analyses
  
  imp = c("CATEGORY","COMMON.NAME","OBSERVATION.COUNT",
          "LOCALITY.ID", "REVIEWED","APPROVED",
          #"LOCALITY.TYPE",
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
  
  data = data %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Green Warbler", "Greenish Warbler")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Green/Greenish Warbler",
                              "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Green/Greenish Warbler",
                                 "Greenish Warbler")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Hume's Whitethroat", 
                                 "Lesser Whitethroat")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Desert Whitethroat", 
                                 "Lesser Whitethroat")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Sylvia sp.",
                              "species")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Hume's/Lesser Whitethroat",
                              "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Sylvia sp.",
                                 "Lesser Whitethroat")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Hume's/Lesser Whitethroat",
                                 "Lesser Whitethroat")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Sykes's Short-toed Lark", 
                                 "Greater Short-toed Lark")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Greater/Sykes's Short-toed Lark",
                              "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Greater/Sykes's Short-toed Lark", 
                                 "Greater Short-toed Lark")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Taiga Flycatcher", 
                                 "Red-breasted Fycatcher")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Taiga/Red-breasted Flycatcher",
                              "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Taiga/Red-breasted Flycatcher", 
                                 "Red-breasted Fycatcher")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Changeable Hawk-Eagle", 
                               "Crested Hawk-Eagle")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Changeable/Crested Hawk-Eagle",
                            "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Changeable/Crested Hawk-Eagle", 
                                 "Crested Hawk-Eagle")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Chestnut Munia", 
                                 "Tricolored Munia")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Tricolored x Chestnut Munia (hybrid)",
                                "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Tricolored x Chestnut Munia (hybrid)", 
                                   "Tricolored Munia")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "House Swift", 
                                 "Little Swift")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Little/House Swift",
                                "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Little/House Swift", 
                                   "Little Swift")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Sykes's Warbler", 
                                 "Booted Warbler")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Booted/Sykes's Warbler",
                              "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Booted/Sykes's Warbler", 
                                 "Booted Warbler")) %>%
    mutate(CATEGORY = replace(CATEGORY, COMMON.NAME == "Iduna sp.",
                              "species")) %>%
    mutate(COMMON.NAME = replace(COMMON.NAME, COMMON.NAME == "Iduna sp.", 
                                 "Booted Warbler"))
  
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
  
  data = data %>% filter(year < 2019)
  
  ## remove probable mistakes
  
  data = data %>%
    filter(!group.id %in% c("S52427664","S52427820","S56787688","S52402064","S53042612",
                            "S33740440"),
           !COMMON.NAME == "Nilgiri Pipit" | !group.id %in% c("S29668163","S29668256"),
           !COMMON.NAME == "Ashambu Laughingthrush" | !group.id %in% c("S56229913","S56933026"),
           !COMMON.NAME == "Asian Barred Owlet" | !group.id %in% c("G2041445"),
           !COMMON.NAME == "Blue-eared Kingfisher" | !group.id %in% c("S32961812"),
           !COMMON.NAME == "Blue-capped Redstart" | !group.id %in% c("S53495281"),
           !COMMON.NAME == "Painted Bush-Quail" | !group.id %in% c("S27051065"),
           !COMMON.NAME == "Blue-fronted Redstart" | !group.id %in% c("S21233304"),
           !COMMON.NAME == "Brown Hornbill" | !group.id %in% c("S43551831"),
           !COMMON.NAME == "Common Hill Myna" | !group.id %in% c("S52357374","S53043299",
                                                                 "G3013891","S42764844",
                                                                 "S46360523","S27918190"),
           !COMMON.NAME == "Brown-cheeked Fulvetta" | !group.id %in% c("S44659529","S40566180",
                                                                 "G3974238"),
           !COMMON.NAME == "Common Raven" | !group.id %in% c("G2347843","S34953487","S49490692"),
           !COMMON.NAME == "Crimson-backed Sunbird" | !group.id %in% c("G2337363","S41299024","S42586621",
                                                                       "S44235135","S55677832"),
           !COMMON.NAME == "Fire-tailed Myzornis" | !group.id %in% c("S42743967"),
           !COMMON.NAME == "Flame-throated Bulbul" | !group.id %in% c("S46388374"),
           !COMMON.NAME == "Golden-fronted Leafbird" | !group.id %in% c("S51010614","S20859862",
                                                                        "S35527622","S40801323",
                                                                        "S52222471","S42293210",
                                                                        "S42565278","G2925840",
                                                                        "G2925839"),
           !COMMON.NAME == "Greater Adjutant" | !group.id %in% c("S42194637","S34314503"),
           !COMMON.NAME == "Jungle Owlet" | !group.id %in% c("S29235308","S27617824",
                                                             "S32956267","S40541020",
                                                             "S56307563","S56320053",
                                                             "S56321414","S56345734",
                                                             "G2353309","S22537736",
                                                             "S56129491","S50441851",
                                                             "S50442387","G1486831",
                                                             "S32601844"),
           !COMMON.NAME == "Large Woodshrike" | !group.id %in% c("G2907799","S23455075"),
           !COMMON.NAME == "Lesser Racket-tailed Drongo" | !group.id %in% c("S49094276"),
           !COMMON.NAME == "Long-billed Sunbird" | !group.id %in% c("S54163831","S52859270"),
           !COMMON.NAME == "Malabar Gray Hornbill" | !group.id %in% c("S40060028"),
           !COMMON.NAME == "Gray Sibia" | !group.id %in% c("S29321497"),
           !COMMON.NAME == "Nicobar Imperial-Pigeon" | !group.id %in% c("G3671404","S30505431",
                                                                        "S32591199","S34756528",
                                                                        "S34773949","S34779113",
                                                                        "S24322442"),
           !COMMON.NAME == "Nicobar Parakeet" | !group.id %in% c("S56338976","S55428987",
                                                                 "S50021690","S42333230"),
           !COMMON.NAME == "Nilgiri Sholakili" | !group.id %in% c("G2809524"),
           !COMMON.NAME == "Palani Laughingthrush" | !group.id %in% c("S37500763","S31503751",
                                                                      "G1050022","S26314587",
                                                                      "G3893563"),
           !COMMON.NAME == "Green Imperial-Pigeon" | !group.id %in% c("S42404810","S54172419",
                                                                      "S54171884","S19503596"),
           !COMMON.NAME == "Rufous-fronted Prinia" | !group.id %in% c("S40763806","S36754186",
                                                                      "S33722312","S47874534",
                                                                      "S55205625","S26234020",
                                                                      "S47854569","S47903089",
                                                                      "S42242712","S22225425",
                                                                      "S44388179","G1261451",
                                                                      "S47862934","S46497353",
                                                                      "S47901972","S45790541"),
           !COMMON.NAME == "Scaly Laughingthrush" | !group.id %in% c("S34973616"),
           !COMMON.NAME == "Sikkim Treecreeper" | !group.id %in% c("G4224790","S21504333"),
           !COMMON.NAME == "Slaty-headed Parakeet" | !group.id %in% c("G1588346"),
           !COMMON.NAME == "Southern Hill Myna" | !group.id %in% c("G3967773"),
           !COMMON.NAME == "White-bellied Woodpecker" | !group.id %in% c("G2804044","S37750658"),
           !COMMON.NAME == "Chestnut-crowned Laughingthrush" | !group.id %in% c("G1098538","G3643918",
                                                                                "G3665046","S35804354",
                                                                                "S47949100","S50609308"),
           !COMMON.NAME == "White-browed Fantail" | !group.id %in% c("S17017380","S22224770",
                                                                     "S19422597","G1506668")
           )
  
  assign("data",data,.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data")), pos = ".GlobalEnv")
  
  # save workspace
  save.image("rawdata.RData")
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
  require(sf)

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
  
  x = as(indiamap,"sf") %>% sf::st_buffer(dist=0)
  # to calculate total number of grids of each size
  c1 = st_intersection(as(gridmapg1,"sf"), x)
  areag1 = data.frame(id = as.character(c1$id), area = round(st_area(c1)*12345.65))
  c1 = as(c1,"Spatial")
  c2 = st_intersection(as(gridmapg2,"sf"), x)
  areag2 = data.frame(id = as.character(c2$id), area = round(st_area(c2)*12345.65))
  c2 = as(c2,"Spatial")
  c3 = st_intersection(as(gridmapg3,"sf"), x)
  areag3 = data.frame(id = as.character(c3$id), area = round(st_area(c3)*12345.65))
  c3 = as(c3,"Spatial")
  c4 = st_intersection(as(gridmapg4,"sf"), x)
  areag4 = data.frame(id = as.character(c4$id), area = round(st_area(c4)*12345.65))
  c4 = as(c4,"Spatial")
  area = round(st_area(x)*12345.65)
  
  
  totalcells = c(length(unique(fortify(c1)$id)),length(unique(fortify(c2)$id)),
                 length(unique(fortify(c3)$id)),length(unique(fortify(c4)$id)))
  
  assign("areag1",areag1,.GlobalEnv)
  assign("areag2",areag2,.GlobalEnv)
  assign("areag3",areag3,.GlobalEnv)
  assign("areag4",areag4,.GlobalEnv)
  assign("totalcells",totalcells,.GlobalEnv)
  assign("area",area,.GlobalEnv)
  assign("gridlevels",c(g1,g2,g3,g4),.GlobalEnv)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", 
                                            "gridmapg2", "gridmapg3", "gridmapg4","nb4g1",
                                            "nb4g2", "nb4g3", "nb4g4", "nb8g1", "nb8g2", "nb8g3", 
                                            "nb8g4", "totalcells", "gridlevels","area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("nb4g1", "nb4g2", "nb4g3", "nb4g4", 
                                            "nb8g1", "nb8g2", "nb8g3", "nb8g4",
                                            "totalcells","gridlevels","area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
  
  save.image("neighbours.RData")
  
  load("maps.RData", envir = globalenv())
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("districtmap", "statemap", "indiamap", "gridmapg1", "gridmapg2", 
                                            "gridmapg3", "gridmapg4",
                                            "totalcells","gridlevels","area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
  
  save.image("maps.RData")
  
  rm(districtmap,statemap,indiamap,gridmapg1,gridmapg2, 
       gridmapg3,gridmapg4,
       totalcells,gridlevels,area,
       areag1, areag2, areag3, areag4, pos = ".GlobalEnv")
}


######################################################################################


## prepare data for analyses, add map variables, grids
## place the 'maps' workspace in working directory

addmapvars = function(datapath = "rawdata.RData", mappath = "maps.RData")
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
  
  assign("area",area,.GlobalEnv)
  assign("areag1",areag1,.GlobalEnv)
  assign("areag2",areag2,.GlobalEnv)
  assign("areag3",areag3,.GlobalEnv)
  assign("areag4",areag4,.GlobalEnv)
  assign("totalcells",totalcells,.GlobalEnv)
  assign("gridlevels",gridlevels,.GlobalEnv)
  
  regions = read.csv("districtlist.csv")
  data = left_join(data,regions)
  
  assign("data",data,.GlobalEnv)
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data", "totalcells", "gridlevels", "area",
                                            "areag1","areag2","areag3","areag4")), 
     pos = ".GlobalEnv")
    
  save.image("data.RData")
  rm(data, totalcells, gridlevels, area,
     areag1, areag2, areag3, areag4, pos = ".GlobalEnv")
  
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
  
  vel = 20
  time = 2
  
  # choose checklists without info on duration with 3 or fewers species
  grp = temp %>%
    filter(no.sp <= 3, is.na(DURATION.MINUTES)) %>%
    distinct(group.id)
  grp = grp$group.id
  
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
  migstatus = read.csv("Migratory Status - Migratory Status.csv")
  migspecies = migstatus %>%
    filter(Summer.Visitor == 1 | Winter.Visitor == 1 | 
             Strictly.Passage == 1) %>%
    select(eBird.English.Name)
  migspecies = as.vector(migspecies$eBird.English.Name)
  
  d = data %>%
    filter(COMMON.NAME %in% migspecies) %>%
    group_by(gridg4,month,COMMON.NAME) %>% summarize (nyear = n_distinct(year)) %>% ungroup %>%
    filter(nyear <= 3) %>% select(gridg4,month,COMMON.NAME)
  
  d = left_join(d,data)
  d = d %>%
    filter(year > 2013)
  
  save(d,file = "vagrantdata.RData")
  
  data = anti_join(data,d)
  return(data)
}



#################################################################

## select species for State of India's Birds, and species for historical and recent trends
## includes all diurnal endemics (endemicity) and essential species (SelectSpecies)

dataspeciesfilter = function(datapath = "data.RData",
                             locationlimit = 15,gridlimit = 4)
{
  require(tidyverse)
  require(DataCombine)
  
  diu = read.csv("Activity - Activity.csv")
  end = read.csv("Endemicity - Endemicity.csv")
  ess = read.csv("Select Species from List - Select Species from List.csv")
  mig = read.csv("Migratory Status - Migratory Status.csv")
  resident = as.character(mig$eBird.English.Name[!is.na(mig$Resident)])
  
  load(datapath)
  
  x1 = paste(nrow(data),"filter 0 observations")
  x2 = paste(length(unique(data$group.id)),"filter 0 unique checklists")
  
  data$gridg1 = as.character(data$gridg1)
  data$gridg2 = as.character(data$gridg2)
  data$gridg3 = as.character(data$gridg3)
  data$gridg4 = as.character(data$gridg4)
  areag1$id = as.character(areag1$id)
  areag2$id = as.character(areag2$id)
  areag3$id = as.character(areag3$id)
  areag4$id = as.character(areag4$id)
  
  ## exclude pelagic lists
  data = data %>%
    filter(!gridg1 %in% setdiff(data$gridg1, intersect(areag1$id,unique(data$gridg1))) &
             !gridg2 %in% setdiff(data$gridg2, intersect(areag2$id,unique(data$gridg2))) &
             !gridg3 %in% setdiff(data$gridg3, intersect(areag3$id,unique(data$gridg3))) &
             !gridg4 %in% setdiff(data$gridg4, intersect(areag4$id,unique(data$gridg4))) &
             !is.na(gridg1) & !is.na(gridg2) & !is.na(gridg3) & !is.na(gridg4))
  
  data = data %>%
    filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
    filter(REVIEWED == 0 | APPROVED == 1)
  
  data = data %>%
    mutate(timegroups = as.character(year)) %>%
    mutate(timegroups = ifelse(year <= 1999, "before 2000", timegroups)) %>%
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
  
  data = data %>%
    mutate(timegroups1 = as.character(year)) %>%
    mutate(timegroups1 = ifelse(year <= 2006, "before 2006", timegroups1)) %>%
    mutate(timegroups1 = ifelse(year > 2006 & year <= 2013, "2007-2013", timegroups1)) %>%
    mutate(timegroups1 = ifelse(year > 2013, "2014-2018", timegroups1))
  
  data = removevagrants(data)

  x3 = paste(nrow(data),"filter 1 observations")
  x4 = paste(length(unique(data$group.id)),"filter 1 unique checklists")
  x5 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x6 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 1 unique complete checklists")
  
  data = completelistcheck(data)
  
  x7 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
             "filter 1 usable observations")
  x8 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "filter 2 unique complete checklists")
  x9 = paste(length(unique(data[data$timegroups == "before 2000" &
                          data$ALL.SPECIES.REPORTED == 1,]$group.id)),
             "pre-2000 checklists")
  
  databins = data %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(timegroups) %>% summarize(lists = n_distinct(group.id), year = round(median(year)))
  
  data1 = data
  data1 = data1 %>% select(-CATEGORY,-LOCALITY.ID,-DISTRICT,-REVIEWED,-APPROVED,
                           -LATITUDE,-LONGITUDE,-TIME.OBSERVATIONS.STARTED,-PROTOCOL.TYPE,
                           -DURATION.MINUTES,-EFFORT.DISTANCE.KM,-day,-cyear)
  
  assign("data",data1,.GlobalEnv)
  assign("databins",databins,.GlobalEnv)
  
  datah = data %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% summarize(locs = n_distinct(LOCALITY.ID), 
                                                   cells = n_distinct(gridg4)) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% summarize(years = n()) %>%
    filter(years == 10) %>%
    mutate(ht = 1) %>% select (COMMON.NAME,ht)
  
  datar = data %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2013) %>%
    group_by(COMMON.NAME,year) %>% summarize(locs = n_distinct(LOCALITY.ID), 
                                             cells = n_distinct(gridg4)) %>%
    filter(locs > locationlimit, cells > gridlimit) %>%
    group_by(COMMON.NAME) %>% summarize(years = n()) %>%
    filter(years == 5) %>%
    mutate(rt = 1) %>% select(COMMON.NAME,rt)
  
  dataresth1 = data %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf") %>%
    group_by(COMMON.NAME,timegroups) %>% summarize(cells = n_distinct(gridg4)) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% summarize(years = n()) %>%
    filter(years == 10) %>%
    select (COMMON.NAME)
  
  speciesresth = data.frame(species = intersect(unique(dataresth1$COMMON.NAME),resident))
  speciesresth$validh = NA
  
  for (i in 1:length(speciesresth$species))
  {
    tempresth1 = data %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      distinct(gridg1)
    tempresth1 = tempresth1 %>% left_join(data)
    tempresth2 = tempresth1 %>%
      filter(COMMON.NAME == speciesresth$species[i]) %>%
      group_by(timegroups) %>% summarize(n = n_distinct(group.id)) %>%
      filter(n > 10)
    tempresth1 = tempresth1 %>%
      group_by(timegroups) %>% summarize(n = n_distinct(group.id)) %>%
      filter(n > 50)
    if (length(tempresth1$timegroups) == 10 & length(tempresth2$timegroups) == 10)
      speciesresth$validh[speciesresth$species == speciesresth$species[i]] = 1
  }
  
  datarestr1 = data %>%
    filter(ALL.SPECIES.REPORTED == 1, CATEGORY == "species" | CATEGORY == "issf", year > 2013) %>%
    group_by(COMMON.NAME,timegroups) %>% summarize(cells = n_distinct(gridg4)) %>%
    filter(cells <= gridlimit) %>%
    group_by(COMMON.NAME) %>% summarize(years = n()) %>%
    filter(years == 5) %>%
    select (COMMON.NAME)
  
  speciesrestr = data.frame(species = intersect(unique(datarestr1$COMMON.NAME),resident))
  speciesrestr$validr = NA
  
  for (i in 1:length(unique(datarestr1$COMMON.NAME)))
  {
    temprestr1 = data %>%
      filter(COMMON.NAME == speciesrestr$species[i]) %>%
      distinct(gridg1)
    temprestr1 = temprestr1 %>% left_join(data)
    temprestr2 = temprestr1 %>%
      filter(COMMON.NAME == speciesrestr$species[i],year > 2013) %>%
      group_by(timegroups) %>% summarize(n = n_distinct(group.id)) %>%
      filter(n > 20)
    temprestr1 = temprestr1 %>%
      filter(year > 2013) %>%
      group_by(timegroups) %>% summarize(n = n_distinct(group.id)) %>%
      filter(n > 100)
    if (length(temprestr1$timegroups) == 5 & length(temprestr2$timegroups) == 5)
      speciesrestr$validr[speciesrestr$species == speciesrestr$species[i]] = 1
  }
  
  dataf = data.frame(COMMON.NAME = as.character(ess$species), 
                     SCIENTIFIC.NAME = as.character(ess$scientific))
  
  dataf = left_join(dataf,datah,by = c("COMMON.NAME"))
  dataf = left_join(dataf,datar,by = c("COMMON.NAME"))
  dataf = left_join(dataf,diu,by = c("COMMON.NAME" = "eBird.English.Name"))
  dataf = left_join(dataf,end,by = c("COMMON.NAME" = "eBird.English.Name"))
  dataf = left_join(dataf,ess,by = c("COMMON.NAME" = "species"))
  
  specieslist = dataf %>%
    filter((essential == 1 | Subcontinent == 1 | Himalayas == 1 | 
              ht == 1 | rt == 1) & 
             (!is.na(B.Diurnal) | !is.na(NB.Diurnal) | COMMON.NAME == "Jerdon's Courser") & 
             (is.na(discard))) %>%
    select(COMMON.NAME,ht,rt)
  specieslist$ht[specieslist$COMMON.NAME == "Besra"] = NA
  specieslist$rt[specieslist$COMMON.NAME == "Besra"] = NA
  specieslist$ht[specieslist$COMMON.NAME == "Common Flameback"] = NA
  specieslist$rt[specieslist$COMMON.NAME == "Common Flameback"] = NA
  dataf$ht[is.na(dataf$B.Diurnal) & is.na(dataf$NB.Diurnal)] = NA
  dataf$rt[is.na(dataf$B.Diurnal) & is.na(dataf$NB.Diurnal)] = NA
  
  restrictedspecieslist  = data.frame(species = specieslist$COMMON.NAME)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesresth)
  restrictedspecieslist = left_join(restrictedspecieslist,speciesrestr)
  
  restrictedspecieslist = restrictedspecieslist %>%
    filter(!is.na(validh) | !is.na(validr))
  check1 = restrictedspecieslist$species[!is.na(restrictedspecieslist$validh)]
  check2 = restrictedspecieslist$species[!is.na(restrictedspecieslist$validr)]
  
  names(restrictedspecieslist) = c("COMMON.NAME","ht","rt")
  
  randomcheck = data %>% filter(ALL.SPECIES.REPORTED == 1, 
                                COMMON.NAME %in% restrictedspecieslist$COMMON.NAME) %>%
    group_by(COMMON.NAME) %>% summarize(n = n_distinct(gridg1)) %>% filter(n > 4)
  
  restrictedspecieslist = restrictedspecieslist %>% filter(COMMON.NAME %in% randomcheck$COMMON.NAME)
  restrictedspecieslist = restrictedspecieslist %>% filter(!COMMON.NAME %in% c("Indian Bustard"))
  
  t1 = dataf %>%
    filter((ht == 1 | rt == 1) & (!is.na(B.Diurnal) | !is.na(NB.Diurnal)))
  x10 = paste(length(t1$COMMON.NAME),"filter 1 number of species")
  t2 = dataf %>%
    filter((Subcontinent == 1 | Himalayas == 1 | 
              ht == 1 | rt == 1) & (!is.na(B.Diurnal) | !is.na(NB.Diurnal)))
  x11 = paste(length(t2$COMMON.NAME),"filter 2 number of species")
  t3 = dataf %>%
    filter((essential == 1 | Subcontinent == 1 | Himalayas == 1 | 
              ht == 1 | rt == 1) & (!is.na(B.Diurnal) | !is.na(NB.Diurnal)))
  x12 = paste(length(t3$COMMON.NAME),"filter 3 number of species")
  
  specieslist1 = specieslist
  specieslist1$selected = 1
  specieslist1 = specieslist1 %>% select(COMMON.NAME,selected)
  
  dataf = dataf %>%
    select(COMMON.NAME,SCIENTIFIC.NAME,ht,rt,Subcontinent,Himalayas,essential,discard,NB.Diurnal)
  dataf = left_join(dataf,specieslist1)
  names(dataf) = c("COMMON.NAME","SCIENTIFIC.NAME","Long-term Analysis","Current change Analysis",
                   "Subcontinental Endemics",
                   "Himalayan/NE Near-Endemics","Significant - Indian Context",
                   "Not Relevant - Indian Context","Diurnal","Selected - SOIB")
  
  sampledcells = c(length(unique(data$gridg1)),length(unique(data$gridg2)),
                   length(unique(data$gridg3)),length(unique(data$gridg4)))
  
  x = c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12)
  
  clemcheck = read.csv("eBird-Clements-v2018-integrated-checklist-August-2018.csv")
  clemcheck = clemcheck %>%
    filter(English.name %in% dataf$COMMON.NAME)
  clemcheck = data.frame(COMMON.NAME = unique(clemcheck$English.name))
  dataf = left_join(clemcheck,dataf)
  
  assign("stats",x,.GlobalEnv) 
  assign("areag1",areag1,.GlobalEnv)
  assign("areag2",areag2,.GlobalEnv)
  assign("areag3",areag3,.GlobalEnv)
  assign("areag4",areag4,.GlobalEnv)
  assign("area",area,.GlobalEnv)
  assign("sampledcells",sampledcells,.GlobalEnv)
  assign("totalcells",totalcells,.GlobalEnv)
  assign("gridlevels",gridlevels,.GlobalEnv)
  assign("specieslist",specieslist,.GlobalEnv)
  assign("restrictedspecieslist",restrictedspecieslist,.GlobalEnv)
  
  dataf[is.na(dataf)] = ""
  dataf[dataf == 1] = "X"
  
  species2018 = read.csv("eBird-Clements-v2018-integrated-checklist-August-2018.csv")
  species2018 = species2018 %>%
    filter(category == "species") %>%
    distinct(English.name,scientific.name) %>%
    filter(English.name %in% dataf$COMMON.NAME)
  names(species2018)[1] = "name2018"
  
  species2019 = read.csv("eBird-Clements-v2019-integrated-checklist-August-2019.csv")
  species2019 = species2019 %>%
    filter(category == "species") %>%
    distinct(English.name,scientific.name)
  
  change = species2019 %>% filter(English.name %in% c("Spoon-billed Sandpiper","Ludlow's Fulvetta",
                                                      "Slaty-backed Flycatcher","Brown Rock Chat",
                                                      "Yellow-bellied Flowerpecker",
                                                      "Yellow-throated Sparrow"))
  change$name2018 = c("Spoon-billed Sandpiper","Ludlow's Fulvetta",
                      "Slaty-backed Flycatcher","Indian Chat",
                      "Yellow-bellied Flowerpecker",
                      "Chestnut-shouldered Petronia")
  
  species2018$scientific.name = as.character(species2018$scientific.name)
  species2018[species2018$name2018 %in% change$name2018,]$scientific.name = 
    as.character(change$scientific.name)
  
  dataf$SCIENTIFIC.NAME = as.character(dataf$SCIENTIFIC.NAME)
  dataf[dataf$COMMON.NAME %in% change$name2018,]$SCIENTIFIC.NAME = 
    as.character(change$scientific.name)
  
  map1819 = left_join(species2018,species2019)
  map1819 = map1819[!is.na(map1819$English.name),]
  map1819 = map1819 %>% select(-name2018)
  
  dataf = left_join(dataf,map1819,by = c("SCIENTIFIC.NAME" = "scientific.name"))
  dataf = dataf[!is.na(dataf$English.name),]
  dataf$COMMON.NAME = dataf$English.name
  dataf = dataf %>% select(-English.name)
  
  pos = match("Indian Roller",dataf$COMMON.NAME)
  toadd = dataf[pos,]
  toadd$COMMON.NAME = "Indochinese Roller"
  toadd$SCIENTIFIC.NAME = "Coracias affinis"
  toadd$`Long-term Analysis` = toadd$`Current change Analysis` = toadd$`Subcontinental Endemics` =
    toadd$`Significant - Indian Context` = toadd$`Selected - SOIB` = ""
  
  dataf = InsertRow(dataf,toadd,pos+1)
  
  names(dataf)[1] = "eBird English Name"
  names(dataf)[2] = "eBird Scientific Name"
  
  dataf$'Long-term Analysis'[dataf$'eBird English Name' %in% check1] = "X"
  dataf$'Current change Analysis'[dataf$'eBird English Name' %in% check2] = "X"
  write.csv(dataf,"fullspecieslist.csv",row.names = F)
  
  rm(list=setdiff(ls(envir = .GlobalEnv), c("data","specieslist","databins",
                                            "sampledcells","totalcells","gridlevels","area",
                                            "areag1","areag2","areag3","areag4","stats",
                                            "restrictedspecieslist")), 
     pos = ".GlobalEnv")
  
  save.image("dataforanalyses.RData")
  save(specieslist,restrictedspecieslist,file = "specieslists.RData")
  
  rm(data, specieslist, databins, sampledcells, totalcells, gridlevels, area,
     areag1, areag2, areag3, areag4, stats, restrictedspecieslist, pos = ".GlobalEnv")
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
    distinct(gridg1,gridg2,gridg3,gridg4,
             ALL.SPECIES.REPORTED,OBSERVER.ID,
             #city,
             #DURATION.MINUTES,EFFORT.DISTANCE.KM,
             group.id,month,year,no.sp,timegroups,
             timegroups1,
             region)
  
  checklistinfo = checklistinfo %>%
    filter(ALL.SPECIES.REPORTED == 1) %>%
    group_by(group.id) %>% slice(1) %>% ungroup
  
  ## expand data frame to include all bird species in every list
  
  expanded = checklistinfo
  expanded$COMMON.NAME = species
  
  ## join the two, deal with NAs next
  
  expanded = left_join(expanded,data)
  expanded = expanded %>%
    dplyr::select(-c("COMMON.NAME","gridg2","gridg4","OBSERVER.ID",
                     "ALL.SPECIES.REPORTED","group.id","year"))
  
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
  require(parallel)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$region = as.factor(data$region)
  
  specieslist = specieslist %>%
    filter(COMMON.NAME == species)
  
  ## filters data based on whether the species has been selected for long-term trends (ht) 
  ## or short-term trends (rt) 
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    g1 = data.frame(timegroups = unique(data$timegroups))
    g1$se = g1$freq = NA
    g1$timegroups = factor(g1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    g1 = g1[order(g1$timegroups),]
    names(g1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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
    f1$se = f1$freq = NA
    f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    f1 = f1[order(f1$timegroups),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
                                    "2011-2012","2013","2014","2015","2016","2017","2018"), 
                    timegroups = as.numeric(databins))
    f1 = left_join(f1,mp)
    f1$species = species
    f1 = f1 %>% filter(!is.na(f1$timegroups))
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
  
  ## calculate a median list length to use to predict
  
  datay = data %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
    summarize(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data,species)
  tm = unique(data$timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg3/gridg1), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month)
  
  f1 = data.frame(timegroups = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  
  ## bootstrap to get errors
  
  if (error)
  {
    predFun = function(m1) {
      predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
    }
    
    cr = max(1, detectCores() - 4)
    cl = makeCluster(cr)
    clusterEvalQ(cl, library(lme4))
    
    pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "snow", seed = 1000,
                   use.u = FALSE, type = "parametric", ncpus = cr, cl = cl)
    
    stopCluster(cl)
    
    for (i in 1:length(ltemp$no.sp))
    {
      f2$freq[i] = median(na.omit(pred$t[,i]))
      f2$se[i] = sd(na.omit(pred$t[,i]))
    }
    
    f2$freqt = cloglog(f2$freq,inverse = T)
    f2$cl = cloglog((f2$freq-f2$se),inverse = T)
    f2$set = f2$freqt-f2$cl
    
    fx = f2 %>%
      filter(!is.na(freqt) & !is.na(set)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n())) 
    
    f1 = left_join(f1,fx)
  }
  
  if(!isTRUE(error))
  {
    f2$freq = predict(m1, newdata = ltemp,
                      type="response", re.form = NA)
    f1 = f2 %>%
      filter(!is.na(freq)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freq))
    f1$se = NA
  }
  
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016","2017","2018"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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
  
  tg = trends %>%
    distinct(timegroups,timegroupsf)
  
  compositetrends = modtrends %>%
    group_by(timegroups) %>% mutate(tempfreq = prod(nmfreqbyspec)) %>% ungroup %>%
    group_by(timegroups) %>% mutate(tempse = max(tempfreq)*sqrt(sum((nmsebyspec/nmfreqbyspec)^2))) %>% 
    ungroup %>%
    group_by(timegroups) %>% summarize(nmfreqbyspec = exp(mean(log(nmfreqbyspec))),
                                       nmsebyspec = (1/length(unique(species)))*
                                         nmfreqbyspec*max(tempse)/max(tempfreq)) %>%
    ungroup %>%
    mutate(species = name)
  
  compositetrends = left_join(compositetrends,tg)
  
  return(compositetrends)
}



########################################################################################

################## to plot either a comparison of species trends or methods #############
## trends can be recent or historical
## input a list of species or a single species
## input a method
## returns a ggplot object
## MAXIMUM of 8 species



plottrends = function(trends,selectspecies,leg = T,rem = F)
{
  require(tidyverse)
  require(ggthemes)
  
  theme_set(theme_tufte())
  
  recenttrends = trends %>%
    filter(species %in% selectspecies)
  
  if (rem)
  {
    map = read.csv("Map to Other Lists - map.csv")
    map = map %>%
      filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                             "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                             "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
      select(eBird.English.Name.2018,eBird.English.Name.2019)
    
    lists = read.csv("stateofindiasbirds.csv")
    lists = left_join(lists,map,by = c("eBird.English.Name" = "eBird.English.Name.2019"))
    lists = lists %>% select(-eBird.English.Name) %>% mutate(species = eBird.English.Name.2018) %>% 
      select(-eBird.English.Name.2018) %>% filter(!is.na(species))
    
    
    for (i in 1:length(selectspecies))
    {
      if (lists$Long.Term.Status[lists$species == selectspecies[i]] == "Uncertain")
      {
        recenttrends$freq[recenttrends$species == selectspecies[i] &
                            recenttrends$timegroups %in% c(1993,2004,2009,2012,2013)] = NA
        recenttrends$freq[recenttrends$species == selectspecies[i] &
                            recenttrends$timegroups %in% c(1993,2004,2009,2012,2013)] = NA
      }
    }
  }
  
  if (names(recenttrends)[2] != "nmfreqbyspec")
  {
    recenttrends = stdtrends(recenttrends)
  }
  
  cols = c("#869B27", "#E49B36", "#A13E2B", "#78CAE0", "#B69AC9", "#EA5599", "#31954E", "#493F3D",
           "#CC6666", "#9999CC", "#000000", "#66CC99")
  
  ns = length(selectspecies)
  
  
  cols1 = cols[c(1:ns)]
  bks1 = selectspecies
  lbs1 = selectspecies
  
  
  recenttrends$species = factor(recenttrends$species, levels = selectspecies)
  
  temp = recenttrends
  xbreaks = temp$timegroups[c(1:4,6,8,10)]
  lbreaks = temp$timegroupsf[c(1:4,6,8,10)]
  
  xbreaksl = temp$timegroups[c(1:3,6,8,10)]
  lbreaksl = temp$timegroupsf[c(1:3,6,8,10)]
  
  require(extrafont)
  #loadfonts(device = "win")
  
  #temp = temp[temp$timegroups > 2013,]
  
  maxci = temp$nmfreqbyspec + temp$nmsebyspec*1.96
  minci = temp$nmfreqbyspec - temp$nmsebyspec*1.96
  
  #liml = round(min(minci))
  #liml = liml-5

  #limu = round(max(maxci))
  #limu = limu+5
  
  liml = 0
  limu = 270
  
  #liml = 1
  #limu = 149
  
  #pd = position_dodge(0.2)
  
  if (leg)
  {
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
      geom_point(
        #position = pd,
        size = 3) +
      geom_line(
        #position = pd,
        size = 1.5) +
      #geom_line(aes(group = species),size = 1.5) +
      #geom_hline(yintercept = 200, linetype = "dotted", size = 0.5) +
      #geom_hline(yintercept = 150, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 125, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 100, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 75, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 50, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
      geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                      ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.1) +
      #geom_errorbar(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
      #ymax = (nmfreqbyspec + nmsebyspec*1.96)), width = 0.2,
      #position = pd,
      #size = 0.5) +
      xlab("years") +
      ylab("change in frequency of reporting")
    
    ggp1 = ggp +
      theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
            axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
      theme(text=element_text(family="Gill Sans MT")) +
      scale_colour_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_fill_manual(breaks = bks1, 
                        labels = lbs1,
                        values = cols1) +
      scale_x_continuous(breaks = xbreaksl,
                         #limits = c(1993,2018),
                         labels = lbreaksl) +
      scale_y_continuous(breaks = c(0,50,75,100,125), 
                         #limits = c(liml,limu),
                         labels = c("-100%","-50%","-25%",
                                    "0%","+25%")
      )
    #theme(legend.position = "none")
    
    require(gridExtra)
    require(grid)
    
    grid_arrange_shared_legend <- function(...) {
      plots <- list(...)
      g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
      legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      lheight <- sum(legend$height)
      grid.arrange(
        do.call(arrangeGrob, c(lapply(plots, function(x)
          x + theme(legend.position="none")), list(nrow = 1))),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight))
    }
    
    #tiff('plot1.tiff', units="in", width=10, height=7, res=1000)
    #grid_arrange_shared_legend(ggp1)
    #dev.off()
    name = paste(selectspecies[1],".png",sep="")
    
    png(name, units="in", width=10, height=7, res=1000)
    grid_arrange_shared_legend(ggp1)
    dev.off()
  }
  
  if(!isTRUE(leg))
  {
    ggp = ggplot(temp, aes(x=timegroups, y=nmfreqbyspec, colour=species)) + 
      geom_point(size = 3) +
      geom_line(size = 1.5) +
      #geom_line(aes(group = species),size = 1.5) +
      #geom_hline(yintercept = 150, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 125, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 100, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 75, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 50, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 25, linetype = "dotted", size = 0.5) +
      geom_hline(yintercept = 0, linetype = "dotted", size = 0.5) +
      geom_ribbon(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
                      ymax = (nmfreqbyspec + nmsebyspec*1.96), fill = species), colour = NA, alpha = 0.1) +
      #geom_errorbar(aes(x = timegroups, ymin = (nmfreqbyspec - nmsebyspec*1.96),
      #ymax = (nmfreqbyspec + nmsebyspec*1.96)), width = 0.1, size = 0.1, position = pd) +
      xlab("years") +
      ylab("change in frequency of reporting")
    
    xbreaks1 = temp$timegroups[1:10]
    lbreaks1 = temp$timegroupsf[1:10]
    
    ggp1 = ggp +
      theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
            axis.title.y = element_text(angle = 90, size = 16), 
            axis.text.y = element_text(size = 14, colour = "#56697B", face = "italic"),
            axis.ticks.y = element_blank()) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
      theme(text=element_text(family="Gill Sans MT")) +
      scale_colour_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_fill_manual(breaks = bks1, 
                        labels = lbs1,
                        values = cols1) +
      scale_x_continuous(breaks = xbreaksl, labels = lbreaksl) +
      scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                         #limits = c(liml,limu),
                         labels = c("-100%","-75%","-50%","-25%","0%",
                                    "+25%")
      ) +
    theme(legend.position = "bottom")
    
    ggpx = ggp +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.y = element_text(size = 14, colour = "#56697B", face = "italic"),
            axis.ticks.y = element_blank()) +
      theme(legend.title = element_blank(), legend.text = element_text(size = 12)) +
      theme(text=element_text(family="Gill Sans MT")) +
      scale_colour_manual(breaks = bks1, 
                          labels = lbs1,
                          values = cols1) +
      scale_fill_manual(breaks = bks1, 
                        labels = lbs1,
                        values = cols1) +
      scale_x_continuous(breaks = xbreaks1, labels = lbreaks1) +
      scale_y_continuous(breaks = c(0,25,50,75,100,125), 
                         #limits = c(liml,limu),
                         labels = c("-100%","-75%","-50%","-25%","0%",
                                    "+25%")
      ) +
    theme(legend.position = "none")
    
    p1 = ggp1
    require(cowplot)
    sepleg = get_legend(p1)
    
    gg = list(ggpx,sepleg,ggp1)
    return(gg)
  }
}



#####################################################


# plot composite trends

plotcompositetrends = function(trends,specieslist,name="composite",g1=NA,g2=NA,g3=NA,g4=NA,g5=NA,g6=NA,
                               g7=NA,g8=NA,n1=NA,n2=NA,n3=NA,n4=NA,n5=NA,n6=NA,
                               n7=NA,n8=NA)
{
  require(tidyverse)
  require(ggthemes)
  
  theme_set(theme_tufte())
  
  g = list(g1,g2,g3,g4,g5,g6,g7,g8)
  g = Filter(Negate(anyNA), g)
  l = length(g)
  if (l == 0)
    break
  
  n = c(n1,n2,n3,n4,n5,n6,n7,n8)
  n = n[!is.na(n)]
  n1 = as.character(c(1:l))
  
  speciesl = as.character()
  for (i in 1:l)
  {
    speciesl = c(speciesl,g[[i]])
  }
  
  speciesl = unique(speciesl)
  
  glmr = read.csv("glmr.csv")
  
  glmr$mintrend = glmr$trend - glmr$trendci
  glmr$maxtrend = glmr$trend + glmr$trendci
  glmr$minslope = glmr$slope - glmr$slopeci
  glmr$maxslope = glmr$slope + glmr$slopeci
  
  glmr$mintrend[glmr$mintrend < -100] = -100
  
  trendscat = glmr %>%
    mutate(longcat = 
             case_when(is.na(trend) ~ "Data Deficient",
                       trendci > 50 ~ "Uncertain",
                       maxtrend <= -50 ~ "Strong Decline",
                       maxtrend <= -25 ~ "Moderate Decline",
                       mintrend >= 50 ~ "Strong Increase",
                       mintrend >= 25 ~ "Moderate Increase",
                       trendci > 30 & mintrend > -(0.5*trendci) ~ "Uncertain",
                       trendci > 30 & maxtrend < (0.5*trendci) ~ "Uncertain",
                       TRUE ~ "Stable")
    ) %>%
    mutate(shortcat = 
             case_when(is.na(slope) ~ "Data Deficient",
                       slopeci > 20 ~ "Uncertain",
                       maxslope <= -2.7 ~ "Strong Decline",
                       maxslope <= -1.1 ~ "Moderate Decline",
                       minslope >= 1.6 ~ "Strong Increase",
                       minslope >= 0.9 ~ "Moderate Increase",
                       TRUE ~ "Stable")
    ) %>%
    select(species,trend,trendci,mintrend,maxtrend,slope,slopeci,minslope,maxslope,longcat,shortcat)
  
  trendscat$longcat[trendscat$species %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                             "Taiga Flycatcher","Chestnut Munia")] = NA
  trendscat$shortcat[trendscat$species %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                              "Taiga Flycatcher","Chestnut Munia")] = NA
  
  trendscat$longcat = factor(trendscat$longcat, levels = c("Strong Increase","Moderate Increase",
                                                           "Stable","Uncertain","Data Deficient",
                                                           "Moderate Decline","Strong Decline"))
  trendscat$shortcat = factor(trendscat$shortcat, levels = c("Strong Increase","Moderate Increase",
                                                             "Stable","Uncertain","Data Deficient",
                                                             "Moderate Decline","Strong Decline"))
  
  trendscat = trendscat %>% filter(species %in% speciesl)
  
  for (i in 1:l)
  {
    gp = composite(trends[trends$species %in% g[[i]],], name = n[i])
    temp = trendscat[trendscat$species %in% g[[i]],]
    temp$name = n1[i]
    if (i == 1)
    {
      gp1 = gp
      temp1 = temp
    }
    if (i > 1)
    {
      gp1 = rbind(gp1,gp)
      temp1 = rbind(temp1,temp)
    }
  }
  
  forbarlong = temp1 %>%
    filter(!longcat == "Uncertain") %>%
    group_by(name) %>% mutate(specs = n()) %>%
    group_by(name,longcat) %>% summarize(perc = n()*100/max(specs))
  names(forbarlong)[2] = "cat"
  forbarlong$type = "Long-term Trend"
  
  forbarshort = temp1 %>%
    filter(!shortcat == "Uncertain") %>%
    group_by(name) %>% mutate(specs = n()) %>%
    group_by(name,shortcat) %>% summarize(perc = n()*100/max(specs))
  names(forbarshort)[2] = "cat"
  forbarshort$type = "Current Annual Change"
  
  forbar = rbind(forbarlong,forbarshort)
  
  forbar$type = factor(forbar$type, levels = c("Long-term Trend","Current Annual Change"))
  
  theme_set(theme_tufte())
  
  ggpt = plottrends(gp1, n, leg = F)
  ggp1 = ggpt[[1]]
  ggpz = ggpt[[3]]
  
  require(extrafont)
  #loadfonts(device = "win")
  #font_import()
  
  t_names = c(
    'Long-term Trend'="Long-term\nTrend",
    'Current Annual Change'="Current\nChange"
  )
  
  forbar$cat = as.character(forbar$cat)
  forbar = forbar %>% filter(!cat == "Uncertain")
  forbar$cat = factor(forbar$cat, levels = c("Strong Increase","Moderate Increase","Stable",
                                             "Moderate Decline","Strong Decline"))
  #forbar$cat = factor(forbar$cat, levels = c("Strong Decline","Moderate Decline","Stable",
  #                                           "Moderate Increase","Strong Increase"))
  ord = sort(unique(forbar$cat))
  #ord = factor(ord, levels = c("Strong Decline","Moderate Decline","Stable","Moderate Increase",
  #                             "Strong Increase"))
  catcols = c("#378AB1","#8AC8A4","#E4EBB8","#CCA5BB","#B02A83")
  #catcols = c("#B02A83","#CCA5BB","#E4EBB8","#8AC8A4","#378AB1")
  
  
  theme_set(theme_tufte())

  
  ggp = ggplot(forbar, aes(x=name, y=perc, fill=cat)) + 
    facet_wrap(~type, nrow = 1, ncol = 2, labeller = as_labeller(t_names)) +
    geom_bar(stat = 'identity') +
    xlab("groups") +
    ylab("percentage of species")
  
  ggp2 = ggp +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    #theme(strip.text.x = element_text(size = 8, face = "bold")) +
    theme(strip.text.x = element_blank()) +
    theme(text=element_text(family="Gill Sans MT")) +
    scale_fill_manual(breaks = c("Strong Increase","Moderate Increase","Stable","Moderate Decline",
                                 "Strong Decline"), 
                      labels = c("Strong\nIncrease","Moderate\nIncrease","Stable","Moderate\nDecline",
                                 "Strong\nDecline"),
                      values = catcols[ord]) +
    theme(legend.position = c("bottom"))
  
  ggpy = ggp +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 5)) +
    #theme(strip.text.x = element_text(size = 8, face = "bold")) +
    theme(strip.text.x = element_blank()) +
    theme(text=element_text(family="Gill Sans MT")) +
    scale_fill_manual(breaks = c("Strong Increase","Moderate Increase","Stable","Moderate Decline",
                                 "Strong Decline"), 
                      labels = c("Strong\nIncrease","Moderate\nIncrease","Stable","Moderate\nDecline",
                                 "Strong\nDecline"),
                      values = catcols[ord]) +
    theme(legend.position = "none")
  
  ext = c("z1","z2","z3","z4","z5")
  ext = ext[1:(5-l)]
  lims = rev(unique(forbar$name))
  if (l != 5)
  {
    lims = rev(c(unique(forbar$name),ext))
  }
  
  ggpp1 = ggplot(forbar, aes(x=name, y=perc, fill=cat, width = .5)) + 
    facet_wrap(~type, nrow = 2, ncol = 1, labeller = as_labeller(t_names)) +
    geom_bar(stat = 'identity') +
    scale_x_discrete(limits = lims) +
    xlab("groups") +
    ylab("percentage of species")
  
  #spacebar = ((7.2*(72.27-11))/(2*l+2.5))
  #print((2*spacebar)/((2*l+2.5)*spacebar+11))
  
  ggpp = ggpp1 +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(legend.title = element_blank(), legend.text = element_text(size = 5)) +
    #theme(strip.text.x = element_text(size = 8, face = "bold")) +
    theme(strip.text.x = element_blank()) +
    theme(text=element_text(family="Gill Sans MT")) +
    scale_fill_manual(breaks = c("Strong Increase","Moderate Increase","Stable","Moderate Decline",
                                 "Strong Decline"), 
                      labels = c("Strong\nIncrease","Moderate\nIncrease","Stable","Moderate\nDecline",
                                 "Strong\nDecline"),
                      values = catcols[ord]) +
    theme(legend.position = "none") +
    coord_flip() +
    theme(panel.spacing = unit(100, "pt"))
  
  #http://www.sthda.com/english/wiki/colors-in-r
  
  p3 = ggp2
  
  require(cowplot)
  g1 = plot_grid(ggpz,ggpy,nrow=1,ncol=2,rel_widths = c(4/5, 1/5))
  
  sepleg1 = ggpt[[2]]
  sepleg2 = get_legend(p3)
  
  g2 = plot_grid(sepleg1,sepleg2,align = "h",nrow=1,ncol=2,rel_widths = c(3/5, 2/5))
  
  g = plot_grid(g1,g2,align = "v",nrow=2,ncol=1,rel_heights = c(7/8, 1/8))
  
  gtemp = plot_grid(ggpz,sepleg1,align = "v",nrow=2,ncol=1,rel_heights = c(7/8, 1/8))
  
  theme_set(theme_tufte())
  
  #n1 = paste(name,".tiff",sep="")
  n2 = paste(name,".png",sep="")
  n3 = paste(name,"_composite.svg",sep="")
  n4 = paste(name,"_speciestrends.svg",sep="")
  ntemp = paste(name,"_no_bars.png",sep="")

  #tiff(n1, units="in", width=10, height=7, res=1000)
  #grid::grid.draw(g)
  #dev.off()
  
  png(n2, units="in", width=10, height=7, res=1000)
  grid::grid.draw(g)
  dev.off()
  
  png(ntemp, units="in", width=10, height=7, res=1000)
  grid::grid.draw(gtemp)
  dev.off()
  
  #png(n2, units="in", width=10, height=7.2, res=1000)
  #print(ggpp)
  #dev.off()
  
  print(ggpt[[1]])
  ggsave(file=n3, units="in", width=11, height=7)
  
  print(ggpp)
  ggsave(file=n4, units="in", width=10, height=7.2)
  
  #theme(legend.position = "none")
  
}


## funtion to calculate slope from trends


calculatetrendslope = function(trends, species, specieslist, composite = F)
{
  require(tidyverse)
  name = species
  trends = trends %>% filter(species == name)
  
  corr = data.frame(species = name, trend = NA, trendci = NA, slope = NA, slopeci = NA)

  speciesl = specieslist %>%
    filter(COMMON.NAME == species)
  
  if (is.na(speciesl$ht) & is.na(speciesl$rt))
  {
    #return(corr)
  }
  
  trends = na.omit(trends)
  trends1 = trends
  if (!isTRUE(composite))
  {
    trends = stdtrends(trends)
  }
  
  if (!is.na(speciesl$ht) & !is.na(speciesl$rt))
  {
    corr$trend = tail(trends,1)$nmfreqbyspec
    corr$trendci = tail(trends,1)$nmsebyspec*1.96
  }
  
  if (!is.na(speciesl$rt))
  {
    trends1 = trends1 %>% filter(timegroups > 2013)
    if (!isTRUE(composite))
    {
      trends1 = stdtrends(trends1)
    }
    
    tm = trends1$timegroups
    tm = tm - min(tm)
    sl = numeric(1000)
    
    for (i in 1:1000)
    {
      samp = numeric(length(trends1$timegroups))
      for (j in 1:length(trends1$timegroups))
      {
        samp[j] = rnorm(1,trends1$nmfreqbyspec[j],trends1$nmsebyspec[j])
      }
      samp = samp - 100
      result = summary(lm(samp~0+tm))
      sl[i] = result$coefficients[1,1]
    }
    
    corr$slope = mean(sl)
    corr$slopeci = sd(sl)*1.96
    corr$trend = corr$trend - 100
  }
  
  trends = corr
  
  return(trends)
}


############################################################

## occupancy analyses for bird abundance/range, reports area in units of 10000 sq. km.
## Requires tidyverse, reshape2, data.table and unmarked
## type = trivial, null, nosp, nosptime, nb, nosptimenb

occufreq = function(data, species, areag, rerun = F, datatofill)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  load("neighbours.RData")
  
  migstatus = read.csv("Migratory Status - Migratory Status.csv")

  migstatus = migstatus %>%
    mutate(mig = 
             case_when(!is.na(Summer.Visitor) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Resident) & !is.na(Winter.Visitor) ~ "LM",
                       !is.na(Summer.Visitor) & !is.na(Resident) ~ "LM",
                       !is.na(Summer.Visitor) ~ "S",
                       !is.na(Winter.Visitor) | !is.na(Strictly.Passage) ~ "W/P",
                       !is.na(Uncertain.Vagrant) & is.na(Resident) ~ "U",
                       TRUE ~ "R")
    ) %>%
    select(eBird.English.Name,mig)
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Himalayan Cuckoo","Common Cuckoo",
                                                    "Watercock")] = "S"
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Indian Skimmer","Black-bellied Tern",
                                                    "Black-capped Kingfisher",
                                                    "Mountain Chiffchaff","Red-rumped Swallow",
                                                    "Fire-capped Tit")] = "R"
  
  migstatus$mig[migstatus$eBird.English.Name %in% c("Smoky Warbler","Wallcreeper",
                                                    "Long-billed Pipit")] = "W/P"
  
  if(rerun)
  {
    species = datatofill$species
    
    temp = datatofill %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    
    species = species[species %in% temp$species]
  }
  
  migstatus = migstatus %>%
    filter(eBird.English.Name %in% species)
  
  species = as.character(migstatus$eBird.English.Name)
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
  

  est = array(data=NA,dim=c(length(speciesf),11),
              dimnames=list(speciesf,c("detprobB","occB","occB.ci","trivB","sampareaB","detprobM",
                                       "occM","occM.ci","trivM","sampareaM","status")))
  
  if(rerun)
  {
    est[,1] = datatofill$detprobB
    est[,2] = datatofill$occB*10000
    est[,3] = datatofill$occB.ci*10000
    est[,4] = datatofill$trivB*10000
    est[,5] = datatofill$sampareaB*10000
    est[,6] = datatofill$detprobM
    est[,7] = datatofill$occM*10000
    est[,8] = datatofill$occM.ci*10000
    est[,9] = datatofill$trivM*10000
    est[,10] = datatofill$sampareaM*10000
    est[,11] = datatofill$migstatus
    
    if (migstatus == "LM")
    {
      index = 1:length(species)
      x = numeric(0)
      
      for (i in unique(species))
      {
        temp = datatofill %>%
          filter(species == i, migstatus == "LM")
        
        if (length(temp$species) != 0)
        {
          if (!is.na(temp$trivB) & (!is.na(temp$occB) & !is.na(temp$occB.ci)))
          {
            x1 = intersect(index[species == i], index[mig == "MS"])
            x = c(x,index[x1])
          }
          if (!is.na(temp$trivM) & (!is.na(temp$occM) & !is.na(temp$occM.ci)))
          {
            x2 = intersect(index[species == i], index[mig == "MW"])
            x = c(x,index[x2])
          }
        }
      }
      species = species[-x]
      mig = mig[-x]
    }
    

  }

  for(s in 1:length(species))
  {
    if(rerun)
    {
      if (is.na(datatofill$sampareaB[s]) & is.na(datatofill$sampareaM[s]))
        next
    }
    
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
    
    sampledarea = left_join(datac,areag,by = c("gridg1" = "id"))
    sampledarea = sampledarea %>% distinct(gridg1,area)
    len = length(sampledarea$area)
    sampledarea = sum(sampledarea$area)
    
    if (length(datac$COMMON.NAME[datac$COMMON.NAME == species[s]]) == 0)
    {
      estdf = data.frame(rep(rownames(est)))
      names(estdf) = "species"
      
      estdf$detprobB = NA
      estdf$occB = NA
      estdf$occB.ci = NA
      estdf$trivB = NA
      estdf$sampareaB = NA
      estdf$detprobM = NA
      estdf$occM = NA
      estdf$occM.ci = NA
      estdf$trivM = NA
      estdf$sampareaM = NA
      
      names(migstatus)[2] = "migstatus"
      estdf = left_join(estdf,migstatus,by = c("species" = "eBird.English.Name"))
      
      return(estdf)
    }
    
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
      group_by(gridg) %>% mutate(group.id = 1:n()) %>% ungroup %>%
      left_join(areag,by = c("gridg" = "id"))
      
    
    nbt = selexp %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
    nbt$nb8 = 0
      
    nbti = inc %>%
      filter(COMMON.NAME == species[s]) %>%
      left_join(areag,by = c("gridg" = "id")) %>%
      group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT), area = mean(area)) %>%
      mutate(fl=replace(fl, fl > 1, 1))
      
    fil = sum(nbti$fl*nbti$area)
      
      
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
      f2$nb = newdat2$nb8g
      f2$gridg = nbtx$gridg
      f2 = left_join(f2,areag,by = c("gridg" = "id"))
      f2 = f2 %>% filter(!is.na(Predicted))
      f2a = sum(f2$Predicted*f2$area) + fil
      f2b = round((erroradd(f2$SE*f2$area))*1.96)
      
      
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
      est[species[s],"trivB"] = fil
      est[species[s],"sampareaB"] = sampledarea
    }
    
    if (mig[s] == "W/P" | mig[s] == "MW" | mig[s] == "U")
    {
      est[species[s],"trivM"] = fil
      est[species[s],"sampareaM"] = sampledarea
    }
  }  
  estdf = data.frame(rep(rownames(est)))
  names(estdf) = "species"

  
  estdf$detprobB = round(as.numeric(est[,1]),3)
  estdf$occB = round(as.numeric(est[,2]),3)/10000
  estdf$occB.ci = round(as.numeric(est[,3]),3)/10000
  estdf$trivB = round(as.numeric(est[,4]),3)/10000
  estdf$sampareaB = round(as.numeric(est[,5]),3)/10000
  estdf$detprobM = round(as.numeric(est[,6]),3)
  estdf$occM = round(as.numeric(est[,7]),3)/10000
  estdf$occM.ci = round(as.numeric(est[,8]),3)/10000
  estdf$trivM = round(as.numeric(est[,9]),3)/10000
  estdf$sampareaM = round(as.numeric(est[,10]),3)/10000
  
  names(migstatus)[2] = "migstatus"
  estdf = left_join(estdf,migstatus,by = c("species" = "eBird.English.Name"))

  return(estdf)
}

SoIBoccupancy = function(data,species,areag)
{
  a = occufreq(data,species,areag)
  c = 0
  repeat
  {
    c = c + 1
    temp = a %>%
      filter((!is.na(trivB) & (is.na(occB) | is.na(occB.ci))) | 
               (!is.na(trivM) & (is.na(occM) | is.na(occM.ci))))
    if(length(temp$species) == 0)
      break
    if(c == 10)
      break
    a = occufreq(data,species,areag,rerun=T,datatofill=a)
  }
  return(a)
}

#####################################

## including regions

freqtrendsr = function(data,species,specieslist,
                       databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018),
                       error=T,nsim = 1000)
{
  require(tidyverse)
  require(lme4)
  require(VGAM)
  require(parallel)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$region = as.factor(data$region)
  
  specieslist = specieslist %>%
    filter(COMMON.NAME == species)
  
  ## filters data based on whether the species has been selected for long-term trends (ht) 
  ## or short-term trends (rt) 
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    g1 = data.frame(timegroups = unique(data$timegroups))
    g1$se = g1$freq = NA
    g1$timegroups = factor(g1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    g1 = g1[order(g1$timegroups),]
    names(g1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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
    f1$se = f1$freq = NA
    f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    f1 = f1[order(f1$timegroups),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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
  
  ## calculate a median list length to use to predict
  
  datay = data %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
    summarize(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  ## calculate region
  
  datay = data %>%
    filter(COMMON.NAME == species) %>%
    group_by(region) %>% summarize(l = n())
  datay$l = datay$l/sum(datay$l)
  #rg = as.character(datay[datay$l == max(datay$l),]$region)
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data,species)
  tm = unique(data$timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  m1 = glmer(OBSERVATION.COUNT ~ month:log(no.sp) + timegroups + log(no.sp)*region +
               (1|gridg3/gridg1), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ln = length(f$month)
  f = do.call("rbind", replicate(length(unique(ed$region)),f,simplify=F))
  f$region = rep(unique(ed$region), each = ln)
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month, region = f$region)
  
  f1 = data.frame(timegroups = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  f2$region = ltemp$region
  
  ## bootstrap to get errors
  
  if (error)
  {
    predFun = function(m1) {
      predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
    }
    
    cr = max(1, detectCores() - 1)
    cl = makeCluster(cr)
    clusterEvalQ(cl, library(lme4))
    
    pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "snow", seed = 1000,
                   use.u = FALSE, type = "parametric", ncpus = cr, cl = cl)
    
    stopCluster(cl)
    
    for (i in 1:length(ltemp$no.sp))
    {
      f2$freq[i] = median(pred$t[,i])
      f2$se[i] = sd(pred$t[,i])
    }
    
    f2$freqt = cloglog(f2$freq,inverse = T)
    f2$cl = cloglog((f2$freq-f2$se),inverse = T)
    f2$set = f2$freqt-f2$cl
    
    fx = f2 %>%
      filter(!is.na(freqt) & !is.na(set)) %>%
      group_by(timegroups,region) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n()))
    fx = left_join(fx,datay)
    fx$freq = fx$freq*fx$l
    fx$se = fx$se*fx$l
    fx = fx %>%
      filter(!is.na(freq) & !is.na(se)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freq), se = sqrt(sum(se^2)/n()))
    
    f1 = left_join(f1,fx)
  }
  
  if(!isTRUE(error))
  {
    f2$freq = predict(m1, newdata = ltemp,
                      type="response", re.form = NA)
    f1 = f2 %>%
      filter(!is.na(freq)) %>%
      group_by(timegroups,region) %>% summarize(freq = mean(freq))
    f1 = left_join(f1,datay)
    f1$freq = f1$freq*f1$l
    f1 = f1 %>%
      filter(!is.na(freq)) %>%
      group_by(timegroups) %>% summarize(freq = mean(freq))
    
    f1$se = NA
  }
  
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016","2017","2018"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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


#######################################################################################

freqtrendsrestricted = function(data,species,specieslist,
                      databins=c(1993,2004,2009,2012,2013,2014,2015,2016,2017,2018),nsim=1000)
{
  require(tidyverse)
  require(VGAM)
  require(lme4)

  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)

  specieslist = specieslist %>%
    filter(COMMON.NAME == species)
  
  ## filters data based on whether the species has been selected for long-term trends (ht) 
  ## or short-term trends (rt) 
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    g1 = data.frame(timegroups = unique(data$timegroups))
    g1$se = g1$freq = NA
    g1$timegroups = factor(g1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    g1 = g1[order(g1$timegroups),]
    names(g1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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
    f1$se = f1$freq = NA
    f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                     "2011-2012","2013","2014","2015","2016","2017","2018"))
    f1 = f1[order(f1$timegroups),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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
  data$gridg = data$gridg1
  temp = data %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg1)
  data = temp %>% left_join(data)
  
  ## calculate a median list length to use to predict
  
  datay = data %>%
    group_by(group.id) %>% slice(1) %>% ungroup %>%
    summarize(medianlla = median(no.sp))
  
  medianlla = datay$medianlla
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data,species)
  
  ed$month = as.numeric(ed$month)
  ed$month[ed$month %in% c(11,12,1,2)] = "Win"
  ed$month[ed$month %in% c(3,4,5,6)] = "Sum"
  ed$month[ed$month %in% c(7,8,9,10)] = "Mon"
  
  ed$month = as.factor(ed$month)
  ed$gridg1 = as.factor(ed$gridg1)
  
  tm = unique(data$timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups + (1|gridg1), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups"
  f$month = rep(unique(ed$month), each = length(f$timegroups)/length(unique(ed$month)))
  ltemp = data.frame(timegroups = f$timegroups,
                     no.sp = medianlla, month = f$month)
  
  f1 = data.frame(timegroups = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups = ltemp$timegroups
  
  ## bootstrap to get errors
  

  predFun = function(m1) {
    predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
  }
  
  cr = max(1, detectCores() - 4)
  cl = makeCluster(cr)
  clusterEvalQ(cl, library(lme4))
  
  pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "snow", seed = 1000,
                 use.u = FALSE, type = "parametric", ncpus = cr, cl = cl)
  
  stopCluster(cl)
  
  for (i in 1:length(ltemp$no.sp))
  {
    f2$freq[i] = median(na.omit(pred$t[,i]))
    f2$se[i] = sd(na.omit(pred$t[,i]))
  }
  
  f2$freqt = cloglog(f2$freq,inverse = T)
  f2$cl = cloglog((f2$freq-f2$se),inverse = T)
  f2$set = f2$freqt-f2$cl
  
  fx = f2 %>%
    filter(!is.na(freqt) & !is.na(set)) %>%
    group_by(timegroups) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n())) 
  
  f1 = left_join(f1,fx)

  
  
  f1$timegroups = factor(f1$timegroups, levels = c("before 2000","2000-2006","2007-2010",
                                                   "2011-2012","2013","2014","2015","2016","2017","2018"))
  f1 = f1[order(f1$timegroups),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2000","2000-2006","2007-2010",
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





#######################################################################################

freqtrendssparrow = function(data,species,specieslist,
                      databins=c(1999,2012,2017),
                      error=T,nsim = 1000)
{
  require(tidyverse)
  require(lme4)
  require(VGAM)
  require(parallel)
  
  data$gridg1 = as.factor(data$gridg1)
  data$gridg2 = as.factor(data$gridg2)
  data$gridg3 = as.factor(data$gridg3)
  data$gridg4 = as.factor(data$gridg4)
  data$region = as.factor(data$region)
  
  specieslist = specieslist %>%
    filter(COMMON.NAME == species)
  
  ## filters data based on whether the species has been selected for long-term trends (ht) 
  ## or short-term trends (rt) 
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    g1 = data.frame(timegroups1 = unique(data$timegroups1))
    g1$se = g1$freq = NA
    g1$timegroups1 = factor(g1$timegroups1, levels = c("before 2006","2007-2013",
                                                       "2014-2018"))
    g1 = g1[order(g1$timegroups1),]
    names(g1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                    "2014-2018"), 
                    timegroups1 = as.numeric(databins))
    g1 = left_join(g1,mp)
    g1$species = species
    g1 = g1 %>%
      filter(timegroups1 < 2014)
    
    data = data %>%
      filter(year >= 2014)
  }
  
  if (is.na(specieslist$ht) & is.na(specieslist$rt))
  {
    f1 = data.frame(timegroups1 = unique(data$timegroups1))
    f1$se = f1$freq = NA
    f1$timegroups1 = factor(f1$timegroups1, levels = c("before 2006","2007-2013",
                                                       "2014-2018"))
    f1 = f1[order(f1$timegroups1),]
    names(f1)[1] = "timegroupsf"
    mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                    "2014-2018"), 
                    timegroups1 = as.numeric(databins))
    f1 = left_join(f1,mp)
    f1$species = species
    f1 = f1 %>% filter(!is.na(f1$timegroups1))
    return(f1)
  }
  
  ## errors for wrong parameter values
  
  ## considers only complete lists
  
  data = data %>%
    filter(ALL.SPECIES.REPORTED == 1)
  
  data$month = as.factor(data$month)
  
  if (!species %in% unique(data$COMMON.NAME))
    return(paste(species,"is not a valid species name for the region selected"))
  
  
  data$timegroups1 = as.factor(data$timegroups1)
  data$gridg = data$gridg3
  temp = data %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3,month)
  data = temp %>% left_join(data)
  
  ## calculate a median list length to use to predict
  
  datay = data %>%
    group_by(gridg3,gridg1,group.id) %>% slice(1) %>% ungroup %>%
    group_by(gridg3,gridg1) %>% summarize(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% summarize(medianlla = mean(medianlla)) %>%
    summarize(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla
  
  ## expand dataframe to include absences as well
  
  ed = expandbyspecies(data,species)
  tm = unique(data$timegroups1)
  #rm(data, pos = ".GlobalEnv")
  
  ## the model
  
  m1 = glmer(OBSERVATION.COUNT ~ month + month:log(no.sp) + timegroups1 + (1|gridg3/gridg1), data = ed, 
             family=binomial(link = 'cloglog'), nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  ## prepare a new data file to predict
  
  f = data.frame(unique(tm))
  f = do.call("rbind", replicate(length(unique(ed$month)),f,simplify=F))
  names(f) = "timegroups1"
  f$month = rep(unique(ed$month), each = length(f$timegroups1)/length(unique(ed$month)))
  ltemp = data.frame(timegroups1 = f$timegroups1,
                     no.sp = medianlla, month = f$month)
  
  f1 = data.frame(timegroups1 = tm)
  
  f2 = data.frame(freq = numeric(length(ltemp$no.sp)))
  f2$se = numeric(length(ltemp$no.sp))
  f2$timegroups1 = ltemp$timegroups1
  
  ## bootstrap to get errors
  
  if (error)
  {
    predFun = function(m1) {
      predict(m1,ltemp, re.form = NA, allow.new.levels=TRUE)
    }
    
    cr = max(1, detectCores() - 4)
    cl = makeCluster(cr)
    clusterEvalQ(cl, library(lme4))
    
    pred = bootMer(m1, nsim = nsim, FUN = predFun, parallel = "snow", seed = 1000,
                   use.u = FALSE, type = "parametric", ncpus = cr, cl = cl)
    
    stopCluster(cl)
    
    for (i in 1:length(ltemp$no.sp))
    {
      f2$freq[i] = median(na.omit(pred$t[,i]))
      f2$se[i] = sd(na.omit(pred$t[,i]))
    }
    
    f2$freqt = cloglog(f2$freq,inverse = T)
    f2$cl = cloglog((f2$freq-f2$se),inverse = T)
    f2$set = f2$freqt-f2$cl
    
    fx = f2 %>%
      filter(!is.na(freqt) & !is.na(set)) %>%
      group_by(timegroups1) %>% summarize(freq = mean(freqt), se = sqrt(sum(set^2)/n())) 
    
    f1 = left_join(f1,fx)
  }
  
  if(!isTRUE(error))
  {
    f2$freq = predict(m1, newdata = ltemp,
                      type="response", re.form = NA)
    f1 = f2 %>%
      filter(!is.na(freq)) %>%
      group_by(timegroups1) %>% summarize(freq = mean(freq))
    f1$se = NA
  }
  
  
  f1$timegroups1 = factor(f1$timegroups1, levels = c("before 2006","2007-2013",
                                                     "2014-2018"))
  f1 = f1[order(f1$timegroups1),]
  names(f1)[1] = "timegroupsf"
  mp = data.frame(timegroupsf = c("before 2006","2007-2013",
                                  "2014-2018"), 
                  timegroups = as.numeric(databins))
  f1 = left_join(f1,mp)
  f1$species = species
  
  if (is.na(specieslist$ht) & !is.na(specieslist$rt))
  {
    f1 = rbind(g1,f1)
  }
  
  return(f1)
}