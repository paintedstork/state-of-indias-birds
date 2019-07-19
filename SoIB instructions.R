###################                       PART 1                     ###################################


## initialize and create spatialpolygon and spatialgrid databases from shapefiles
## has to be run only once unless any of these change
## requires R packages tidyverse, rgdal, sp
## district, state and country shapefiles (2011 currently) MUST BE in the working directory
## creates grids at 25, 50, 100, 200 resolutions and lists of 4 and 8 nearest neighbours
## writes 'maps.RData' and 'neighbours.RData' to the home folder (called in other functions)

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
createmaps()


### Sequence of steps to clean data starting from .txt file

## clean the eBird EBD, add some important columns, select only few
## has to be run after every new EBD download
## requires R packages lubridate and tidyverse
## txt data file MUST BE in the working directory
## writes 'indiaspecieslist.csv' (common and scientific names of all species)
## writes 'data.RData' to the home folder

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
readcleanrawdata("ebd_IN_relMay-2019.txt","Sensitive_India_may 2019.csv") 

## add map and grid variables to the dataset (dataframe)
## has to be run after the previous step
## requires R packages tidyverse, data.table, sp and rgeos
## data.RData and maps.RData files MUST BE in the working directory
## writes 'data.RData' to the home folder

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
addmapvars()

## clean up and filter data for analyses
## has to be run after previous step
## requires tidyverse, lubridate
## "indiaspecieslist.csv", "Activity - Activity.csv", "Migratory Status - Migratory Status.csv",
## "Endemicity - Endemicity.csv", "Select Species from List - Select Species from List.csv"
## and data.RData files MUST BE in the home folder
## writes 'dataforanalyses.RData' to the home folder, workspace contains info about
## amount of data in each temporal bin, full species list (with all attribute columns) 
## and selected species list, data

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
dataspeciesfilter(locationlimit = 15,gridlimit = 4)




###################                       PART 2                     ###################################
## provide "dataforanalyses.RData", "neighbours.RData", "indiaspecieslist.csv" and 
## "Migratory Status - Migratory Status.csv"

## ALL SUBSEQUENT ANALYSES REQUIRE DATA THAT HAS BEEN THROUGH THE PREVIOUS STEPS
## if "dataforanalyses.RData" can be loaded and "indiaspecieslist.csv" is available, part 1 not required


## provides occupancy estimates for one or many species
## select one or more Indian bird species
## requires tidyverse, reshape2, data.table, unmarked
## "neighbours.RData", "indiaspecieslist", "Migratory Status - Migratory Status.csv" 
## MUST BE in the home folder
## returns a dataframe with occupancy values


source('~/GitHub/state-of-indias-birds/SoIB functions.R')
occ = SoIBoccupancy(data,species,areag=areag1)

## for the final run, species = specieslist$COMMON.NAME (or this incrementally)



## THE MAIN RATE-LIMITING STEP
## provides trends estimates for only ONE species at a time
## requires tidyverse, lme4 and VGAM
## the dataframe specieslist MUST BE present in the environment
## returns a dataframe with trend values
## with error = F, errors are not computed, function runs faster

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("dataforanalyses.RData")
species = "Indian Peafowl"
start = Sys.time()
tre <- freqtrends(data,species,specieslist,error=T,nsim=100)
end = Sys.time()
print(end-start)

## this has to be run for all species in specieslist

#c = 0
#for (species in specieslist$COMMON.NAME)
#{
#  c = c + 1
#  tre = freqtrends(data,species,specieslist,error=T,nsim=1000)
#  if (c == 1)
#  {
#    trends = tre
#  }
#  if (c > 1)
#  {
#    trends = rbind(trends,tre)
#  }
#}







###################                       PART 3                     ###################################

## calculates composite values
## use a dataframe called 'trends' created from multiple 'tre's
## reuires tidyverse
## no data files called in the function
## returns a single composite trend

source('~/GitHub/state-of-indias-birds/SoIB functions.R')

## all information to be added as columns to trend file
## create separate composite data frames and merge








###################                       PART 4                     ###################################

## only single species for calculatetrendslope
## plots trends, provide trend data for up to 8 species, or 8 composites
## requires tidyverse
## no data files called in either function but environment MUST HAVE specieslist
## MUST HAVE a dataframe called trends which has trends for all species in 'specieslist'
## ensure that selectspecies has a maximum of 8 species

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
trendslope = calculatetrendslope(trends, species, specieslist, composite = F)

## this has to be run for all species in specieslist

#c = 0
#for (species in specieslist$COMMON.NAME)
#{
#  c = c + 1
#  trendslopetmp = calculatetrendslope(trends, species, specieslist, composite = F)
#  if (c == 1)
#  {
#    trendslope = trendslopetmp
#  }
#  if (c > 1)
#  {
#    trendslope = rbind(trendslope,trendslopetmp)
#  }
#}


## to plot trends for up to 8 species
plottrends(trends, selectspecies)


