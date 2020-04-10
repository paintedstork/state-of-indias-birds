load("AllTrends.RData")
load("specieslists.RData")
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)

ambi = c("Curlew Sandpiper","Dunlin")
list1 = c("Curlew Sandpiper","Dunlin","Gray Plover","Pacific Golden-Plover","Whimbrel","Little Stint","Temminck's Stint")
list2 = c("Lesser Sand-Plover","Eurasian Curlew","Black-tailed Godwit","Ruff","Common Snipe","Pin-tail Snipe",
          "Common Sandpiper","Green Sandpiper","Common Greenshank","Marsh Sandpiper","Wood Sandpiper","Common Redshank")
list3 = c("Great Thick-knee","Black-winged Stilt","Kentish Plover","Little Ringed Plover","Yellow-wattled lapwing",
        "Red-wattled Lapwing")

gps = c("Arctic Migrants","Shorter-distance Migrants","Near Resident")



plotcompositetrends(trends, specieslist = specieslist, name = "shorebirds",
                    g1 = list3, 
                    g2 = list2,
                    #g3 = list1, 
                    n1 = gps[3],
                    n2 = gps[2]
                    #n3 = gps[1]
)

