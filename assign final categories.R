## assign categories to trends and occupancy

library(tidyverse)
map = read.csv("Map to Other Lists - map.csv")
#map = map %>%
#  select(eBird.English.Name.2018,eBird.English.Name.2019,eBird.Scientific.Name.2019,IUCN,Schedule)

#init = "./All Trends"
#nms = list.files(path = init)

#load(paste(init,"/",nms[1], sep = ""))
#trends = tre

#for (i in 2:length(nms))
#{
#  load(paste(init,"/",nms[i], sep = ""))
#  trends = rbind(trends,tre)
#}

#rm(list=setdiff(ls(envir = .GlobalEnv), c("trends")), pos = ".GlobalEnv")
#save.image("AllTrends.RData")
#rm(list = ls(all.names = TRUE))

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("dataforanalyses.RData")
load("AllTrends.RData")
load("sortedspecieslist3.RData")

migstatus = read.csv("Migratory Status - Migratory Status.csv")

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1
trends = trends %>% filter(species %in% specieslist$COMMON.NAME, !species %in% srt)

specs = unique(trends$species)
temp = calculatetrendslope(trends,specs[1],specieslist)

for(i in 2:length(specs))
{
  print(specs[i])
  temp1 = calculatetrendslope(trends,specs[i],specieslist)
  temp = rbind(temp,temp1)
}

trendcats = data.frame(species = specieslist$COMMON.NAME)
trendcats = left_join(trendcats,temp)

## occupancy

occ = read.csv("occ.csv")
mg = migstatus %>% select(eBird.English.Name,Breeding.Range.Mostly.Outside)
occ = left_join(occ,mg,by = c("species" = "eBird.English.Name"))
occuse = data.frame(species = occ$species)
occuse$migstatus = occ$migstatus
occuse$range = NA
occuse$rangeci = NA
occuse$minrange = NA
occuse$maxrange = NA
occuse$occcat = NA


for (i in 1:length(occuse$species))
{
  if (occuse$migstatus[i] %in% c("R","LM") & !is.na(occ$occB[i]))
  {
    occuse$range[i] = occ$occB[i]
    occuse$rangeci[i] = occ$occB.ci[i]
    occuse$minrange[i] = occ$occB[i] - occ$occB.ci[i]
    occuse$maxrange[i] = occ$occB[i] + occ$occB.ci[i]
  }
  
  if (occuse$migstatus[i] %in% c("R","LM") & is.na(occ$occB[i]))
  {
    occuse$range[i] = occ$trivB[i]
    occuse$minrange[i] = occ$trivB[i]
    occuse$maxrange[i] = occ$trivB[i]
  }
  
  if (occuse$migstatus[i] %in% c("LM") & !is.na(occ$trivM[i]) & !is.na(occ$occM[i]) &
      !is.na(occ$Breeding.Range.Mostly.Outside[i]))
  {
    occuse$range[i] = occ$occM[i]
    occuse$rangeci[i] = occ$occM.ci[i]
    occuse$minrange[i] = occ$occM[i] - occ$occM.ci[i]
    occuse$maxrange[i] = occ$occM[i] + occ$occM.ci[i]
  }
  
  if (occuse$migstatus[i] %in% c("LM") & !is.na(occ$trivM[i]) & is.na(occ$occM[i]) &
      !is.na(occ$Breeding.Range.Mostly.Outside[i]))
  {
    occuse$range[i] = occ$trivM[i]
    occuse$minrange[i] = occ$trivM[i]
    occuse$maxrange[i] = occ$trivM[i]
  }
  
  if (occuse$migstatus[i] %in% c("S") & !is.na(occ$occB[i]))
  {
    occuse$range[i] = occ$occB[i]
    occuse$rangeci[i] = occ$occB.ci[i]
    occuse$minrange[i] = occ$occB[i] - occ$occB.ci[i]
    occuse$maxrange[i] = occ$occB[i] + occ$occB.ci[i]
  }
  
  if (occuse$migstatus[i] %in% c("S") & is.na(occ$occB[i]))
  {
    occuse$range[i] = occ$trivB[i]
    occuse$minrange[i] = occ$trivB[i]
    occuse$maxrange[i] = occ$trivB[i]
  }
  
  if (occuse$migstatus[i] %in% c("W/P") & !is.na(occ$occM[i]))
  {
    occuse$range[i] = occ$occM[i]
    occuse$rangeci[i] = occ$occM.ci[i]
    occuse$minrange[i] = occ$occM[i] - occ$occM.ci[i]
    occuse$maxrange[i] = occ$occM[i] + occ$occM.ci[i]
  }
  
  if (occuse$migstatus[i] %in% c("W/P") & is.na(occ$occM[i]))
  {
    occuse$range[i] = occ$trivM[i]
    occuse$minrange[i] = occ$trivM[i]
    occuse$maxrange[i] = occ$trivM[i]
  }
    
}


occuse = occuse %>%
  mutate(occcat = 
           case_when(is.na(range) ~ "Data Deficient",
                     maxrange < 0.4 ~ "Very Restricted",
                     maxrange < 2 ~ "Restricted",
                     minrange > 100 ~ "Very Large",
                     minrange > 10 ~ "Large",
                     TRUE ~ "Moderate")
  )

occuse$occcat[occuse$species %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                            "Taiga Flycatcher","Chestnut Munia")] = NA

end = read.csv("Endemicity - Endemicity.csv")
end = end %>%
  select(eBird.English.Name,Andaman.and.Nicobar.islands)

occuse = left_join(occuse,end,by = c("species" = "eBird.English.Name"))
occuse = occuse %>%
  mutate(occcat = 
           case_when(!is.na(Andaman.and.Nicobar.islands) & occcat == "Very Restricted" ~ "Restricted",
                     TRUE ~ occcat)
  ) %>%
  select(-Andaman.and.Nicobar.islands)

occuse = occuse %>%
  mutate(rangefull = round(range,2)) %>%
  mutate(rangefull = case_when(!is.na(rangeci) ~ paste(round(range,2),"±",round(rangeci,2),sep=""),
                               TRUE ~ as.character(rangefull))
         )

trendcats = trendcats %>%
  mutate(trendfull = "NA") %>%
  mutate(trendfull = case_when(!is.na(trendci) ~ paste(round(trend,2),"±",round(trendci,2),sep=""),
                               TRUE ~ "NA")
  )
trendcats$trendfull[trendcats$trendfull == "NA"] = NA

trendcats = trendcats %>%
  mutate(slopefull = "NA") %>%
  mutate(slopefull = case_when(!is.na(slopeci) ~ paste(round(slope,2),"±",round(slopeci,2),sep=""),
                               TRUE ~ "NA")
  )
trendcats$slopefull[trendcats$slopefull == "NA"] = NA

soib = trendcats %>% left_join(occuse) %>%
  left_join(map,by = c("species" = "eBird.English.Name.2018")) %>%
  select(eBird.English.Name.2019,eBird.Scientific.Name.2019,migstatus,IUCN,Schedule,trendfull,
         slopefull,rangefull,longcat,shortcat,occcat)

soibfull = trendcats %>% left_join(occuse) %>%
  left_join(map,by = c("species" = "eBird.English.Name.2018")) %>%
  select(eBird.English.Name.2019,eBird.Scientific.Name.2019,migstatus,IUCN,Schedule,trend,CITES.Appendix,
         CMS.Appendix,mintrend,maxtrend,trendfull,slope,minslope,maxslope,
         slopefull,range,minrange,maxrange,rangefull,longcat,shortcat,occcat)

names(soib) = c("eBird.English.Name","eBird.Scientific.Name","Migratory.Status","IUCN","WLPA.Schedule",
                "Long.Term.Trend","Current.Annual.Change","Range.Size","Long.Term.Status",
                "Current.Status","Range.Status")

names(soibfull) = c("eBird.English.Name","eBird.Scientific.Name","Migratory.Status","IUCN",
                    "WLPA.Schedule","CITES.Appendix","CMS.Appendix",
                    "Long.Term.Trend.Mean","Long.Term.Trend.Lower","Long.Term.Trend.Upper",
                    "Long.Term.Trend","Current.Annual.Change.Mean","Current.Annual.Change.Lower",
                    "Current.Annual.Change.Upper","Current.Annual.Change","Range.Size.Mean",
                    "Range.Size.Lower","Range.Size.Upper","Range.Size","Long.Term.Status",
                    "Current.Status","Range.Status")

concerncats = read.csv("concernclassification.csv")
soib = left_join(soib,concerncats)
soibfull = left_join(soibfull,concerncats)

write.csv(soib,"stateofindiasbirds.csv",row.names=FALSE,na="")
write.csv(soibfull,"stateofindiasbirdsfull.csv",row.names=FALSE,na="")
