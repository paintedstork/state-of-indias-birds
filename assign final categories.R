## assign categories to trends and occupancy

library(tidyverse)
source('~/GitHub/state-of-indias-birds/SoIB functions.R')
migstatus = read.csv("Migratory Status - Migratory Status.csv")
load("specieslists.RData")
allspecies = specieslist$COMMON.NAME

glmr = read.csv("glmr.csv")

glmr$mintrend = glmr$trend - glmr$trendci
glmr$maxtrend = glmr$trend + glmr$trendci
glmr$minslope = glmr$slope - glmr$slopeci
glmr$maxslope = glmr$slope + glmr$slopeci

glmr$mintrend[glmr$mintrend < -100] = -100

trendscat = glmr %>%
  mutate(longcat = 
           case_when(is.na(trend) ~ "Data Deficient",
                     maxtrend <= -50 ~ "Strong Decline",
                     maxtrend <= -25 ~ "Moderate Decline",
                     mintrend >= 50 ~ "Strong Increase",
                     mintrend >= 25 ~ "Moderate Increase",
                     trendci > 25 ~ "Uncertain",
                     TRUE ~ "Stable")
  ) %>%
  mutate(shortcat = 
           case_when(is.na(slope) ~ "Data Deficient",
                     maxslope <= -2.7 ~ "Strong Decline",
                     maxslope <= -1.1 ~ "Moderate Decline",
                     minslope >= 1.6 ~ "Strong Increase",
                     minslope >= 0.9 ~ "Moderate Increase",
                     slopeci > 2 ~ "Uncertain",
                     TRUE ~ "Stable")
  ) %>%
  select(species,trend,trendci,mintrend,maxtrend,slope,slopeci,minslope,maxslope,longcat,shortcat)

trendscat$longcat[trendscat$species %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                           "Taiga Flycatcher","Chestnut Munia")] = NA
trendscat$shortcat[trendscat$species %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                            "Taiga Flycatcher","Chestnut Munia")] = NA

trendscat$longcat = factor(trendscat$longcat, levels = c("Strong Decline","Moderate Decline",
                                                         "Data Deficient","Uncertain","Stable",
                                                         "Moderate Increase","Strong Increase"))
trendscat$shortcat = factor(trendscat$shortcat, levels = c("Strong Decline","Moderate Decline",
                                                           "Data Deficient","Uncertain","Stable",
                                                           "Moderate Increase","Strong Increase"))




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
                     maxrange < 0.75 ~ "Very Restricted",
                     maxrange < 4.25 ~ "Restricted",
                     minrange > 100 ~ "Very Large",
                     minrange > 25 ~ "Large",
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

trendscat = trendscat %>%
  mutate(trendfull = "NA") %>%
  mutate(trendfull = case_when(!is.na(trendci) ~ paste(round(trend,2),"±",round(trendci,2),sep=""),
                               TRUE ~ "NA")
  )
trendscat$trendfull[trendscat$trendfull == "NA"] = NA

trendscat = trendscat %>%
  mutate(slopefull = "NA") %>%
  mutate(slopefull = case_when(!is.na(slopeci) ~ paste(round(slope,2),"±",round(slopeci,2),sep=""),
                               TRUE ~ "NA")
  )
trendscat$slopefull[trendscat$slopefull == "NA"] = NA

map = read.csv("Map to Other Lists - map.csv")

species2018 = read.csv("eBird-Clements-v2018-integrated-checklist-August-2018.csv")
species2018 = species2018 %>%
  filter(category == "species") %>%
  distinct(English.name) %>%
  filter(English.name %in% allspecies)
names(species2018) = "species"


soib = trendscat %>% left_join(occuse) %>%
  dplyr::left_join(map,by = c("species" = "eBird.English.Name.2018"))
soib = left_join(species2018,soib)

soib = soib %>%
  select(eBird.English.Name.2019,eBird.Scientific.Name.2019,migstatus,IUCN,Schedule,trendfull,
         slopefull,rangefull,longcat,shortcat,occcat)

soibfull = trendscat %>% left_join(occuse) %>%
  left_join(map,by = c("species" = "eBird.English.Name.2018")) 
soibfull = left_join(species2018,soibfull)

soibfull = soibfull %>%
  select(eBird.English.Name.2019,eBird.Scientific.Name.2019,migstatus,IUCN,Schedule,CITES.Appendix,
         CMS.Appendix,trend,mintrend,maxtrend,trendfull,slope,minslope,maxslope,
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

concerncats = read.csv("concernclassification - concernclassification.csv")
soib = left_join(soib,concerncats)
soibfull = left_join(soibfull,concerncats)

downlist = c("Data Deficient","Uncertain")
rrange = c("Very Restricted","Restricted")
decl = c("Moderate Decline","Strong Decline")
soib = soib %>%
  mutate(Concern.Status = as.character(Concern.Status)) %>%
  mutate(Concern.Status = 
           case_when(Long.Term.Status %in% downlist & Current.Status %in% downlist &
                       IUCN %in% c("Endangered","Critically Endangered") ~ "High",
                     Long.Term.Status %in% decl & Current.Status %in% decl &
                       IUCN %in% c("Endangered","Critically Endangered") ~ "High",
                     Long.Term.Status %in% downlist & Current.Status %in% downlist &
                       Range.Status %in% rrange &
                       IUCN %in% c("Vulnerable") ~ "High",
                     Long.Term.Status %in% downlist & Current.Status %in% downlist &
                       IUCN %in% c("Near Threatened","Vulnerable") & Concern.Status == "Low" ~ "Moderate",
                     TRUE ~ Concern.Status))

soibfull = soibfull %>%
  mutate(Concern.Status = as.character(Concern.Status)) %>%
  mutate(Concern.Status = 
           case_when(Long.Term.Status %in% downlist & Current.Status %in% downlist &
                       IUCN %in% c("Endangered","Critically Endangered") ~ "High",
                     Long.Term.Status %in% decl & Current.Status %in% decl &
                       IUCN %in% c("Endangered","Critically Endangered") ~ "High",
                     Long.Term.Status %in% downlist & Current.Status %in% downlist &
                       Range.Status %in% rrange &
                       IUCN %in% c("Vulnerable") ~ "High",
                     Long.Term.Status %in% downlist & Current.Status %in% downlist &
                       IUCN %in% c("Near Threatened","Vulnerable") & Concern.Status == "Low" ~ "Moderate",
                     TRUE ~ Concern.Status))

write.csv(soib,"stateofindiasbirds.csv",row.names=FALSE,na="")
write.csv(soibfull,"stateofindiasbirdsfull.csv",row.names=FALSE,na="")

soib = soib %>% filter(!is.na(Long.Term.Status),!is.na(Current.Status))

soib$Concern.Status = factor(soib$Concern.Status, 
                              levels = c("Low","Moderate","High"))
soib$Long.Term.Status = factor(soib$Long.Term.Status, 
                               levels = c("Strong Decline","Moderate Decline","Data Deficient",
                                          "Uncertain","Stable","Moderate Increase","Strong Increase"))
soib$Current.Status = factor(soib$Current.Status, 
                             levels = c("Strong Decline","Moderate Decline","Data Deficient",
                                        "Uncertain","Stable","Moderate Increase","Strong Increase"))
soib$Range.Status = factor(soib$Range.Status, 
                             levels = c("Data Deficient","Very Restricted","Restricted","Moderate",
                                        "Large","Very Large"))

print(table(soib$Concern.Status))
length(soib$Concern.Status[soib$Concern.Status == "High" &
                             soib$Long.Term.Status %in% downlist &
                             soib$Current.Status %in% downlist])
print(table(soib$Long.Term.Status))
print(table(soib$Current.Status))
print(table(soib$Range.Status))


soib = read.csv("stateofindiasbirds.csv")
soib = soib %>%
  filter(Concern.Status == "High") %>%
  select(-Migratory.Status,-Long.Term.Trend,-Current.Annual.Change,-Range.Size)
write.csv(soib,"highconcern.csv",row.names=FALSE,na="")

#write(paste(soib$eBird.English.Name, collapse = ', '), 'test.txt')