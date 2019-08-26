## select species for composite trends

map = read.csv("Map to Other Lists - map.csv")

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)
load("dataforanalyses.RData")
rm(data)

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

taxgroups = read.csv("eBird-Clements-v2018-integrated-checklist-August-2018.csv")
habitat = read.csv("Habitat - Habitat.csv")
migration = read.csv("Migratory Status - Migratory Status.csv")
endemism = read.csv("Endemicity - Endemicity.csv")
diet = read.csv("Diet - Food Type.csv")

setdiff(migration$eBird.English.Name,habitat$eBird.English.Name)

taxgroups = taxgroups %>%
  filter(category == "species", English.name %in% specieslist$COMMON.NAME) %>%
  select(English.name,eBird.species.group,order,family)

ltset = specieslist %>% filter(ht == 1)
ltset = ltset$COMMON.NAME



############## Habitat specialists

cats = names(habitat)[3:41]

tropcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp")
temp1 = habitat %>% select(tropcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(tropcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,tropcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,tropcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$tropspecialist = temp1$sum - temp2$sum
habitat$tropspecialist[habitat$tropspecialist < 0] = 0

tempcats = c("Subtropical.Montane.Forest","Mixed.Forest","Oak.Forest","Coniferous.Forest","Bamboo.Forest",
             "Forest.Swamp","High.Shrubbery","High.Alpine")
temp1 = habitat %>% select(tempcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(tempcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,tempcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,tempcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$tempspecialist = temp1$sum - temp2$sum
habitat$tempspecialist[habitat$tempspecialist < 0] = 0

grasscats = c("Wet.Paddy.fields","Open.Areas","Dry.Grassland","Hill.Grassland","Wet.Grassland",
              "High.Altitude.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","High.Alpine",
              "Hot.Desert","Semi.Desert")
temp1 = habitat %>% select(grasscats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(grasscats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,grasscats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,grasscats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$grassspecialist = temp1$sum - temp2$sum
habitat$grassspecialist[habitat$grassspecialist < 0] = 0

wetcats = c("Wet.Paddy.fields","Wet.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","Inland.Water",
            "Inland.Mudflats","Coastal.Mudflats","Beaches","Pelagic.Waters","Mangrove.Swamps",
            "Salt.Pans","Lagoons","Rivers","High.Altitude.Lakes","Brackish.Backwaters")
temp1 = habitat %>% select(wetcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(wetcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,wetcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,wetcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$wetspecialist = temp1$sum - temp2$sum
habitat$wetspecialist[habitat$wetspecialist < 0] = 0

scrubcats = c("Open.Areas","Dry.Deciduous","Hill.Shrubland","Thorny.Scrub","Rocky.Scrub","Bamboo.Forest",
              "High.Shrubbery","Semi.Desert")
temp1 = habitat %>% select(scrubcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(scrubcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,scrubcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,scrubcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$scrubspecialist = temp1$sum - temp2$sum
habitat$scrubspecialist[habitat$scrubspecialist < 0] = 0


############ Migrants

migration = migration %>%
  mutate(mig = 
           case_when(!is.na(Resident) ~ "R",
                     !is.na(Summer.Visitor) & !is.na(Winter.Visitor) ~ "LM",
                     !is.na(Summer.Visitor) ~ "M",
                     !is.na(Winter.Visitor) | !is.na(Strictly.Passage) ~ "M",
                     !is.na(Uncertain.Vagrant) & is.na(Resident) ~ "U",
                     TRUE ~ "R")
  ) %>%
  select(eBird.English.Name,mig)
resident = as.character(migration$eBird.English.Name[migration$mig == "R"])
localmigrant = as.character(migration$eBird.English.Name[migration$mig == "LM"])
migrant = as.character(migration$eBird.English.Name[migration$mig == "M"])




########### Diet groups

frne = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "FruiNect"])
invt = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "Invertebrate"])
omni = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "Omnivore"])
seed = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "PlantSeed"])
carn = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "VertFishScav"])



########### Endemics

# WG
# Peninsular
# Him/NE

wg = as.character(endemism$eBird.English.Name[!is.na(endemism$Western.Ghats)])
extra = c("Southern Hill Myna","Dark-fronted Babbler","Yellow-browed Bulbul","Square-tailed Bulbul",
          "Hill Swallow","Orange Minivet","Sri Lanka Bay-Owl","Indian Swiftlet","Sri Lanka Frogmouth",
          "Painted Bush-Quail")
wg = c(wg,extra)

hine = as.character(endemism$eBird.English.Name[!is.na(endemism$Western.Himalayas) |
                                                  !is.na(endemism$Eastern.Himalayas) |
                                                  !is.na(endemism$Himalayas) |
                                                  !is.na(endemism$Assam.Plains) |
                                                  !is.na(endemism$Indus.Plains) |
                                                  !is.na(endemism$Northern.Myanmar.Lowlands)])
extra = c("Slender-billed Babbler")
hine = c(hine,extra)

sub = as.character(endemism$eBird.English.Name[!is.na(endemism$Subcontinent) &
                                                 !endemism$eBird.English.Name %in% wg &
                                                 !endemism$eBird.English.Name %in% hine &
                                                 is.na(endemism$Andaman.and.Nicobar.islands)])



####################### Raptors

taxset1 = taxgroups %>%
  filter(order %in% c("Falconiformes","Accipitriformes"), English.name %in% ltset)

# Scavengers
scav = diet %>% filter(Diet.Scav >= 60) %>% select(eBird.English.Name)
scav = as.character(scav$eBird.English.Name)
raptorset4 = taxset1 %>% filter(English.name %in% scav) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% select(eBird.English.Name.2019)
raptorset4 = as.character(raptorset4$eBird.English.Name.2019)


# Commensals
raptorset1 = habitat %>% filter(Habitation == 1, eBird.English.Name %in% taxset1$English.name) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
raptorset1 = as.character(raptorset1$eBird.English.Name.2019)

# Wooded habitat
raptorset2 = habitat %>% filter(Groves.Gardens.and.Plantations == 1 | Tropical.Rainforest == 1 |
                             Shola.Forest == 1 | Moist.Deciduous == 1 | Dry.Deciduous == 1 |
                             Subtropical.Montane.Forest | Mixed.Forest == 1 | Oak.Forest == 1 |
                             Coniferous.Forest == 1 | Forest.Swamp == 1 | Rivers == 1,
                             eBird.English.Name %in% taxset1$English.name,
                             !eBird.English.Name %in% raptorset4) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
raptorset2 = as.character(raptorset2$eBird.English.Name.2019)

# Open habitat
raptorset3 = habitat %>% filter(Wet.Paddy.fields == 1 | Open.Areas == 1 |
                             Dry.Grassland == 1 | Hill.Grassland == 1 | Wet.Grassland |
                             High.Altitude.Grassland | Reeds == 1 |
                             Hill.Shrubland == 1 | Thorny.Scrub == 1 | Rocky.Scrub == 1 |
                             Short.Grass.Marshes == 1 | High.Shrubbery == 1 | High.Alpine == 1 |
                             Inland.Water == 1 | Inland.Mudflats == 1 | Coastal.Mudflats == 1 |
                             Beaches == 1 |Hot.Desert == 1 | Semi.Desert == 1 | Cold.Desert == 1 |
                             Mangrove.Swamps == 1 | Salt.Pans == 1 | Lagoons == 1 |
                             High.Altitude.Lakes == 1,
                             eBird.English.Name %in% taxset1$English.name,
                             !eBird.English.Name %in% raptorset4) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
raptorset3 = as.character(raptorset3$eBird.English.Name.2019)

raptors = list(raptorset1,raptorset2,raptorset3,raptorset4)
names(raptors) = c("Commensals","Habitat-Wooded","Habitat-Open","Scavengers")



####################### Waterbirds

#- resident waterbirds (waterfowl)
#- resident waterbirds (other)
#- migratory waterbirds (waterfowl)
#- migratory waterbirds (shorebirds)
#- migratory waterbirds (other)

waterbirds = habitat %>% filter(wetspecialist == 1)
waterbirds = as.character(waterbirds$eBird.English.Name)

taxset2res = taxgroups %>%
  filter(order %in% c("Pelecaniformes","Suliformes","Ciconiiformes","Charadriiformes","Gruiformes",
                      "Podicipediformes","Phoenicopteriformes","Anseriformes") | 
                      English.name %in% waterbirds, English.name %in% ltset, 
                      English.name %in% resident)

taxset2mig = taxgroups %>%
  filter(order %in% c("Pelecaniformes","Suliformes","Ciconiiformes","Charadriiformes","Gruiformes",
                      "Podicipediformes","Phoenicopteriformes","Anseriformes") | 
           English.name %in% waterbirds, English.name %in% ltset, 
         English.name %in% migrant | English.name %in% localmigrant)


waterset1 = taxset2res %>% filter(eBird.species.group %in% c("Waterfowl","Grebes")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset1 = as.character(waterset1$eBird.English.Name.2019)

waterset2 = taxset2res %>% filter(!English.name %in% waterset1,English.name %in% waterbirds) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset2 = as.character(waterset2$eBird.English.Name.2019)

waterset3 = taxset2mig %>% filter(eBird.species.group %in% c("Waterfowl","Grebes")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset3 = as.character(waterset3$eBird.English.Name.2019)

waterset4 = taxset2mig %>% filter(eBird.species.group %in% c("Shorebirds")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset4 = as.character(waterset4$eBird.English.Name.2019)

waterset5 = taxset2mig %>% filter(!English.name %in% c(waterset3,waterset4),English.name %in% 
                                    waterbirds) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset5 = as.character(waterset5$eBird.English.Name.2019)

waterbirds = list(waterset1,waterset2,waterset3,waterset4,waterset5)
names(waterbirds) = c("Waterfowl-Resident","Others-Resident","Waterfowl-Migratory","Shorebirds-Migratory",
                      "Others-Migratory")




############# Habitat groups
# tropical
# temperate
# grassland
# wetland
# scrub
# generalists

habitatset1 = habitat %>% filter(tropspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset1 = as.character(habitatset1$eBird.English.Name.2019)

habitatset2 = habitat %>% filter(tempspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset2 = as.character(habitatset2$eBird.English.Name.2019)

habitatset3 = habitat %>% filter(grassspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset3 = as.character(habitatset3$eBird.English.Name.2019)

habitatset4 = habitat %>% filter(wetspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset4 = as.character(habitatset4$eBird.English.Name.2019)

habitatset5 = habitat %>% filter(scrubspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset5 = as.character(habitatset5$eBird.English.Name.2019)

habitatset6 = data.frame(eBird.English.Name = ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset6 = as.character(habitatset6$eBird.English.Name.2019)
habitatset6 = setdiff(habitatset6,unique(c(habitatset1,habitatset2,habitatset3,habitatset4,habitatset5)))

habitats = list(habitatset1,habitatset2,habitatset3,habitatset4,habitatset5,habitatset6)
names(habitats) = c("Forests-Tropical","Forests-Temperate","Grassland","Wetlands","Scrub","Generalists")




########## Migrants

migrantset1 = data.frame(eBird.English.Name = resident[resident %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
migrantset1 = as.character(migrantset1$eBird.English.Name.2019)

migrantset2 = data.frame(eBird.English.Name = localmigrant[localmigrant %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
migrantset2 = as.character(migrantset2$eBird.English.Name.2019)

migrantset3 = data.frame(eBird.English.Name = migrant[migrant %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
migrantset3 = as.character(migrantset3$eBird.English.Name.2019)

migrants = list(migrantset1,migrantset2,migrantset3)
names(migrants) = c("Resident","Migratory-Local","Migratory-Long-Distance")



########## Endemism

endemicset1 = data.frame(eBird.English.Name = wg[wg %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
endemicset1 = as.character(endemicset1$eBird.English.Name.2019)
  
  
endemicset2 = data.frame(eBird.English.Name = sub[sub %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
endemicset2 = as.character(endemicset2$eBird.English.Name.2019)
  

endemics = list(endemicset1,endemicset2)
names(endemics) = c("Western-Ghats","Others-Subcontinent")


########## Diet

dietset1 = data.frame(eBird.English.Name = frne[frne %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
dietset1 = as.character(dietset1$eBird.English.Name.2019)
  
dietset2 = data.frame(eBird.English.Name = invt[invt %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
dietset2 = as.character(dietset2$eBird.English.Name.2019)
  
dietset3 = data.frame(eBird.English.Name = omni[omni %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
dietset3 = as.character(dietset3$eBird.English.Name.2019)
  
dietset4 = data.frame(eBird.English.Name = seed[seed %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
dietset4 = as.character(dietset4$eBird.English.Name.2019)
  
dietset5 = data.frame(eBird.English.Name = carn[carn %in% ltset]) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
dietset5 = as.character(dietset5$eBird.English.Name.2019)

diets = list(dietset1,dietset2,dietset3,dietset4,dietset5)
names(diets) = c("Fruit-Nectar","Invertebrate","Omnivorous","Plant-Seed","Carnivorous")


rm(list=setdiff(ls(envir = .GlobalEnv), c("raptors","waterbirds","habitats","migrants","endemics",
                                          "diets")), pos = ".GlobalEnv")

diets = data.frame(groups = rep(names(diets),lengths(diets)), species = unlist(diets, use.names = F))
endemics = data.frame(groups = rep(names(endemics),lengths(endemics)), 
                      species = unlist(endemics, use.names = F))
habitats = data.frame(groups = rep(names(habitats),lengths(habitats)), 
                      species = unlist(habitats, use.names = F))
migrants = data.frame(groups = rep(names(migrants),lengths(migrants)), 
                      species = unlist(migrants, use.names = F))
raptors = data.frame(groups = rep(names(raptors),lengths(raptors)), 
                     species = unlist(raptors, use.names = F))
waterbirds = data.frame(groups = rep(names(waterbirds),lengths(waterbirds)), 
                        species = unlist(waterbirds, use.names = F))

diets$composite = "composite-diet"
endemics$composite = "composite-endemics"
habitats$composite = "composite-habitats"
migrants$composite = "composite-migrants"
raptors$composite = "composite-raptors"
waterbirds$composite = "composite-waterbirds"

compositespecieslist = rbind(diets,endemics,habitats,migrants,raptors,waterbirds) %>%
  select(composite,groups,species)

status = read.csv("stateofindiasbirds.csv")
status = status %>% filter(Long.Term.Status == "Uncertain")
status = as.character(status$eBird.English.Name)

compositespecieslist = compositespecieslist %>% filter(!species %in% status)

write.csv(compositespecieslist,"compositespecieslist.csv",row.names = F)
