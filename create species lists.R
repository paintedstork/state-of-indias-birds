## select species for composite trends

map = read.csv("Map to Other Lists - map.csv")

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
library(tidyverse)
load("specieslists.RData")

check1 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$ht)]
check2 = restrictedspecieslist$COMMON.NAME[!is.na(restrictedspecieslist$rt)]

specieslist$rt[specieslist$COMMON.NAME %in% check2] = 1
specieslist$ht[specieslist$COMMON.NAME %in% check1] = 1

taxgroups = read.csv("eBird-Clements-v2018-integrated-checklist-August-2018.csv")
habitat = read.csv("Habitat - Habitat.csv")
migration = read.csv("Migratory Status - Migratory Status.csv")
endemism = read.csv("Endemicity - Endemicity.csv")
diet = read.csv("Diet - Food Type.csv")

taxgroups = taxgroups %>%
  filter(category == "species", English.name %in% specieslist$COMMON.NAME) %>%
  select(English.name,eBird.species.group,order,family)

ltset = specieslist %>% filter(ht == 1)
ltset = ltset$COMMON.NAME



############## Habitat specialists

cats = names(habitat)[3:41]

tropcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp",
             "Mangrove.Swamps")
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

forcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp",
            "Mangrove.Swamps","Subtropical.Montane.Forest","Mixed.Forest","Oak.Forest",
            "Coniferous.Forest","High.Shrubbery","High.Alpine","Dry.Deciduous")
temp1 = habitat %>% select(forcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(forcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,forcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,forcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$forspecialist = temp1$sum - temp2$sum
habitat$forspecialist[habitat$forspecialist < 0] = 0

woodcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp",
             "Subtropical.Montane.Forest","Mixed.Forest","Oak.Forest","Coniferous.Forest",
             "High.Shrubbery","High.Alpine","Groves.Gardens.and.Plantations",
             "Dry.Deciduous","Thorny.Scrub","Rocky.Scrub","Mangrove.Swamps")
temp1 = habitat %>% select(woodcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(woodcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,woodcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,woodcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$woodspecialist = temp1$sum - temp2$sum
habitat$woodspecialist[habitat$woodspecialist < 0] = 0

grasscats = c("Wet.Paddy.fields","Open.Areas","Dry.Grassland","Hill.Grassland","Wet.Grassland",
              "High.Altitude.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","High.Alpine",
              "Hot.Desert","Semi.Desert","Salt.Pans")
temp1 = habitat %>% select(grasscats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(grasscats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,grasscats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,grasscats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$grassspecialist = temp1$sum - temp2$sum
habitat$grassspecialist[habitat$grassspecialist < 0] = 0

wetcats = c("Wet.Paddy.fields","Wet.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","Inland.Water",
            "Inland.Mudflats","Coastal.Mudflats","Beaches","Pelagic.Waters","Mangrove.Swamps",
            "Salt.Pans","Lagoons","Rivers","High.Altitude.Lakes","Brackish.Backwaters","Habitation")
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

grubcats = c("Wet.Paddy.fields","Open.Areas","Dry.Grassland","Hill.Grassland","Wet.Grassland",
             "High.Altitude.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","High.Alpine",
             "Hot.Desert","Semi.Desert","Salt.Pans","Dry.Deciduous",
             "Hill.Shrubland","Thorny.Scrub","Rocky.Scrub","Bamboo.Forest","High.Shrubbery")
temp1 = habitat %>% select(grubcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(grubcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,grubcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,grubcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$grubspecialist = temp1$sum - temp2$sum
habitat$grubspecialist[habitat$grubspecialist < 0] = 0

opencats = c("Wet.Paddy.fields","Wet.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes",
             "Inland.Water","Inland.Mudflats","Coastal.Mudflats","Beaches","Pelagic.Waters",
             "Mangrove.Swamps","Salt.Pans","Lagoons","Rivers","High.Altitude.Lakes","Brackish.Backwaters",
             "Open.Areas","Dry.Grassland","Hill.Grassland","High.Altitude.Grassland",
             "High.Alpine","Hot.Desert","Semi.Desert","Dry.Deciduous","Hill.Shrubland","Thorny.Scrub",
             "Rocky.Scrub","Bamboo.Forest","High.Shrubbery")
temp1 = habitat %>% select(opencats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(opencats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,opencats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,opencats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$openspecialist = temp1$sum - temp2$sum
habitat$openspecialist[habitat$openspecialist < 0] = 0


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




###################### Waterbirds

waterspecs = habitat %>% filter(wetspecialist == 1)
waterspecs = as.character(waterspecs$eBird.English.Name)

waterbirdfilter = taxgroups %>%
  filter(order %in% c("Pelecaniformes","Suliformes","Ciconiiformes","Charadriiformes","Gruiformes",
                      "Podicipediformes","Phoenicopteriformes","Anseriformes","Coraciiformes"),
         English.name %in% waterspecs)

waterbirdfilter = as.character(waterbirdfilter$English.name)





####################### Raptors

taxset1 = taxgroups %>%
  filter(order %in% c("Falconiformes","Accipitriformes"), English.name %in% ltset)

# Scavengers
scav = diet %>% filter(Diet.Scav >= 60) %>% select(eBird.English.Name)
scav = as.character(scav$eBird.English.Name)
raptorset4 = taxset1 %>% filter(English.name %in% scav) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% select(eBird.English.Name.2019)
raptorset4 = as.character(raptorset4$eBird.English.Name.2019)


# Wooded habitat
raptorset2 = habitat %>% filter(woodspecialist == 1,
                             eBird.English.Name %in% taxset1$English.name,
                             !eBird.English.Name %in% raptorset4) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
raptorset2 = as.character(raptorset2$eBird.English.Name.2019)

# Open habitat including scrub and wetlands
raptorset3 = habitat %>% filter(openspecialist == 1, 
                             eBird.English.Name %in% taxset1$English.name,
                             !eBird.English.Name %in% raptorset4) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
raptorset3 = as.character(raptorset3$eBird.English.Name.2019)

# Generalists

raptorset1 = data.frame(eBird.English.Name = taxset1$English.name) %>%
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
raptorset1 = as.character(raptorset1$eBird.English.Name.2019)
raptorset1 = c(setdiff(raptorset1,unique(c(raptorset2,raptorset3,raptorset4))),"Black Kite")

raptors = list(raptorset1,raptorset2,raptorset3,raptorset4)
names(raptors) = c("Generalists","Habitat-Wooded","Habitat-Open","Scavengers")





####################### Waterbirds

#- resident waterbirds (waterfowl)
#- resident waterbirds (other)
#- migratory waterbirds (waterfowl)
#- migratory waterbirds (shorebirds)
#- migratory waterbirds (other)

taxset2res = taxgroups %>%
  filter(English.name %in% waterbirdfilter, English.name %in% ltset,
         English.name %in% resident)

taxset2mig = taxgroups %>%
  filter(English.name %in% waterbirdfilter, English.name %in% ltset, 
         English.name %in% migrant | English.name %in% localmigrant)


waterset1 = taxset2res %>% filter(eBird.species.group %in% c("Waterfowl")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset1 = as.character(waterset1$eBird.English.Name.2019)

waterset1a = taxset2mig %>% filter(eBird.species.group %in% c("Waterfowl")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset1a = as.character(waterset1a$eBird.English.Name.2019)

waterset1 = c(waterset1,waterset1a)

waterset2 = taxset2res %>% filter(family %in% c("Laridae (Gulls, Terns, and Skimmers)")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset2 = as.character(waterset2$eBird.English.Name.2019)

waterset2a = taxset2mig %>% filter(family %in% c("Laridae (Gulls, Terns, and Skimmers)")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset2a = as.character(waterset2a$eBird.English.Name.2019)

waterset2 = c(waterset2,waterset2a)

waterset3 = taxset2mig %>% filter(eBird.species.group %in% c("Shorebirds")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset3 = as.character(waterset3$eBird.English.Name.2019)

waterset4 = taxset2res %>% filter(!English.name %in% c(waterset1,waterset2)) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
waterset4 = as.character(waterset4$eBird.English.Name.2019)



waterbirds = list(waterset1,waterset2,waterset3,waterset4)
names(waterbirds) = c("Waterfowl","Gulls and Terns","Shorebirds-Migratory","Others-Resident")




############# Habitat groups
# tropical
# temperate
# grassland
# wetland
# scrub
# generalists

habitatset1 = habitat %>% filter(forspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset1 = as.character(habitatset1$eBird.English.Name.2019)

habitatset2 = habitat %>% filter(grubspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset2 = as.character(habitatset2$eBird.English.Name.2019)

habitatset3 = habitat %>% filter(wetspecialist == 1, eBird.English.Name %in% ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset3 = as.character(habitatset3$eBird.English.Name.2019)

habitatset4 = data.frame(eBird.English.Name = ltset) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(eBird.English.Name.2019)
habitatset4 = as.character(habitatset4$eBird.English.Name.2019)
habitatset4 = setdiff(habitatset4,unique(c(habitatset1,habitatset2,habitatset3)))

habitats = list(habitatset1,habitatset2,habitatset3,habitatset4)
names(habitats) = c("Forests","Grassland/Scrub","Wetlands","Generalists")




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


########### Diet groups

frne = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "FruiNect" &
                                              !diet$eBird.English.Name %in% waterbirdfilter])
invt = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "Invertebrate" &
                                              !diet$eBird.English.Name %in% waterbirdfilter])
omni = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "Omnivore" &
                                              !diet$eBird.English.Name %in% waterbirdfilter])
seed = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "PlantSeed" &
                                              !diet$eBird.English.Name %in% waterbirdfilter])
carn = as.character(diet$eBird.English.Name[diet$Diet.5Cat == "VertFishScav" &
                                              !diet$eBird.English.Name %in% waterbirdfilter])

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
uncertain = status %>% filter(Long.Term.Status == "Uncertain")
uncertain = as.character(uncertain$eBird.English.Name)
status = status %>%
  select(eBird.English.Name,Long.Term.Status,Current.Status)
names(status)[1] = "species"

compositespecieslist = compositespecieslist %>% filter(!species %in% uncertain)

write.csv(compositespecieslist,"compositespecieslist.csv",row.names = F)

compositesummary = left_join(compositespecieslist,status)



temp1 = compositesummary %>%
  group_by(composite,groups,Long.Term.Status) %>% summarize(count = n())
temp1$Long.Term.Status = factor(temp1$Long.Term.Status, 
                                           levels = c("Strong Decline","Moderate Decline","Stable",
                                                      "Moderate Increase","Strong Increase"))

temp2 = compositesummary %>%
  filter(!Current.Status == "Uncertain") %>%
  group_by(composite,groups,Current.Status) %>% summarize(count = n())
temp2$Current.Status = factor(temp2$Current.Status, 
                                         levels = c("Strong Decline","Moderate Decline","Stable",
                                                    "Moderate Increase","Strong Increase"))

total1 = compositesummary %>%
  group_by(composite) %>% summarize(compositetotal = n_distinct(species))
total2 = compositesummary %>%
  group_by(composite,groups) %>% summarize(grouptotal = n_distinct(species))

library(reshape)
longtermcompositesummary = cast(temp1, composite + groups ~ Long.Term.Status, fill=F)
longtermcompositesummary = longtermcompositesummary %>% left_join(total1) %>% left_join(total2)
currentcompositesummary = cast(temp2, composite + groups ~ Current.Status, fill=F)
currentcompositesummary = currentcompositesummary %>% left_join(total1) %>% left_join(total2)

longtermcompositesummary$composite = paste(longtermcompositesummary$composite," (",
                                           longtermcompositesummary$compositetotal,")",sep="")
longtermcompositesummary$groups = paste(longtermcompositesummary$groups," (",
                                           longtermcompositesummary$grouptotal,")",sep="")
longtermcompositesummary = longtermcompositesummary %>%
  select(-compositetotal,-grouptotal)

currentcompositesummary$composite = paste(currentcompositesummary$composite," (",
                                           currentcompositesummary$compositetotal,")",sep="")
currentcompositesummary$groups = paste(currentcompositesummary$groups," (",
                                        currentcompositesummary$grouptotal,")",sep="")
currentcompositesummary = currentcompositesummary %>%
  select(-compositetotal,-grouptotal)

write.csv(longtermcompositesummary,"longtermcompositesummary.csv",row.names = F)
write.csv(currentcompositesummary,"currentcompositesummary.csv",row.names = F)