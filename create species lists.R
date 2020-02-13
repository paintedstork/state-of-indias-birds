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
ltset = ltset %>% left_join(map, by = c("COMMON.NAME" = "eBird.English.Name.2018"))
ltset = as.character(ltset$India.Checklist.Name)

master = read.csv("stateofindiasbirdsfull.csv")
map1 = map[,c(1,3,5)]
master = left_join(master,map1,by = c("Common.Name" = "India.Checklist.Name"))
master = master %>% distinct(Common.Name, .keep_all= TRUE)
master = left_join(master,diet,by = c("eBird.English.Name.2018" = "eBird.English.Name"))
master$Long.Term.Trend = master$Long.Term.Trend.Mean
master$Long.Term.Trend.CI = master$Long.Term.Trend.Upper - master$Long.Term.Trend.Mean
master$Current.Annual.Change = master$Current.Annual.Change.Mean
master$Current.Annual.Change.CI = master$Current.Annual.Change.Upper - master$Current.Annual.Change.Mean
master$Range.Size = master$Range.Size.Mean
master$Range.Size.CI = master$Range.Size.Upper - master$Range.Size.Mean
master = left_join(master,specieslist,by = c("eBird.English.Name.2018" = "COMMON.NAME"))
master$ht[master$ht == 1] = "X"
master$rt[master$rt == 1] = "X"
master$ht[is.na(master$ht)] = ""
master$rt[is.na(master$rt)] = ""
names(master)[67:68] = c("Long.Term.Analysis","Current.Analysis")
master = left_join(master,taxgroups,by = c("eBird.English.Name.2018" = "English.name"))
master = master %>% select(Common.Name,Scientific.Name,eBird.English.Name.2018,eBird.English.Name.2019,
                           eBird.species.group,order,family,IUCN,WLPA.Schedule,
                           Long.Term.Analysis,Current.Analysis,Long.Term.Trend,Long.Term.Trend.CI,
                           Current.Annual.Change,Current.Annual.Change.CI,Range.Size,
                           Range.Size.CI,Long.Term.Status,Current.Status,Range.Status,Concern.Status,
                           Migratory.Status,Diet.5Cat)
names(master)[23] = "Diet.Category"


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


master$Migratory.Status = "Resident"
master$Migratory.Status[master$eBird.English.Name.2018 %in% localmigrant] = "Migratory-Local"
master$Migratory.Status[master$eBird.English.Name.2018 %in% migrant] = "Migratory-Long-Distance"


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

master$Waterbirds = ""
master$Waterbirds[master$eBird.English.Name.2018 %in% waterbirdfilter] = "X"



####################### Raptors

taxset1 = taxgroups %>%
  filter(order %in% c("Falconiformes","Accipitriformes"))

# Scavengers
scav = diet %>% filter(Diet.Scav >= 60) %>% select(eBird.English.Name)
scav = as.character(scav$eBird.English.Name)
raptorset4 = taxset1 %>% filter(English.name %in% scav) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% select(India.Checklist.Name)
raptorset4 = as.character(raptorset4$India.Checklist.Name)



# Wooded habitat
raptorset2 = habitat %>% filter(woodspecialist == 1,
                             eBird.English.Name %in% taxset1$English.name) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  filter(!India.Checklist.Name %in% raptorset4) %>%
  select(India.Checklist.Name)
raptorset2 = as.character(raptorset2$India.Checklist.Name)

# Open habitat including scrub and wetlands
raptorset3 = habitat %>% filter(openspecialist == 1, 
                             eBird.English.Name %in% taxset1$English.name) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  filter(!India.Checklist.Name %in% raptorset4 | India.Checklist.Name == "Griffon Vulture") %>%
  select(India.Checklist.Name)
raptorset3 = as.character(raptorset3$India.Checklist.Name)

# Generalists

raptorset1 = data.frame(eBird.English.Name = taxset1$English.name) %>%
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
raptorset1 = as.character(raptorset1$India.Checklist.Name)
raptorset1 = c(setdiff(raptorset1,unique(c(raptorset2,raptorset3,raptorset4))),"Black Kite")

master$Raptors = ""
master$Raptors[master$eBird.English.Name.2018 %in% taxset1$English.name] = "X"

master$Raptors.Scavengers = ""
master$Raptors.Scavengers[master$Common.Name %in% raptorset4] = "X"

master$Raptors.Composite = ""
master$Raptors.Composite[master$Common.Name %in% raptorset1] = "Generalists"
master$Raptors.Composite[master$Common.Name %in% raptorset2] = "Habitat-Wooded"
master$Raptors.Composite[master$Common.Name %in% raptorset3] = "Habitat-Open"

raptorset1 = intersect(raptorset1,ltset)
raptorset2 = intersect(raptorset2,ltset)
raptorset3 = intersect(raptorset3,ltset)
raptorset4 = intersect(raptorset4,ltset)

raptors = list(raptorset1,raptorset2,raptorset3,raptorset4)
names(raptors) = c("Generalists","Habitat-Wooded","Habitat-Open","Scavengers")




####################### Waterbirds

#- resident waterbirds (waterfowl)
#- resident waterbirds (other)
#- migratory waterbirds (waterfowl)
#- migratory waterbirds (shorebirds)
#- migratory waterbirds (other)

taxset2res = taxgroups %>%
  filter(English.name %in% waterbirdfilter,
         English.name %in% resident)

taxset2mig = taxgroups %>%
  filter(English.name %in% waterbirdfilter, 
         English.name %in% migrant | English.name %in% localmigrant)


waterset1 = taxset2res %>% filter(eBird.species.group %in% c("Waterfowl")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
waterset1 = as.character(waterset1$India.Checklist.Name)

waterset1a = taxset2mig %>% filter(eBird.species.group %in% c("Waterfowl")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
waterset1a = as.character(waterset1a$India.Checklist.Name)

waterset1 = c(waterset1,waterset1a)

waterset2 = taxset2res %>% filter(family %in% c("Laridae (Gulls, Terns, and Skimmers)")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
waterset2 = as.character(waterset2$India.Checklist.Name)

waterset2a = taxset2mig %>% filter(family %in% c("Laridae (Gulls, Terns, and Skimmers)")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
waterset2a = as.character(waterset2a$India.Checklist.Name)

waterset2 = c(waterset2,waterset2a)

waterset3 = taxset2mig %>% filter(eBird.species.group %in% c("Shorebirds")) %>% 
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
waterset3 = as.character(waterset3$India.Checklist.Name)

waterset4 = taxset2res %>%
  left_join(map,by = c("English.name" = "eBird.English.Name.2018")) %>% 
  filter(!India.Checklist.Name %in% c(waterset1,waterset2) | 
           India.Checklist.Name %in% c("Lesser Whistling Duck","Cotton Teal")) %>% 
  select(India.Checklist.Name)
waterset4 = as.character(waterset4$India.Checklist.Name)

master$Waterbirds.Composite = ""
master$Waterbirds.Composite[master$Common.Name %in% waterset4] = "Others-Resident"
master$Waterbirds.Composite[master$Common.Name %in% waterset1] = "Ducks & Geese"
master$Waterbirds.Composite[master$Common.Name %in% waterset2] = "Gulls & Terns"
master$Waterbirds.Composite[master$Common.Name %in% waterset3] = "Shorebirds-Migratory"

waterset1 = intersect(waterset1,ltset)
waterset2 = intersect(waterset2,ltset)
waterset3 = intersect(waterset3,ltset)
waterset4 = intersect(waterset4,ltset)

waterbirds = list(waterset1,waterset2,waterset3,waterset4)
names(waterbirds) = c("Waterfowl","Gulls and Terns","Shorebirds-Migratory","Others-Resident")




############# Habitat groups
# tropical
# temperate
# grassland
# wetland
# scrub
# generalists

habitatset1 = habitat %>% filter(forspecialist == 1) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
habitatset1 = as.character(habitatset1$India.Checklist.Name)

habitatset2 = habitat %>% filter(grubspecialist == 1) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
habitatset2 = as.character(habitatset2$India.Checklist.Name)

habitatset3 = habitat %>% filter(wetspecialist == 1) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
habitatset3 = as.character(habitatset3$India.Checklist.Name)

habitatset4 = data.frame(eBird.English.Name = specieslist$COMMON.NAME) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
habitatset4 = as.character(habitatset4$India.Checklist.Name)
habitatset4 = setdiff(habitatset4,unique(c(habitatset1,habitatset2,habitatset3)))

master$Habitat.Composite = ""
master$Habitat.Composite[master$Common.Name %in% habitatset1] = "Forests"
master$Habitat.Composite[master$Common.Name %in% habitatset2] = "Grassland/Scrub"
master$Habitat.Composite[master$Common.Name %in% habitatset3] = "Wetlands"
master$Habitat.Composite[master$Common.Name %in% habitatset4] = "Generalists"

habitatset1 = intersect(habitatset1,ltset)
habitatset2 = intersect(habitatset2,ltset)
habitatset3 = intersect(habitatset3,ltset)
habitatset4 = intersect(habitatset4,ltset)

habitats = list(habitatset1,habitatset2,habitatset3,habitatset4)
names(habitats) = c("Forests","Grassland/Scrub","Wetlands","Generalists")




########## Migrants

migrantset1 = data.frame(eBird.English.Name = resident) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
migrantset1 = as.character(migrantset1$India.Checklist.Name)

migrantset2 = data.frame(eBird.English.Name = localmigrant) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
migrantset2 = as.character(migrantset2$India.Checklist.Name)

migrantset3 = data.frame(eBird.English.Name = migrant) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
migrantset3 = as.character(migrantset3$India.Checklist.Name)

migrantset1 = intersect(migrantset1,ltset)
migrantset2 = intersect(migrantset2,ltset)
migrantset3 = intersect(migrantset3,ltset)

migrants = list(migrantset1,migrantset2,migrantset3)
names(migrants) = c("Resident","Migratory-Local","Migratory-Long-Distance")



########## Endemism

endemicset1 = data.frame(eBird.English.Name = wg) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
endemicset1 = as.character(endemicset1$India.Checklist.Name)
  
  
endemicset2 = data.frame(eBird.English.Name = sub) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
endemicset2 = as.character(endemicset2$India.Checklist.Name)


endemicset3 = setdiff(migrantset1,c(endemicset1,endemicset2))

master$Endemics.Composite = ""
master$Endemics.Composite[master$Common.Name %in% endemicset1] = "Western-Ghats"
master$Endemics.Composite[master$Common.Name %in% endemicset2] = "Others-Subcontinent"
master$Endemics.Composite[master$Common.Name %in% endemicset3] = "Resident-Non-Endemics"


endemicset1 = intersect(endemicset1,ltset)
endemicset2 = intersect(endemicset2,ltset)
endemicset3 = intersect(endemicset3,ltset)


endemics = list(endemicset3,endemicset1,endemicset2)
names(endemics) = c("Resident-Non-Endemics","Western-Ghats","Others-Subcontinent")





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

dietset1 = data.frame(eBird.English.Name = frne) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
dietset1 = as.character(dietset1$India.Checklist.Name)
  
dietset2 = data.frame(eBird.English.Name = invt) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
dietset2 = as.character(dietset2$India.Checklist.Name)
  
dietset3 = data.frame(eBird.English.Name = omni) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
dietset3 = as.character(dietset3$India.Checklist.Name)
  
dietset4 = data.frame(eBird.English.Name = seed) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
dietset4 = as.character(dietset4$India.Checklist.Name)
  
dietset5 = data.frame(eBird.English.Name = carn) %>% 
  left_join(map,by = c("eBird.English.Name" = "eBird.English.Name.2018")) %>% 
  select(India.Checklist.Name)
dietset5 = as.character(dietset5$India.Checklist.Name)

master$Diet.Composite = ""
master$Diet.Composite[master$Common.Name %in% dietset1] = "Fruit-Nectar"
master$Diet.Composite[master$Common.Name %in% dietset2] = "Invertebrate"
master$Diet.Composite[master$Common.Name %in% dietset3] = "Omnivorous"
master$Diet.Composite[master$Common.Name %in% dietset4] = "Plant-Seed"
master$Diet.Composite[master$Common.Name %in% dietset5] = "Carnivorous"


dietset1 = intersect(dietset1,ltset)
dietset2 = intersect(dietset2,ltset)
dietset3 = intersect(dietset3,ltset)
dietset4 = intersect(dietset4,ltset)
dietset5 = intersect(dietset5,ltset)


diets = list(dietset1,dietset2,dietset3,dietset4,dietset5)
names(diets) = c("Fruit-Nectar","Invertebrate","Omnivorous","Plant-Seed","Carnivorous")


rm(list=setdiff(ls(envir = .GlobalEnv), c("raptors","waterbirds","habitats","migrants","endemics",
                                          "diets","master")), pos = ".GlobalEnv")

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
uncertain = as.character(uncertain$Common.Name)
status = status %>%
  select(Common.Name,Long.Term.Status,Current.Status)
names(status)[1] = "species"

len = length(master$Common.Name)
master = cbind(sl = 1:len, master)

compositespecieslist = compositespecieslist %>% filter(!species %in% uncertain)

write.csv(compositespecieslist,"compositespecieslist.csv",row.names = F)
write.csv(master,"master-stateofindiasbirds.csv",row.names = F)

delimcomp = compositespecieslist %>%
  group_by(composite,groups) %>%
  summarise(species = toString(species)) %>%
  ungroup()

ctemp = compositespecieslist
ctemp = ctemp %>% group_by(composite,groups) %>% mutate(spno = 1:n())
ctemp$spno = as.factor(ctemp$spno)
ctemp = spread(ctemp,spno,species)
ctemp = data.frame(lapply(ctemp, as.character), stringsAsFactors=FALSE)

mp = read.csv("stateofindiasbirds.csv")
mp1 = as.character(mp$Common.Name[mp$Concern.Status == "High"])
mp2 = as.character(mp$Common.Name[mp$Concern.Status == "Moderate"])
mp3 = as.character(mp$Common.Name[mp$Concern.Status == "Low"])

# - Red : #A01000
# - Amber : #E5AE37
# - Green : #089148


library(openxlsx)
wb = createWorkbook() # create a workbook
addWorksheet(wb, "Sheet", gridLines = F)
writeData(wb, "Sheet", ctemp)


Style1 = createStyle(fontColour = "#A01000", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)
Style2 = createStyle(fontColour = "#E5AE37", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)
Style3 = createStyle(fontColour = "#089148", bgFill = NULL, fontName = "Gandhi Sans", fontSize = 9)

for (i in 1:length(mp1))
{
  conditionalFormatting(wb, "Sheet", cols = 3:208,
                        rows = 2:24, rule = mp1[i], style = Style1,
                        type = "contains")
}

for (i in 1:length(mp2))
{
  conditionalFormatting(wb, "Sheet", cols = 3:208,
                        rows = 2:24, rule = mp2[i], style = Style2,
                        type = "contains")
}

for (i in 1:length(mp3))
{
  conditionalFormatting(wb, "Sheet", cols = 3:208,
                        rows = 2:24, rule = mp3[i], style = Style3,
                        type = "contains")
}


saveWorkbook(wb, "delimited-compositespecieslist.xlsx", overwrite = TRUE)

write.csv(delimcomp,"delimited-compositespecieslist.csv", row.names = F)

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