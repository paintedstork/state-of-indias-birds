library(tidyverse)

clemcheck = read.csv("eBird-Clements-v2019-integrated-checklist-August-2019.csv")
clemcheck = clemcheck %>% filter(category == "species")

end = read.csv("Endemicity - Endemicity.csv")
habitat = read.csv("Habitat - Habitat.csv")

cats = names(habitat)[3:41]

tropcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp",
             "Mangrove.Swamps")
temp1 = habitat %>% select(tropcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(tropcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,tropcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,tropcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$tropspecialist = temp1$sum - temp2$sum
habitat$tropspecialist[habitat$tropspecialist <= 0] = NA
habitat$tropspecialist[habitat$tropspecialist > 0] = "Tropical Forest"

tempcats = c("Subtropical.Montane.Forest","Mixed.Forest","Oak.Forest","Coniferous.Forest","Bamboo.Forest",
             "Forest.Swamp","High.Shrubbery","High.Alpine")
temp1 = habitat %>% select(tempcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(tempcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,tempcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,tempcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$tempspecialist = temp1$sum - temp2$sum
habitat$tempspecialist[habitat$tempspecialist <= 0] = NA
habitat$tempspecialist[habitat$tempspecialist > 0] = "Temperate Forest"

forcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp",
            "Mangrove.Swamps","Subtropical.Montane.Forest","Mixed.Forest","Oak.Forest",
            "Coniferous.Forest","High.Shrubbery","High.Alpine","Dry.Deciduous")
temp1 = habitat %>% select(forcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(forcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,forcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,forcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$forspecialist = temp1$sum - temp2$sum
habitat$forspecialist[habitat$forspecialist <= 0] = NA
habitat$forspecialist[habitat$forspecialist > 0] = "Forest"

woodcats = c("Tropical.Rainforest","Bamboo.Forest","Shola.Forest","Moist.Deciduous","Forest.Swamp",
             "Subtropical.Montane.Forest","Mixed.Forest","Oak.Forest","Coniferous.Forest",
             "High.Shrubbery","High.Alpine","Groves.Gardens.and.Plantations",
             "Dry.Deciduous","Thorny.Scrub","Rocky.Scrub","Mangrove.Swamps")
temp1 = habitat %>% select(woodcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(woodcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,woodcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,woodcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$woodspecialist = temp1$sum - temp2$sum
habitat$woodspecialist[habitat$woodspecialist <= 0] = NA
habitat$woodspecialist[habitat$woodspecialist > 0] = "Woodland"

grasscats = c("Wet.Paddy.fields","Open.Areas","Dry.Grassland","Hill.Grassland","Wet.Grassland",
              "High.Altitude.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","High.Alpine",
              "Hot.Desert","Semi.Desert","Salt.Pans")
temp1 = habitat %>% select(grasscats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(grasscats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,grasscats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,grasscats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$grassspecialist = temp1$sum - temp2$sum
habitat$grassspecialist[habitat$grassspecialist <= 0] = NA
habitat$grassspecialist[habitat$grassspecialist > 0] = "Grassland"

wetcats = c("Wet.Paddy.fields","Wet.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","Inland.Water",
            "Inland.Mudflats","Coastal.Mudflats","Beaches","Pelagic.Waters","Mangrove.Swamps",
            "Salt.Pans","Lagoons","Rivers","High.Altitude.Lakes","Brackish.Backwaters","Habitation")
temp1 = habitat %>% select(wetcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(wetcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,wetcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,wetcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$wetspecialist = temp1$sum - temp2$sum
habitat$wetspecialist[habitat$wetspecialist <= 0] = NA
habitat$wetspecialist[habitat$wetspecialist > 0] = "Wetland"

scrubcats = c("Open.Areas","Dry.Deciduous","Hill.Shrubland","Thorny.Scrub","Rocky.Scrub","Bamboo.Forest",
              "High.Shrubbery","Semi.Desert")
temp1 = habitat %>% select(scrubcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(scrubcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,scrubcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,scrubcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$scrubspecialist = temp1$sum - temp2$sum
habitat$scrubspecialist[habitat$scrubspecialist <= 0] = NA
habitat$scrubspecialist[habitat$scrubspecialist > 0] = "Scrub"

grubcats = c("Wet.Paddy.fields","Open.Areas","Dry.Grassland","Hill.Grassland","Wet.Grassland",
             "High.Altitude.Grassland","Reeds","Forest.Swamp","Short.Grass.Marshes","High.Alpine",
             "Hot.Desert","Semi.Desert","Salt.Pans","Dry.Deciduous",
             "Hill.Shrubland","Thorny.Scrub","Rocky.Scrub","Bamboo.Forest","High.Shrubbery")
temp1 = habitat %>% select(grubcats) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(grubcats)])) %>% mutate(sum = replace(sum,sum>1,1))
temp2 = habitat %>% select(setdiff(cats,grubcats)) %>% replace(is.na(.), 0) %>% 
  mutate(sum = rowSums(.[1:length(setdiff(cats,grubcats))])) %>% mutate(sum = replace(sum,sum>1,1))
habitat$grubspecialist = temp1$sum - temp2$sum
habitat$grubspecialist[habitat$grubspecialist <= 0] = NA
habitat$grubspecialist[habitat$grubspecialist > 0] = "Grassland and Scrub"

habitat$hab = "Generalist"

for (i in 1:length(habitat$eBird.English.Name))
{
  if (!is.na(habitat$tropspecialist[i]) | !is.na(habitat$tempspecialist[i]) | !is.na(habitat$forspecialist[i]) |
      !is.na(habitat$woodspecialist[i]) | !is.na(habitat$grassspecialist[i]) | !is.na(habitat$wetspecialist[i]) |
      !is.na(habitat$scrubspecialist[i]))
  {
    temp = habitat %>% filter(eBird.English.Name == habitat$eBird.English.Name[i]) %>% 
      select(woodspecialist,tropspecialist,tempspecialist,wetspecialist,grassspecialist,
             scrubspecialist)
    temp = as.vector(temp[!is.na(temp)])
    a = paste(temp, sep = ",", collapse = ", ")
    habitat$hab[i] = a
  }
}

adcats = habitat %>% select(eBird.English.Name,hab)

end$Assam.Plains[end$eBird.English.Name == "Slender-billed Babbler"] = 1
end$Southern.Deccan.Plateau[end$eBird.English.Name == "Jerdon's Courser"] = 1
end$Eastern.Himalayas[end$eBird.English.Name == "Cachar Bulbul"] = 1
end$Himalayas[end$eBird.English.Name == "Cachar Bulbul"] = 1
wgsl = as.character(end$eBird.English.Name[!is.na(end$Western.Ghats)])
extra = c("Southern Hill Myna","Dark-fronted Babbler","Square-tailed Bulbul",
          "Hill Swallow","Orange Minivet","Sri Lanka Bay-Owl","Indian Swiftlet","Sri Lanka Frogmouth")
wgsl = c(wgsl,extra)
end$Western.Ghats.Sri.Lanka = NA
end$Western.Ghats.Sri.Lanka[end$eBird.English.Name %in% wgsl] = 1


end$India[!is.na(end$India)] = "India"
end$Subcontinent[!is.na(end$Subcontinent)] = "South Asia"
end$Mainland.India[!is.na(end$Mainland.India)] = "Mainland India"
end$Western.Ghats[!is.na(end$Western.Ghats)] = "Western Ghats"
end$Southern.Andhra.Pradesh[!is.na(end$Southern.Andhra.Pradesh)] = "Southern Andhra Pradesh"
end$Southern.Deccan.Plateau[!is.na(end$Southern.Deccan.Plateau)] = "Southern Deccan"
end$Central.India[!is.na(end$Central.India)] = "Central India"
end$Western.Himalayas[!is.na(end$Western.Himalayas)] = "Western Himalayas"
end$Eastern.Himalayas[!is.na(end$Eastern.Himalayas)] = "NE India"
end$Himalayas[!is.na(end$Himalayas)] = "Himalayas & NE India"
end$Assam.Plains[!is.na(end$Assam.Plains)] = "Assam Plains"
end$Indus.Plains[!is.na(end$Indus.Plains)] = "Indus Plains"
end$Northern.Myanmar.Lowlands[!is.na(end$Northern.Myanmar.Lowlands)] = "Northern Myanmar Lowlands"
end$Andaman.and.Nicobar.islands[!is.na(end$Andaman.and.Nicobar.islands)] = "A & N Islands"
end$Western.Ghats.Sri.Lanka[!is.na(end$Western.Ghats.Sri.Lanka)] = "Western Ghats & SL"

end$endemic = "Non-Endemic"

for (i in 1:length(end$eBird.English.Name))
{
  temp = end %>% filter(eBird.English.Name == end$eBird.English.Name[i]) %>% 
    select(Subcontinent,India,Mainland.India,Andaman.and.Nicobar.islands,Central.India,Southern.Deccan.Plateau,
           Western.Ghats.Sri.Lanka,Western.Ghats,Himalayas,Western.Himalayas,Eastern.Himalayas,Indus.Plains,
           Assam.Plains)
  temp = as.vector(temp[!is.na(temp)])
  if (length(temp) != 0)
  {
    a = paste(temp, sep = ",", collapse = ", ")
    end$endemic[i] = a
  }
}

endcats = end %>% select(eBird.English.Name,endemic)
adcats = left_join(adcats,endcats)

map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Desert Whitethroat","Hume's Whitethroat","Changeable Hawk-Eagle"))
map = map %>% select(eBird.English.Name.2019,eBird.English.Name.2018,India.Checklist.Name)

adcats = left_join(adcats,map,by = c("eBird.English.Name" = "eBird.English.Name.2018"))
adcats = adcats %>% select(-eBird.English.Name)


lists = read.csv("stateofindiasbirds.csv")
lists = lists %>% select(Common.Name,Migratory.Status)
lists$Migratory.Status[lists$Migratory.Status == "R"] = "Resident"
lists$Migratory.Status[lists$Migratory.Status == "LM"] = "Within-Region Migrant"
lists$Migratory.Status[lists$Migratory.Status == "W/P"] = "Long-Distance Migrant"

lists = left_join(lists,adcats,by = c("Common.Name" = "India.Checklist.Name"))
lists = lists %>% mutate(India.Checklist.Name = Common.Name)

clemcheck = clemcheck %>% select(English.name,eBird.species.code.2019)
lists = left_join(lists,clemcheck,by = c("eBird.English.Name.2019" = "English.name"))
lists = lists %>% select(India.Checklist.Name, eBird.species.code.2019, Migratory.Status, hab, endemic)
names(lists)[4:5] = c("Habitat Specialization","Endemicity")

write.csv(lists, "species_codes_categories_for_website.csv", row.names = F)
