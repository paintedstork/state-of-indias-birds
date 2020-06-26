library(tidyverse)

map = read.csv("Map to Other Lists - map.csv")
map = map %>%
  filter(!eBird.English.Name.2018 %in% c("Sykes's Short-toed Lark","Green Warbler","Sykes's Warbler",
                                         "Taiga Flycatcher","Chestnut Munia","Desert Whitethroat",
                                         "Hume's Whitethroat","Changeable Hawk-Eagle")) %>%
  dplyr::select(eBird.English.Name.2018,India.Checklist.Name,eBird.English.Name.2019,eBird.Scientific.Name.2019)

lists = read.csv("stateofindiasbirds.csv")
lists = left_join(lists,map,by = c("Common.Name" = "India.Checklist.Name"))
lists = lists %>% mutate(India.Checklist.Name = Common.Name) %>% dplyr::select(-Common.Name) %>% 
  filter(!is.na(eBird.English.Name.2019)) %>%
  dplyr::select(eBird.Scientific.Name.2019,Long.Term.Status,Current.Status,Range.Status,
                Concern.Status,WLPA.Schedule,eBird.English.Name.2018,
                Long.Term.Trend,Current.Annual.Change,Range.Size)

bliclem = read.csv("birdlife_clements_2019.csv")
bli = read.csv("birdlife_2019.csv")

bliclem = bliclem %>%
  filter(Rank == "species") %>% dplyr::select(-Rank)

bliclem = left_join(bliclem,bli)

lists = left_join(lists,bliclem)

endemism = read.csv("Endemicity - Endemicity.csv")

allend = as.character(endemism$eBird.English.Name[!is.na(endemism$Western.Himalayas) |
                                                  !is.na(endemism$Eastern.Himalayas) |
                                                  !is.na(endemism$Himalayas) |
                                                  !is.na(endemism$Assam.Plains) |
                                                  !is.na(endemism$Indus.Plains) |
                                                  !is.na(endemism$Northern.Myanmar.Lowlands) |
                                                  !is.na(endemism$Subcontinent) |
                                                  !is.na(endemism$Mainland.India) |
                                                  !is.na(endemism$Western.Ghats) |
                                                  !is.na(endemism$Southern.Andhra.Pradesh) |
                                                  !is.na(endemism$Southern.Deccan.Plateau) |
                                                  !is.na(endemism$Central.India) |
                                                  !is.na(endemism$Andaman.and.Nicobar.islands)])

lists$Endemic.Near.Endemic = ""
lists$Endemic.Near.Endemic[lists$eBird.English.Name.2018 %in% allend] = "X"

lists = lists %>%
  dplyr::select(Common.Name,Scientific.Name,Endemic.Near.Endemic,Long.Term.Trend,Current.Annual.Change,Range.Size,
                Long.Term.Status,Current.Status,Range.Status,
                Concern.Status,WLPA.Schedule,IUCN) %>%
  filter(!is.na(Common.Name)) %>%
  group_by(Scientific.Name) %>% slice(1)

write.csv(lists,"lists_inform_IUCN_red_list.csv",row.names=F)
  





