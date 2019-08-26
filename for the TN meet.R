library(auk)

allin = system.file("extdata/ebd_rossta2_relAug-2018.txt", package = "auk")

allout = tempfile()
auk_clean(allin,allout, sep = "\t", remove_text = FALSE)

data = allout %>%
  read_ebd()


require(lubridate)
require(tidyverse)


days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

imp = c("common_name","observation_count",
        "locality_id","locality_type","state","county",
        "latitude","longitude","observation_date","time_observations_started","observer_id",
        "protocol_type","duration_minutes","effort_distance_km",
        "number_observers","all_species_reported","group_id")


data = data %>%
  filter(approved == 1, state == "Tamil Nadu") %>%
  mutate(group_id = ifelse(is.na(group_identifier), sampling_event_identifier, group_identifier)) %>%
  group_by(group_id,common_name) %>% slice(1) %>% ungroup %>%
  dplyr::select(imp) %>%
  mutate(observation_date = as.Date(observation_date), 
         month = month(observation_date), year = year(observation_date),
         day = day(observation_date) + cdays[month], week = week(observation_date),
         fort = ceiling(day/14)) %>%
  ungroup


