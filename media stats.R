preimp = c("OBSERVATION.DATE","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")
rawpath = "ebd_IN_relDec-2018.txt"
nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))

data = data %>%
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         year = year(OBSERVATION.DATE))

d2018 = data %>%
  filter(year == 2018)

media = read.csv("IN_media_2018.csv")

tn = media %>%
  filter(SUBNATIONAL1_CODE_S %in% c("IN-TN","IN-PY"), 
         SUB_ID %in% unique(d2018$SAMPLING.EVENT.IDENTIFIER),
         MEDIA_TYPE == "P")

tn1 = media %>%
  filter(SUBNATIONAL1_CODE_S %in% c("IN-TN","IN-PY"), 
         SUB_ID %in% unique(d2018$SAMPLING.EVENT.IDENTIFIER),
         MEDIA_TYPE == "P",
         CATEGORY == "species")

images = length(tn$SUB_ID)
species = length(unique(tn1$PRIMARY_COM_NAME))

topimages = tn %>%
  group_by(GET_USER_DISPLAY_NAME.OAM.USER_ID.) %>%
  summarize(images = n()) %>% ungroup %>%
  arrange(desc(images))

topspecies = tn1 %>%
  group_by(GET_USER_DISPLAY_NAME.OAM.USER_ID.) %>%
  summarize(species = n_distinct(PRIMARY_COM_NAME)) %>% ungroup %>%
  arrange(desc(species))

byspecies = tn1 %>%
  group_by(PRIMARY_COM_NAME) %>%
  summarize(images = n()) %>% ungroup %>%
  arrange(desc(images))

toplists = tn %>%
  group_by(GET_USER_DISPLAY_NAME.OAM.USER_ID.) %>%
  summarize(lists = n_distinct(SUB_ID)) %>% ungroup %>%
  arrange(desc(lists))

write.csv(toplists,"toplists.csv")
