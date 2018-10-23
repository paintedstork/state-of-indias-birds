a = read.csv("CAF.csv")

b = c("Eurasian Curlew","Crab-Plover","Black-tailed Godwit","Bar-tailed Godwit","Red Knot","Long-toed Stint",
      "Indian Skimmer","Greater Flamingo","Lesser Flamingo","Curlew Sandpiper","Little Stint","Lesser Sand-Plover",
      "Black-bellied Tern","Great Knot","Common Pochard","Ferruginous Duck","European Roller","Sociable Lapwing")

caf = merge(data,a)

cafh = caf %>%
  filter(LOCALITY.TYPE == "H")

prih = cafh %>%
  filter(COMMON.NAME %in% b)


data2 = prih %>%
  group_by(LOCALITY.ID) %>% summarize(rich = n_distinct(COMMON.NAME)) %>%
  filter(rich > 5)

tomerge = prih %>%
  select(LOCALITY.ID,LATITUDE,LONGITUDE) %>%
  group_by(LOCALITY.ID) %>% slice(1) %>% ungroup()

data2 = merge(data2,tomerge)

############# priority species specific #########################

hotspots = data %>%
  filter(LOCALITY.TYPE == "H", year >= 2013) 

freqs = hotspots %>%
  group_by(LOCALITY.ID) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(OBSERVATION.COUNT != "X", lists > 5) %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(LOCALITY.ID, COMMON.NAME) %>% summarize(freq = n()/max(lists), count = quantile(OBSERVATION.COUNT,0.9)) %>%
  filter(COMMON.NAME %in% b)

tomerge = data %>%
  select(LOCALITY.ID,LATITUDE,LONGITUDE) %>%
  group_by(LOCALITY.ID) %>% slice(1) %>% ungroup()

freqs1 = merge(freqs,tomerge)

freqs2 = freqs1 %>%
  filter(COMMON.NAME == "Sociable Lapwing")

plotindiamap +
  geom_point(data = freqs2, aes(x = LONGITUDE, y = LATITUDE, col = freq, size = freq)) +
  #geom_polygon(data = mask, aes(x = long, y = lat, group = group), col = 'grey92', fill = 'grey92')+
  geom_path(data = fortify(statemap), aes(x = long, y = lat, group = group), col = 'black', size = 1) +
  scale_color_gradientn(colours = heat.colors(5), trans = 'reverse') +
  theme(legend.justification=c(1,1), legend.position=c(0.99,0.99)) +
  theme(legend.title = element_blank(), legend.text = element_text(size = 16)) +
  guides(col = guide_legend(reverse = TRUE)) +
  guides(color = guide_legend(override.aes = list(size=5))) +
  scale_size(guide = "none") +
  ggtitle("Sociable Lapwing - frequency") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.1, size = 20))


########################################### generate tables ########################


freqs = hotspots %>%
  group_by(LOCALITY.ID) %>% mutate(lists = n_distinct(group.id)) %>%
  filter(OBSERVATION.COUNT != "X", lists > 5) %>%
  mutate(OBSERVATION.COUNT = as.numeric(OBSERVATION.COUNT)) %>%
  group_by(LOCALITY.ID) %>% summarize(rich = n_distinct(COMMON.NAME), count = round(sum(OBSERVATION.COUNT)/max(lists)))

tomerge = data %>%
  select(LOCALITY.ID,ST_NM) %>%
  group_by(LOCALITY.ID) %>% slice(1) %>% ungroup()

freqs1 = merge(freqs,tomerge)


KA_rich = freqs1 %>%
  filter(ST_NM == "Karnataka") %>%
  arrange(desc(rich)) %>%
  slice(1:5)

KA_count = freqs1 %>%
  filter(ST_NM == "Karnataka") %>%
  arrange(desc(count)) %>%
  slice(1:5)

TN_rich = freqs1 %>%
  filter(ST_NM == "Tamil Nadu") %>%
  arrange(desc(rich)) %>%
  slice(1:5)

TN_count = freqs1 %>%
  filter(ST_NM == "Tamil Nadu") %>%
  arrange(desc(count)) %>%
  slice(1:5)

KL_rich = freqs1 %>%
  filter(ST_NM == "Kerala") %>%
  arrange(desc(rich)) %>%
  slice(1:5)

KL_count = freqs1 %>%
  filter(ST_NM == "Kerala") %>%
  arrange(desc(count)) %>%
  slice(1:5)

AP_rich = freqs1 %>%
  filter(ST_NM == "Andhra Pradesh") %>%
  arrange(desc(rich)) %>%
  slice(1:5)

AP_count = freqs1 %>%
  filter(ST_NM == "Andhra Pradesh") %>%
  arrange(desc(count)) %>%
  slice(1:5)

TS_rich = freqs1 %>%
  filter(ST_NM == "Telangana") %>%
  arrange(desc(rich)) %>%
  slice(1:5)

TS_count = freqs1 %>%
  filter(ST_NM == "Telangana") %>%
  arrange(desc(count)) %>%
  slice(1:5)

GA_rich = freqs1 %>%
  filter(ST_NM == "Goa") %>%
  arrange(desc(rich)) %>%
  slice(1:5)

GA_count = freqs1 %>%
  filter(ST_NM == "Goa") %>%
  arrange(desc(count)) %>%
  slice(1:5)

write.csv(KA_count,"KA_count.csv")
