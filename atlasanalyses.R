library(tidyverse)
library(ggthemes)

theme_set(theme_tufte())

temp = read.csv("KLcompare.csv")

temp$mresid = abs(temp$modelled - temp$atlas)
temp$sresid = abs(temp$simple - temp$atlas)

temp$mpropresid = temp$mresid/temp$atlas
temp$spropresid = temp$sresid/temp$atlas

msum = temp %>%
  summarize(s = sum(mresid)/n())

mpropsum = temp %>%
  summarize(s = sum(mpropresid)/n())

ssum = temp %>%
  summarize(s = sum(sresid)/n())

spropsum = temp %>%
  summarize(s = sum(spropresid)/n())

msum
mpropsum
ssum
spropsum

ggp = ggplot(temp, aes(x=atlas, y=simple)) + 
  geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point(size = 3) +
  xlab("freq. of detection - KL atlas") +
  ylab("freq. of detection - simple")

ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 18),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20)) +
  scale_x_continuous(limits = c(0,0.45)) +
  scale_y_continuous(limits = c(0,0.45))


ggp = ggplot(temp[c(1,3,4,6,7),], aes(x=species, y=modelled)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = modelled - se, ymax = modelled + se), width = 0.1, size = 1) +
  xlab("species") +
  ylab("abundance")

ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20))

occ = read.csv("occ.csv")

occ$type = as.character(occ$type)

occ[occ$type == "nb" & occ$nb == 8,]$type = "nb8"
occ[occ$type == "nosptimenb" & occ$nb == 8,]$type = "nosptimenb8"

occ$type = factor(occ$type, levels = c("trivial","null","nosp","nosptime","nb","nosptimenb","nb8","nosptimenb8"))

occ1 = occ %>% filter(!is.na(occ))
occ1 = occ1 %>%
  group_by(species,resolution) %>% mutate(occf = occ[1], occg = occ/occf) %>% ungroup %>%
  filter(type %in% c("trivial","null","nosptime","nb","nosptimenb"))

ggp = ggplot(occ1[occ1$resolution == "g2" & occ1$species == "Brahminy Kite",], aes(x=type, y=occ)) + 
  geom_point(size = 3, col = "dark green") +
  geom_line(aes(group = species), size = 1, col = "dark green") +
  geom_errorbar(aes(ymin = occ - occ.se, ymax = occ + occ.se), width = 0.1, size = 1, col = "dark green") +
  xlab("model") +
  ylab("occupancy")

ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20))

data1 = data %>%
  group_by(month) %>% summarize(n = n_distinct(group.id))

ggp = ggplot(data1, aes(x=month, y=n)) + 
  geom_point(size = 3, col = "dark green") +
  geom_line(size = 1, col = "dark green") +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  xlab("month") +
  ylab("unique checklists")

ggp +
  theme(axis.title.x = element_text(size = 22), axis.text.x = element_text(size = 16),
        axis.title.y = element_text(angle = 90, size = 22), axis.text.y = element_text(size = 20))
