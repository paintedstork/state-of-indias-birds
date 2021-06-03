
##########################################################
########################## atlas model comparison and plotting


rm(list = ls(all.names = TRUE))
require(tidyverse)
load("keralaatlasvsebird.RData")
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)

## names of selected 353 species n >= 50

selected_species = dataebird %>% filter(CATEGORY %in% c("species","ISSF")) %>% group_by(COMMON.NAME) %>% 
  summarize(n = n()) %>% arrange(desc(n))
a = unique(dataatlas$COMMON.NAME)
b = selected_species$COMMON.NAME
c = setdiff(b,a)
selected_species = dataebird %>% filter(CATEGORY %in% c("species","ISSF"), !COMMON.NAME %in% c) %>% 
  group_by(COMMON.NAME) %>% summarize(n = n()) %>% arrange(desc(n)) %>%  filter(n>=1)
a = unique(comp$species)
b = selected_species$COMMON.NAME
c = setdiff(b,a)


##########################################################
########################## atlas model comparison (final)


fit = comp %>%
  group_by(type) %>% summarize(int = coefficients(summary(lm(freq~atlas)))[1,1],
                               slp = coefficients(summary(lm(freq~atlas)))[2,1])

comp = left_join(comp,fit)
comp$fit = comp$int + comp$slp*comp$atlas
comp$sqdev = ((comp$freq-comp$fit))^2

fit = comp %>%
  group_by(type) %>% summarize(rsq = summary(lm(freq~atlas))$r.squared,
                               ssd = mean(na.omit(sqdev)))
fit$ssd = sqrt(fit$ssd)
fit$type = factor(fit$type, levels = c("trivial","grid","onlyfixeddis","onlyfixeddur",
                                       "onlyfixedlla","onlyrandom","logitdis",
                                       "logitdur","logitlla","dis","dur","lla","lla1"))

fit1 = fit[c(1:2,4),]
fit1$gp = "comparison of covariates\n(binomial mixed models with cloglog-link)"

fit2 = fit[c(3:4,7,10:12),]
fit2$type[fit2$type == "lla"] = "lla1"
fit2$gp = "comparison of various analyses\n(List Length as covariate)"

fit3 = rbind(fit1,fit2)

# compare 

ggp = ggplot(fit3, aes(x=type, y=ssd)) + 
  facet_wrap(. ~ gp, scale="free", ncol = 1) +
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_point(size = 4, col = "dark green") +
  geom_hline(yintercept = min(fit$ssd), linetype = "dotted", size = 0.5) +
  xlab("Model type") +
  ylab("Root mean square error (RMSE) from best fit line\n(eBird vs. Atlas frequencies)")

ggp1 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  #theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  scale_x_discrete(breaks = c("trivial","grid","onlyfixedlla","onlyrandom","logitlla",
                              "dis","dur","lla","lla1"),
                   labels = c("average (1)\n(simple)","average (2)\n(spatial)",
                              "fixed effect only (3)\n(cloglog-link)",
                              "random effect only (4)\n(cloglog-link)","mixed effects (5)\n(logit-link)",
                              "Distance (8)\n(km)",
                              "Duration (7)\n(min)",
                              "List Length (6)\n(no. of species)","mixed effects (6)\n(cloglog-link)")) +
  scale_y_continuous(limits = c(0,NA)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  #theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 12, colour = "black", face = 'italic')) +
  #guides(size=guide_legend(title="R sq. (eBird vs. atlas)")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )


tiff('Fig. 4.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp1)
dev.off()




###########################################################
##### correlations (atlas)

rm(list = ls(all.names = TRUE))
require(tidyverse)
load("keralaatlasvsebird.RData")
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)

a1 = comp %>% filter(type %in% c("lla"))
a2 = comp %>% filter(type %in% c("trivial"))
aw = data.frame(freq1 = a1$atlas, freq2 = a2$freq)
bw = data.frame(int = coefficients(summary(lm(aw$freq2~aw$freq1)))[1,1],
                slp = coefficients(summary(lm(aw$freq2~aw$freq1)))[2,1])
aw$type = "simple average"
bw$type = "simple average"

a1 = comp %>% filter(type %in% c("lla"))
a2 = comp %>% filter(type %in% c("grid"))
ax = data.frame(freq1 = a1$atlas, freq2 = a2$freq)
bx = data.frame(int = coefficients(summary(lm(ax$freq2~ax$freq1)))[1,1],
                slp = coefficients(summary(lm(ax$freq2~ax$freq1)))[2,1])
ax$type = "spatial average"
bx$type = "spatial average"

a1 = comp %>% filter(type %in% c("lla"))
a2 = comp %>% filter(type %in% c("lla"))
ay = data.frame(freq1 = a1$atlas, freq2 = a2$freq)
by = data.frame(int = coefficients(summary(lm(ay$freq2~ay$freq1)))[1,1],
                slp = coefficients(summary(lm(ay$freq2~ay$freq1)))[2,1])
ay$type = "list length model"
by$type = "list length model"

a1 = comp %>% filter(type %in% c("lla"))
a2 = comp %>% filter(type %in% c("dur"))
az = data.frame(freq1 = a1$freq, freq2 = a2$freq)
bz = data.frame(int = coefficients(summary(lm(az$freq2~az$freq1)))[1,1],
                slp = coefficients(summary(lm(az$freq2~az$freq1)))[2,1])
az$type = "duration"
bz$type = "duration"

a = rbind(aw,ax,ay)
b = rbind(bw,bx,by)

a$type = factor(a$type, levels = c("simple average","spatial average","list length model"))
b$type = factor(b$type, levels = c("simple average","spatial average","list length model"))

new.lab = as_labeller(c('simple average'="simple~average~(1)", 'spatial average'="spatial~average~(2)", 
                        'list length model'="bold(List~Length~model~(6))"), 
                      label_parsed)

ggp = ggplot(a, aes(x=freq1, y=freq2)) + 
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_abline(data = b, aes(intercept = int, slope = slp), col = "red", size = 1) + 
  facet_wrap(type ~ ., ncol = 1, scale = 'free_y', strip.position = "right", labeller = new.lab) +
  geom_point(size = 2) +
  ylab("eBird frequency of reporting") +
  xlab("Atlas frequency of reporting")

ggp1 = ggp +
  #ggtitle("frequencies of reporting - eBird vs. atlas") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(axis.title.x = element_text(size = 14, face = 'bold'), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 14, face = 'bold', angle = 90), axis.text.y = element_text(size = 10)) +
  #theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.y = element_text(size = 14, colour = "black", face = 'bold')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )


tiff('Fig. 5.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp1)
dev.off()



ggp = ggplot(az, aes(x=freq1, y=freq2)) + 
  #geom_abline(intercept = 0, slope = 1, col = "blue") + 
  geom_abline(data = bz, aes(intercept = int, slope = slp), col = "red", size = 1) + 
  geom_point(size = 2) +
  ylab("list length") +
  xlab("duration")

ggp1 = ggp +
  ggtitle("comparison of modelled frequency of reporting using different covariates ") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold')) +
  theme(axis.title.x = element_text(size = 14, face = 'bold'), axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 14, face = 'bold', angle = 90), 
        axis.text.y = element_text(size = 10)) +
  #theme(legend.title = element_text(size = 12), legend.text = element_text(size = 12)) +
  theme(text=element_text(family="Gill Sans MT", face = 'bold')) +
  theme(strip.text.y = element_text(size = 14, colour = "black", face = 'bold')) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )


tiff('Fig. 2a.tif', units="in", width=10, height=7, res=300)
grid::grid.draw(ggp1)
dev.off()



########################## distribution function (ignore)


rm(list = ls(all.names = TRUE))
require(tidyverse)
load("dataforanalyses.RData")
data = data %>% filter(month %in% c(1,2,3))
library(ggthemes)

theme_set(theme_tufte())
require(extrafont)

# Common Myna + Orange Minivet + Heart Spotted Woodpecker + Forest Wagtail

# Common Myna

data1 = data
temp = data1 %>%
  filter(COMMON.NAME == "Common Myna") %>%
  distinct(gridg3)
data1 = temp %>% left_join(data1)

dataf1 = data1 %>% group_by(gridg1) %>% mutate(lists = n_distinct(group.id)) %>% filter(lists > 20) %>%
  filter(COMMON.NAME == "Common Myna") %>% group_by(gridg1) %>% summarize(freq = n()/max(lists))
