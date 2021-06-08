### compare precision of estimates ###


#####
# model comparison

################################################################
############## diagnostics
# model comparison with complete data


library(tidyverse)
rm(list = ls(all.names = TRUE))
load("modelcomparison_fulldata.RData")
require(ggthemes)
theme_set(theme_tufte())
require(extrafont)

require(gridExtra)
require(grid)

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), list(nrow = 2))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

comp$prop = abs(comp$se/comp$est)
rat = comp %>%
  group_by(type) %>% summarize(mn = median(prop),cir = quantile(prop,0.975),cil = quantile(prop,0.025))

ggp = ggplot(rat, aes(type,mn))+
  geom_errorbar(aes(ymin = cil, ymax = cir), size = 0.5,
                #col = "dark green",
                width = 0.1) +
  xlab("Covariate")+
  ylab("Relative standard error")+
  theme_bw()

ggp1 = ggp+
  geom_point(size = 4) +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  scale_x_discrete(breaks = c("dur","dis","lla"),
                   labels = c("Duration\n(min)","Distance\n(km)","List Length\n(no. of species)")) +
  coord_cartesian(ylim=c(0,0.5))+
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

ggp2 = ggp+
  geom_point(size = 1) +
  geom_rect(aes(xmin = 0, xmax = 3.6, ymin = 0,ymax = 0.5), fill = alpha("lightblue", 0.2))+
  theme(axis.title.x = element_blank(), axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(), axis.text.y = element_text(size = 14)) +
  scale_x_discrete(breaks = c("dur","dis","lla"),
                   labels = c("Duration\n(min)","Distance\n(km)","List Length\n(no. of species)")) +
  theme(text=element_text(family="Gill Sans MT")) +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )

vp = viewport(width = 0.4, height = 0.4, x = 0.98,
              y = unit(20, "lines"), just = c("right","bottom"))

full = function() {
  print(ggp1)
  print(ggp2, vp = vp)
}

full()

tiff('Fig. 6.tif', units="in", width=10, height=7, res=300)
full()
dev.off()

jpeg('Fig. 6.jpg', units="in", width=10, height=7, res=1000)
full()
dev.off()

