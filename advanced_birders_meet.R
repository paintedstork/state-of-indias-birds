library(tidyverse)
library(ggthemes)
theme_set(theme_tufte())
require(extrafont)

data = read.delim("trepip-olbpip.txt", sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data[2:3,]
data = data[,-1]
data = t(data)
row.names(data) = NULL

rp = as.vector(data[1,])
data = data[-1,]

a = data.frame(species = rep(rp,each = length(data[,1])))
a$freq = c(as.numeric(data[,1]),as.numeric(data[,2]))
a$week = c(1:(length(a$freq)/2),1:(length(a$freq)/2))

set = seq(1,48,1)

spline_int1 = as.data.frame(spline(a[set,]$week, a[set,]$freq))
spline_int1$species = rp[1]

spline_int2 = as.data.frame(spline(a[(set+48),]$week, a[(set+48),]$freq))
spline_int2$species = rp[2]

spline_int = rbind(spline_int1,spline_int2)


ggp = ggplot(data = a[set,], aes(x = week, y = freq, col = species)) +
  #geom_point(size = 4) +
  xlab("month") +
  ylab("percentage of lists") +
  geom_line(data = spline_int, aes(x = x, y = y, col = species), size = 2) 
  #geom_vline(xintercept = 42.3, linetype = "dotted", size = 2, col = "#E49B36") +
  #geom_vline(xintercept = 46.5, linetype = "dotted", size = 2, col = "#31954E")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(2,46,4),
                   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_colour_manual(values = c("#31954E","#E49B36")) +
  theme(legend.position = "none")

png('trpi_obpi1.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



###########################################################



data = read.delim("grewar3-lblwar1-grnwar1.txt", sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data[2:4,]
data = data[,-1]
data = t(data)
row.names(data) = NULL

rp = as.vector(data[1,])
data = data[-1,]


a = data.frame(species = rep(rp,each = length(data[,1])))
a$freq = c(as.numeric(data[,1]),as.numeric(data[,2]),as.numeric(data[,3]))
a$week = c(1:(length(a$freq)/3),1:(length(a$freq)/3),1:(length(a$freq)/3))

set = seq(1,48,1)

spline_int1 = as.data.frame(spline(a[set,]$week, a[set,]$freq))
spline_int1$species = rp[1]

spline_int2 = as.data.frame(spline(a[(set+48),]$week, a[(set+48),]$freq))
spline_int2$species = rp[2]

spline_int3 = as.data.frame(spline(a[(set+96),]$week, a[(set+96),]$freq))
spline_int3$species = rp[3]

spline_int = rbind(spline_int1,spline_int2,spline_int3)


ggp = ggplot(data = a, aes(x = week, y = freq, col = species)) +
  #geom_point(size = 4) +
  xlab("month") +
  ylab("percentage of lists") +
  geom_line(data = spline_int, aes(x = x, y = y, col = species), size = 2) +
  geom_vline(xintercept = 40, linetype = "dotted", size = 2, col = "#E49B36") +
  geom_vline(xintercept = 42.1, linetype = "dotted", size = 2, col = "#31954E") +
  geom_vline(xintercept = 37.7, linetype = "dotted", size = 2, col = "#78CAE0")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(2,46,4),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_colour_manual(values = c("#31954E","#E49B36","#78CAE0")) +
  theme(legend.position = "none")

png('grewar3-lblwar1-grnwar14.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()


ggp = ggplot(data = a[97:144,], aes(x = week, y = freq), col = "#78CAE0") +
  #geom_point(size = 4) +
  xlab("month") +
  ylab("percentage of lists") +
  geom_line(col = "#78CAE0", size = 2) +
  geom_vline(xintercept = 37.9, linetype = "dotted", size = 2, col = "#78CAE0")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(2,46,4),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  theme(legend.position = "none")

png('grewar3-lblwar1-grnwar15.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



###########################################################



data = read.delim("grewar3-lblwar1-grnwar1.txt", sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data[2:4,]
data = data[,-1]
data = t(data)
row.names(data) = NULL

rp = as.vector(data[1,])
data = data[-1,]


a = data.frame(species = rep(rp,each = length(data[,1])))
a$freq = c(as.numeric(data[,1]),as.numeric(data[,2]),as.numeric(data[,3]))
a$week = c(1:(length(a$freq)/3),1:(length(a$freq)/3),1:(length(a$freq)/3))

set = seq(1,48,1)

spline_int1 = as.data.frame(spline(a[set,]$week, a[set,]$freq))
spline_int1$species = rp[1]

spline_int2 = as.data.frame(spline(a[(set+48),]$week, a[(set+48),]$freq))
spline_int2$species = rp[2]

spline_int3 = as.data.frame(spline(a[(set+96),]$week, a[(set+96),]$freq))
spline_int3$species = rp[3]

spline_int = rbind(spline_int1,spline_int2,spline_int3)


ggp = ggplot(data = a, aes(x = week, y = freq, col = species)) +
  #geom_point(size = 4) +
  xlab("month") +
  ylab("percentage of lists") +
  geom_line(data = spline_int, aes(x = x, y = y, col = species), size = 2) +
  geom_vline(xintercept = 37, linetype = "dotted", size = 2, col = "#E49B36") +
  geom_vline(xintercept = 42.1, linetype = "dotted", size = 2, col = "#31954E") +
  geom_vline(xintercept = 38.3, linetype = "dotted", size = 2, col = "#78CAE0")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(2,46,4),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_colour_manual(values = c("#31954E","#E49B36","#78CAE0")) +
  theme(legend.position = "none")

png('grewar3-lblwar1-grnwar1g4.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()



###########################################################



data = read.delim("grewar3-lblwar1-grnwar1.txt", sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

data = data[2:4,]
data = data[,-1]
data = t(data)
row.names(data) = NULL

rp = as.vector(data[1,])
data = data[-1,]


a = data.frame(species = rep(rp,each = length(data[,1])))
a$freq = c(as.numeric(data[,1]),as.numeric(data[,2]),as.numeric(data[,3]))
a$week = c(1:(length(a$freq)/3),1:(length(a$freq)/3),1:(length(a$freq)/3))

set = seq(1,48,1)

spline_int1 = as.data.frame(spline(a[set,]$week, a[set,]$freq))
spline_int1$species = rp[1]

spline_int2 = as.data.frame(spline(a[(set+48),]$week, a[(set+48),]$freq))
spline_int2$species = rp[2]

spline_int3 = as.data.frame(spline(a[(set+96),]$week, a[(set+96),]$freq))
spline_int3$species = rp[3]

spline_int = rbind(spline_int1,spline_int2,spline_int3)


ggp = ggplot(data = a, aes(x = week, y = freq, col = species)) +
  #geom_point(size = 4) +
  xlab("month") +
  ylab("percentage of lists") +
  geom_line(data = spline_int, aes(x = x, y = y, col = species), size = 2) +
  #geom_vline(xintercept = 37, linetype = "dotted", size = 2, col = "#E49B36") +
  geom_vline(xintercept = 43.5, linetype = "dotted", size = 2, col = "#31954E")
  #geom_vline(xintercept = 38.3, linetype = "dotted", size = 2, col = "#78CAE0")

ggp1 = ggp +
  theme(axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(angle = 90, size = 16), axis.text.y = element_text(size = 14)) +
  theme(text=element_text(family="Gill Sans MT")) +
  scale_x_continuous(breaks = seq(2,46,4),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_colour_manual(values = c("#31954E","#E49B36","#78CAE0")) +
  theme(legend.position = "none")

png('grewar3-lblwar1-grnwar1c2.png', units="in", width=10, height=7, res=1000)
ggp1
dev.off()