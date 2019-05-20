list = selectedspecies %>%
  filter(ht == 1 | rt == 1)
list$freq1 = list$freq2 = NA

for (i in 68:100)
{
  start = Sys.time()
  a = freqtrends(data,species=list$COMMON.NAME[i],error=F)
  if (!is.na(list$ht[i]))
  {
    list$freq1[i] = a[a$timegroupsf == "before 1999",]$freq[1]
  }
  list$freq2[i] = a[a$timegroupsf == "2014",]$freq[1]
  end = Sys.time()
  print(end-start)
}

rm(list=setdiff(ls(envir = .GlobalEnv), c("data","list")), pos = ".GlobalEnv")

list1 = list
list1$freq = list1$freq1
list1$type = "long-term"
list2 = list
list2$freq = list1$freq2
list2$type = "short-term"
list3 = rbind(list1,list2)

require(ggthemes)
theme_set(theme_tufte())

fhist = ggplot(list3, aes(freq, fill = type))+
  geom_histogram(binwidth = c(0.02), col = "black")+
  facet_grid(type ~ ., scale="free_y")+
  geom_vline(xintercept = 0.5, linetype = "dotted", size = 1) +
  xlab("frequency of reporting in base temporal bin")+
  ylab("count")+
  theme_bw()
fhist+
  theme(axis.title.x = element_text(vjust = 0.3, size = 30), axis.text.x = element_text(size = 14), 
        axis.title.y = element_text(vjust = 0.3, angle = 90, size = 30), axis.text.y = element_text(size = 14)) +
  scale_x_continuous(limits = c(0,1))+
  scale_fill_manual(values=c("#CCCCCC","#FFFFFF")) +
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        ##panel.border = theme_blank(),
        ##panel.background = theme_blank()
  )+
  theme(strip.text.y = element_text(size = 30, angle = -90))