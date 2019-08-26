require(tidyverse)

source('~/GitHub/state-of-indias-birds/SoIB functions.R')
load("dataforanalyses.RData")
load("listforfig3.RData")

list = specieslist %>%
  filter(ht == 1)
list = list[sample(nrow(list), 100), ]
list$freq1 = list$freq2 = NA



for (i in (length(na.omit(list)$ht)+1):length(list$COMMON.NAME))
{
  print(i)
  start = Sys.time()
  a = freqtrends(data,species=list$COMMON.NAME[i],specieslist,error=F)
  if (!is.na(list$ht[i]))
  {
    list$freq1[i] = a[a$timegroupsf == "before 2000",]$freq[1]
  }
  list$freq2[i] = a[a$timegroupsf == "2014",]$freq[1]
  end = Sys.time()
  print(end-start)
}

rm(list=setdiff(ls(envir = .GlobalEnv), c("list")), pos = ".GlobalEnv")
