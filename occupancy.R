## occupancy analyses for bird abundance/range
###Requires tidyverse, reshape2, data.table and unmarked####
occufreq = function(data,species)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  selexp = expandbyspecies(data,species)
  eff = quantile(selexp$EFFORT.DISTANCE.KM, 0.975, na.rm=TRUE)
  
  lpg = selexp %>%
    group_by(gridg4) %>% summarize(lpg = n())
  listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
  
  selexp = selexp %>%
    filter(EFFORT.DISTANCE.KM < eff)
  
  selexp = selexp[sample(1:nrow(selexp)),]
  
  selexp = selexp %>% 
    arrange(gridg4) %>%
    mutate(gridg = as.numeric(gridg4)) %>%
    group_by(gridg) %>% mutate(group.id = 1:n())
  
  selexp$month[selexp$month %in% c(11,12,1,2)] = "Win"
  selexp$month[selexp$month %in% c(3,4,5,6)] = "Sum"
  selexp$month[selexp$month %in% c(7,8,9,10)] = "Mon"
  
  setDT(selexp)
  
  det = dcast(selexp, gridg ~ group.id, value.var = "OBSERVATION.COUNT")
  cov.month = dcast(selexp, gridg ~ group.id, value.var = "month")
  cov.nosp = dcast(selexp, gridg ~ group.id, value.var = "no.sp")
  
  det = setDF(det)
  cov.month = setDF(cov.month)
  cov.nosp = setDF(cov.nosp)
  
  det = det[,1:listcutoff]
  cov.month = cov.month[,1:listcutoff]
  cov.nosp = cov.nosp[,1:listcutoff]
  
  umf = unmarkedFrameOccu(y=det[,-1], siteCovs =NULL, obsCovs = list(cov1 = cov.nosp[,-1], cov2 = cov.month[,-1]))
  
  occ_det = occu(~cov1+cov2 ~1, data=umf)
  g = backTransform(occ_det, type="state")
  
  newdat = data.frame(cov1=20, cov2=factor(c("Mon","Win","Sum")))
  f = predict(occ_det, newdata = newdat, type = "det")
  
  f = mean(f$Predicted)
  
  fg = data.frame(species)
  names(fg) = "species"
  fg$detection = f
  fg$occupancy = g@estimate
  return(fg)
}

