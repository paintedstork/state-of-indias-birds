############################################################

## occupancy analyses for bird abundance/range
## Requires tidyverse, reshape2, data.table and unmarked####
## type = trivial, null, nosp, nosptime, nb, nosptimenb
## cutoff removes vagrants

occufreq = function(data, species, mig, resolution, type = "null", nb = 4, cutoff = 0)
{
  require(tidyverse)
  require(reshape2)
  require(data.table)
  require(unmarked)
  
  load("neighbours.RData")
  
  spec = species[mig == "LM"]
  species = c(species,spec)
  mig[mig == "LM"] = "LMS"
  mig = c(mig,rep("LMW",length(spec)))
  
  data = data %>%
    filter(EFFORT.DISTANCE.KM <= 50, year > 2013)
  
  ## remove unlikely observations/passage
  
  if (cutoff != 0)
  {
    d = data %>%
      filter(ALL.SPECIES.REPORTED == 1) %>%
      group_by(gridg5,month) %>% mutate(lists = n_distinct(group.id)) %>% ungroup %>%
      filter(COMMON.NAME %in% species) %>%
      group_by(gridg5,month,COMMON.NAME) %>% summarize(freq = n_distinct(group.id)/max(lists)) %>% ungroup %>%
      filter(freq < cutoff) %>% select(gridg5,month,COMMON.NAME)
      
      d = left_join(d,data)
      data = anti_join(data,d)
  }
  
  # create dataframe to store occupancy and detection proabability estimates across species and spatial resolutions
  res = c("g2","g3","g4")
  
  est = array(data=NA,dim=c(length(species),3,length(res)),
              dimnames=list(species,c("detprob","occ","occ.se"),c("50km","80km","100km")))
  
  data1 = nocturnallistcheck(data)
  
  for(s in 1:length(species))
  {
    if (mig[s] == "S" | mig[s] == "W")
    {
      temp1 = data %>%
        filter(COMMON.NAME == species[s]) %>%
        distinct(month)
      
      temp2 = data1 %>%
        filter(COMMON.NAME == species[s]) %>%
        distinct(month)
      
      datai = temp1 %>% left_join(data)
      datac = temp2 %>% left_join(data1)
    }
    
    if (mig[s] == "R")
    {
      datai = data
      datac = data1
    }

    if (mig[s] == "LMS")
    {
      datai = data %>%
        filter(month %in% c(5:8))
      
      datac = data1 %>%
        filter(month %in% c(5:8))
    }
    
    if (mig[s] == "LMW")
    {
      datai = data %>%
        filter(month %in% c(11:12,1:2))
      
      datac = data1 %>%
        filter(month %in% c(11:12,1:2))
    }
    
    selexp = expandbyspecies(datac,species[s])
    
    selexp = selexp[sample(1:nrow(selexp)),]
    
    selexp$month[selexp$month %in% c(11,12,1,2)] = "Win"
    selexp$month[selexp$month %in% c(3,4,5,6)] = "Sum"
    selexp$month[selexp$month %in% c(7,8,9,10)] = "Mon"
    
    for(r in resolution)
    {
  
      if(r == "g2")
      { nb4g = nb4g2
        nb8g = nb8g2
        lpg = selexp %>%
        group_by(gridg2) %>% summarize(lpg = n())
      listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
      inc = datai %>%
        mutate(gridg = gridg2)
      selexp = selexp %>% 
        arrange(gridg2) %>%
        mutate(gridg = gridg2) %>%
        group_by(gridg) %>% mutate(group.id = 1:n())}
      
      if(r == "g3")
      { nb4g = nb4g3
        nb8g = nb8g3
        lpg = selexp %>%
        group_by(gridg3) %>% summarize(lpg = n())
      listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
      inc = datai %>%
        mutate(gridg = gridg3)
      selexp = selexp %>% 
        arrange(gridg3) %>%
        mutate(gridg = gridg3) %>%
        group_by(gridg) %>% mutate(group.id = 1:n())}
      
      if(r == "g4")
      { nb4g = nb4g4
        nb8g = nb8g4
        lpg = selexp %>%
        group_by(gridg4) %>% summarize(lpg = n())
      listcutoff = quantile(lpg$lpg, 0.95, na.rm=TRUE)
      inc = datai %>%
        mutate(gridg = gridg4)
      selexp = selexp %>% 
        arrange(gridg4) %>%
        mutate(gridg = gridg4) %>%
        group_by(gridg) %>% mutate(group.id = 1:n())}
      
      nbt = selexp %>%
        group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
        mutate(fl=replace(fl, fl > 1, 1))
      
      fil = sum(nbt$fl)
      len = length(nbt$fl)
      
      nbti = inc %>%
        group_by(gridg) %>% summarize(fl = sum(OBSERVATION.COUNT)) %>%
        mutate(fl=replace(fl, fl > 1, 1))
      
      fil1 = sum(nbti$fl)
      len = len-fil+fil1
      fil = fil1
        
      
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
      
      if(type == "nb" | type == "nosptimenb")
      {
        nbt$gridg = as.character(nbt$gridg)
        nbt$gridg = as.numeric(nbt$gridg)
        nbti$gridg = as.character(nbti$gridg)
        nbti$gridg = as.numeric(nbti$gridg)
        
        for (i in 1:length(nbt$gridg))
        {
          temp = as.numeric(nb4g[[nbt$gridg[i]]])
          sm = sum(nbti[nbti$gridg %in% temp,]$fl)/length(temp)
          nbt$nb4[i] = sm
          temp = as.numeric(nb8g[[nbt$gridg[i]]])
          sm = sum(nbti[nbti$gridg %in% temp,]$fl)/length(temp)
          nbt$nb8[i] = sm
        }
        
        nbt$gridg = as.character(nbt$gridg)
        tp1 = nbt %>% select(-fl)
        tp = left_join(nbti,tp1)
        nbt = nbt[,-2]
        
        nbtx = tp[tp$fl != 1,]
        
        detn = data.frame(gridg = det[,1])
        detn= left_join(detn,nbt)
      }
      
      if (type == "trivial")
      {
        est[s,"detprob", match(r,res)] =  NA
        est[s,"occ", match(r,res)] = fil/len
        est[s,"occ.se", match(r,res)] = 0
      }
      
      if (type == "null")
      {
        umf = unmarkedFrameOccu(y=det[,-1], siteCovs =NULL, obsCovs = NULL)
        occ_det = tryCatch({occu(~1 ~1, data=umf, starts = c(0,0))},error = function(cond){"skip"})
  
        if (!is.character(occ_det))
        {
          f1 = predict(occ_det, type = "det", newdata = data.frame(0))
          f1 = mean(f1$Predicted)
          f2 = predict(occ_det, type = "state", newdata = data.frame(0))
          f2 = f2 %>% filter(!is.na(Predicted))
          f2a = f2$Predicted
          f2b = f2$SE
          
          est[s,"detprob", match(r,res)] =  f1
          est[s,"occ", match(r,res)] = f2a
          est[s,"occ.se", match(r,res)] = f2b
        }
        
        
      }
      
      if (type == "nosp")
      {
        umf = unmarkedFrameOccu(y=det[,-1], siteCovs =NULL, obsCovs = list(cov1 = cov.nosp[,-1]))
        occ_det = tryCatch({occu(~cov1 ~1, data=umf, starts = c(0,0,0))},error = function(cond){"skip"})
        
        if (!is.character(occ_det))
        {
          f1 = predict(occ_det, type = "det", newdata = data.frame(cov1=20))
          f1 = mean(f1$Predicted)
          f2 = predict(occ_det, type = "state", newdata = data.frame(0))
          f2 = f2 %>% filter(!is.na(Predicted))
          f2a = f2$Predicted
          f2b = f2$SE
          
          est[s,"detprob", match(r,res)] =  f1
          est[s,"occ", match(r,res)] = f2a
          est[s,"occ.se", match(r,res)] = f2b
        }
        
      }
      
      if (type == "nosptime")
      {
        umf = unmarkedFrameOccu(y=det[,-1], siteCovs =NULL, obsCovs = list(cov1 = cov.nosp[,-1], 
                                                                           cov2 = cov.month[,-1]))
        
        occ_det = tryCatch({occu(~cov1*cov2 ~1, data=umf, starts = c(0,0,0,0,0,0,0))},
                           error = function(cond){"skip"})
        
        if (!is.character(occ_det))
        {
          f1 = predict(occ_det, type = "det", newdata = data.frame(cov1=20, cov2=factor(c("Mon","Win","Sum"))))
          f1 = mean(f1$Predicted)
          f2 = predict(occ_det, type = "state", newdata = data.frame(0))
          f2 = f2 %>% filter(!is.na(Predicted))
          f2a = f2$Predicted
          f2b = f2$SE
          
          est[s,"detprob", match(r,res)] =  f1
          est[s,"occ", match(r,res)] = f2a
          est[s,"occ.se", match(r,res)] = f2b
        }
        
      }
      
      if (type == "nb")
      {
        if (nb == 4)
        {
          umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb4g = detn$nb4), obsCovs = NULL)
          
          occ_det = tryCatch({occu(~1 ~nb4g, data=umf, starts = c(0,0,0))},
                             error = function(cond){"skip"})
          
          newdat1 = data.frame(0)
          newdat2 = data.frame(nb4g=nbtx$nb4)
        }
        
        if (nb == 8)
        {
          umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb8g = detn$nb8), obsCovs = NULL)
          
          occ_det = tryCatch({occu(~1 ~nb8g, data=umf, starts = c(0,0,0))},
                             error = function(cond){"skip"})
          
          newdat1 = data.frame(0)
          newdat2 = data.frame(nb8g=nbtx$nb8)
        }
        
        if (!is.character(occ_det))
        {
          f1 = predict(occ_det, newdata = newdat1, type = "det")
          f1 = mean(f1$Predicted)
          f2 = predict(occ_det, newdata = newdat2, type = "state")
          f2 = f2 %>% filter(!is.na(Predicted))
          f2a = (sum(f2$Predicted) + fil)/len
          f2b = erroradd(f2$SE)/len
          
          est[s,"detprob", match(r,res)] =  f1
          est[s,"occ", match(r,res)] = f2a
          est[s,"occ.se", match(r,res)] = f2b
        }
        
      }
      
      if (type == "nosptimenb")
      {
        if (nb == 4)
        {
          umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb4g = detn$nb4), obsCovs = list(cov1 = cov.nosp[,-1], 
                                                                                                     cov2 = cov.month[,-1]))
          
          occ_det = tryCatch({occu(~cov1*cov2 ~nb4g, data=umf, starts = c(0,0,0,0,0,0,0,0))},
                             error = function(cond){"skip"})
          
          newdat1 = data.frame(cov1=20, cov2=factor(c("Mon","Win","Sum")))
          newdat2 = data.frame(nb4g=nbtx$nb4)
        }
        
        if (nb == 8)
        {
          umf = unmarkedFrameOccu(y=det[,-1], siteCovs = data.frame(nb8g = detn$nb8), obsCovs = list(cov1 = cov.nosp[,-1], 
                                                                                                     cov2 = cov.month[,-1]))
          
          occ_det = tryCatch({occu(~cov1*cov2 ~nb8g, data=umf, starts = c(0,0,0,0,0,0,0,0))},
                             error = function(cond){"skip"})
          
          newdat1 = data.frame(cov1=20, cov2=factor(c("Mon","Win","Sum")))
          newdat2 = data.frame(nb8g=nbtx$nb8)
        }
        
        if (!is.character(occ_det))
        {
          f1 = predict(occ_det, newdata = newdat1, type = "det")
          f1 = mean(f1$Predicted)
          f2 = predict(occ_det, newdata = newdat2, type = "state")
          f2 = f2 %>% filter(!is.na(Predicted))
          f2a = (sum(f2$Predicted) + fil)/len
          f2b = erroradd(f2$SE)/len
          
          est[s,"detprob", match(r,res)] =  f1
          est[s,"occ", match(r,res)] = f2a
          est[s,"occ.se", match(r,res)] = f2b
        }
        
      }
    
    
    
    }
  }  
  estdf = data.frame(rep(res, each = length(species)))
  names(estdf) = "resolution"
  estdf$species = rep(rownames(est), length(res))

  for (i in 1:length(res))
  {
    if (i == 1){temp = est[,,i]}
    if (i != 1){temp = rbind(temp,est[,,i])}
  }
  
  estdf$detprob = temp[,1]
  estdf$occ = temp[,2]
  estdf$occ.se = temp[,3]
  estdf$type = type
  estdf$nb = nb
  
  return(estdf)
}