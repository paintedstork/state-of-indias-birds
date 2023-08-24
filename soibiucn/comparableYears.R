# cT = Comparability Thesholds
cT <- rbind (c (10, 10, 10, 5, 2),   #High
             c (20, 20, 20, 10, 5),  #Moderate
             c (30, 30, 30, 15, 10)  #Low
             ) %>% as.data.frame()

rownames (cT) <- c ("High", "Moderate", "Low")
colnames (cT) <- c ("ListLength", "Duration", "Distance", "Hotspots", "NoEffort")



makeYearList <- function (Compare_Years, MaxYear, MinYear, Species, ComparableYears)
{
  # Make a list of comparable years   
  # YEAR1 and YEAR2 are start and end years
  for (YEAR2 in MaxYear:(MinYear +1))
  {
    for (YEAR1 in MinYear:(YEAR2-1))
    {
      if(Compare_Years[YEAR1 - MinYear+1, YEAR2 - MinYear+1])
      {
        ComparableYears <- ComparableYears %>% rbind (data.frame(Species = Species, YEAR = YEAR1))
        for(YEAR3 in (YEAR1 + 1) : (YEAR2 - 1))
        {
          #              print(paste(YEAR1, YEAR2, YEAR3))
          if( (YEAR1 != YEAR3) & (YEAR2 != YEAR3) &
              Compare_Years[YEAR1 - MinYear+1, YEAR3 - MinYear+1] &
              Compare_Years[YEAR3 - MinYear+1, YEAR2 - MinYear+1])
          { # YEAR3 is comparable to YEAR1 and YEAR2
            ComparableYears <- ComparableYears %>% rbind ( data.frame(Species = Species, YEAR = YEAR3))
          }
          
        }
        ComparableYears <- ComparableYears %>% rbind (data.frame(Species = Species, YEAR = YEAR2))
        break;
      }
      else
      {
        # YEAR1 and YEAR2 not comparable. Take next YEAR
      }
    }
    if(nrow(ComparableYears) > 0)
    {
      break;
    }
  }
  
  return (ComparableYears)
}

getComparableYears <- function(ebd_lists, species, range, MinYear, MaxYear)
{
  Years <- c(MinYear:MaxYear)
  TrendYears <- MaxYear - MinYear + 1
  
  YearMatrix <<- data.frame(Species = as.character(), 
                             YEAR = as.integer())
  ComparableYears  <-  list ("High" = YearMatrix, "Moderate" = YearMatrix, "Low" = YearMatrix)                       
  
  parameters <- NULL

  for (sp in 1:length(species))
  {
    # Read the range grids of each species
    range_f <- range %>% filter (Species == species[sp])
    
    # Filter the lists of only those grids
    ebd_lists_f <- inner_join(ebd_lists, range_f, by = c ("gridg2" = "Grid"))
    # Do comparability analysis and identify years of comparable data
    
    
    if(nrow(ebd_lists_f) > 0)
    {
      print(species[sp])
      
      ebd_lists_min <- ebd_lists_f %>% 
        filter (!is.na(DURATION.MINUTES))
      
      d_mean  <- ebd_lists_min %>% 
        group_by(year) %>% 
        summarize (MeanDuration = mean(DURATION.MINUTES),
                   CI_Duration = 1.96 * sd(DURATION.MINUTES)/sqrt(n())) %>%
        ungroup()
      
      ll_mean  <- ebd_lists_f %>% 
        group_by(year) %>% 
        summarize (mean = mean(no.sp),
                   ci = 1.96 * sd(no.sp)/sqrt(n()),
                   no_effort = 100 * sum((PROTOCOL.TYPE != 'Stationary' & is.na(EFFORT.DISTANCE.KM)) | 
                                            is.na(DURATION.MINUTES))/n(),
                   totallists = n(),
                   hotspot = 100 * sum(LOCALITY.TYPE == "H") / (sum (LOCALITY.TYPE == "P") + sum (LOCALITY.TYPE == "H")) 
        ) %>%
        ungroup()
      
      
      ll_mean <- left_join (ll_mean, d_mean, by = "year")
      
      ebd_lists_km <- ebd_lists_f %>% 
        filter (!is.na(EFFORT.DISTANCE.KM))
      
      d_mean  <- ebd_lists_km %>% 
        group_by(year) %>% 
        summarize (MeanDistance = mean(EFFORT.DISTANCE.KM),
                   CI_Distance = 1.96 * sd(EFFORT.DISTANCE.KM)/sqrt(n())) %>%
        ungroup()
      
      ll_mean <- left_join (ll_mean, d_mean, by = "year")
      
      
      ll_mean$Species <- species [sp]
      
      colnames(ll_mean) <- c("YEAR", "MeanListLength", "CI_ListLength", "NoEffortPerCent", "TotalLists","HotspotPerCent", "MeanDuration", "CI_Duration", "MeanDistance", "CI_Distance","Species")
      parameters <- rbind (parameters, ll_mean)
      
      
      SimMatrix <- matrix(
        rep(NA, TrendYears * TrendYears),
        nrow = TrendYears,  
        ncol = TrendYears,        
        byrow = TRUE         
      )
      rownames(SimMatrix) = Years
      colnames(SimMatrix) = Years
      
      
      CompareLL_Years   <- list(High = SimMatrix, Moderate = SimMatrix, Low = SimMatrix)
      CompareDur_Years  <- list(High = SimMatrix, Moderate = SimMatrix, Low = SimMatrix)
      CompareDist_Years <- list(High = SimMatrix, Moderate = SimMatrix, Low = SimMatrix)
      CompareHS_Years   <- list(High = SimMatrix, Moderate = SimMatrix, Low = SimMatrix)
      CompareNE_Years   <- list(High = SimMatrix, Moderate = SimMatrix, Low = SimMatrix)
      Compare_Years     <- list(High = SimMatrix, Moderate = SimMatrix, Low = SimMatrix)
      
      for (YEAR2 in MaxYear:(MinYear +1))
      {
        for (YEAR1 in MinYear:(YEAR2-1))
        {
          ll_mean1 <- ll_mean %>% filter (YEAR == YEAR1)
          ll_mean2 <- ll_mean %>% filter (YEAR == YEAR2)
          
          M1 <-  ll_mean1$MeanListLength
          M2 <-  ll_mean2$MeanListLength
          CI1 <- ll_mean1$CI_ListLength
          CI2 <- ll_mean2$CI_ListLength
          CompareLL_Years$High[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["High", "ListLength"]
          CompareLL_Years$Moderate[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["Moderate", "ListLength"]
          CompareLL_Years$Low[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["Low", "ListLength"]
          
          M1 <-  ll_mean1$MeanDuration
          M2 <-  ll_mean2$MeanDuration
          CI1 <- ll_mean1$CI_Duration
          CI2 <- ll_mean2$CI_Duration
          CompareDur_Years$High[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["High", "Duration"]
          CompareDur_Years$Moderate[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["Moderate", "Duration"]
          CompareDur_Years$Low[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["Low", "Duration"]
          
          M1 <-  ll_mean1$MeanDistance
          M2 <-  ll_mean2$MeanDistance
          CI1 <- ll_mean1$CI_Distance
          CI2 <- ll_mean2$CI_Distance
          CompareDist_Years$High[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["High", "Distance"]
          CompareDist_Years$Moderate[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["Moderate", "Distance"]
          CompareDist_Years$Low[YEAR1-MinYear+1,YEAR2-MinYear+1] <-  abs(as.numeric(100 *(abs(M2-M1) + CI1 + CI2) / (M2 + M1))) < cT["Low", "Distance"]
          
          M1 <-  ll_mean1$HotspotPerCent
          M2 <-  ll_mean2$HotspotPerCent
          CompareHS_Years$High[YEAR1-MinYear+1,YEAR2-MinYear+1] <- abs (M1-M2) < cT["High", "Hotspots"]
          CompareHS_Years$Moderate[YEAR1-MinYear+1,YEAR2-MinYear+1] <- abs (M1-M2) < cT["Moderate", "Hotspots"]
          CompareHS_Years$Low[YEAR1-MinYear+1,YEAR2-MinYear+1] <- abs (M1-M2) < cT["Low", "Hotspots"]
          
          M1 <-  ll_mean1$NoEffortPerCent
          M2 <-  ll_mean2$NoEffortPerCent
          CompareNE_Years$High[YEAR1-MinYear+1,YEAR2-MinYear+1] <- abs (M1-M2) < cT["High", "NoEffort"]
          CompareNE_Years$Moderate[YEAR1-MinYear+1,YEAR2-MinYear+1] <- abs (M1-M2) < cT["Moderate", "NoEffort"]
          CompareNE_Years$Low[YEAR1-MinYear+1,YEAR2-MinYear+1] <- abs (M1-M2) < cT["Low", "NoEffort"]
          
          Compare_Years$High <- CompareLL_Years$High & 
            CompareDur_Years$High &
            CompareDist_Years$High &
            CompareHS_Years$High &
            CompareNE_Years$High
          
          Compare_Years$Moderate <- CompareLL_Years$Moderate & 
            CompareDur_Years$Moderate &
            CompareDist_Years$Moderate &
            CompareHS_Years$Moderate &
            CompareNE_Years$Moderate
          
          Compare_Years$Low <- CompareLL_Years$Low & 
            CompareDur_Years$Low &
            CompareDist_Years$Low &
            CompareHS_Years$Low &
            CompareNE_Years$Low
        }
      }    
      print(Compare_Years)
      
      ComparableYears$High <- makeYearList (Compare_Years$High, MaxYear, MinYear, species [sp], ComparableYears$High)
      ComparableYears$Moderate <- makeYearList (Compare_Years$Moderate, MaxYear, MinYear, species [sp], ComparableYears$Moderate)
      ComparableYears$Low <- makeYearList (Compare_Years$Low, MaxYear, MinYear, species [sp],ComparableYears$Low)
      ebd_lists_km <-NULL
      ebd_lists_min <- NULL
      ebd_lists_f <- NULL
    }
  }
  print(nrow(ComparableYears$High))
  print(nrow(ComparableYears$Moderate))
  print(nrow(ComparableYears$Low))
  return(ComparableYears)
}


