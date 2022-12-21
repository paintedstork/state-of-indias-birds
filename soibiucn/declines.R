#Calculate max declines. IUCN recommends max declines from all possible declines

maxDeclines <- function (CompareYears, threegen, declines)
{
  species <- CompareYears$Species %>% unique()
  for (sp in 1:length(species))
  {
    print(species[sp])
    ComparableYears_s <- CompareYears %>% filter (Species == species[sp])
    threegenspecies <- max (10, threegen %>% filter (English.Name == species[sp]) 
                                %>% select (GEN)
                                %>% as.numeric()
                                %>% round())
    MinYear <- ComparableYears_s %>% select (YEAR) %>% min()
    MaxYear <- ComparableYears_s %>% select (YEAR) %>% max()
  
    years <- ComparableYears_s$YEAR   
    max_decline <- 0
    max_ci <- 0
    max_Year1 <- 0
    max_Year2 <- 0
    for (year in years)
    {
      if (year + threegenspecies <= MaxYear)
      {
         M1 <-  ComparableYears_s %>% filter (YEAR == year) %>% select (Frequency) 
         M2 <-  ComparableYears_s %>% filter (YEAR == year + threegenspecies) %>% select (Frequency) 
         SE1 <- ComparableYears_s %>% filter (YEAR == year) %>% select (SE) 
         SE2 <- ComparableYears_s %>% filter (YEAR == year + threegenspecies) %>% select (SE) 
         
         if(M2 < M1)
         {
           mean_decline <- 100 * (M1 - M2)/M1
           mean_CI <- 196 * (M2/M1) * sqrt((SE1/M1)^2 + (SE2/M2)^2) 
           
           if ( (mean_CI < 50) & (M1/SE1 > 2 ))
           {
             print(paste(mean_decline, mean_CI))
             mean_decline <- mean_decline - mean_CI
             
             if ((mean_decline > 20) & (mean_decline > max_decline ))
             {
               max_decline <- mean_decline
               max_Year1 <- year
               max_Year2 <- year + threegenspecies
             }
           }
           else
           { # Mean to be twice the SE, CI < 50%
             print(paste(M1,SE1,M2,SE2,mean_CI))
           }
         }
      }
    }
    if(max_decline != 0)
    {
      print (paste(species [sp], max_decline, max_Year1, max_Year2))
      declines <- declines %>% rbind (data.frame (Species = species[sp],
                                                  YEAR1 = max_Year1,
                                                  YEAR2 = max_Year2,
                                                  mean_decline = max_decline))
    }
  }
  return (declines)
}
  
maxDeclinesAll <- function (CompareYears, threegen)
{
  decline <- data.frame(Species = as.character(), 
                        YEAR1 = as.integer(),
                        YEAR2 = as.integer(),
                        mean_decline = as.numeric())
  declines <- list ("High" = decline, "Moderate" = decline, "Low" = decline)
  
  declines$High <- maxDeclines (CompareYears$High, threegen, declines$High)
  declines$Moderate <- maxDeclines (CompareYears$Moderate, threegen, declines$Moderate)
  declines$Low <- maxDeclines (CompareYears$Low, threegen, declines$Low)
  return (declines)
}


