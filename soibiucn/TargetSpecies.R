###################################################################
#  Find of which species is likely to meet some relevant thresholds
#  This function can be further optimised for species that are likely to have negative trends
#  Execution time is not all that bad, thence no further optimization is done
###################################################################

getTargetSpecies <- function (freq, threegen, MinYear, MaxYear)
{
  Years <- c(MinYear:MaxYear)
  TrendYears <- MaxYear - MinYear + 1
  threegenperiod <- 2 * (TrendYears - 1) -1 
  
  # Make a list of species for which freq data is available
  species <- freq %>% 
    select (Species) %>% 
    unique() %>% 
    as.data.frame()
  
  # Filter species whose 3GEN data is shorter than the period of reliable data
  threegen <- threegen %>%
    filter (GEN <= threegenperiod)
  
  # Join with the species for which comparable trend data exists.
  species <- inner_join (species, threegen, by = c("Species" = "English.Name"))
  
  
  return (species$Species %>%  unique())
}
