extrapolate <- function (species, df, yearlist)
{
  f <- data.frame(YEAR = integer(), Mean = numeric())
  
  # A custom function that randomly generates n means based on mean & SE of a year
  # It returns a dataframe of Year and means
  rnormN <- \(x, y, N) cbind (YEAR = rep(y,N), Frequency = rnorm(N,x$Frequency,x$SE))
  
  # Iterate and concatenate the generated means for all years
  for (x in 1:nrow(df))
  {
    f <- f %>% rbind (rnormN(df[x,], df$YEAR[x], 1000))
  }
  
  # Fit an expontential model
  exp_model <- lm(log(Frequency) ~ YEAR, data = f)

  yearlist <- as.data.frame(yearlist)
  colnames(yearlist) <- c("YEAR")
  extrapolate <- predict( exp_model, 
                               newdata = yearlist, 
                               se.fit = TRUE)
  
  retdf <- data.frame (YEAR = yearlist$YEAR, 
                          Frequency = exp(extrapolate$fit), 
                          SE = extrapolate$se.fit)
  retdf$Species <- species
  return (retdf)
}


extrapolateYears <- function (ComparableYears)
{
  species <- ComparableYears$Species %>% unique()
  for (sp in 1:length(species))
  {
    ComparableYears_s <- ComparableYears %>% filter (Species == species[sp])
    MinYear <- ComparableYears_s %>% select (YEAR) %>% min()
    MaxYear <- ComparableYears_s %>% select (YEAR) %>% max()
    AllYears <- seq(MinYear, MaxYear + MaxYear - MinYear)
    
    ExistingYears <- ComparableYears_s$YEAR %>% sort()
    MissingYears <- setdiff (AllYears, ExistingYears)
    ComparableYears <- ComparableYears %>% rbind (extrapolate (species[sp], ComparableYears_s, MissingYears))
  }
  return (ComparableYears)
}

extrapolateYearsAll <- function (CompareYears)
{
  CompareYears$High <- extrapolateYears (CompareYears$High)
  CompareYears$Moderate <- extrapolateYears (CompareYears$Moderate)
  CompareYears$Low <- extrapolateYears (CompareYears$Low)
  return (CompareYears)
}
