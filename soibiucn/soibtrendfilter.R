library (tidyverse)
# Exploratory analysis for finding how birding behaviour is different
# across years in different parts of India


# Based on exploratory analysis, all years before 2014 were considered unsuitable
MaxYear <- 2021
MinYear <- 2014
Years <- c(MinYear:MaxYear)
TrendYears <- MaxYear - MinYear + 1
plotting <- 0

freq <- read.csv("rawtrends.csv")

# Remove freq data outside comparable data range
freq <- freq %>% 
  filter (Time.Bands >= MinYear, Time.Bands <= MaxYear, !is.na(Frequency)) %>% 
  mutate (Time.Bands = as.integer(Time.Bands),
          Frequency  = as.numeric(Frequency),
          SE         = as.numeric(SE))

ranges <- read.csv("ranges.csv")
threegen <- read.csv("3genbli.csv")

load("..\\soib_v2\\dataforanalyses_extra.RData")

######################################################################
#  Find out comparable years from various parameters
######################################################################
data0 <- data0 %>% select ("LOCALITY.TYPE",
                           "PROTOCOL.TYPE",
                           "DURATION.MINUTES",
                           "EFFORT.DISTANCE.KM",
                           "ALL.SPECIES.REPORTED",
                           "no.sp",
                           "month",
                           "year",
                           "gridg2",
                           "group.id")

# Use only lists. No need of per species data
ebd_lists <- data0 %>% distinct (group.id, .keep_all = TRUE)

# Add HOUR and SPEED
ebd_lists  <-  ebd_lists %>%
  mutate (
    HOUR  = 12,
    # Lists that are between 4am & 8pm have already been filtered
    # Uncomment the next line when Ashwin V provides START.TIME                   
    #                HOUR  = strptime(TIME.OBSERVATIONS.STARTED, format = "%H:%M:%S") %>% format("%H") %>% as.integer(),
    SPEED = 60 * EFFORT.DISTANCE.KM / DURATION.MINUTES,
    # SEASON = Seasons [month]
    ) %>% 
  mutate(gridg2 = as.integer(gridg2))

# All SoIB filters come here. Some has been already done by Ashwin V

ebd_lists <- ebd_lists %>% 
  filter (PROTOCOL.TYPE %in% c("Historical", "Traveling", "Stationary"),
          ALL.SPECIES.REPORTED == 1,
          (HOUR < 20)  | is.na(HOUR),
          (HOUR > 3) | is.na(HOUR),
          SPEED < 20 | is.na(SPEED), # Speed less than 20kmph
          year >= MinYear,
          year <= MaxYear
  )


# Fewer than 2 species/hour and reported three or fewer species in all.
# Less than four species in all if Duration was not specified.
ebd_lists <- ebd_lists %>% 
  filter ( (is.na(DURATION.MINUTES) & no.sp >= 4) |
             (!is.na(DURATION.MINUTES) & 60 * no.sp/DURATION.MINUTES >=2))   

# Make SoIB Year bands - not required as start year is 2014
#ebd_lists <- ebd_lists %>% mutate (SOIB_YEAR = SoIBYearBand[year-1999])

ebd_lists <- ebd_lists %>% select ("LOCALITY.TYPE",
                                   "PROTOCOL.TYPE",
                                   "DURATION.MINUTES",
                                   "EFFORT.DISTANCE.KM",
                                   "no.sp",
                                   "gridg2",
                                   "year")

source("TargetSpecies.R")
species <- getTargetSpecies (freq, threegen, MinYear, MaxYear)

source("ComparableYears.R")
CompareYears <- getComparableYears (ebd_lists, species, range = ranges, MinYear, MaxYear)
  
CompareYears$High <- CompareYears$High %>% inner_join (freq, by = c("Species" = "Species", "YEAR" = "Time.Bands"))
CompareYears$Moderate <- CompareYears$Moderate %>% inner_join (freq, by = c("Species" = "Species", "YEAR" = "Time.Bands"))
CompareYears$Low <- CompareYears$Low %>% inner_join (freq, by = c("Species" = "Species", "YEAR" = "Time.Bands"))

source("predict.R")

CompareYears <<- extrapolateYearsAll (CompareYears)

CompareYears$High <- CompareYears$High %>% filter (!is.na(Frequency))
CompareYears$Moderate <- CompareYears$Moderate %>% filter (!is.na(Frequency)) 
CompareYears$Low <- CompareYears$Low %>% filter (!is.na(Frequency))

source("declines.R")

declines <- maxDeclinesAll (CompareYears, threegen)

write.csv(declines$High, "HighCDecline.csv")
write.csv(declines$Moderate, "ModCDecline.csv")
write.csv(declines$Low, "LowCDecline.csv")
