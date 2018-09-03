#############################################################################
##### FUNCTION TO READ SPECIFIC COLUMNS IN THE LARGE EBD DATA SET  ##########
##### NEED AWK INSTALLED IN YOUR COMPUTER TO RUN                   ##########
#############################################################################

#############################################################################
# FUNCTION: readEbdColumns                                                ###         
# PARAMETER : ebd_file_name: Name of the ebd file without .zip            ###
# PARAMETER : ebd_file_location: Absolute/Relative directory of the file  ###
# USAGE: readEbdColumns ('ebd_IN_relMay-2018', '..\\..\\data\\')          ###
#############################################################################

# Uncomment the next line to test this function
# Test <- 1
readEbdColumns <- function (ebd_file_name, ebd_file_location)
{
  
  filter_str <- '{print '

  #####################COLUMN CONFIGURATIONS######################
# Uncomment the fields that should be retained in the final ebd dataset

# filter_str <- paste(filter_str, '$01,\\"\\t\\"', sep='') #GLOBAL_UNIQUE_IDENTIFIER
# filter_str <- paste(filter_str, '$02,\\"\\t\\"', sep='') #LAST EDITED DATE
# filter_str <- paste(filter_str, '$03,\\"\\t\\"', sep='') #TAXONOMIC ORDER
  filter_str <- paste(filter_str, '$04,\\"\\t\\"', sep='') #CATEGORY	
  filter_str <- paste(filter_str, '$05,\\"\\t\\"', sep='') #COMMON NAME	
  filter_str <- paste(filter_str, '$06,\\"\\t\\"', sep='') #SCIENTIFIC NAME	
  filter_str <- paste(filter_str, '$07,\\"\\t\\"', sep='') #SUBSPECIES COMMON NAME	
# filter_str <- paste(filter_str, '$08,\\"\\t\\"', sep='') #SUBSPECIES SCIENTIFIC NAME	
# filter_str <- paste(filter_str, '$09,\\"\\t\\"', sep='') #OBSERVATION COUNT	
# filter_str <- paste(filter_str, '$10,\\"\\t\\"', sep='') #BREEDING BIRD ATLAS CODE	
# filter_str <- paste(filter_str, '$11,\\"\\t\\"', sep='') #BREEDING BIRD ATLAS CATEGORY	
# filter_str <- paste(filter_str, '$12,\\"\\t\\"', sep='') #AGE/SEX	
# filter_str <- paste(filter_str, '$13,\\"\\t\\"', sep='') #COUNTRY	
# filter_str <- paste(filter_str, '$14,\\"\\t\\"', sep='') #COUNTRY CODE	
# filter_str <- paste(filter_str, '$15,\\"\\t\\"', sep='') #STATE	
# filter_str <- paste(filter_str, '$16,\\"\\t\\"', sep='') #STATE CODE	
# filter_str <- paste(filter_str, '$17,\\"\\t\\"', sep='') #COUNTY	
# filter_str <- paste(filter_str, '$18,\\"\\t\\"', sep='') #COUNTY CODE	
# filter_str <- paste(filter_str, '$19,\\"\\t\\"', sep='') #IBA CODE	
# filter_str <- paste(filter_str, '$20,\\"\\t\\"', sep='') #BCR CODE	
# filter_str <- paste(filter_str, '$21,\\"\\t\\"', sep='') #USFWS CODE	
# filter_str <- paste(filter_str, '$22,\\"\\t\\"', sep='') #ATLAS BLOCK	
# filter_str <- paste(filter_str, '$23,\\"\\t\\"', sep='') #LOCALITY	
# filter_str <- paste(filter_str, '$24,\\"\\t\\"', sep='') #LOCALITY ID	
# filter_str <- paste(filter_str, '$25,\\"\\t\\"', sep='') #LOCALITY TYPE	
  filter_str <- paste(filter_str, '$26,\\"\\t\\"', sep='') #LATITUDE	
  filter_str <- paste(filter_str, '$27,\\"\\t\\"', sep='') #LONGITUDE	
  filter_str <- paste(filter_str, '$28,\\"\\t\\"', sep='') #OBSERVATION DATE	
# filter_str <- paste(filter_str, '$29,\\"\\t\\"', sep='') #TIME OBSERVATIONS STARTED 
# filter_str <- paste(filter_str, '$30,\\"\\t\\"', sep='') #OBSERVER ID	
  filter_str <- paste(filter_str, '$31,\\"\\t\\"', sep='') #SAMPLING EVENT IDENTIFIER	
# filter_str <- paste(filter_str, '$32,\\"\\t\\"', sep='') #PROTOCOL TYPE	
# filter_str <- paste(filter_str, '$33,\\"\\t\\"', sep='') #PROTOCOL CODE	
# filter_str <- paste(filter_str, '$34,\\"\\t\\"', sep='') #PROJECT CODE	
# filter_str <- paste(filter_str, '$35,\\"\\t\\"', sep='') #DURATION MINUTES	
# filter_str <- paste(filter_str, '$36,\\"\\t\\"', sep='') #EFFORT DISTANCE KM	
# filter_str <- paste(filter_str, '$37,\\"\\t\\"', sep='') #EFFORT AREA HA	
# filter_str <- paste(filter_str, '$38,\\"\\t\\"', sep='') #NUMBER OBSERVERS	
# filter_str <- paste(filter_str, '$39,\\"\\t\\"', sep='') #ALL SPECIES REPORTED	
  filter_str <- paste(filter_str, '$40,\\"\\t\\"', sep='') #GROUP IDENTIFIER	
# filter_str <- paste(filter_str, '$41,\\"\\t\\"', sep='') #HAS MEDIA	APPROVED	
  filter_str <- paste(filter_str, '$42,\\"\\t\\"', sep='') #REVIEWED		
  filter_str <- paste(filter_str, '$43,\\"\\t\\"', sep='') #REASON	
# filter_str <- paste(filter_str, '$44,\\"\\t\\"', sep='') #TRIP COMMENTS	
# filter_str <- paste(filter_str, '$45,\\"\\t\\"', sep='') #SPECIES COMMENTS	
################################################################

  # Remove terminal tab and add the closing braces
  filter_str <- substr(filter_str,0, nchar(filter_str)-7) 
  filter_str <- paste(filter_str, ';}', sep='')

#Unzip and read eBird records. This assumes the file is stored in a top level folder called 'data'
  print("Unzipping file...")
  unzip(paste(ebd_file_location, ebd_file_name,'.zip',sep=''))

# Strip eBird file using an awk script as the file cant load fully in R due to excessive memory requirements
# The field that interest us are indicated by the column numbers in filter_str
   awkscript <- paste('\"BEGIN {FS=\\"\\t\\"};',filter_str,'\"',sep='')

# Run the awk command and wait till the operation is completed   
   print("Stripping columns...")
   system2 ("awk", 
             args = c(awkscript, paste(ebd_file_name,".txt", sep='')),
             wait = TRUE, # Wait till awk completes
             stdout = 'ebd.tmp')

# Read the stripped tmp file
   print("Reading columns...")
   ebd <- read.delim('ebd.tmp', na.strings = c("NA", "", "null"), as.is=TRUE, quote="")
# Remove the tmp file   
   unlink('ebd.tmp')
   return (ebd)
}

# Test Code
if(Test)
{
  ebd <- readEbdColumns ('ebd_IN_relMay-2018', '..\\..\\data\\')
  print(names(ebd))
  print(nrow(ebd))
  print(head(ebd, 5))
}
