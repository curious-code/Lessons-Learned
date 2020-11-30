# Load libraries
library(tidyverse)
library(stringi)
library(postmastr)

#Read in dataset with address column that needs to be parsed
BD <- read_csv('/Users/nonyeo./Documents/TMG/BD.csv')

#Assign unique id numbers for each row
BD <- pm_identify(BD, var = "Address1")

#Create minimal address object eliminating non-full or unrecognizable information
BD_min <- pm_prep(BD, var = "Address1", type = "street")

#Check that address information is present in the data
pm_has_address(BD_min)
pm_country_any(BD_min)
pm_postal_any(BD_min) # FALSE because Country appears last

#Parse addresses starting from the end with country
BD_min <- pm_country_parse(BD_min)
#Test for postal codes again
pm_postal_any(BD_min) # TRUE because codes now appear last
BD_min <- pm_postal_parse(BD_min)

#Create dictionary for parsing city data; requires 
# a Census Bureau API key, see  ?pm_dictionary for details
# install.packages('tidycensus')
library(tidycensus)
census_api_key('xxx-apikey',
               overwrite = TRUE, install = TRUE)
# #To use API key..
readRenviron("~/.Renviron")

#Create dictionaries for states and cities
stateDict <- pm_dictionary(locale = "us", type = "state", case = "title")
#Save RDS object in directory for use in subsequent tasks
saveRDS(stateDict, 'StateDictionary.rds')
#readRDS('StateDictionary.rds')


#Check existence of states
pm_state_any(BD_min, dictionary = stateDict)
pm_state_all(BD_min, dictionary = stateDict) #FALSE not all addresses have states on the end
#Check which addresses have no states
pm_state_none(BD_min, dictionary = stateDict) #Shows that POBoxes, incomplete and non-US addresses are the issue

#Parse state names using dictionary
BD_min <- pm_state_parse(BD_min, dictionary = stateDict)

#Build object with names of all states in the U.S
city <- c('AL','AK','AZ','AR','CA','CO','CT','DE','FL','GA','HI','ID','IL','IN','IA','KS',
          'KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY',
          'NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV',
          'WI','WY','DC')
#Build dictionary with city names
cityDict <- pm_dictionary(locale = "us", type = "city", 
                          filter = city, case = c("title", "lower", "upper"))
#Save RDS object in directory for use in subsequent tasks
saveRDS(cityDict, 'CityDictionary.rds')
#readRDS('CityDictionary.rds')


#Check existence of cities in data
pm_city_any(BD_min, dictionary = cityDict)
pm_city_all(BD_min, dictionary = cityDict)
pm_city_none(BD_min, dictionary = cityDict)

#Parse city names using dictionary
BD_min <- pm_city_parse(BD_min, dictionary = cityDict)

#Prep data for parsing house and street addresses
BDSt <- BD_min[2:6] %>% pm_identify(var = pm.address) #BD_min[2:6] to exclude uid
BDSt_min <- pm_prep(BDSt, var = pm.address, type = "street")

#Parse house details
#House numbers
BDSt_min <- pm_house_parse(BDSt_min) 
#Directionals e.g. NW, SW etc.
BDSt_min <- pm_streetDir_parse(BDSt_min)
#Street suffixes e.g. St, Blvd, Ave etc.
BDSt_min <- pm_streetSuf_parse(BDSt_min)
#Street names
BDSt_min <- pm_street_parse(BDSt_min, ordinal = TRUE, drop = TRUE)

#Put it all back together
#Add parsed data to source
BDSt_parsed <- pm_replace(BDSt_min, source = BDSt)
#Rebuild addresses
####Note: parsing addresses with pm.type = 'intersection' throws out an "Error in quos(...) : object 'endQ' not found"
####Solution: edit data to exclude rows with pm.type = 'intersection'
BDSt_parsed <- BDSt_parsed[-2939,]
BDSt_parsed <- pm_rebuild(BDSt_parsed, output = "short", 
                            include_commas = FALSE, include_units = TRUE, 
                            keep_parsed = "no")

#Create unique identifier based on source data
ids <- BD_min$pm.uid
ids <- ids[-2939]
BDSt_parsed$id <- ids
#Clean dataset with state and zip info
colnames(BDSt_parsed)[1] <- 'Address'
colnames(BD_min)[3:5] <- c('City/Town', 'State', 'Zip')
BD_parsed <- merge(BD_min, BDSt_parsed, by.x = "pm.uid", by.y = "id")
BD_parsed <- BD_parsed[,c(1,7,3:5)]

#Merge parsed data to original dataset
BD_clean <- dplyr::right_join(BD_parsed, BD, by = "pm.uid")
BD_clean <- BD_clean[,c(8,2:5, 10:12)]
#colnames(BD_clean)[c(2,6)] <- c('Address', 'Phone')

# Clean data: convert all text to initial caps
BD_clean <- BD_clean %>% mutate(Name = stri_trans_totitle(gsub("\\s+", " ", Name)),
                                Address = stri_trans_totitle(gsub("\\s+", " ", Address)),
                                `City/Town` = stri_trans_totitle(gsub("\\s+", " ", `City/Town`)),
                                Description = stri_trans_totitle(gsub("\\s+", " ", Description)),
                                Website = stri_trans_tolower(gsub("\\s+", " ", Website)))

#Save clean dataset
write_csv(BD_clean, '~/Documents/BD_Clean.csv')



