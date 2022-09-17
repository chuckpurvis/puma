########################################################################
##  PUMA_in_TIGER_tidycensus.r
##  Analyze the different PUMAs from Census Bureau TIGER/Line files
##   using tidycensus
##  ACS 2006/2010 using the 2000 Census-based PUMAs
##  ACS 2016/2020 using the 2010 Census-based PUMAs
##     -- September 2, 2022 --
##     -- September 17, 2022 --
########################################################################

library(tidyverse)
library(tidycensus)

# setwd("~/Desktop/tidycensus_work/output")
setwd("~/Desktop/tidycensus_work")
getwd()

# Census API Key was installed in previous sessions, so no need to re-install
# census_api_key("fortycharacterkeysentbyCensusBureau",install=TRUE)
Sys.getenv("CENSUS_API_KEY")

# The steps to create a cleaned-up 2020 PUMA Name File
# 1. Download the PDF file: 
# https://www2.census.gov/geo/pdfs/reference/puma2020/2020_PUMA_Names.pdf
# 2. Convert from PDF to Word Format (online web app, I can't recall which.)
# 3. Copy all text and paste into a new BBEDIT app text file. Save as a text file,
#       PUMA2020_Names_bbedit2.txt
# 4. Read into R and clean up the names. Too many tabs in the input edited file.

# Try reading in the Census Bureau's 2020 PUMA Name File..... 9/9/2022
setwd("~/Desktop")
getwd()

puma2 <- read.delim("PUMA2020_Names_bbedit2.txt",header=TRUE,sep="\t")

puma2$pumaname2 <- paste(puma2$PUMANAME,puma2$name2,puma2$name3,puma2$name4,puma2$name5,puma2$name6)

puma2 <- puma2 %>% 
   relocate(pumaname2,.before=PUMANAME)

puma3 <- na.omit(puma2)

puma4 <- puma2 %>%  drop_na() %>% 
         arrange(STATEFP,PUMA5CE) %>% 
         select(STATEFP,PUMA5CE,pumaname2) %>% 
         rename(state=STATEFP,pumaname=pumaname2)

write.csv (puma4,"PUMA_names_2020_1.csv")
# This is the end of creating a cleaned-up 2020 PUMA Names file!

# But let's tally the new 2020 PUMAs to state level!!

newpuma2020 <- puma4 %>% 
               group_by(state) %>% 
               summarize(numpuma_2020Census = n()) 

# Set a list of variables to extract in each iteration of get_acs

selvars <- c(households_     = "B25044_001", # Total Households
             totalpop_      = "B01001_001", # Total Population (HHPOP+GQPOP)
             hhpop_total_   = "B25008_001") # Population in Households, Total

# Retrieve ACS 2006-2010 Data at PUMA level, 2000 Census-Based PUMAs!
puma2010 <- tidycensus::get_acs(survey="acs5", variables=selvars, geography="puma", # state="CA",
                             year=2010,output='wide') %>% 
            dplyr::arrange(GEOID)  %>% 
            dplyr::mutate(state =substr(GEOID,1,2))

state2010 <- puma2010 %>% 
             dplyr::arrange(state) %>% 
             dplyr::group_by(state) %>% 
             dplyr::summarize(TOTHH_0610  = sum(households_E),
                              TOTPOP_0610 = sum(totalpop_E),
                              HHPOP_0610  = sum(hhpop_total_E),
                              numpuma_2000Census = n())

# Retrieve ACS 2016-2020 Data at PUMA level, 2010 Census-Based PUMAs!
puma2020 <- tidycensus::get_acs(survey="acs5", variables=selvars, geography="puma", # state="CA",
                                year=2020,output='wide') %>% 
            dplyr::arrange(GEOID) %>% 
            dplyr::mutate(state =substr(GEOID,1,2))

state2020 <- puma2020 %>% 
  dplyr::arrange(state) %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(TOTHH_1620  = sum(households_E),
                   TOTPOP_1620 = sum(totalpop_E),
                   HHPOP_1620  = sum(hhpop_total_E),
                   numpuma_2010Census = n())

############################################################################
# Retrieve 2020 Decennial Total Population

selvars20  <- c(TotalPop_2020   = "P2_001N",   # Total Population
                GQ_Total_2020   = "P5_001N",   # Group Quarters Population, Total
                Total_HH_2020   = "H1_002N") # Occupied Housing Units

us_states2020  <- tidycensus::get_decennial(year=2020,  sumfile="pl", 
                                  geography = "state",
                                  show_call = TRUE,output="wide", variables = selvars20) %>% 
                  dplyr::mutate(state=GEOID) %>% 
                  dplyr::arrange(state) %>% 
                  dplyr::mutate(HH_Pop_2020 = TotalPop_2020 - GQ_Total_2020) %>% 
                  dplyr::relocate(state,.before=GEOID)

###########################################################################
## Stitch together the 2006/10 ACS, 2016/20 ACS and 2020 Decennial
##    and the tally of new 2020 Census-based PUMAs

combined1 <- us_states2020 %>% 
             full_join(state2010,by='state') %>% 
             full_join(state2020,by='state') %>% 
             relocate(numpuma_2000Census,.before=numpuma_2010Census)

combined2 <- combined1 %>% 
             mutate(state=as.numeric(state)) %>% 
             full_join(newpuma2020,by='state')

# Cleanup the Guam data cells.
combined2$GEOID[combined2$state == 66] <- "66"
combined2$NAME[combined2$state == 66] <- "Guam"

###########################################################################
# Write out data into csv files!
setwd("~/Desktop/tidycensus_work/output")
## setwd("~/Desktop/pl94171/output")

write.csv (combined2,"PUMAs_2000_2010_2020_us_states.csv")


### End of Process ########################################################