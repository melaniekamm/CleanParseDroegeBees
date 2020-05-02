#this code creates a 'clean' version of the Droege GBIF data, calling dependent scripts where necessary

library(data.table); library(plyr); library(dplyr)


#download 'Source archive' of Droege data from GBIF then read in raw data
#GBIF URL: https://www.gbif.org/dataset/f519367d-6b9d-411c-b319-99424741e7de
#downloaded November 9, 2018

occur <- fread("./data/dwca-usgs-pwrc-biml-v1.5/occurrence.txt")

#####Part One: Basic data clean up to expose data within strings

#parse 'samplingProtocol','verbatimEventDate', and 'eventRemarks' columns to extract data
occur <-  tidyr::separate(data=occur, col=samplingProtocol, sep=";", into=c('SampleType','NTraps','TrapVolume','TrapColor','TrapLiquid')) %>%
          tidyr::separate(col=verbatimEventDate, into=c("time1", 'time2'), sep=";") %>%
          tidyr::separate(col=eventRemarks, sep="Field_Notes:", into=c('todelete', "field_note")) %>% 
          dplyr::select(-todelete) %>%
          tidyr::separate(col=field_note, sep="Note:", into=c("field_note", "note")) %>%
          dplyr::mutate(orig_field_note = field_note, orig_note = note)

#remove column name from data contained within each column (only applies to separated columns)
find_string <- paste0(paste0(names(occur), ":", collapse="|"), paste("|trapType:", "trapCount:", "trapVolume:", "trapColor:", 
              "trapLiqu", "StartDateTime:", "EndDateTime:", sep="|"), collapse = "|")
occur <- data.frame(occur)

for (i in c(19:23, 27:28)){
  mr <- gsub(occur[, i],pattern=find_string, replacement="")
  occur[, i] <- mr
}

#make copy of raw occurence data to manipulate
#rename columns to match older version of GBIF dataset (rest of workflow uses these names)
#remove unneeded columns
data <- occur %>%
        dplyr::rename(elevation=verbatimElevation, longitude=verbatimLongitude, latitude=verbatimLatitude, 
                      SPECIMEN= catalogNumber, name=scientificName, state=stateProvince) %>%
        dplyr::select(-c(language:occurrenceID), -recordNumber, -recordedBy, -samplingEffort,
                      -c(lifeStage:associatedReferences), -verbatimLocality, -verbatimDepth, -geodeticDatum, 
                      -coordinatePrecision, -dateIdentified, -kingdom, -vernacularName, -taxonRemarks)


##### Part Two: Use TimeDate functions to process Droege start and end time into R readable time and date
#note: these functions take a very long time to run, proceed with caution!

#load custom functions
source('./code/functions/TimeDate_functions.R'); source('./code/functions/RunTimeDateExtract.R')

#this code outputs  'startdate.csv', 'enddate.csv', etc files
#format_timedates(data=data, outputfolder = './data/alldata_dates', dotime=F)


##### Part Three: Merge previous info, add necessary ID variables
#add above extracted date and time info to full dataset, take out some columns, add sampling event, site, and transect ID variables
library(Hmisc); library(R.utils); library(stringr)

source('./code/functions/Droege_clean_function.R')
clean_data <- clean_things(data=data, datefolder='./data/alldata_dates', remove_togenus=T)

fwrite(clean_data, './data/DroegeAllData_clean.csv')

