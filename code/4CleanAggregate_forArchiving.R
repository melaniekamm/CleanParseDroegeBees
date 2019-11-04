rm(list=ls())
clean_data <- read.csv('./data/DroegeAllData_clean.csv')
mdde <- read.csv('./data/Droege_MDDE_cleaned.csv')
storecolor <- read.csv('./data/Droege_MDDE_subset_withtrapinfo.csv')

source('./code/functions/subsum_to_siteyear_transect.R')
library(dplyr)
#Dataset #1: All Maryland/Delaware/DC occurences
#take out some redundant columns
mdde <- dplyr::select(mdde,-X, -V1)

#Dataset #2: Maryland/Delaware/DC occurences with sampling info

#reorder columns and take out some redundant columns
storecolor_archive <- dplyr::select(storecolor,-X) %>%
              dplyr::select(-TrapVolume, -NTraps,
                            -month, -week, -week2, -biweek, -eventDate, -Nmatch, -VolumesAgree) %>%
              dplyr::select(identifier, id, SPECIMEN, TransectID, SamplEvent, SiteID_Year, SiteID, year,
                            name, Genus, species, sex, identifiedBy,
                            latitude, longitude,
                            time1, time2, startdate, startdate_num, enddate, enddate_num,
                            middate_num, mid_DOY, trapdays,
                            country, countryCode, state, county, municipality, habitat,
                            coordinateUncertaintyInMeters,
                            field_note, note, orig_field_note, orig_note,
                            everything(),  -elevation)

#Dataset 3: Bee abundance per transect & transect-level sampling method (only occurences with sampling effort)
transect <- subsum_to_siteyear_transect(df=storecolor,output='transect')

#reorder columns and take out some redundant columns
transect <- dplyr::rename(transect, TrapColor=Color) %>%
            dplyr::select(TransectID, SamplEvent, SiteID_Year, apis_abund, prop_apis, 
                          TrapVolume, TrapColor, ColorVolume, VolumeSimple, ColorSimple, 
                          trapdays, NTrapsFinal, Abundance, AbundDayTrap)

write.csv(mdde,'./data/to_archive/1OccurrenceLevel_AllBees.csv')
write.csv(storecolor_archive,'./data/to_archive/2OccurrenceLevel_WithTrapInfo.csv')
write.csv(transect,'./data/to_archive/3TransectLevel_WithTrapInfo.csv')

#sample for technical validation (check color and volume matches field notes)
techval <- sample_n(storecolor, size=1000, replace=F) %>%
           dplyr::select(TransectID, time1, startdate, time2, enddate,
                         TrapColor, TrapVolume, NTraps, orig_field_note, 
                         orig_note, TrapVolumeFinal, Nreported, Nparse, 
                         Nmissing, Nmatch, NTrapsFinal, starts_with('is'), starts_with('n'),
                        -name, -note)

write.csv(techval, './data/technical_validation.csv')
