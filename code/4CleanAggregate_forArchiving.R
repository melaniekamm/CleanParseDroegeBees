rm(list=ls())
clean_data <- read.csv('./data/DroegeAllData_clean.csv')
mdde <- read.csv('./data/Droege_MDDE_cleaned.csv')
storecolor <- read.csv('./data/Droege_MDDE_subset_withtrapinfo.csv')

source('./code/functions/subsum_to_siteyear_transect.R')
library(dplyr)
#Dataset #1: All Maryland/Delaware/DC occurences
#take out some redundant columns
mdde_toarchive <- dplyr::select(mdde,-X, -V1, -startdate, -enddate, -middate_num, -mid_DOY, -SPECIMEN,
                -month, -week, -week2, -biweek, -eventDate, -trapdays) %>%
        dplyr::rename(startdate=startdate_num, enddate=enddate_num, modif_fieldnote=field_note,
                modif_note=note, field_note=orig_field_note, note=orig_note) %>%
        dplyr::select(identifier, id, TransectID, SamplEvent,  SiteID, SiteID_Year, year,
                name, Genus, species, grouped_name, orig_name, sex, identifiedBy,
                latitude, longitude, coordinateUncertaintyInMeters,
                time1, time2, startdate, enddate,
                country, countryCode, state, county, municipality, habitat,
                coordinateUncertaintyInMeters,
                field_note, note, modif_fieldnote, modif_note,
                SampleType, TrapLiquid, TrapColor, TrapVolume, NTraps,
                dplyr::everything(),  -elevation)

#Dataset #2: Maryland/Delaware/DC occurences with sampling info

#reorder columns and take out some redundant columns
storecolor_toarchive <- dplyr::select(storecolor,-X) %>%
              dplyr::select(-month, -week, -week2, -biweek, -eventDate, -Nmatch, -VolumesAgree, 
                            -startdate, -enddate, -middate_num, -mid_DOY, -SPECIMEN, -Nreported) %>%
              dplyr::rename(startdate=startdate_num, enddate=enddate_num, modif_fieldnote=field_note,
                     modif_note=note, field_note=orig_field_note, note=orig_note) %>%
              dplyr::select(identifier, id, TransectID, SamplEvent,  SiteID, SiteID_Year, year,
                            name, Genus, species, grouped_name, orig_name, sex, identifiedBy,
                            latitude, longitude, coordinateUncertaintyInMeters,
                            time1, time2, startdate, enddate,
                            trapdays,
                            country, countryCode, state, county, municipality, habitat,
                            coordinateUncertaintyInMeters,
                            field_note, note, modif_fieldnote, modif_note,
                            SampleType, TrapLiquid, TrapColor, TrapVolume, NTraps,
                            everything(),  -elevation)

#Dataset 3: Bee abundance per transect & transect-level sampling method (only occurences with sampling effort)
transect <- subsum_to_siteyear_transect(df=storecolor,output='transect')

#reorder columns and take out some redundant columns
transect <- dplyr::rename(transect, TctColor=Color, TctVolume=TrapVolume) %>%
            dplyr::select(TransectID, SamplEvent, SiteID_Year, apis_abund, prop_apis, 
                          TctVolume, TctColor, ColorVolume, VolumeSimple, ColorSimple, 
                          trapdays, NTrapsFinal, Abundance, AbundDayTrap)

write.csv(mdde_toarchive,'./data/to_archive/1OccurrenceLevel_AllBees.csv')
write.csv(storecolor_toarchive,'./data/to_archive/2OccurrenceLevel_WithTrapInfo.csv')
write.csv(transect,'./data/to_archive/3TransectLevel_WithTrapInfo.csv')

#sample for technical validation (check color and volume matches field notes)
# techval <- sample_n(storecolor, size=1000, replace=F) %>%
#            dplyr::select(TransectID, time1, startdate, time2, enddate,
#                          TrapColor, TrapVolume, NTraps, orig_field_note, 
#                          orig_note, TrapVolumeFinal, Nreported, Nparse, 
#                          Nmissing, Nmatch, NTrapsFinal, starts_with('is'), starts_with('n'),
#                         -name, -note)
# 
# write.csv(techval, './data/technical_validation.csv')
