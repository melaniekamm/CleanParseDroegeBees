rm(list=ls())
mdde <- read.csv('./data/Droege_MDDE_cleaned.csv')
storecolor <- read.csv('./data/Droege_MDDE_subset_withtrapinfo.csv')

source('./code/functions/subsum_to_siteyear_transect.R')
library(dplyr)
#Dataset #1: All Maryland/Delaware/DC occurrences
#take out some redundant columns
mdde_toarchive <- dplyr::select(mdde,-X, -V1, -startdate, -enddate, -middate_num, -mid_DOY, -SPECIMEN,
                -month, -week, -week2, -biweek, -eventDate, -trapdays, -state_code) %>%
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

#add family names
family <- read.csv('./data/checking_bee_names/family_genera.csv')

#generate list of unique species (original names and corrected) for table 
species_list <- dplyr::filter(mdde_toarchive, !duplicated(orig_name)) %>%
                dplyr::full_join(family) %>%
                dplyr::select(orig_name, name,  grouped_name, Genus, species, Family) %>%
                dplyr::mutate(name_altered = if_else(as.character(orig_name) == as.character(name), 'No', 'Yes'))
write.csv(species_list, './data/checking_bee_names/original_names_species_list.csv', row.names = F)

#Dataset #2: Maryland/Delaware/DC occurrences with sampling info

#reorder columns and take out some redundant columns
storecolor_toarchive <- dplyr::select(storecolor,-X) %>%
              dplyr::select(-month, -week, -week2, -biweek, -eventDate, -Nmatch, -VolumesAgree, 
                            -startdate, -enddate, -middate_num, -mid_DOY, -SPECIMEN, -Nreported, -state_code) %>%
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

#Dataset 3: Bee abundance per transect & transect-level sampling method (only occurrences with sampling effort)
transect <- subsum_to_siteyear_transect(df=storecolor,output='transect')

#reorder columns and take out some redundant columns
transect <- dplyr::rename(transect, TctColor=Color, TctVolume=TrapVolume) %>%
            dplyr::select(TransectID, SamplEvent, SiteID_Year, apis_abund, prop_apis, 
                          TctVolume, TctColor, ColorVolume, VolumeSimple, ColorSimple, 
                          trapdays, NTrapsFinal, Abundance, AbundDayTrap)

write.csv(mdde_toarchive,'./data/to_archive/1OccurrenceLevel_AllBees.csv', row.names = F)
write.csv(storecolor_toarchive,'./data/to_archive/2OccurrenceLevel_WithTrapInfo.csv', row.names = F)
write.csv(transect,'./data/to_archive/3TransectLevel_WithTrapInfo.csv', row.names = F)

#sample for technical validation (check color and volume matches field notes)
# techval <- sample_n(storecolor, size=1000, replace=F) %>%
#            dplyr::select(TransectID, time1, startdate, time2, enddate,
#                          TrapColor, TrapVolume, NTraps, orig_field_note, 
#                          orig_note, TrapVolumeFinal, Nreported, Nparse, 
#                          Nmissing, Nmatch, NTrapsFinal, starts_with('is'), starts_with('n'),
#                         -name, -note)
# 
# write.csv(techval, './data/technical_validation.csv')
