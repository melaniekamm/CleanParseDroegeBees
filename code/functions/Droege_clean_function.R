
clean_things <- function(data, datefolder, remove_togenus){

##Remove rows lacking lat/long or taxonomic ID, add identifier, genus species columns
data <- dplyr::filter(data, name != '' & SPECIMEN != "" & !is.na(longitude) & !is.na(latitude)) %>%
        dplyr::distinct(SPECIMEN, .keep_all = T) %>%
        dplyr::mutate(identifier= as.character(SPECIMEN)) %>% #add identifier column
        tidyr::separate(name, sep=" ", into=c("Genus", 'species'), remove=F) #add genus and species columns

#add 'identifier' column so time/date functions will work
data <- data.table(data, key='identifier')

####translate date and time, remove occurrences with no recorded date

#import date and time variables in R format
startdate <- fread(paste(datefolder, '/startdate.csv', sep=""), header=T)
startdate <- data.table(startdate[,2:4], key='identifier')

#take out startdate rows that do not have identifier
takeout <- which(is.na(startdate$identifier))
startdate <- startdate[!takeout,]

#change startdate identifier to match data
startdate$identifier <- gsub(startdate$identifier, pattern= "http://www.discoverlife.org/mp/20l?id=", 
                             replacement="", fixed=T)

enddate <- fread(paste(datefolder, '/enddate.csv', sep=""), header=T)
enddate <- data.table(enddate[,2:4], key='identifier')

#take out enddate rows that do not have identifier
takeout <- which(is.na(enddate$identifier))
enddate <- enddate[!takeout,]
enddate$identifier <- gsub(enddate$identifier, pattern= "http://www.discoverlife.org/mp/20l?id=", replacement="", fixed=T)

#take out duplicated rows
enddate <- dplyr::filter(enddate, !duplicated(identifier))
startdate <- dplyr::filter(startdate, !duplicated(identifier))

#merge dates with bee occurrence data
data <- merge(data, startdate, all.x=T)
data <- merge(data, enddate, all.x=T)

#remove specimens that do not have a start or end date recorded
data <- data[!(is.na(data$startdate) & is.na(data$enddate)),]

#convert dates to R 'Date' format
data$enddate_num <- as.Date(data$enddate, format='%Y-%m-%d %z')
data$startdate_num <- as.Date(data$startdate, format='%Y-%m-%d %z')

#calculate number of days that traps were left out (add # of hours later)
data$trapdays <- difftime(data$enddate_num, data$startdate_num, units='days')

#assume data with start date but no end date are net collected or trapped for single day, assign 0 for 'trapdays'
data$trapdays[!is.na(data$startdate_num) & is.na(data$enddate_num)] <- 0

#if number of days trap was left out is less than 0, take the opposite (-x), assuming start and end date were reversed
data$trapdays[data$trapdays < 0 & !is.na(data$trapdays)] <- -data$trapdays[data$trapdays < 0 & !is.na(data$trapdays)]

# sampling sites in the US should have NEGATIVE longitude values, fix longitudes that are > 0
data$longitude[data$country == 'USA' & data$longitude > 0] <- -data$longitude[data$country == 'USA' & data$longitude > 0]

### Make site and sampling event identifier variables ###
gps <- data.frame(identifier=data$identifier,state= data$state,latitude=as.numeric(data$latitude),
                  longitude=as.numeric(data$longitude), startdate=data$startdate_num, enddate=data$enddate_num,
                  field_note=data$field_note, note=data$note)

#calculate unique combinations of lat, long and call these 'sites'
gps$latlong <- paste(gps$latitude, gps$longitude, sep="_")
gps <- data.table(gps, key='latlong')

sites <- gps[which(!duplicated(gps$latlong)),]
sites$SiteID <- paste(substr(sites$state, start=1, stop=2), c(1:nrow(sites)), sep="")
sites <- data.table(sites, key='latlong')

sitesub <- dplyr::select(sites, latlong, SiteID)
gps <- merge(gps, sitesub)

#calculate unique combinations of lat, long, and date and call this 'SamplEvent'
gps$SamplEvent <- paste(gps$SiteID, gps$startdate, gps$enddate, sep="_")
gps <- data.table(gps, key='identifier')

#calculate unique combinations of 'SamplEvent' and field notes, call this 'TransectID'
gps$SE_note <- paste(gps$SamplEvent, gps$field_note, gps$note, sep="_")
transectIDs <- gps[which(!duplicated(gps$SE_note)),]

transectIDs$TransectID <- paste(transectIDs$SamplEvent, paste0("T", c(1:nrow(transectIDs))), sep="_")
gps <- merge(gps, dplyr::select(transectIDs, SE_note, TransectID),by="SE_note")


#add SiteID, SamplEvent, and TransectID to dataframe
data <- merge(data, dplyr::select(gps, identifier, SiteID, SamplEvent, TransectID), by='identifier')
#add column denoting year of sampling
data$year <- format(data$startdate_num, format="%Y")
data$month <- format(data$startdate_num, format="%m")

#add column for week of sampling within year
#use base function rather than lubridate because it changes 'week' on Sundays always
#add one to week # to count first partial week in Jan as a 'week'
data$week <- as.numeric(format(data$startdate_num, format="%U"))+1


#clean up SampleType when noted in field_note or note
#format field note and note as lower case
data$field_note <- as.character(data$field_note)
data$field_note <- stringr::str_to_lower(data$field_note)
data$field_note <- gsub(data$field_note, pattern='bowl', replacement='BOWL', fixed=T)

data$note <- as.character(data$note)
data$note <- stringr::str_to_lower(data$note)

#some observations actually net collected or vane traps, note this in SampleType
data$SampleType[grepl(data$field_note, pattern= "vane trap") & 
                  !grepl(data$field_note, pattern='also 3 springstar blue vane traps')] <- 'vane trap'
data$SampleType[grepl(data$note, pattern= "vane trap")] <- 'vane trap'

data$SampleType[grepl(data$note, pattern= "hand net")] <- 'hand net'
data$SampleType[grepl(data$field_note, pattern= "hand collected") | data$field_note == 'hand net' | 
                  grepl(data$field_note, pattern= "hand caught") | grepl(data$field_note, pattern= "netted")| grepl(data$field_note, pattern= "caught by hand")] <- 'hand net'

#####fix name, Genus, species columns

#capitalize genus columns
data$Genus <- R.utils::capitalize(data$Genus)

#fix capitalization so species names match
data$species <- R.utils::decapitalize(data$species)

#save Droege original name (useful for occurrences with multiple names)
data$original_name <- data$name

#fix spelling errors in genus name
data$Genus[data$Genus %in% c("Adrena", "Amdrema", "Anderna")] <- 'Andrena'
data$Genus[data$Genus %in% c("Haclictus", "Halitcus")] <- 'Halictus'

#add cleaned up genus and species name together
data$name <- paste(data$Genus, data$species, sep=" ")


#fix species mis-spellings
data$name[data$name == 'Andrena morisonella'] <- 'Andrena morrisonella'
data$name[data$name == 'Andrena puni'] <- 'Andrena pruni'
data$name[data$name == "Bombus binaculatus"] <- "Bombus bimaculatus"
data$name[data$name == "Bombus imaptiens"] <- "Bombus impatiens"
data$name[data$name == 'Ceratina miqmaki'] <- 'Ceratina mikmaqi'
data$name[data$name == 'Ceratina deformed_calcarata'] <- 'Ceratina calcarata'
data$name[data$name == "Lasioglossum brikmanni"] <- "Lasioglossum birkmanni"
data$name[data$name == "Lasioglossum coeropsis"] <- "Lasioglossum coreopsis"
data$name[data$name == "Lasioglossum geminum"] <- "Lasioglossum geminatum"
data$name[data$name == "Nomada luteolodies"] <- "Nomada luteoloides"
data$name[data$name == "Osmia atriventis"] <- "Osmia atriventris"
data$name[data$name == "Osmia conjucta"] <- "Osmia conjuncta"
data$name[data$name == "Nomada sayi/illinoense"] <- "Nomada sayi/illinoensis"
data$name[data$name == "Megachile apicata"] <- "Megachile apicalis"
data$name[data$name == "Melissodes tineta"] <- "Melissodes tinctus"
data$name[data$name == "Osmia callinsia"] <- "Osmia collinsiae"


#fix incorrect genus listed
data$name[data$name == "Augochlora aurata"] <- "Augochlorella aurata"
data$name[data$name == "Andrena bruneri"] <- "Lasioglossum bruneri"
data$name[data$name == "Andrena hitchensi"] <- "Lasioglossum hitchensi"
data$name[data$name == "Ceratina callidum"] <- "Lasioglossum callidum"
data$name[data$name == "Lasioglossum aurata"] <- "Augochlorella aurata"
data$name[data$name == "Lasioglossum carlini"] <- "Andrena carlini"
data$name[data$name == "Lasioglossum conjuncta"] <- "Osmia conjuncta"
data$name[data$name == "Lasioglossum perplexa"] <- "Andrena perplexa"
data$name[data$name == "Melissodes nivalis"] <- "Andrena nivalis"
data$name[data$name == "Nomada lustrans"] <- "Lasioglossum lustrans"
data$name[data$name == "Nomada personata"] <- "Andrena personata"

#fix duplicate names for occurrences with multiple IDs (list needs to be in same order)
data$name[data$name == 'Andrena arabis/algida'] <- 'Andrena algida/arabis'
data$name[data$name == 'Andrena morrisonella/imitatrix'] <- 'Andrena imitatrix/morrisonella'    
data$name[data$name == 'Andrena tridens/erythronii'] <- 'Andrena erythronii/tridens'

data$name[data$name == 'Ceratina calcarata/mikmaqi/dupla'|
          data$name == 'Ceratina dupla/mikmaqi/calcarata' | 
          data$name == 'Ceratina dupla/calcarata/mikmaqi' | 
          data$name == 'Ceratina mikmaqi/calcarata/dupla'|
          data$name == 'Ceratina mikmaqi/dupla/calcarata'] <- 'Ceratina calcarata/dupla/mikmaqi'

data$name[data$name == 'Ceratina dupla/calcarata'] <- 'Ceratina calcarata/dupla'
data$name[data$name == 'Ceratina mikmaqi/calcarata'] <- 'Ceratina calcarata/mikmaqi'
data$name[data$name == 'Ceratina mikmaqi/dupla'] <- 'Ceratina dupla/mikmaqi'

data$name[data$name == 'Coelioxys sayi/octodentata'] <- 'Coelioxys octodentata/sayi'
data$name[data$name == 'Halictus poeyi/ligatus'] <- 'Halictus ligatus/poeyi'
data$name[data$name == 'Hoplitis producta/pilosifrons'] <- 'Hoplitis pilosifrons/producta'
data$name[data$name == 'Hylaeus modestus/affinis'] <- 'Hylaeus affinis/modestus'
data$name[data$name == 'Lasioglossum rohweri/admirandum'] <- 'Lasioglossum admirandum/rohweri'
data$name[data$name == 'Lasioglossum weemsi/hitchensi'] <- 'Lasioglossum hitchensi/weemsi'
data$name[data$name == 'Lasioglossum smilacinae/lineatulum'] <- 'Lasioglossum lineatulum/smilacinae'
data$name[data$name == 'Megachile mendica/brevis'] <- 'Megachile brevis/mendica'
data$name[data$name == 'Melissodes nivea/agilis'] <- 'Melissodes agilis/nivea'
data$name[data$name == 'Melissodes fumosa/boltoniae'] <- 'Melissodes boltoniae/fumosa'
data$name[data$name == 'Melissodes subillata/illata'] <- 'Melissodes illata/subillata'
data$name[data$name == 'Nomada sayi/illinoensis'] <- 'Nomada illinoensis/sayi'
data$name[data$name == 'Nomada sulphurata/luteola'] <- 'Nomada luteola/sulphurata'
data$name[data$name == 'Osmia taurus/cornifrons'] <- 'Osmia cornifrons/taurus'
data$name[data$name == 'Osmia pumila/atriventris'] <- 'Osmia atriventris/pumila'

data$name[data$name == 'Sphecodes atlantis/cressonii/banksii' | 
            data$name == 'Sphecodes banksii/cressonii/atlantis'|
            data$name == 'Sphecodes banksii/atlantis/cressonii'|
            data$name == 'Sphecodes cressonii/banksii/atlantis'|
            data$name == 'Sphecodes cressonii/atlantis/banksii'] <- 'Sphecodes atlantis/banksii/cressonii'

data$name[data$name == 'Sphecodes banksii/atlantis'] <- 'Sphecodes atlantis/banksii'
data$name[data$name == 'Sphecodes cressonii/atlantis'] <- 'Sphecodes atlantis/cressonii'
data$name[data$name == 'Sphecodes cressonii/banksii'] <- 'Sphecodes banksii/cressonii'


#collapse names into applicable grouped names
data$grouped_name <- data$name

data$grouped_name[data$grouped_name == 'Andrena arabis'|
          data$grouped_name == 'Andrena algida'] <- 'Andrena algida/arabis'

data$grouped_name[data$grouped_name == 'Andrena imitatrix'|
          data$grouped_name == 'Andrena morrisonella'] <- 'Andrena imitatrix/morrisonella'    

data$grouped_name[data$grouped_name == 'Andrena erythronii'|
          data$grouped_name == 'Andrena tridens'] <- 'Andrena erythronii/tridens'


data$grouped_name[data$grouped_name == 'Ceratina calcarata/dupla'|
          data$grouped_name == 'Ceratina calcarata/mikmaqi' | 
          data$grouped_name == 'Ceratina dupla/mikmaqi'|
          data$grouped_name == 'Ceratina calcarata'|
          data$grouped_name == 'Ceratina dupla'|
          data$grouped_name == 'Ceratina mikmaqi'] <- 'Ceratina calcarata/dupla/mikmaqi'

data$grouped_name[data$grouped_name == 'Coelioxys octodentata'|
          data$grouped_name == 'Coelioxys sayi'] <- 'Coelioxys octodentata/sayi'

data$grouped_name[data$grouped_name == 'Halictus poeyi' |
          data$grouped_name == 'Halictus ligatus'] <- 'Halictus ligatus/poeyi'

data$grouped_name[data$grouped_name == 'Hoplitis producta'|
          data$grouped_name == 'Hoplitis pilosifrons'] <- 'Hoplitis pilosifrons/producta'

data$grouped_name[data$grouped_name == 'Hylaeus affinis'|
          data$grouped_name == 'Hylaeus modestus'] <- 'Hylaeus affinis/modestus'

data$grouped_name[data$grouped_name == 'Lasioglossum admirandum'|
          data$grouped_name == 'Lasioglossum rohweri'] <- 'Lasioglossum admirandum/rohweri'

data$grouped_name[data$grouped_name == 'Lasioglossum hitchensi'|
          data$grouped_name == 'Lasioglossum weemsi'] <- 'Lasioglossum hitchensi/weemsi'

data$grouped_name[data$grouped_name == 'Lasioglossum lineatulum'|
          data$grouped_name == 'Lasioglossum smilacinae'] <- 'Lasioglossum lineatulum/smilacinae'

data$grouped_name[data$grouped_name == 'Megachile mendica'|
          data$grouped_name == 'Megachile brevis'] <- 'Megachile brevis/mendica'

data$grouped_name[data$grouped_name == 'Melissodes agilis'|
          data$grouped_name == 'Melissodes nivea'] <- 'Melissodes agilis/nivea'

data$grouped_name[data$grouped_name == 'Melissodes boltoniae'|
          data$grouped_name == 'Melissodes fumosa'] <- 'Melissodes boltoniae/fumosa'


data$grouped_name[data$grouped_name == 'Melissodes illata'|
          data$grouped_name == 'Melissodes subillata'] <- 'Melissodes illata/subillata'

data$grouped_name[data$grouped_name == 'Nomada illinoensis'|
          data$grouped_name == 'Nomada sayi'] <- 'Nomada illinoensis/sayi'

data$grouped_name[data$grouped_name == 'Nomada luteola'|
          data$grouped_name == 'Nomada sulphurata'] <- 'Nomada luteola/sulphurata'

data$grouped_name[data$grouped_name == 'Osmia cornifrons'|
          data$grouped_name == 'Osmia taurus'] <- 'Osmia cornifrons/taurus'

data$grouped_name[data$grouped_name == 'Osmia atriventris'|
          data$grouped_name == 'Osmia pumila'] <- 'Osmia atriventris/pumila'


data$grouped_name[data$grouped_name == 'Sphecodes atlantis/banksii'|
          data$grouped_name == 'Sphecodes atlantis/cressonii' | 
          data$grouped_name == 'Sphecodes banksii/cressonii'|
          data$grouped_name == 'Sphecodes atlantis'|
          data$grouped_name == 'Sphecodes banksii'|
          data$grouped_name == 'Sphecodes cressonii'] <- 'Sphecodes atlantis/banksii/cressonii'


#remove individuals that were only identified to genus & listed as 'male'
data <- data[!grepl(data$name, pattern = " male", fixed=T),]

#remove observations recorded to sub-genus, but not species
data <- data[!data$name %in% c('Andrena (Melandrena)', 'Andrena (Scrapteropsis)',
                               'Andrena (Trachandrena)', 'Andrena melandrena',
                               'Andrena (Microandrena)', 'Andrena (Scaphandrena)',
                               'Lasioglossum (Dialictus)', 'Lasioglossum (Evylaeus)'),]

#remove observations with NA recorded as species (only ID'd to genus)
if (remove_togenus == T) {
  data <- data[!is.na(data$species),]
}

#remove occurrences of species names that do not exist on Discover Life (or no other occurrences globally)
to_remove <- c('Lasioglossum melanopus', 'Hylaeus cressonii', 'Lasioglossum kevensi', 'Osmia composita')
data <- dplyr::filter(data, !name %in% to_remove)

#translate updated species names to 'Genus' and 'species' columns
data <- dplyr::select(data, -Genus, -species) %>%
        tidyr::separate(name, sep=" ", into=c("Genus", 'species'), remove=F) #add genus and species columns


#remove rows with duplicated identifiers (duplicate specimens)
data <- dplyr::filter(data, !duplicated(identifier))

return(data)
}
