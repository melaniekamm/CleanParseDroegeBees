
clean_things <- function(data, datefolder){

##Remove rows lacking lat/long or taxonomic ID, add identifier, genus species columns
data <- dplyr::filter(data, name != '' & SPECIMEN != "" & !is.na(longitude) & !is.na(latitude)) %>%
        dplyr::distinct(SPECIMEN, .keep_all = T) %>%
        dplyr::mutate(identifier= as.character(SPECIMEN)) %>% #add identifier column
        tidyr::separate(name, sep=" ", into=c("Genus", 'species'), remove=F) #add genus and species columns

#add 'identifier' column so time/date functions will work
data <- data.table(data, key='identifier')

#add genus columns
temp <- strsplit(data$name, split=" ")
data$Genus <- as.character(sapply(temp, '[', 1))
data$Genus <- R.utils::capitalize(data$Genus)

#add species name column, fix capitalization so species names match
data$species <- as.character(sapply(temp, '[', 2))
data$species <- R.utils::decapitalize(data$species)

#fix spelling errors in genus name
data$Genus[data$Genus %in% c("Adrena", "Amdrema", "Anderna")] <- 'Andrena'
data$Genus[data$Genus %in% c("Certina")] <- 'Ceratina'
data$Genus[data$Genus %in% c("Haclictus", "Halitcus")] <- 'Halictus'

#add cleaned up genus and species name together
data$name <- paste(data$Genus, data$species, sep=" ")

#fix names that include 'near' or ?  or 'group' assume identification was correct
data$name <- gsub(data$name, pattern="near_", replacement="")
data$name <- gsub(data$name, pattern="_group", replacement="")
data$name <- gsub(data$name, pattern="\\?", replacement="")

#fix species mis-spellings
data$name[data$name == 'Andrena morisonella'] <- 'Andrena morrisonella'
data$name[data$name == 'Andrena puni'] <- 'Andrena pruni'
data$name[data$name == 'Augochloropsis metallica_fulgida' | data$name == 'Augochloropsis metallica_metallica'] <- 'Augochloropsis metallica'

data$name[data$name == 'Ceratina deformed_calcarata'] <- 'Ceratina calcarata'
data$name[data$name == 'Ceratina dupla/mikmaqi/calcarata' | data$name == 'Ceratina calcarata/mikmaqi' | data$name == 'Ceratina dupla/mikmaqi' | data$name == 'Ceratina mikmaqi/calcarata'] <- 'Ceratina calcarata/dupla/mikmaqi'
data$name[data$name == 'Ceratina miqmaki'] <- 'Ceratina mikmaqi'

data$name[data$name == 'Halictus poeyi/ligatus'] <- 'Halictus ligatus/poeyi'
data$name[data$name == "Lasioglossum brikmanni"] <- "Lasioglossum birkmanni" 

data$name[data$name == "Nomada sayi/illinoense"] <- "Nomada sayi/illinoensis"
data$name[data$name == "Osmia atriventis"] <- "Osmia atriventris"

data$name[data$name == "Osmia atriventis"] <- "Osmia atriventris"
data$name[data$name == "Osmia conjucta"] <- "Osmia conjuncta"

data$name[data$name == "Bombus binaculatus"] <- "Bombus bimaculatus"
data$name[data$name == "Bombus imaptiens"] <- "Bombus impatiens"

data$name[data$name == "Augochlora aurata"] <- "Augochlorella aurata"

#remove individuals that were only identified to genus?
data <- data[!(grepl(data$name, pattern= "species") | grepl(data$name, pattern = "sp.") | grepl(data$name, pattern = "_sp") | grepl(data$name, pattern = "interesting") | grepl(data$name, pattern = "male")),]

#remove Andrena observations recorded to sub-genus, but not species
data <- data[!data$name %in% c('Andrena (Melandrena)', 'Andrena (Scrapteropsis)', 'Andrena (Trachandrena)'),]

#remove observations with NA recorded as species (only ID'd to genus)
data <- data[!is.na(data$species),]

#replace data$NTraps == 'numerous' to NA
data$NTraps[data$NTraps == 'numerous' & !is.na(data$NTraps)] <- NA
data$NTraps[data$NTraps == '~130' & !is.na(data$NTraps)] <- 130


#import date and time variables in R format
startdate <- fread(paste(datefolder, '/startdate.csv', sep=""), header=T)
startdate <- data.table(startdate[,2:4], key='identifier')

#take out startdate rows that do not have identifier
takeout <- which(is.na(startdate$identifier))
startdate <- startdate[!takeout,]

#change startdate identifier to match data
startdate$identifier <- gsub(startdate$identifier, pattern= "http://www.discoverlife.org/mp/20l?id=", replacement="", fixed=T)

enddate <- fread(paste(datefolder, '/enddate.csv', sep=""), header=T)
enddate <- data.table(enddate[,2:4], key='identifier')

#take out enddate rows that do not have identifier
takeout <- which(is.na(enddate$identifier))
enddate <- enddate[!takeout,]
enddate$identifier <- gsub(enddate$identifier, pattern= "http://www.discoverlife.org/mp/20l?id=", replacement="", fixed=T)

data <- merge(data, startdate)
data <- merge(data, enddate)

#remove specimens that do not have a date recorded
data <- data[!is.na(data$startdate),]

#convert dates to R 'Date' format
data$enddate_num <- as.Date(data$enddate, format='%Y-%m-%d %z')
data$startdate_num <- as.Date(data$startdate, format='%Y-%m-%d %z')

#calculate number of days that traps were left out (add # of hours later)
data$trapdays <- difftime(data$enddate_num, data$startdate_num, units='days')

#assume data with start date but no end date are net collected or trapped for single day, assign 0 for 'trapdays'
data$trapdays[!is.na(data$startdate_num) & is.na(data$enddate_num)] <- 0

#if number of days trap was left out is less than 0, take the opposite (-x), assuming start and end date were reversed
data$trapdays[data$trapdays < 0 & !is.na(data$trapdays)] <- -data$trapdays[data$trapdays < 0 & !is.na(data$trapdays)]


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

#remove rows with duplicated identifiers (duplicate specimens)
data <- dplyr::filter(data, !duplicated(identifier))

return(data)
}
