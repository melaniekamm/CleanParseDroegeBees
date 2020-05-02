
clean_things <- function(data, datefolder, remove_togenus){

#####Part 1: Make site, transect, and sampling event ID variables  

##Remove rows lacking lat/long, add identifier
data <- dplyr::filter(data, !is.na(longitude) & !is.na(latitude)) %>%
        dplyr::filter(!(abs(latitude) > 90 | abs(longitude) > 180)) %>%
        dplyr::mutate(identifier= as.character(SPECIMEN)) #add identifier column

#add 'identifier' column so time/date functions will work
data <- data.table(data, key='identifier')

#add state abbreviation column
#add polygon layer of Maryland, Delaware, Washington DC county boundaries
states <- sf::st_read(dsn="D:/SpatialData/state_boundaries/cb_2016_us_state_500k.shp")

# sampling sites in the US should have NEGATIVE longitude values, fix longitudes that are > 0
data$longitude[data$country == 'USA' & data$longitude > 0] <- -data$longitude[data$country == 'USA' & data$longitude > 0]


#make spatial layer of all survey locations and reproject to same CRS as state boundaries
#add state abbreviation code
locations <-  sf::st_as_sf(data, coords=c('longitude', 'latitude'), crs=4326, remove=F) %>%
              sf::st_transform(sf::st_crs(states)) %>%
              sf::st_join(dplyr::select(states, STUSPS)) %>%
              dplyr::rename(state_code = STUSPS)

data <- sf::st_drop_geometry(locations)
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
data <- dplyr::left_join(data, startdate) %>%
        dplyr::left_join( enddate)

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


##### make site, sampling event, and transect ID variables

#hash lat/long to make it slightly shorter, and append to state code
l <- paste0(data$latitude, data$longitude)
hl <- hashr::hash(l, recursive=T)
data$SiteID <- paste0(data$state_code, DescTools::DecToHex(hl))

if(!length(unique(hl)) == length(unique(data$SiteID))){
  stop("something is wrong with site ID hash and encoding.")
}

#sampling event = unique combination of lat/long, start date and end date of sampling
s <- paste0(data$SiteID, as.numeric(data$startdate_num),as.numeric(data$enddate_num))
hs <- hashr::hash(s, recursive=T)

data$SamplEvent <- paste0(data$state_code, DescTools::DecToHex(hs))

if(!length(unique(hs)) == length(unique(data$SamplEvent))){
  stop("something is wrong with sampling event ID hash and encoding.")
}

t <- paste0(s, data$orig_field_note, data$orig_note)
ht <- hashr::hash(t, recursive=T)

data$TransectID <- paste0(data$state_code, DescTools::DecToHex(ht))

if(!length(unique(ht)) == length(unique(data$TransectID))){
  stop("something is wrong with transect ID hash and encoding.")
}


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

#fix species and Genus names that are merged together
data$name[data$name == "Augochlorellagratiosa"] <- "Augochlorella gratiosa"
data$name[data$name == "Chrysurapacifica"] <- "Chrysura pacifica"
data$name[data$name == "Copestylumvittatum"] <- "Copestylum vittatum"
data$name[data$name == "Eristalisobscurus"] <- "Eristalis obscurus"
data$name[data$name == "Copestylummarginatum"] <- "Copestylum marginatum"
data$name[data$name == "Autographaprecationis"] <- "Autographa precationis"
data$name[data$name == "Coelioxysoctodentata"] <- "Coelioxys octodentata"

#remove occurrences with no species names or duplicated names
data <- dplyr::filter(data, name != '' & SPECIMEN != "") %>%
  dplyr::distinct(SPECIMEN, .keep_all = T) %>%
  tidyr::separate(name, sep=" ", into=c("Genus", 'species', 'sub_species'), remove=F) #add genus and species columns


#capitalize genus columns
data$Genus <- R.utils::capitalize(data$Genus)

#fix capitalization so species names match
data$species <- R.utils::decapitalize(data$species)

#save Droege original name
data$orig_name <- data$name

#fix spelling errors in genus name
data$Genus[data$Genus %in% c("Adrena", "Amdrema", "Anderna")] <- 'Andrena'
data$Genus[data$Genus %in% c("Haclictus", "Halitcus")] <- 'Halictus'

#add cleaned up genus and species name together (and sub_species where appropriate)
data$name <- if_else(is.na(data$sub_species), paste(data$Genus, data$species, sep=" "),
                     paste(data$Genus, data$species, data$sub_species, sep=" "))

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

#species where some have sub_species and other observations do not should be one 'grouped species'
data$grouped_name[data$name == 'Augochloropsis metallica fulgida'|
                    data$name == 'Augochloropsis metallica metallica'] <- "Augochloropsis metallica" 

#remove individuals that were only identified to genus & listed as 'male'
data <- data[!grepl(data$name, pattern = " male", fixed=T),]

if (remove_togenus == T) {
  
#remove observations recorded to sub-genus, but not species
data <- data[!data$name %in% c('Andrena (Melandrena)', 'Andrena (Scrapteropsis)',
                               'Andrena (Trachandrena)', 'Andrena melandrena',
                               'Andrena (Microandrena)', 'Andrena (Scaphandrena)',
                               'Lasioglossum (Dialictus)', 'Lasioglossum (Evylaeus)'),]

#remove observations with NA recorded as species (only ID'd to genus)
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
