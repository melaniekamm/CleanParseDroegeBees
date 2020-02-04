#Extract trap volume, color, and number information for abundance analyses
#add trap information to each specimen, calculate Abundance per trap per day
#then aggregate by TransectID or site-year (sum AbundDayTrap and concatenate color and volume strings)
library(dplyr); library(tidyr); library(stringr); library(reshape2); library(data.table); library(dplyr)

#if necessary, read in clean up MDDE specimen data
mdde <- read.csv('./data/Droege_MDDE_cleaned.csv')

##### Part One: Manually clean up some string patterns in field notes that cause issues for regular expressions (below)

#manually change field notes formatting that is not handled by 'extract color' function
mdde$field_note <- gsub(mdde$field_note, pattern='10 bowls each fl yellow fl blue white', replacement="10 fl yellow, 10 fl blue, 10 white", fixed=T) %>%
          gsub(pattern='30 w,y,b', replacement="10w, 10y, 10b", fixed=T) %>%
          gsub(pattern='[(]', replacement="") %>%
          gsub(pattern='[)]', replacement="") %>%
          gsub(pattern='blue, yellow, and white uv 10 of each', replacement="10 w, 10 flyl, 10 flbl", fixed=T) %>%
          gsub(pattern='15 BOWLs alternating between blue, yellow and white', replacement="5b, 5y, 5w", fixed=T) %>%
          gsub(pattern='8 BOWLs 2bl, 2 y, 2lpink, 4 white 1or', replacement="8 BOWLs 2bl, 2 y, 2lpink, 2 white 1or", fixed=T) %>%
          gsub(pattern='three yellow, three blue, three white', replacement="3 yellow, 3 blue, 3 white", fixed=T) %>%
          gsub(pattern='bue', replacement="blue", fixed=T) %>%
          gsub(pattern='3 12 ounce glycol traps , fl bl, fl yl, white;', replacement="12 ounce glycol traps, 1 fl bl,1 fl yl,1 white", fixed=T) %>%
          gsub(pattern='9 12 ounce glycol traps run all year along the powerlines on the estuary center. 
               dates are variable and the usual 3 colors were used', 
               replacement="9 12 ounce glycol traps run all year along the powerlines on the estuary center. 3 fl bl; 3 fl yl; 3 w", fixed=T) %>%
          gsub(pattern='15 blue, yellow and white solo BOWLs', replacement="5b, 5y, 5w", fixed=T) %>%
          gsub(pattern='no missing', replacement="0 missing", fixed=T) %>%
          gsub(pattern='none missing', replacement="0 missing", fixed=T) %>%
          gsub(pattern='lost : ', replacement="lost:", fixed=T) %>%
          gsub(pattern='lost :', replacement="lost:", fixed=T) %>%
          gsub(pattern='unknown color lost', replacement="lost", fixed=T) %>% 
          gsub(pattern='1b, 1y spilled', replacement="2 spilled", fixed=T) %>%
          gsub(pattern='1b, 1w spilled', replacement="2 spilled", fixed=T) %>%
          gsub(pattern='2b, 1w spilled', replacement="3 spilled", fixed=T) %>%
          gsub(pattern='2y, 3b, 1w spilled', replacement="6 spilled", fixed=T) %>%
          gsub(pattern='1y, 1w spilled', replacement="2 spilled", fixed=T) %>%
          gsub(pattern='1 yellow cup spilled', replacement="1 spilled", fixed=T) %>%
          gsub(pattern='1 yellow cup broken', replacement="1 spilled", fixed=T) %>%
          gsub(pattern='1w spilled, 1w gone', replacement="2 spilled", fixed=T) %>%
          gsub(pattern='3 spilled, 4y crushed', replacement="7 spilled", fixed=T) %>%
          gsub(pattern='3 spilled, 4y crushed', replacement="7 spilled", fixed=T) %>%
          gsub(pattern='4w, 3b, 3y knocked over', replacement="10 spilled", fixed=T) %>%
          gsub(pattern='1 cup knocked over', replacement="1 spilled", fixed=T) %>%
          gsub(pattern='1y spilled;', replacement="1 spilled;", fixed=T) %>%
          gsub(pattern='2w,2y,3b lost', replacement="7 lost", fixed=T) %>%
          gsub(pattern='2w,3b,2y lost', replacement="7 lost", fixed=T) %>%
          gsub(pattern='2b,1w lost', replacement="3 lost", fixed=T) %>%
          gsub(pattern='3y,3b,3w lost', replacement="9 lost", fixed=T) %>%
          gsub(pattern='1b,1w lost', replacement="2 lost", fixed=T) %>%
          gsub(pattern='2y,1w,1b lost', replacement="4 lost", fixed=T) %>%
          gsub(pattern='lost 6 blue, 5 white, 5 yellow', replacement="16 lost", fixed=T) %>%
          gsub(pattern='missing-', replacement="missing", fixed=T) %>%
          gsub(pattern='12 oz. BOWL missing', replacement="missing, 12 oz.", fixed=T) %>%
          gsub(pattern='full BOWL missing', replacement="missing", fixed=T) %>%
          gsub(pattern='half ones missing', replacement="missing", fixed=T) %>%
          gsub(pattern='half missing', replacement="missing", fixed=T) %>%
          gsub(pattern='BOWL missing', replacement="missing", fixed=T) %>%
          gsub(pattern='missing;1y 0b 0w', replacement="1 missing", fixed=T) %>%
          gsub(pattern='missing;0y 0b 1w', replacement="1 missing", fixed=T) %>%
          gsub(pattern='missing;0y 2b 1w', replacement="3 missing", fixed=T) %>%
          gsub(pattern='missing;1y 0b 1w', replacement="2 missing", fixed=T) %>%
          gsub(pattern='missing;0y 0b 1w', replacement="1 missing", fixed=T) %>%
          gsub(pattern='missing;0y 1b 1w', replacement="2 missing", fixed=T) %>%
          gsub(pattern='missing;1y 2b 2w', replacement="5 missing", fixed=T) %>%
          gsub(pattern='missing 1w 1y 1b', replacement="3 missing", fixed=T) %>%
          gsub(pattern='1w 1y missing', replacement="2 missing", fixed=T) %>%
          gsub(pattern='2y 1w', replacement="3 missing", fixed=T) %>%
          gsub(pattern='8y 2w missing', replacement="10 missing", fixed=T) %>%
          gsub(pattern='12y 10w missing', replacement="22 missing", fixed=T) %>%
          gsub(pattern='1 w 1 y missing', replacement="2 missing", fixed=T) %>%
          gsub(pattern='1 yellow cup overturned', replacement="1 missing", fixed=T) %>%
          gsub(pattern='2 blue missing;run#112BOWL 1 white missing;run#124BOWL 1 blue, 1 yellow missing', replacement="", fixed=T) %>%
          gsub(pattern='20 fl 20 fb 20 w;0 missing;sunny 90s', replacement="20 fy 20 fb 20 w;0 missing;sunny 90s", fixed=T) %>%
          gsub(pattern='16 BOWLs mostly yellow but 4 blue and a couple white', replacement="16 BOWLs placed, 4 blue, 
          9 yellow, 3 white, yellow and white approx", fixed=T) %>%
          gsub(pattern='north-south direction;22 jul: 10 blue, 10 white, 10 yellow;24 jul: 10 blue, 10 white, 10 yellow;', 
          replacement="20 blue, 20 white, 20 yellow; represents two dates, 22 jul and 24 jul; north-south direction", fixed=T) %>%
          gsub(pattern='east-west direction;22 jul: 5 blue, 5 white, 5 yellow;24 jul: 5 blue, 5 white, 5 yellow;', 
          replacement="15 blue, 15 white, 15 yellow; represents two dates, 22 jul and 24 jul; east-west direction;", fixed=T) %>%
          gsub(pattern='8 w, 8 fy, 8 fb, 8 fy/for;', replacement="8 w, 16 fy, 8 fb", fixed=T) %>%
          gsub(pattern='hq ballfield 1;missing;0y 0b 1w;sunny;', replacement="hq ballfield 1", fixed=T) %>%
          gsub(pattern='hq ballfield 2;missing;0y 0b 1w;sunny;', replacement="hq ballfield 2", fixed=T) %>%
          gsub(pattern='golf course 3;missing;1y 0b 1w;sunny;', replacement="golf course 3", fixed=T) %>%
          gsub(pattern='golf course 2;missing;1y 0b 0w;sunny;', replacement="golf course 2", fixed=T) %>%
          gsub(pattern='annuals library;missing;0y 2b 1w;sunny at setup, cloudy at pickup;', replacement="annuals library", fixed=T) %>%
          gsub(pattern='butterfly habitat;missing;1y 0b 1w;sunny;', replacement="butterfly habitat", fixed=T) %>%
          gsub(pattern='heirloom garden;missing;0y 0b 1w;sunny at setup, cloudy at pickup;', replacement="heirloom garden", fixed=T) %>%
          gsub(pattern='san martin park;missing;1y 2b 2w;sunny;', replacement="san martin park", fixed=T) %>%
          gsub(pattern='john marshall park;missing;1y 0b 0w;sunny at setup, cloudy at pickup;', replacement="john marshall park", fixed=T) %>%
          gsub(pattern='blue dawn', replacement="original dawn dish soap", fixed=T) %>%
          gsub(pattern='dawn blue', replacement="original dawn dish soap", fixed=T) %>%
          gsub(pattern='1y spilled;', replacement="1 spilled;", fixed=T) %>%
          gsub(pattern='dark blue has 6 BOWLs and that white has 7', replacement="6 blue, 7 white", fixed=T)

#same thing for second field notes ('note') column
mdde$note <-  gsub(mdde$note, pattern='bombus', replacement="BOMBUS", fixed=T) %>%
        gsub(pattern='15 blue, yellow and white', replacement="5b, 5y, 5w", fixed=T) %>%
        gsub(pattern='15 blue,yellow and white', replacement="5b, 5y, 5w", fixed=T) %>%
        gsub(pattern='15 blue yellow and white', replacement="5b, 5y, 5w", fixed=T) %>%
        gsub(pattern='15 blue, yellow and white', replacement="5b, 5y, 5w", fixed=T) %>%
        gsub(pattern='15 yellow, blue and white bowls', replacement="5b, 5y, 5w", fixed=T) %>%
        gsub(pattern='15 yellow,blue and white solo bowls', replacement="5b, 5y, 5w", fixed=T) %>%
        gsub(pattern='*2 yellow and one blue cups were empty when picking', replacement="3 empty", fixed=T) %>%
        gsub(pattern='*one cup was empty', replacement="1 empty", fixed=T) %>%
        gsub(pattern='1 white cup destroyed', replacement="1 empty", fixed=T)


##### Part Two: Extract trap color, trap number, and number of missing traps from field_note and note columns for each specimen
#create binary color variables (i.e. isBlue) to enable summarized color by transect or site
source('./code/functions/ExtractColor_function.R')
storecolor <- extractcolor(DroegeDF=mdde)

#add trap color and number to dataset
storecolor <- dplyr::select(mdde, -X, -V1) %>%
              dplyr::full_join(storecolor)

#make binary color colums to indicate which colors were used for each transect
storecolor <-  dplyr::mutate(storecolor, isblue=dplyr::if_else(TrapColor == 'blue' | nblue > 0, "blue", "no"), 
               isyellow=dplyr::if_else(TrapColor == 'yellow' | nyellow > 0,  "yellow", "no"),
               iswhite=dplyr::if_else(TrapColor == 'white' | nwhite > 0,  "white", "no"),
               isFLblue=dplyr::if_else(TrapColor == 'blue-uv' | nFLblue > 0,  "FLblue", "no"),
               isFLyellow=dplyr::if_else(TrapColor == 'yellow-uv' | nFLyellow > 0, "FLyellow", "no"),
               isFLwhite=dplyr::if_else(TrapColor == 'white-uv' | nFLwhite > 0, "FLwhite", "no"),
               ispaleblue=dplyr::if_else(TrapColor == 'pale blue', 'PaleBlue', "no"),
               isFLpaleblue=dplyr::if_else(TrapColor == 'pale blue-uv','FLPaleBlue', "no"))

#identify transects that are missing color information
trans_check <-  dplyr::select(storecolor, TransectID, SiteID, year,state, field_note, note, starts_with('is'), nmissing) %>%
                filter(!duplicated(TransectID)) %>%
                filter( year %in% c('2004', '2005') & state == 'District of Columbia')

#assign colors to a few observations where trap color could be assumed from field notes indicating all specimens were the same study
#washington dc study field_notes all same style, and other observations record 'b', 'y', 'w' characters in other words as colors
storecolor$isblue[is.na(storecolor$isblue) & storecolor$year %in% c('2004', '2005') & 
                    storecolor$state == 'District of Columbia' & 
                    !storecolor$SiteID %in% c('DCbf0934a8', 'DC0199853a', 'DC27cac6c4', 'DC4deb18cf', 'DCb8f4b6c7') ]  <- 'blue'
storecolor$isyellow[is.na(storecolor$isyellow) & storecolor$year %in% c('2004', '2005') & 
                    storecolor$state == 'District of Columbia' & 
                    !storecolor$SiteID %in% c('DCbf0934a8', 'DC0199853a', 'DC27cac6c4', 'DC4deb18cf', 'DCb8f4b6c7') ]  <- 'yellow'
storecolor$iswhite[is.na(storecolor$iswhite) & storecolor$year %in% c('2004', '2005') & 
                    storecolor$state == 'District of Columbia' & 
                    !storecolor$SiteID %in%c('DCbf0934a8', 'DC0199853a', 'DC27cac6c4', 'DC4deb18cf', 'DCb8f4b6c7') ]  <- 'white'

storecolor$iswhite[storecolor$note == 'groundworks farm, in wicomico county' & storecolor$field_note == "fl bl;fl yl;white; "] <- 'white'
storecolor$isFLblue[storecolor$note == 'groundworks farm, in wicomico county' & storecolor$field_note == "fl bl;fl yl;white; "] <- 'FLblue'
storecolor$isFLyellow[storecolor$note == 'groundworks farm, in wicomico county' & storecolor$field_note == "fl bl;fl yl;white; "] <- 'FLyellow'


#parse field notes to extract color (which colors are present, not the number of traps of specific colors)
#fl blue
toMatchFLblue <- "(?:fb|fl bl|flbl|flb|fl blue|fluorescent blue|uv-blue|fl. blue|uv blue|florescent blue|blue-uv)"
flbluetransects <- unique(storecolor$TransectID[!is.na(str_match(storecolor$field_note, toMatchFLblue))])
storecolor$isFLblue[storecolor$TransectID %in% flbluetransects] <- 'FLblue'

#fl yellow
toMatchFLyellow <- "(?:fy|fl yl|flyl|fly|fl yellow|fluorescent yellow|uv-yellow|fl. yellow|uv yellow|yellow-uv|florescent yellow)"
flyellowtransects <- unique(storecolor$TransectID[!is.na(str_match(storecolor$field_note, toMatchFLyellow))])
storecolor$isFLyellow[storecolor$TransectID %in% flyellowtransects] <- 'FLyellow'

#fl white
toMatchFLwhite <- "(?:fw|fl wh|flwh|flw|fl white|fluorescent white|uv-white|fl. white|uv white)"
flwhitetransects <- storecolor$TransectID[!is.na(str_match(storecolor$field_note, toMatchFLwhite))]
storecolor$isFLwhite[storecolor$TransectID %in% flwhitetransects] <- 'FLwhite'

#blue
toMatchblue <- "[ ,;.]+(?:b|bl|bly)[ ,;.)]|[ ,;.]+(?:b|bl|bly)[ ,;.]|blue"
bluetransects <- storecolor$TransectID[!is.na(str_match(storecolor$field_note, toMatchblue))]
bluetransects <- bluetransects[!bluetransects %in% flbluetransects]
storecolor$isblue[storecolor$TransectID %in% bluetransects] <- 'blue'

#yellow
toMatchyellow <- "[ ,;.]+(?:y|yl|ylw|yllw)[,;.]|yellow"
yellowtransects <- storecolor$TransectID[!is.na(str_match(storecolor$field_note, toMatchyellow))]
yellowtransects <- yellowtransects[!yellowtransects %in% flyellowtransects]
storecolor$isyellow[storecolor$TransectID %in% yellowtransects] <- 'yellow'

#white
toMatchwhite <- "[ ,;.]+(?:w|wh|wt|wht)[,;.]|white"
whitetransects <- storecolor$TransectID[!is.na(str_match(storecolor$field_note, toMatchwhite))]
whitetransects <- whitetransects[!whitetransects %in% flwhitetransects]
storecolor$iswhite[storecolor$TransectID %in% whitetransects] <- 'white'

##### Part Three: Extract trap volume from field_note and note columns for each specimen

#Fill in trap volume from external data sources
#glycol woods study used 12 oz bowls (personal communication from Grace Savoy Burke, University of Delaware)
storecolor$TrapVolume[grepl(storecolor$note, pattern='woods glycol study', fixed=T)] <- 'bowl 12.0oz'
storecolor$TrapVolume[storecolor$TrapVolume == 'in field note'] <- NA

#parse trap volume from field notes if not recorded in 'TrapVolume' column
toMatchVolume <- "([0-9]+(\\.[0-9]+)?)(?:oz|oz.|ounce|Oz|Oz.)[ ,;.]|([0-9]+(\\.[0-9]+)?)[ ,;.]+(?:oz|oz.|ounce|Oz|Oz.)[ ,;.]"
matching <- str_match(storecolor$field_note, toMatchVolume)
matching <- data.frame(matching); matching <- dplyr::select(matching, X1, X2, X4)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X4 <- as.numeric(as.character(matching$X4))

matching2 <- str_match(storecolor$note, toMatchVolume)
matching2 <- data.frame(matching2); matching2 <- dplyr::select(matching2, X1, X2, X4)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X4 <- as.numeric(as.character(matching2$X4))

#For trap volume > 40 oz, assign NA value. > 40 oz is a mistake in parsing, not actual sampling method
sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0
trapvolume <-  sum1 + sum2 

storecolor$TrapVolumeParsed <- paste0("bowl ", trapvolume, "oz")
storecolor$TrapVolumeParsed[storecolor$TrapVolumeParsed == 'bowl 0oz'] <- NA

#reassign trap volumes within half an ounce to the most frequent value (3, 3,25, and 3.5 become 3.25)
storecolor$TrapVolumeParsed[storecolor$TrapVolumeParsed %in% c("bowl 3oz", "bowl 3.5oz")] <- "bowl 3.25oz"

#reformat parsed volumes so they match 'TrapVolume' column
storecolor$TrapVolume <- gsub(storecolor$TrapVolume, pattern=".0", replacement="", fixed=T)

#how many observations have trap volume information that conflicts? 
storecolor <- dplyr::mutate(storecolor, VolumesAgree= TrapVolume == TrapVolumeParsed) %>%
  dplyr::mutate(VolumesAgree = replace_na(VolumesAgree, FALSE))

#when recorded trapvolume and parsed trap volume disagree, use recorded value assuming field note is incorrect 
#(seems to be same field note with different pan volumes recorded, maybe different volume traps at same site, but probably unlikely)
storecolor <- dplyr::mutate(storecolor, TrapVolumeFinal= dplyr::if_else(is.na(TrapVolume), TrapVolumeParsed, TrapVolume)) %>%
              dplyr::mutate(TrapVolumeFinal= if_else(is.na(TrapVolumeFinal), "UNK", TrapVolumeFinal))

##### Part Four: Summarize number of successful traps associated with each specimen (successful = number set out - number missing)

#create columns for number of traps reported and NTraps parsed from field notes
#create column for final number of traps by taking minimum of reported and parsed NTraps
#evidence that sometimes missing traps were excluded from field notes and recorded in NTraps AND
#sometimes the reverse. aka NTraps doesn't include missing traps, but they are noted in field notes.
storecolor <- dplyr::mutate(storecolor, Nreported=as.character(NTraps)) %>%
               dplyr::mutate(Nreported=if_else(Nreported %in% c('in field note', 'UNK', "") | is.na(Nreported), '0', Nreported)) %>%
               dplyr::mutate(Nreported= as.numeric(Nreported),
                      Nparse=rowSums(dplyr::select(storecolor, nwhite, nblue, nyellow, nFLblue, nFLyellow, nFLwhite), na.rm=T), 
                      Nmissing=nmissing,
                      Nmatch= (Nreported == (Nparse-Nmissing))) %>%
              dplyr::group_by(identifier) %>%
              dplyr::mutate(NTrapsFinal = dplyr::if_else(Nreported > 0 & Nparse > 0 , min(Nreported, (Nparse-Nmissing)), max(Nreported, (Nparse-Nmissing))),
                            trapdays = as.numeric(trapdays)) %>%
              dplyr::select(-nmissing)

#check NTrapsFinal (against field notes and NTraps reported)
ntraps_check <-  ungroup(storecolor) %>%
              dplyr::select(TransectID, year,state, field_note, note, starts_with("N"),starts_with("is"), -name) %>%
              dplyr::filter(!duplicated(paste(TransectID, NTrapsFinal))) %>%
              dplyr::arrange(TransectID)

#create Abundance per day and Abundance per trap per day variables
#subset data to observations that have number of traps reported (from NTraps OR parsed field_note)
storecolor2 <-  dplyr::mutate(storecolor, MaxNTraps = if_else(NTrapsFinal == 0, NTraps, as.factor(NTrapsFinal)))

storecolor2$MaxNTraps[storecolor2$MaxNTraps %in% c("", "in field note", "UNK")] <- NA
storecolor2$MaxNTraps <- as.numeric(storecolor2$MaxNTraps)                     
                     
storecolor <- dplyr::filter(storecolor2, MaxNTraps > 0) %>%
              dplyr::mutate(Abundance=as.numeric(Abundance),
                            trapdays= if_else(trapdays == 0, 1, trapdays),  
                            AbundDay=Abundance/trapdays,
                            AbundTrap=Abundance/NTrapsFinal,
                            AbundDayTrap=AbundDay/NTrapsFinal)

storecolor <- ungroup(storecolor) %>%
              dplyr::na_if('no')

write.csv(storecolor, './data/Droege_MDDE_subset_withtrapinfo.csv')

rm(list=ls())
