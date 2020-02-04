##### Part 1: Subset data to pan trapped bees from Maryland and Delaware #####

#load libraries, read in cleaned up data 
library(data.table); library(reshape2); library(rgdal); library(sf); library(dplyr); library(raster)
clean_data <- fread('./data/DroegeAllData_clean.csv')

##### Part One: Filter occurences to specimens collected in Maryland, Delaware, Washington DC
# also includes three Virginia sites that were part of a structured sampling effort
mdde <- clean_data

#make site-year ID variable
mdde$SiteID_Year <- paste0(mdde$SiteID, "_", mdde$year)

#add polygon layer of Maryland, Delaware, Washington DC county boundaries
marydel <- st_read(dsn="D:/SpatialData/county_boundaries/cb_2013_us_county_500k.shp") %>%
  filter(STATEFP %in% c(24, 10, 11))

#make spatial layer of all survey locations and reproject to same CRS as county boundaries
locations <-  distinct(mdde, SiteID_Year, .keep_all = T) %>%
  dplyr::select(SiteID, SiteID_Year, year, latitude, longitude) %>%
  st_as_sf(coords=c('longitude', 'latitude'), crs=4326)
locations <- st_transform(locations, st_crs(marydel))


###take out spatial points that actually aren't in Maryland, Delaware, or DC
bound <- st_union(marydel) #dissolve boundaries between counties
two <- st_within(locations, bound, sparse=F) #store indication of intersection between points and MD/DE boundary
locations <- locations[two,]

#subset pan trap data to SiteIDs in Maryland, Delaware, and DC spatial object
mdde <- filter(mdde, SiteID %in% c(locations$SiteID))
 #st_write(locations, './spatial_points/site_sampling_locations4269.shp', delete_layer=T)


##### Part Two: Filter data to pan trapped specimens, take out specimens that aren't bees

#clean up SampleType, and TrapVolume, TrapColor, TrapLiquid columns
#reassign data to correct column when it was incorrectly parsed into different column
source('./code/functions/fix_TrapVolumeColor.R')

#fill in some missing trap type information, when possible to determine from field note
mdde$SampleType[mdde$field_note == "Bowl and glycol trapping of various kinds, numbers were not kept"] <- 'pan trap'

#look at genera included in dataset
genera <- data.frame(table(mdde$Genus))
names(genera) <- c('Genus', 'Abundance')
genera <- genera[order(genera$Abundance, decreasing=T),]

#read list of genera, specifying which are bees
isbee <- read.csv('./data/isbee_genera_populated.csv')

#take out specimens that aren't bees
mdde <- mdde[mdde$Genus %in% isbee$Genus[isbee$IsBee == 'Y' & isbee$NameVerified == 'Y'],]

#take out specimens that aren't identifed to species
mdde <- mdde[!grepl(mdde$name, pattern=" NA", fixed=T),]

#correct or take out species from outside mid-Atlantic USA
mdde$name[mdde$name == "Agapostemon angelicus/texanus"] <- "Agapostemon texanus" #Agapostemon angelicus is not found in Mid-Atlantic US
mdde$name[mdde$name == "Pseudopanurgus rudbeckiae"] <- "Pseudopanurgus near rudbeckiae"

#species not in our region
mdde <- mdde[!mdde$name %in% c('Lasioglossum incompletum'),]

#take out specimens that were not sampled with pan traps?
mdde <- mdde[mdde$SampleType %in% c('pan trap', 'in field note'),]

##### Part 3. Create abundance column, convert dates to R friendly format
#add abundance value
mdde$Abundance <- 1

#remove 1999 and 2001 from data, these years have fewer than 10 observations total
mdde <- mdde[mdde$year > 2001,]

#convert sampling dates to R 'Date' class
mdde$startdate_num <- as.Date(mdde$startdate_num); mdde$enddate_num <- as.Date(mdde$enddate_num)

#if there is no end date, aassume start and end date are the same
mdde$enddate_num[is.na(mdde$enddate_num)] <- mdde$startdate_num[is.na(mdde$enddate_num)]
#create middle of sampling period date object
mdde$middate_num <- mdde$startdate_num + floor((mdde$enddate_num-mdde$startdate_num)/2)
#convert middate to day of year and biweek
mdde$mid_DOY <- lubridate::yday(mdde$middate_num)
mdde$week2 <- lubridate::week(mdde$middate_num)

#create integer indicating 2 biweek intervals
mdde$biweek <- round(mdde$mid_DOY/14, digits=0)

#reclassify 'year' as a factor rather than integer
mdde$year <- as.factor(mdde$year)


##### Part 4. Manually reassign TransectID for a set of sites (same study) that used 1 GPS point for two habitat types

view2 <- dplyr::filter(mdde, grepl(mdde$field_note, pattern='road', fixed=T) | grepl(mdde$field_note, pattern='field', fixed=T)) %>%
  dplyr::filter(year == '2005')

view2 <- dplyr::mutate(view2, field_note = gsub(view2$field_note, pattern= " ;", replacement=";", fixed=T)) %>%
  dplyr::filter(grepl(view2$field_note, pattern= "blue;", fixed=T) | grepl(view2$field_note, pattern= "yellow;", fixed=T) | 
                  grepl(view2$field_note, pattern= "white;", fixed=T) | grepl(view2$field_note, pattern= "white ;", fixed=T) |
                  grepl(view2$field_note, pattern= "yellow ;", fixed=T) | grepl(view2$field_note, pattern= "blue ;", fixed=T) |
                  grepl(view2$field_note, pattern= "white  ;", fixed=T))

#manually clean up some string patterns in TransectID and field notes 
view2 <- dplyr::mutate(view2, TransectID = if_else(grepl(view2$field_note, pattern='road', fixed=T), paste0(view2$SamplEvent, "Troad"), 
                                                   paste0(view2$SamplEvent, "Tfield"))) %>%
  dplyr::mutate(field_note = dplyr::if_else(TransectID %in% c("MD7e9dcce0Tfield", 
                                                              "MD1ce30960Tfield", 
                                                              "MD157184c2Troad",
                                                              "MD28df34b4Troad",
                                                              "MDae4ed8e3Troad"), "5 white, 5 yellow, 5 blue; 1 missing",
                                            dplyr::if_else(TransectID %in% c("MDc9c94b23Tfield",
                                                                             "MD7c2c2431Tfield",
                                                                             "MD6b0d5076Tfield"), "5 white, 5 yellow, 5 blue; 2 missing",
                                                           "5 white, 5 yellow, 5 blue; 0 missing")))

#put cleaned up observations back into dataset
mdde <- filter(mdde, !SiteID_Year %in% view2$SiteID_Year) %>%
  full_join(view2)

#manually fix some transect IDs that are incorrect (assigned as two transects, but actually only one)
mdde$TransectID[mdde$SamplEvent == 'MD79cd0211'] <- "MDbfbf9079"
mdde$TransectID[mdde$SamplEvent == 'MDf0f5117e'] <- "MD5f0275ef"


write.csv(mdde, './data/Droege_MDDE_cleaned.csv')
