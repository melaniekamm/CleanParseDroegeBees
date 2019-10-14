#function to summarize abundance or richness data to site-year (ready for analysis)
#also subsets by season and takes out sites with poor phenological coverage

subsum_to_siteyear_transect <- function(df, datatype, output='transect') {
  
  #aggregate long 'storecolor' by site-year or transect
  #Volume and Color variables are all values associated with each transect
  #read in subset mdde data with trap information
  
  #rename dataset to work with existing code
  storecolor <- df
  storecolor$startdate_num <- as.Date(storecolor$startdate_num); storecolor$enddate_num <- as.Date(storecolor$enddate_num); 
  #if there is no end date, aassume start and end date are the same
  storecolor$enddate_num[is.na(storecolor$enddate_num)] <- storecolor$startdate_num[is.na(storecolor$enddate_num)]
   
  
  if (output == 'siteyear') {
  #calculate the proportion of honey bees in each site-year, as an average of transect level apis abund
  apis_abund <- dplyr::group_by(storecolor, TransectID, SiteID_Year) %>%
    dplyr::mutate(total_abund= sum(Abundance)) %>%
    dplyr::filter(name == 'Apis mellifera') %>%
    dplyr::summarise(apis_abund = sum(Abundance), total_abund=unique(total_abund)) %>%
    dplyr::group_by(SiteID_Year) %>%
    dplyr::summarize(apis_abund = mean(apis_abund),  total_abund=mean(total_abund),
                     prop_apis = apis_abund/total_abund,
                     has_apis = 'Yes')
  
  } else if (output == 'transect') {
    #calculate the proportion of honey bees in each transect
    apis_abund <- dplyr::group_by(storecolor, TransectID, SiteID_Year) %>%
      dplyr::mutate(total_abund= sum(Abundance)) %>%
      dplyr::filter(name == 'Apis mellifera') %>%
      dplyr::summarise(apis_abund = sum(Abundance), total_abund=unique(total_abund),
                       prop_apis = apis_abund/total_abund,
                       has_apis = 'Yes')
  }
  #regex expression for cleaning up color variable
  comma_outer <- '^[,]+|[,]+[ ]?$'; comma_inner <- '[,][,]+' 
  
  #aggregate to transect level
  #remove honey bee observations
  #translate 'isColor' variables into one 'Color' variable representing all colors of traps in the transect
  transect <- dplyr::filter(storecolor, name != 'Apis mellifera') %>%
    dplyr::select(TransectID,SamplEvent, SiteID, SiteID_Year, 
                  year, month, biweek, starts_with('is'), TrapVolumeFinal, 
                  AbundDayTrap, AbundDay, AbundTrap, Abundance) %>%
    dplyr::group_by(TransectID, SamplEvent, SiteID_Year) %>%
    dplyr::summarize(AbundDayTrap=sum(AbundDayTrap), 
                     AbundDay=sum(AbundDay), 
                     AbundTrap=sum(AbundTrap), 
                     Abundance = sum(Abundance),
                     TrapVolume=paste(sort(unique(TrapVolumeFinal)), collapse=", "),
                     isblue=paste(sort(unique(isblue)), collapse=", "),
                     isyellow=paste(sort(unique(isyellow)), collapse=", "),
                     iswhite=paste(sort(unique(iswhite)), collapse=", "),
                     isFLblue=paste(sort(unique(isFLblue)), collapse=", "),
                     isFLyellow=paste(sort(unique(isFLyellow)), collapse=", "),
                     isFLwhite=paste(sort(unique(isFLwhite)), collapse=", "),
                     ispaleblue=paste(sort(unique(ispaleblue)), collapse=", "),
                     isFLpaleblue=paste(sort(unique(isFLpaleblue)), collapse=", ")) %>%
    tidyr::unite("Color", dplyr::starts_with("is"), remove=F, sep=",") %>%
    dplyr::mutate(Color=gsub(gsub(Color, pattern=comma_outer, replacement=""), pattern=comma_inner, replacement=",")) %>%
    dplyr::select(-starts_with("is")) %>%
    dplyr::mutate(ColorVolume= paste0(Color, TrapVolume),
                  trapdays= Abundance/AbundDay,NTrapsFinal= Abundance/(AbundDayTrap*trapdays),
                  Color = if_else(Color == "", 'UNK', Color))
  
  #reclassify color and volume with very few observations as 'Other' to minimize number of categories
  volumefreq <- data.frame(table(transect$TrapVolume))
  names(volumefreq) <- c('TrapVolume', 'Freq')
  tokeepvol <- as.character(volumefreq$TrapVolume[volumefreq$Freq >= length(transect$TransectID)*0.01])
  
  colorfreq <- data.frame(table(transect$Color))
  names(colorfreq) <- c('Color', 'Freq')
  tokeepcolor <- as.character(colorfreq$Color[colorfreq$Freq >= length(transect$TransectID)*0.01])
  
  #within a year, at the same site, replace UNK sampling method if another transect had sampling info recorded
  unksites <- dplyr::filter(transect, TrapVolume == 'UNK' | Color == 'UNK') %>%
              dplyr::ungroup() %>%
              dplyr::select(SiteID_Year)
  
  transectunk <- dplyr::filter(transect, SiteID_Year %in% unksites$SiteID_Year) %>%
             dplyr::group_by(SiteID_Year) %>%
             dplyr::mutate(TrapVolume = sort(unique(TrapVolume))[1], 
                                Color = sort(unique(Color))[1], 
                                ColorVolume = sort(unique(ColorVolume))[1])
  
  #rejoin fixed transects with original data
  transect <- dplyr::filter(transect, !TransectID %in% transectunk$TransectID) %>%
              dplyr::full_join(transectunk) %>%
              dplyr::mutate(VolumeSimple = if_else(TrapVolume %in% tokeepvol, TrapVolume, 'Other, Mostly Mixed'),
                     ColorSimple = if_else(Color %in% tokeepcolor, Color, 'Other'))

  # ID sites that have multiple sampling methods in the same year
  transect <-  dplyr::group_by(transect, SiteID_Year) %>%
               dplyr::mutate(NSamplingMethods = length(unique(ColorVolume)),
                     NSamplingSimple = length(unique(paste0(ColorSimple, VolumeSimple))))

  #if sites actually DO have multiple sampling methods in the same year (not just missing info), 
  #use the most common one (only affects 2 sites)
  transect <- dplyr::mutate(transect,
                      ColorSimple = dplyr::if_else(NSamplingSimple > 1, names(sort(table(ColorSimple), 
                                                                            decreasing=T)[1]), ColorSimple),
                      VolumeSimple = dplyr::if_else(NSamplingSimple > 1, names(sort(table(VolumeSimple), 
                                                                             decreasing=T)[1]), VolumeSimple)) %>%
              dplyr::select(-NSamplingSimple, -NSamplingMethods) 
  
  
  if (output == 'siteyear') {
    #summarize numeric variables to site-year (MEAN)
    abund <- dplyr::summarise_if(transect, is.numeric, mean, na.rm = TRUE)
    
    #summarize categorical variables to site-year (unique value)
    #join numeric and categorical variables
    siteyear <- dplyr::select(transect, -ColorVolume, -TrapVolume, -Color) %>%
                dplyr::summarise_if(is.character, unique) %>%
                dplyr::full_join(abund)
    data <- siteyear
    
    #KEEP ~100 sites that do not have sampling information
    data$VolumeSimple <-  as.factor(data$VolumeSimple)
    data$ColorSimple <- as.factor(data$ColorSimple)
    
    #join landscape predictors with proportion apis and AbundDayTrap at each site
    results_abund <- dplyr::select(apis_abund, apis_abund, prop_apis, has_apis, SiteID_Year) %>%
      dplyr::right_join(data) %>%
      tidyr::replace_na(list(prop_apis=0, apis_abund=0, has_apis = 'No')) %>%
      dplyr::select(SiteID_Year, everything())

    } else if (output == 'transect') {
    data <- transect
    
    #KEEP ~100 sites that do not have sampling method information, result doesn't change significantly.
    data$VolumeSimple <-  as.factor(data$VolumeSimple)
    data$ColorSimple <- as.factor(data$ColorSimple)
    
    #join landscape predictors with proportion apis and AbundDayTrap at each site
    results_abund <- dplyr::select(apis_abund, apis_abund, prop_apis, has_apis, TransectID, SiteID_Year) %>%
      dplyr::right_join(data) %>%
      tidyr::replace_na(list(prop_apis=0, apis_abund=0, has_apis = 'No')) %>%
      dplyr::select(SiteID_Year, everything())
  }
  
  return(results_abund)
}