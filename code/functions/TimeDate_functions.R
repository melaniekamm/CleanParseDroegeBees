
zonelookup <- function(date, latitude, longitude){
library(XML)
  for (i in 1:length(date)) {
    tz <- 'placeholder'
    if (is.na(date[i])) {
      tz <- 'NA_ROW' } else {
        time1 <- as.POSIXct(date[i])
        lat <- latitude[i]; long <- longitude[i]
  
        # https://developers.google.com/maps/documentation/timezone/
        tryCatch(apiurl <- sprintf("https://maps.googleapis.com/maps/api/timezone/%s?location=%s,%s&timestamp=%d&sensor=%s", 
                             "xml", lat, long, as.numeric(time1), "false"), error = function(c) {
                               tz <- 'NA_ROW'
        })
      }
    if (tz == 'placeholder') {tz <- xmlParse(readLines(apiurl))[["string(//time_zone_id)"]]}
    if (i == 1) { zones <- tz} else {zones <- c(zones, tz)}
  }
  return(zones)
}


#function to extract time from Droege raw date/time format

extract_time <- function(rawtime, latitude, longitude){
  #extract date from rawtime
  date <- strptime(substr(rawtime, start=1, stop=8), format='%Y%m%d')
                
  #save proper time zone for lat/long
  zone <- zonelookup(date=date, latitude=latitude, longitude=longitude)  
  
  time <- gsub(gsub(substr(rawtime, start=1, stop=14), pattern='xxxxxx', replacement='notime'), pattern='xx', replacement='00')
  time <- format(as.POSIXct(time, format='%Y%m%d%H%M%S', tz=zone), format='%H:%M %z')
  return(time)
}

get_timeDF <- function(ID, df, timecol) {
  rawr <- as.numeric(which(names(df) %in% timecol))
  selectedrow <- as.numeric(which(df$identifier == ID))
  rawtime <- as.character(df[selectedrow, rawr , with=F])
  latitude <- df$latitude[df$identifier == ID][1]
  longitude <- df$longitude[df$identifier == ID][1]
  
  timefinal <- extract_time(rawtime=rawtime, latitude=latitude, longitude=longitude)
  return(timefinal)  
}


extract_date <- function(rawtime, date, latitude, longitude){
  #extract date from rawtime
  date <- strptime(substr(rawtime, start=1, stop=8), format='%Y%m%d')
  #save proper time zone for lat/long
  zone <- zonelookup(date=date, latitude=latitude, longitude=longitude)  
  
  dateout <- substr(rawtime, start=1, stop=8)
  dateout <- format(as.POSIXct(dateout, format='%Y%m%d', tz=zone), format='%Y-%m-%d %z')
  return(dateout)
}



get_dateDF <- function(ID, df, timecol) {
  rawr <- as.numeric(which(names(df) %in% timecol))
  selectedrow <- as.numeric(which(df$identifier == ID))
  rawtime <- as.character(df[selectedrow, rawr , with=F])
  latitude <- df$latitude[df$identifier == ID][1]
  longitude <- df$longitude[df$identifier == ID][1]
  
  datefinal <- extract_date(rawtime=rawtime, latitude=latitude, longitude=longitude)
  return(datefinal)  
}
