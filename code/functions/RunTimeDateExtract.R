
format_timedates <- function(data, outputfolder, dotime) {
  
library(data.table)
source('./functions/TimeDate_functions.R')

#reformat time and date and assign correct time zone to each observation
#These functions assume that times were assigned according to local time zones!

#These functions take a long (about 20 hrs each) to run for such a large dataframe, proceed with caution
b <- nrow(data)

if (dotime== T){
#start time
for (i in 1:b) {
ah <- data.frame(data$identifier[1], get_timeDF(timecol='time1', df=data, ID=data$identifier[1]))
names(ah) <- c('identifier', 'starttime')
  if (i ==1) {starttime <- ah} else {starttime <- rbind(ah, starttime)}
}
write.csv(starttime, paste(outputfolder, '/starttime.csv', sep=""))



#end time
for (i in 1:b) {
  ah <- data.frame(data$identifier[i], get_timeDF(timecol='time2', df=data, ID=data$identifier[i]))
  names(ah) <- c('identifier', 'endtime')
  if (i ==1) {endtime <- ah} else {endtime <- rbind(ah, endtime)}
}
write.csv(endtime, paste(outputfolder, '/endtime.csv', sep=""))
}

startdate <- data.frame(identifier=NA, startdate=NA)
#start date
for (i in 1:b) {
  ah <- data.frame(data$identifier[i], get_dateDF(timecol='time1', df=data, ID=data$identifier[i]))
  names(ah) <- c('identifier', 'startdate')
  if (i ==1) {startdate <- ah} else {startdate <- rbind(ah, startdate)}
}

startdate$startdate_num <- as.Date(startdate$startdate, format='%Y-%m-%d %z')

write.csv(startdate, paste(outputfolder, '/startdate.csv', sep=""))


#end date
for (i in 1:b) {
  ah <- data.frame(data$identifier[i], get_dateDF(timecol='time2', df=data, ID=data$identifier[i]))
  names(ah) <- c('identifier', 'enddate')
  if (i ==1) {enddate <- ah} else {enddate <- rbind(ah, enddate)}
}

enddate$enddate_num <- as.Date(enddate$enddate, format='%Y-%m-%d %z')
write.csv(enddate, paste(outputfolder, '/enddate.csv', sep=""))

}

