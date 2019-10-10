#function to extract colors from 'field note' and 'note' variables of Droege database

extractcolor <- function(DroegeDF) {
  
test <- DroegeDF

#clean up 'field_note' and 'note' variables, convert to all lower case
test$field_note <- as.character(test$field_note)
test$field_note <- gsub(test$field_note, pattern='flourescent', replacement='fluorescent')


#make empty output dataframe
storecolor <- data.frame(field_note=test$field_note, identifier=test$identifier, note=test$note)

#save regular expression for white (and each corresponding color)
toMatchwhite <- "([0-9]{1,2})+(?:w|wt|wh|wht)[ ,;.]|([0-9]{1,2})[ ,;.]+(?:w|wt|wh|wht)[ ,;.]|([0-9]{1,2})+[ ,;.]+white|([0-9]{1,2})+white|(?:w|wt)[ ]([0-9]{1,2})[,;]"

library(stringr)
#white
matching <- str_match(test$field_note, toMatchwhite)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))
matching$X6 <- as.numeric(as.character(matching$X6))

matching2 <- str_match(test$note, toMatchwhite)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))
matching2$X6 <- as.numeric(as.character(matching2$X6))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0
storecolor$nwhite <-  sum1 + sum2 

#blue
toMatchblue <- "([0-9]{1,2})+(?:b|bl|bly)[ ,;.)]|([0-9]{1,2})[ ,;.]+(?:b|bl|bly)[ ,;.]|([0-9]{1,2})[ ,;.]+blue|([0-9]{1,2})+blue"

matching <- str_match(test$field_note, toMatchblue)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))

matching2 <- str_match(test$note, toMatchblue)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0

storecolor$nblue <- sum1 + sum2 

#yellow
toMatchyellow <- "([0-9]{1,2})+(?:y|yl|ylw|yllw)[ ,;.]|([0-9]{1,2})[ ,;.]+(?:y|yl|ylw|yllw)[ ,;.]|([0-9]{1,2})[ ,;.]+yellow|([0-9]{1,2})+yellow"

matching <- str_match(test$field_note, toMatchyellow)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))

matching2 <- str_match(test$note, toMatchyellow)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0

storecolor$nyellow <- sum1 + sum2 

#fl blue
toMatchFLblue <- "([0-9]{1,2})+(?:fb|fl bl|flbl|flb)|([0-9]{1,2})[ ,;.]+(?:fb|fl bl|flbl|flb)+[ ,;.]|([0-9]{1,2})[ ,;.]+(?:fl blue|fluorescent blue|uv-blue|fl. blue|uv blue)|([0-9]{1,2})+(?:fl blue|fluorescent blue|uv-blue|fl. blue|uv blue)|(?:fb|fl bl|flbl|flb)[ ]([0-9]{1,2})[,;]"

matching <- str_match(test$field_note, toMatchFLblue)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))
matching$X6 <- as.numeric(as.character(matching$X6))

matching2 <- str_match(test$note, toMatchFLblue)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))
matching2$X6 <- as.numeric(as.character(matching2$X6))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0

storecolor$nFLblue <- sum1 + sum2 

#fl yellow
toMatchFLyellow <- "([0-9]{1,2})+(?:fy|fl yl|flyl|fly)|([0-9]{1,2})[ ,.]+(?:fy|fl yl|flyl|fly)+[ ,;.]|([0-9]{1,2})[ ,.]+(?:fl yellow|fluorescent yellow|uv-yellow|fl. yellow|uv yellow)|([0-9]{1,2})+(?:fl yellow|fluorescent yellow|uv-yellow| fl. yellow|uv yellow)|(?:fy|fl yl|flyl|fly)[ ]([0-9]{1,2})[,;]"

matching <- str_match(test$field_note, toMatchFLyellow)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))
matching$X6 <- as.numeric(as.character(matching$X6))

matching2 <- str_match(test$note, toMatchFLyellow)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))
matching2$X6 <- as.numeric(as.character(matching2$X6))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0

storecolor$nFLyellow <- sum1 + sum2 

#fl white
toMatchFLwhite <- "([0-9]{1,2})+(?:fw|fl wh|flwh|flw)|([0-9]{1,2})[ ,;.]+(?:fw|fl wh|flwh|flw)+[ ,;.]|([0-9]{1,2})[ ,;.]+(?:fl white|fluorescent white|uv-white|fl. white|uv white)|([0-9]{1,2})+(?:fl white|fluorescent white|uv-white| fl. white|uv white)|(?:fw|fl wh|flwh|flw)[ ]([0-9]{1,2})[,;]"

matching <- str_match(test$field_note, toMatchFLwhite)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))
matching$X6 <- as.numeric(as.character(matching$X6))

matching2 <- str_match(test$note, toMatchFLwhite)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))
matching2$X6 <- as.numeric(as.character(matching2$X6))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0

storecolor$nFLwhite <- sum1 + sum2 

#missing, seems to mess up regex to spread it onto multiple lines...
toMatchmissing <- "([0-9]{1,2})[ :]+(?:missing|empty|lost|gone|spilled|disturbed|tipped|dry|crushed|partially empty|overturned)[ ,;.]|(?:missing|lost)+[ ,.:]([0-9]{1,2})|([0-9]{1,2})[ ]+(?:BOWL|BOWLs|cup|cups|traps)[ ]+(?:missing|empty|lost|gone|spilled|disturbed|tipped|dry|crushed|partially empty|overturned)|([0-9]{1,2})+(?:w|wh|white|y|yl|yellow|b|bl|blue|fb|fy)[ ;]+(?:missing|empty|lost|gone|spilled|disturbed|tipped|dry|crushed|partially empty)[ ,;./]"

matching <- str_match(test$field_note, toMatchmissing)
matching <- data.frame(matching)
matching$X2 <- as.numeric(as.character(matching$X2)); matching$X3 <- as.numeric(as.character(matching$X3))
matching$X4 <- as.numeric(as.character(matching$X4)); matching$X5 <- as.numeric(as.character(matching$X5))

matching2 <- str_match(test$note, toMatchmissing)
matching2 <- data.frame(matching2)
matching2$X2 <- as.numeric(as.character(matching2$X2)); matching2$X3 <- as.numeric(as.character(matching2$X3))
matching2$X4 <- as.numeric(as.character(matching2$X4)); matching2$X5 <- as.numeric(as.character(matching2$X5))

sum1 <- rowSums(matching[,2:length(matching)], na.rm=T); sum1[sum1 > 40] <- 0
sum2 <- rowSums(matching2[,2:length(matching2)], na.rm=T); sum2[sum2 > 40] <- 0

storecolor$nmissing<- sum1 + sum2 

return(storecolor)
}
