#
# SET UP DATA FILE AND EXCLUDE UNWANTED DATA
#
library(dplyr)
library(hash)

source("./companies.r", local = FALSE) # for hashing company names into abbreviations


colsToKeep <- c(3, 5, 6, 9, 10, 17)
easyColNames <- c("start", "duration", "miles", "pickupArea", "dropOffArea", "Company")




######################################################################
#                       HELPER METHODS                               #
######################################################################

#
# remove all trips that do not fall under constraints
#
filterTrips <- function(DF) {
  # remove less than 0.5 miles & more than 100 miles
  DF <- subset(DF, miles >= .5 & miles <= 100.00)
  
  
  # remove less than 60 seconds & greater than 5 hours
  DF <- subset(DF, duration >= 60 & duration <= 60 * 60 * 5)  # 1-4 312969 rows
  
  # all trips that either start or end outside of a Chicago community area   
  DF <- subset(DF, !is.na(pickupArea) & !is.na(dropOffArea))  # 1-4 28676 rows
} 


#
# give easy names to columns
#
renameCols <- function(DF) {
  names(DF) <- easyColNames
  return(DF) 
}


#
# fix trips granularity
#
fixGranularity <- function (DF) {
  
  # TODO
  
  return(DF)
}


#
# encode companies - each company gets a code
#
encodeCompanies <- function(DF) {
  DF$Company <- apply(FUN = function(row) compHash[[row["Company"]]], X = DF, MARGIN = 1)
  return(DF)
}

#
# perform all data parsing in one function
#
parseTripData <- function(DF) {
  DF <- DF[colsToKeep]
  DF <- renameCols(DF)
  DF <- filterTrips(DF)
  DF <- fixGranularity(DF)
  DF <- encodeCompanies(DF)
  
  return(DF)
}


######################################################################
#                       "MAIN" METHOD                                #
######################################################################


getData <- function() {
  # assign file names for 165 files
  filenames <- list()
  for (i in 1:165) {
    filenames[i] <- paste("./datasets/Taxi_Trips_-_2019_", i, ".tsv", sep='')
  }
  
  # get first dataframe with header 
  DF <- read.table(filenames[[1]], sep = ",", header = TRUE)
  
  DF <- parseTripData(DF)
  
  
  # append dataframes without header to original dataframe
  for (i in 2:5) {
    # read next csv 
    temp <- read.table(file = paste0(filenames[[i]]), sep = ",", header = FALSE)
    
    temp <- parseTripData(temp)
    
    # append to main dataframe
    DF <- rbind(DF, temp)  
  }
  
  return(DF)
}



DF <- getData()

