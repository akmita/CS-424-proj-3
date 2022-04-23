#
# SET UP DATA FILE AND EXCLUDE UNWANTED DATA
#
library(dplyr)
library(hash)
library(lubridate)
library(stringr)

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
# format to nice date format
#
formatDate <- function(DF) {
  DF$start <- strptime(DF$start, format='%m/%d/%Y %I:%M:%S %p') 

  return(DF)
}


#
# fix trips granularity - 12:15, 12:30, 12:45 become 12:00 
#
fixGranularity <- function (DF) {             
  DF$start <- as.POSIXct(paste0(format(DF$start,"%Y-%m-%d %H:"), "00:00 CDT"))

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
  
  DF <- sample_n(DF, 100)                           #######################################################################
  
  DF <- filterTrips(DF)
  DF <- encodeCompanies(DF)
  DF <- formatDate(DF)
  DF <- fixGranularity(DF)
  
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
  # ---- MUST START WITH 2 ----
  for (i in 2:165) {
    # read next csv 
    temp <- read.table(file = paste0(filenames[[i]]), sep = ",", header = FALSE)
    
    temp <- parseTripData(temp)
    
    print(paste("parsing file: ", i))
    
    # append to main dataframe
    DF <- rbind(DF, temp)  
  }
  
  return(DF)
}



getCommunityAreas <- function() {
  D <- data.frame(strtoi(chicago$area_num_1), chicago$community)
  names(D) <- c("id", "name")
  
  D <- D[order(D$id),]
  
  return(D)
}


getChicagoData <- function() {
  return(readOGR("./chicagoMapData/geo_export_bd87ad81-9ee0-4a43-a802-c68e88a02bba.shp",
          GDAL1_integer64_policy = TRUE))
}



# chicago <- readOGR("./chicagoMapData/geo_export_bd87ad81-9ee0-4a43-a802-c68e88a02bba.shp",
#                    GDAL1_integer64_policy = TRUE)
# 
# 
# commHash <- hash(chicago$area_num_1, chicago$community)
# 
# commUnhash <- hash(chicago$community, chicago$area_num_1)


