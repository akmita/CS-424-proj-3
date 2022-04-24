

getBasicBarPlot <- function(D) {
  (ggplot(data=D, aes_string(x=names(D)[1], y=names(D)[2]))
   + geom_bar(stat="identity")
  )
}


getBarPlot_angledX <- function(D) {
  (ggplot(data=D, aes_string(x=names(D)[1], y=names(D)[2]))
   + geom_bar(stat="identity")
   + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # fixes overlapping names
  )
}
 


getRidesBarPlot <- function(D) {
  (ggplot(data=D, aes(x=Community_Area_Name, y=Percentage_Rides))
   + geom_bar(stat="identity")
   + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # fixes overlapping names
  )
}


############################################################
#                   HELPERS

sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))

fillMissingCommAreas <- function(D) {
  # get missing IDs
  missingIDs <- sym_diff(communityAreas$id, D$Community_Area)
  
  # new dataframe with names
  zeroVals <- data.frame(matrix(ncol = 3, nrow = 0))
  names(zeroVals) <- names(D)
  
  # fill new dataframe with missing values
  for (i in 1:length(missingIDs)) {
    zeroVals[nrow(zeroVals) + 1,] = c(missingIDs[i], 0.0, communityAreas[strtoi(missingIDs[i]),2])
  }
  
  # append to old dataframe
  D <- rbind(D, zeroVals)
  
  return(D)  
}


#############################################################
#            DATA AGGREGATION - move to separate file


parseByHour = function(D, timeChc) {
  # get hour format
  hourFormat <- if(timeChc == timeChoices[1]) "%H" else "%I %p"
  
  # D <- aggregate(x = rep(1, nrow(D)), by = list(str_replace_all(format(D$start, hourFormat), "0[:digit:]", "0[:digit:]") ), FUN = sum)
  D <- aggregate(x = rep(1, nrow(D)), by = list(format(D$start, hourFormat)), FUN = sum)
  names(D) <- c("Hour", "Rides")
  
  # reorder and prevent auto sorting
  if(timeChc == timeChoices[2])  {
    D <- D[c(23,1,3,5,7,9,11,13,15,17,19,21,24,2,4,6,8,10,12,14,16,18,20,22),]
    D$Hour <- factor(D$Hour, levels = D$Hour)
  }
  
  return(D)
}


#
# parse dataset to show rides per day
#
parseByDay = function(D) {
  D <- aggregate(x = rep(1, nrow(D)), by = list(format(D$start, "%Y-%M-%D")), FUN = sum)
  names(D) <- c("Day", "Rides")
  
  return(D);
}



#
# parse dataset to show rides per month, specific year
#
parseByMonth = function(D) {
  D <- aggregate(x = rep(1, nrow(D)), by = list(format(D$start, "%m")), FUN = sum)
  names(D) <- c("Month", "Rides")
  
  
  return(D)
}


# 
#  parse dataset to show rides per day of week, given year
#
parseByWeekday = function(D) {
  D$dayOfWeek <-    # get weekdays 
  D <- aggregate(x = rep(1, nrow(D)), by = list(weekdays(as.Date(D$start))), sum)         # group by weekday
  names(D) <- c("Day", "Rides")
  # reorder
  D <- D[c(2,6,7,5,1,3,4),]
  # prevent auto-sorting
  D$Day <- factor(D$Day, levels = D$Day)
  
  return(D)
}


#
#  group trips by mileage
#
binByDuration= function(D) {

  D <- aggregate(x = rep(1, nrow(D)), by = list(createDurationBins(D)), sum)
  
  # rename
  names(D) <- c("Duration", "Rides")
  # reorder cols
  D <- D[c(2,10,3,4,6,7,8,9,1,5),]
  # prevent auto-sorting
  D$Duration <- factor(D$Duration, levels = D$Duration)
  
  return(D)
}


#
# creates bins for ride duration increments
#
createDurationBins = function(D) {
  
  return(apply(FUN = function(row) {
    dur <- row["duration"]

    if (dur < 60 * 5) { return("1 - 5 mins") }
    else if (dur > 60 * 5  & dur <= 60 * 10) { return("5 - 10 mins") }
    else if (dur > 60 * 10 & dur <= 60 * 15) { return("10 - 15 mins") }
    else if (dur > 60 * 15 & dur <= 60 * 20) { return("15 - 20 mins") }
    else if (dur > 60 * 20 & dur <= 60 * 25) { return("20 - 25 mins") }
    else if (dur > 60 * 25 & dur <= 60 * 30) { return("25 - 30 mins") }
    else if (dur > 60 * 30 & dur <= 60 * 45) { return("30 - 45 mins") }
    else if (dur > 60 * 45 & dur <= 60 * 60) { return("45 mins - 1 hr") }
    else if (dur > 60 * 60 & dur <= 60 * 60 * 2) { return("1 - 2 hrs") }
    else { return("2 - 5 hrs") }
    
  }, X = D, MARGIN = 1))
}



#
#  group trips by trip time
#
binByMileage = function(D) {
  
  D <- aggregate(x = rep(1, nrow(D)), by = list(createMileageBins(D)), sum)
  
  # rename
  names(D) <- c("Mileage", "Rides")
  # reorder cols
  D <- D[c(1:7,9,10,11,12,8),]
  # prevent auto-sorting
  D$Mileage <- factor(D$Mileage, levels = D$Mileage)
  
  return(D)
}


#
# creates bins for ride duration increments
#
createMileageBins = function(D) {
  
  temp <- apply(FUN = function(row) {
    dist <- row["miles"]
    
    if (dist > 0.5 & dist < 0.6) { return("0.5 - 0.6 mi") }
    else if (dist <= 0.6)   { return("0.5 - 0.6 mi") }
    else if (dist > 0.6 & dist <= 0.7)   { return("0.6 - 0.7 mi") }
    else if (dist > 0.7 & dist <= 0.8)   { return("0.7 - 0.8 mi") }
    else if (dist > 0.8 & dist <= 0.9)   { return("0.8 - 0.9 mi") }
    else if (dist > 0.9 & dist <= 1.0)   { return("0.9 - 1.0 mi") }
    else if (dist > 1.0 & dist <= 1.5)   { return("1 - 1.5 mi") }
    else if (dist > 1.5 & dist <= 2.0)   { return("1.5 - 2 mi") }
    else if (dist > 2.0 & dist <= 3.0)   { return("2 - 3 mi") }
    else if (dist > 3.0 & dist <= 4.0)   { return("3 - 4 mi") }
    else if (dist > 4.0 & dist <= 5.0)   { return("4 - 5 mi") }
    else if (dist > 5.0 & dist <= 10)   { return("5 - 10 mi") }
    else if (dist > 10 & dist <= 50) { return("10 - 50 mi") }
    else {
      return("50-100 mi")
    }
    
  }, X = D, MARGIN = 1)
  
  
  return(temp)
}


#
# returns data for viewing which community areas end or start a trip
#
parseByCommunityArea = function(D, selectedCommArea, start_or_end) {
  
  isStartingArea <- start_or_end == "start"
  
  # subset for start or ending in community area
  subsetCol <- if (isStartingArea) D$pickupArea else D$dropOffArea
  D <- subset(D, subsetCol == selectedCommArea)
  
  numTotal <- length(D[,1])
  
  if (numTotal == 0) {
    return(NULL)
  }
  
  # aggregate trips starting or ending in community area
  aggCol <- if (isStartingArea) D$dropOffArea else D$pickupArea 
  D <- aggregate(x = rep(1, nrow(D)), by = list(strtoi(aggCol)), sum)
  
  # rename cols
  names(D) <- c("Community_Area", "Percentage_Rides")
  
  # bind ids 
  D$Community_Area_Name <- communityAreas[strtoi(D$Community_Area),2]
  
  # # do percentage
  D$Percentage_Rides <- round(100 * D$Percentage_Rides / numTotal, 2)
  # 
  return(D)
}


getCommAreaSubset <- function(D, start_or_end, selectedCommArea) {
  isStartingArea <- start_or_end == "start"
  
  # subset for start or ending in community area
  subsetCol <- if (isStartingArea) D$pickupArea else D$dropOffArea
  D <- subset(D, subsetCol == selectedCommArea)
  
  return(D)
}



