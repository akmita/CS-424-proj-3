

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


#############################################################
#            DATA AGGREGATION - move to separate file


parseByHour = function(D) {
  D <- aggregate(x = rep(1, nrow(D)), by = list(format(D$start, "%H")), FUN = sum)
  names(D) <- c("Hour", "Rides")
  
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

  D <- aggregate(x = rep(1, nrow(DF)), by = list(createDurationBins(DF)), sum)
  
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
    
  }, X = DF, MARGIN = 1))
}



#
#  group trips by trip time
#
binByMileage = function(D) {
  
  D <- aggregate(x = rep(1, nrow(DF)), by = list(createMileageBins(DF)), sum)
  
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
    
  }, X = DF, MARGIN = 1)
  
  
  return(temp)
}



