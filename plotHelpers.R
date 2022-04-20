

getBasicBarPlot <- function(D) {
  
  # update with indexing method
  (ggplot(data=D, aes_string(x=names(D)[1], y=names(D)[2]))
   + geom_bar(stat="identity")
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
# parse dataset to show rides per year
#
parseByYear = function(D) {
  D <- aggregate(x = rep(1, nrow(D)), by = list(format(D$start, "%Y")), FUN = sum)
  names(D) <- c("Year", "Rides")
  
  return(D);
}


#
# parse dataset to show rides per day, for specific year
#
parseByDay = function(D) {
  
  
  return(D)
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
  
  return(D)
}
