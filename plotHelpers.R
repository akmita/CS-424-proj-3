

getBasicBarPlot <- function(D) {
  
  # update with indexing method
  (ggplot(data=D, aes(x=Hour, y=Rides))
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
parseByYear = function(D, location) {
  D <- aggregate(rides~year,D,sum)        # group by year
  return(D);
}


#
# parse dataset to show rides per day, for specific year
#
parseByDay = function(D, yearSelected, location) {
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected) # filter by year
  
  return(D)
}


#
# parse dataset to show rides per month, specific year
#
parseByMonth = function(D, yearSelected, location) {
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected) # get subset only selected year
  D <- aggregate(rides~month,D,sum) # aggregate per month
  return(D)
}


#
#  parse dataset to show rides per day of week, given year
#
parseByWeekday = function(D, yearSelected, location) {
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected)  # get subset only selected year
  D$dayOfWeek <- weekdays(as.Date(D$newDate))   # get weekdays 
  D <- aggregate(rides~dayOfWeek,D,sum)         # group by weekday
  # D <- D[c(4,5,6,7,1,2,3),] # try to rearrange days in better order
  return(D)
}
