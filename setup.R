# SET UP DATA FILE AND EXCLUDE UNWANTED DATA

colsToKeep <- c(3, 5, 6, 9, 10, 17)

# assign file names for 165 files
filenames <- list()
for (i in 1:165) {
  filenames[i] <- paste("./datasets/Taxi_Trips_-_2019_", i, ".tsv", sep='')
}

# get first dataframe with header 
DF <- read.table(filenames[[1]], sep = ",", header = TRUE)

# get names of ALL columns
names <- names(DF)

# remove unnecessary cols
DF <- DF[colsToKeep]



# append dataframes without header to original dataframe
for (i in 2:90) {
  # read next csv 
  temp <- read.table(file = paste0(filenames[[i]]), sep = ",", header = FALSE)
  
  # set names of columns, since there is no header
  colnames(temp) <- names
  
  # remove unnecessary cols
  temp <- temp[colsToKeep]

  # append to main dataframe
  DF <- rbind(DF, temp)  
}




# verify dataframes have the right columns
print(names(DF))




# testing area



