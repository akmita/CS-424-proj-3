#  ./chicagoMapData/geo_export_bd87ad81-9ee0-4a43-a802-c68e88a02bba.shp

library(rgdal)
library(leaflet)
library(geojsonR)
library(geojsonio)
require(dplyr)



#
#
#    POPULATE ENVIRONMENT 
#
#

# DF <- getData()

chicago <- getChicagoData()

# companies <- 

communityAreas <- getCommunityAreas()





#
# 1st attempt - clicking doesn't work - leaflet shiny tutorial
#
states <- readOGR("./chicagoMapData/geo_export_bd87ad81-9ee0-4a43-a802-c68e88a02bba.shp",
                  GDAL1_integer64_policy = TRUE)


leaflet(states) %>% addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                          opacity = 1.0, fillOpacity = 0.5,
                          highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))


#
# 2nd attempt - https://rstudio.github.io/leaflet/choropleths.html
# 
geojson <- geojsonio::geojson_read("./chicagoMapData/boundaries.geojson", what = "sp")






no <- list(DF$dropOffArea)

temp <- aggregate(x = rep(1, nrow(DF)), by = list(communityAreas[strtoi(DF$dropOffArea),2]), sum)

# I want this, but it's not vectorized
yes <- commHash[[DF$dropOffArea]]


yes <- apply(FUN = function(row) print(unlist(row["dropOffArea"])) , X = DF, MARGIN = 1)


typeof(yes)




percentages <- as.double(temp$Percentage_Rides)
colorMapping <- 100 / (percentages^0.5)

pal <- colorNumeric(
  palette = heat.colors(5, alpha = 1), # this works
  # palette = terrain.colors(10, alpha = 1),
  domain = c(colorMapping))


leaflet(chicago) %>% addPolygons(
                                  color = pal(colorMapping), # this works
                                  label = paste(chicago$community, percentages, "%"),
                                  popup = paste(chicago$community, percentages, "%"),
                                 weight = 1, 
                                 smoothFactor = 0.5,
                                 opacity = 1.0, 
                                 fillOpacity = 0.5,
                                 layerId = chicago$area_num_1,
                                 highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                     bringToFront = TRUE))


#
# returns data for viewing which community areas end or start a trip
#
parseByCommunityArea3 = function(D, selectedCommArea, start_or_end) {
  
  
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
  
  # do percentage
  D$Percentage_Rides <- format(round(100 * D$Percentage_Rides / numTotal, 2), 2)
  
  return(D)
}

# PROBLEMATIC IDs      76     29

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

