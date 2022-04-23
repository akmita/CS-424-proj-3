#  ./chicagoMapData/geo_export_bd87ad81-9ee0-4a43-a802-c68e88a02bba.shp

library(rgdal)
library(leaflet)
library(geojsonR)
library(geojsonio)



#
#
#    POPULATE ENVIRONMENT 
#
#

# DF <- getData()

chicago <- readOGR("./chicagoMapData/geo_export_bd87ad81-9ee0-4a43-a802-c68e88a02bba.shp",
                   GDAL1_integer64_policy = TRUE)

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

