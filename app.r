
# FOR MAP DATA IN PART 2
# files already downloaded0
# might have to create a table to reference areas by code
# how to set up map
# https://rstudio.github.io/leaflet/shapes.html

#
# when you sconnect to ssh, make sure to use -p tag for port 2222   cs424! ?? or CS424! ??
# 


library(DT)
library(shiny)
library(ggplot2)
library(rgdal)
library(leaflet)

source("./globals.R", local = FALSE)
source("./plotHelpers.R", local = FALSE)


getGraphTab <- function(id, output_b, output_t) {
  return(
    wellPanel(
      tabsetPanel(
        id=id,
        tabPanel("Bar Graph", id="bar", plotOutput(output_b)),
        tabPanel("Table", id="table", DT::dataTableOutput(output_t)),
      )
    )
  )
}


getBasicControls <- function() {
  wellPanel(
    radioButtons(
      inputId = "time_choice",
      label = "Choose How To Display Time",
      choices = timeChoices
    )
  )
}

getstrtEndControls <- function() {
  wellPanel(
    radioButtons(
      inputId = "start_or_end",
      label = "Show Rides Starting or Ending in Selected Area",
      choices = c("start", "end")
    )
  )
}

createMap = function() {
  leaflet(chicago) %>% addPolygons(color = "#444444", 
                                   weight = 1, 
                                   smoothFactor = 0.5,
                                   opacity = 1.0, 
                                   fillOpacity = 0.5,
                                   layerId = chicago$area_num_1,
                                   highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                       bringToFront = TRUE))
}

updateMap <- function(newData) {
  if (!is.null(newData) & length(newData[,1]) > 0) {
    print("community area data updated, updating map")
    
    # TODO don't proceed if clicked area has 0 rides - pretty much impossible
    
     
    # fill in missing areas wiht 0 values and sort to match chicago areas on map    
    D <- fillMissingCommAreas(newData)
    D <- D[order(strtoi(D$Community_Area)),]
    D <- D[strtoi(chicago$area_num_1),]
    
    # get percentages and mappings 
    percentages <- as.double(D$Percentage_Rides)
    colorMapping <- 100 / ((percentages+0.1)^0.5)
    

    # create palette
    pal <- colorNumeric(
      palette = heat.colors(5, alpha = 1),
      domain = c(colorMapping))
    
    # update map
    leafletProxy("chicagoMap", data = chicago) %>% clearShapes() %>% addPolygons(
      color = pal(colorMapping), 
      # labels for hover and click
      label = paste(chicago$community, "  ", percentages, "%"),
      popup = paste(chicago$community, "  ", percentages, "%"),
      weight = 1, 
      smoothFactor = 0.5,
      opacity = 1.0, 
      fillOpacity = 0.5,
      layerId = chicago$area_num_1,
      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
  }
}


prioritizeSelection <- function() {
  
}


################################
#            UI                #
################################

ui <- fluidPage(
  column(12, 
         
     uiOutput("mapTitle"),
     getBasicControls(),
     getGraphTab("Hour", "ridesByHour", "ridesByHour_t"),
     getGraphTab("Day", "ridesByDay", "ridesByDay_t"),
     getGraphTab("Month", "ridesByMonth", "ridesByMonth_t"),
     getGraphTab("Weekday", "ridesByWeekday", "ridesByWeekday_t"),
     getGraphTab("Mileage", "ridesByMileage", "ridesByMileage_t"),
     getGraphTab("Duration", "ridesByTripTime", "ridesByTripTime_t"),
     leafletOutput("chicagoMap"),
     getstrtEndControls(),
     getGraphTab("percentByCommunity", "ridesByCommunityArea", "ridesByCommunityArea_t"),
  )
)


################################
#         SERVER               #
################################

server <- function(input, output) {
  selectedCommArea <- reactiveVal()   # user-selected community area for viewing to-from data
  
  curDF_comm_area <- reactive({
    parseByCommunityArea(DF, selectedCommArea(), input$start_or_end)
  })
  
  # wait for selected comm area data to update
  # update map
  observe({
      updateMap(curDF_comm_area())
  })
  
  
  # Hour
  output$ridesByHour <- renderPlot({
    getBasicBarPlot(parseByHour(DF, input$time_choice))
  })
  output$ridesByHour_t <- DT::renderDataTable(parseByHour(DF, input$time_choice))
  
  # Day
  output$ridesByDay <- renderPlot({
    getBarPlot_angledX(parseByDay(DF))
  })
  output$ridesByDay_t <- DT::renderDataTable(parseByDay(DF))
  
  
  # Month
  output$ridesByMonth <- renderPlot({
    getBasicBarPlot(parseByMonth(DF))
  })
  output$ridesByMonth_t <- DT::renderDataTable(parseByMonth(DF))
  
  
  # Weekday
  output$ridesByWeekday <- renderPlot({
    getBasicBarPlot(parseByWeekday(DF))
  })
  output$ridesByWeekday_t <- DT::renderDataTable(parseByWeekday(DF))
  
  
  # Mileage
  output$ridesByMileage <- renderPlot({
    getBasicBarPlot((binByMileage(DF)))
  })
  output$ridesByMileage_t <- DT::renderDataTable(binByMileage(DF))
  
  
  # Duration
  output$ridesByTripTime <- renderPlot({
    getBasicBarPlot(binByDuration(DF))
  })
  output$ridesByTripTime_t <- DT::renderDataTable(binByDuration(DF))
  
  # Map
  output$chicagoMap <- renderLeaflet({ 
    createMap()
  })
  
  # Map Title
  output$mapTitle <- renderUI(h4(paste("Rides in: ", selectedCommArea())))
  

  observe({
    proxy <- leafletProxy("chicagoMap")
  })
  
  
  # community heat map on-click 
  observeEvent(input$chicagoMap_shape_click, { 
    comm_id <- input$chicagoMap_shape_click$id
    selectedCommArea(comm_id)
    
    print(paste("selected area:", comm_id))
  })
  
  
  # community area to-from
  output$ridesByCommunityArea <-  renderPlot({
    # TODO don't display graph if not selected
    if (!is.null(selectedCommArea())) {
      getRidesBarPlot(curDF_comm_area())
    }
  })
  output$ridesByCommunityArea_t <- DT::renderDataTable({
    if (!is.null(selectedCommArea())) {
      curDF_comm_area()
    }
  })
  
  
  
  
  
}


################################
#            INIT              #
################################

shinyApp(ui, server)