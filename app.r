
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
      label = "Show Rides Starting or Ending in Community Area",
      choices = c("start", "end")
    )
  )
}


################################
#            UI                #
################################

ui <- fluidPage(
  column(12, 
     # dataTableOutput("allRides"),
     
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
  
  
  # output$allRides = DT::renderDataTable(DF)
  
  
  
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
    leaflet(chicago) %>% addPolygons(color = "#444444", 
                                      weight = 1, 
                                      smoothFactor = 0.5,
                                      opacity = 1.0, 
                                      fillOpacity = 0.5,
                                      layerId = chicago$area_num_1,
                                      highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                        bringToFront = TRUE))
  })
  
  
  #
  # community heat map on-click 
  #
  observeEvent(input$chicagoMap_shape_click, { 
    comm_id <- input$chicagoMap_shape_click$id
    selectedCommArea(comm_id)
    
    print(paste("selected area:", comm_id))
  })
  
  
  #
  # community area to-from
  #
  output$ridesByCommunityArea <-  renderPlot({
    getBarPlot_angledX(parseByCommunityArea(DF, selectedCommArea(), input$start_or_end))
  })
  output$ridesByCommunityArea_t <- DT::renderDataTable({
    if (!is.null(selectedCommArea())) {
      parseByCommunityArea(DF, selectedCommArea(), input$start_or_end)
    }
  })
  
  
}


################################
#            INIT              #
################################

shinyApp(ui, server)