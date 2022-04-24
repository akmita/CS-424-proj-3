


library(DT)
library(shiny)
library(ggplot2)
library(rgdal)
library(leaflet)

source("./globals.R", local = FALSE)
source("./plotHelpers.R", local = FALSE)

getBasicControls <- function() {
    radioButtons(
      inputId = "time_choice",
      label = "Choose How To Display Time",
      choices = timeChoices
    )
}

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

getMapTab <- function() {
  wellPanel(
    tabsetPanel(
      id="mapTabPanel",
      tabPanel("Map of Community areas", id="map", leafletOutput("chicagoMap")),
      tabPanel("Table", id="table", DT::dataTableOutput("commList")),
    ),
    getstrtEndControls()
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


getCurSubset <- function(D_orig, D_subset, select_comm_area) {
  
  print("searching for subset in data")
  
  if (!is.null(select_comm_area)) {
    if (!is.null(D_subset)) {
      print("graphs are now showing subset")
      return(D_subset)
    }
    print("ERRO: subset data null")
  }
  
  print("graphs are now showing full chicago data")
  return(D_orig)
}


################################
#            UI                #
################################

ui <- fluidPage(
  
  tabsetPanel(
    id="about_app_tabs",
    tabPanel("App", id="app",
       column(12,
          column(12, 
                 uiOutput("mapTitle"),
          ),
          column(3, 
                 column(12, getMapTab()),
                 column(12, getGraphTab("percentByCommunity", "ridesByCommunityArea", "ridesByCommunityArea_t")),
          ),
          column(7,
                 column(12, getGraphTab("Day", "ridesByDay", "ridesByDay_t")),
                 column(12,
                        column(4, wellPanel(
                          tabsetPanel(
                            id="hr",
                            tabPanel("Bar Graph", id="bar", plotOutput("ridesByHour")),
                            tabPanel("Table", id="table", DT::dataTableOutput("ridesByHour_t")),
                          ),
                          getBasicControls(),
                        )),
                        column(4, getGraphTab("Month", "ridesByMonth", "ridesByMonth_t")),
                        column(4, getGraphTab("Weekday", "ridesByWeekday", "ridesByWeekday_t")),
                 )
          ),
          column(2,
                 column(12, getGraphTab("Mileage", "ridesByMileage", "ridesByMileage_t")),
                 column(12, getGraphTab("Duration", "ridesByTripTime", "ridesByTripTime_t")),
          )
       )     
    ),
    tabPanel("About", id="about", textOutput("2"))
  ),
  
  
)


################################
#         SERVER               #
################################

server <- function(input, output) {
  selectedCommArea <- reactiveVal()   # user-selected community area for viewing to-from data
  
  selected_Subset <- reactive({
    # subset(DF, subsetCol == selectedCommArea)
    getCommAreaSubset(DF, input$start_or_end, selectedCommArea())
  })
  
  curDF_comm_area <- reactive({
    parseByCommunityArea(DF, selectedCommArea(), input$start_or_end)
  })
  
  # wait for selected comm area data to update
  # update map
  observe({
      updateMap(curDF_comm_area())
  })

  observe({
    click <- input$commList_cell_clicked


    print(paste0("click:<",click,">"))

    
    if (!is.null(click)) {
      if (!is.null(unlist(click))) {
        print("chosen comm area is not null")
        val <- input$commList_cell_clicked$row
    
        print(paste("val:", val))
        selectedCommArea(val)
      }
    }
  })

  
  # Hour
  output$ridesByHour <- renderPlot({
    getBasicBarPlot(parseByHour(
      getCurSubset(DF, selected_Subset(), selectedCommArea()), input$time_choice))
  })
  output$ridesByHour_t <- DT::renderDataTable(parseByHour(
    getCurSubset(DF, selected_Subset(), selectedCommArea()), input$time_choice))
  
  # Day
  output$ridesByDay <- renderPlot({
    getBarPlot_angledX(parseByDay(
      getCurSubset(DF, selected_Subset(), selectedCommArea())))
  })
  output$ridesByDay_t <- DT::renderDataTable(parseByDay(
    getCurSubset(DF, selected_Subset(), selectedCommArea())))
  
  
  # Month
  output$ridesByMonth <- renderPlot({
    getBasicBarPlot(parseByMonth(
      getCurSubset(DF, selected_Subset(), selectedCommArea())
    ))
  })
  output$ridesByMonth_t <- DT::renderDataTable(parseByMonth(
    getCurSubset(DF, selected_Subset(), selectedCommArea())
  ))
  
  
  # Weekday
  output$ridesByWeekday <- renderPlot({
    getBasicBarPlot(parseByWeekday(
      getCurSubset(DF, selected_Subset(), selectedCommArea())
    ))
  })
  output$ridesByWeekday_t <- DT::renderDataTable(parseByWeekday(
    getCurSubset(DF, selected_Subset(), selectedCommArea())
  ))
  
  
  # Mileage
  output$ridesByMileage <- renderPlot({
    getBasicBarPlot((binByMileage(
      getCurSubset(DF, selected_Subset(), selectedCommArea())
    )))
  })
  output$ridesByMileage_t <- DT::renderDataTable(binByMileage(
    getCurSubset(DF, selected_Subset(), selectedCommArea())
  ))
  
  
  # Duration
  output$ridesByTripTime <- renderPlot({
    getBasicBarPlot(binByDuration(
      getCurSubset(DF, selected_Subset(), selectedCommArea())
    ))
  })
  output$ridesByTripTime_t <- DT::renderDataTable(binByDuration(
    getCurSubset(DF, selected_Subset(), selectedCommArea())
  ))
  
  # Map
  output$chicagoMap <- renderLeaflet({ 
    createMap()
  })
  
  # community area list
  output$commList <- DT::renderDataTable({
    communityAreas
  }, selection = 'single')
  
  # Title
  output$mapTitle <- renderUI(h1(paste("Rides that", input$start_or_end, "at:", communityAreas[selectedCommArea(),2] )))
  

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