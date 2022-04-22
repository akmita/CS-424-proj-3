
# FOR MAP DATA IN PART 2
# files already downloaded0
# might have to create a table to reference areas by code
# how to set up map
# https://rstudio.github.io/leaflet/shapes.html


library(DT)
library(shiny)
library(ggplot2)

source("./globals.R", local = FALSE)
source("./plotHelpers.R", local = FALSE)


getGraphTab <- function(id, output_b, output_t) {
  return(
    wellPanel(
      tabsetPanel(
        id="hr",
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
  )
)


################################
#         SERVER               #
################################

server <- function(input, output) {
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
  
}


################################
#            INIT              #
################################

shinyApp(ui, server)