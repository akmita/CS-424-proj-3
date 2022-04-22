
# FOR MAP DATA IN PART 2
# files already downloaded0
# might have to create a table to reference areas by code
# how to set up map
# https://rstudio.github.io/leaflet/shapes.html

library(DT)
library(shiny)
library(ggplot2)

source("./plotHelpers.R", local = FALSE)



ui <- fluidPage(
  column(12, 
     titlePanel("Hello World"),
     dataTableOutput("allRides"),
     plotOutput("ridesByHour"),
     plotOutput("ridesByDay"),
     plotOutput("ridesByMonth"),
     plotOutput("ridesByWeekday"),
     plotOutput("ridesByMileage"),
  )
)



server <- function(input, output) {
  output$allRides = DT::renderDataTable(DF)
  
  output$ridesByHour <- renderPlot({
    getBasicBarPlot(parseByHour(DF))
  })
  
  output$ridesByDay <- renderPlot({
    getBarPlot_angledX(parseByDay(DF))
  })
  
  output$ridesByMonth <- renderPlot({
    getBasicBarPlot(parseByMonth(DF))
  })
  
  output$ridesByWeekday <- renderPlot({
    getBasicBarPlot(parseByWeekday(DF))
  })
  
  output$ridesByMileage <- renderPlot({
    getBasicBarPlot(binByMileage(DF))
  })
}


shinyApp(ui, server)