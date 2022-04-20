
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
     plotOutput("ridesByYear"),
     plotOutput("ridesByMonth"),
     plotOutput("ridesByWeekday")
  )
)



server <- function(input, output) {
  output$allRides = DT::renderDataTable(DF)
  
  output$ridesByHour <- renderPlot({
    getBasicBarPlot(parseByHour(DF))
  })
  
  output$ridesByYear <- renderPlot({
    getBasicBarPlot(parseByYear(DF))
  })
  
  output$ridesByMonth <- renderPlot({
    getBasicBarPlot(parseByMonth(DF))
  })
  
  output$ridesByWeekday <- renderPlot({
    getBasicBarPlot(parseByWeekday(DF))
  })
}


shinyApp(ui, server)