
# FOR MAP DATA IN PART 2
# files already downloaded
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
     plotOutput("ridesByYear"),
  )
)


server <- function(input, output) {
  output$allRides = DT::renderDataTable(DF)
  
  output$ridesByYear <- renderPlot({
    getBasicBarPlot(parseByHour(DF))
  })
}


shinyApp(ui, server)