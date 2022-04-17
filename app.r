
# FOR MAP DATA IN PART 2
# files already downloaded
# might have to create a table to reference areas by code
# how to set up map
# https://rstudio.github.io/leaflet/shapes.html

library(DT)
library(shiny)



ui <- fluidPage(
  column(2, 
     titlePanel("Hello World"),
     dataTableOutput("mytable")
  )
)


server <- function(input, output) {
  output$mytable = DT::renderDataTable(DF)
}


shinyApp(ui, server)