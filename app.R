#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(sf)
library(RColorBrewer)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("'Do No Harm' Valorisation Workshop"),
    
    # leaflet 
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "New points")
    
)

# household survey data
path_mwakaboko2 <- "/Users/masinde/surfdrive/510/housing vulnerability (Echo IV)/TA Mwakaboko GVH Mwakaboko2.csv"

hse_svy <- read.csv(path_mwakaboko2, header = T, sep = ";", dec = ",")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # reactive points
    coords <- eventReactive(input$recalc, {
      cbind(as.numeric(hse_svy$gps_longitude), as.numeric(hse_svy$gps_latitude))
  }, ignoreNULL = FALSE)

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = coords())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
