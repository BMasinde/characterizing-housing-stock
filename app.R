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

# libraries for tif files
library(leafem)
library(stars)

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

# tif file
path_uav_mwakaboko <- "/Users/masinde/surfdrive/510/xter-HV/uav/mwakaboko1_ortho_corrected_big.tif"
tif = system.file(path_uav_mwakaboko, package = "stars")
x1 <- read_stars(tif)
x1 <-  x1[, , , 3] # what the hell does it mean band 3?

tmpfl = tempfile(fileext = ".tif")

write_stars(st_warp(x1, crs = 4326), tmpfl)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # reactive points
    coords <- eventReactive(input$recalc, {
      cbind(as.numeric(hse_svy$gps_longitude), as.numeric(hse_svy$gps_latitude))
  }, ignoreNULL = FALSE)

  output$mymap <- renderLeaflet({
    leaflet() %>%
      # addProviderTiles(providers$CartoDB.Positron,
      #                options = providerTileOptions(noWrap = TRUE)
      # ) %>%
      addTiles() %>%
      addGeotiff(
        file = tmpfl,
        opacity = 0.9,
        colorOptions =  colorOptions(
          palette = hcl.colors(256, palette = "inferno")
          , na.color = "transparent"
        )
      ) #%>%
      #addMarkers(data = coords())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
