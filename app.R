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
#library(stars)
#library (raster)
library(terra)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("'Do No Harm' Valorisation Workshop"),
    
    # leaflet 
    leafletOutput("mymap", width = "50%", height = "800px"),
    p(),
    #actionButton("recalc", "New points")
    
)

# household survey data
path_mwakaboko2 <- "/Users/masinde/surfdrive/510/housing vulnerability (Echo IV)/TA Mwakaboko GVH Mwakaboko2.csv"

hse_svy <- read.csv(path_mwakaboko2, header = T, sep = ";", dec = ",")

# tif file
path_uav_mwakaboko <- "data/mwakaboko1_clipped2.tif"

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Load the GeoTIFF file using terra (handles multi-band rasters)
  uav_tif <- rast(path_uav_mwakaboko)
  
  # Ensure that all bands have the same extent by comparing each band's extent to the first band's extent
  extents_match <- all(sapply(2:nlyr(uav_tif), function(i) ext(uav_tif[[i]]) == ext(uav_tif[[1]])))
  
  if (!extents_match) {
    stop("The bands do not have the same extent. You may need to resample or align them.")
  }
  
  # If the raster has 4 bands, use bands 1-3 for RGB and optionally band 4 for NIR or other purposes
  # This assumes bands 1-3 are RGB and band 4 is NIR
  if (nlyr(uav_tif) >= 4) {
    uav_rgbnir <- uav_tif[[1:4]]  # Extracting all 4 bands
  } else {
    stop("The TIFF file does not have 4 bands.")
  }
  
  # Check if the CRS is EPSG:4326 (WGS84) and reproject if necessary
  target_crs <- "EPSG:4326"  # WGS84 projection
  
  # Compare the CRS of the raster with the target CRS
  if (crs(uav_rgbnir) != target_crs) {
    uav_rgbnir <- terra::project(uav_rgbnir, target_crs)
  }
  
  # Ensure all bands have the same resolution (just in case)
  uav_rgbnir <- resample(uav_rgbnir, uav_rgbnir[[1]], method = "bilinear")
  
  # Convert terra::SpatRaster to raster::Raster
  uav_rgbnir_raster <- raster::stack(uav_rgbnir)
  
  # Get the extent of the raster (bounding box)
  uav_extent <- ext(uav_rgbnir)  # terra::ext() gives xmin, xmax, ymin, ymax
  
  # Reactive coordinates for household survey points
  coords <- eventReactive(input$recalc, {
    cbind(as.numeric(hse_svy$gps_longitude), as.numeric(hse_svy$gps_latitude))
  }, ignoreNULL = FALSE)
  

  output$mymap <- renderLeaflet({
    leaflet() %>%
     addProviderTiles(providers$CartoDB.Positron,
                      options = providerTileOptions(noWrap = TRUE)
       ) %>%
      #addTiles() #%>%
      # Add the raster image to the leaflet map using addRasterImage
      addRasterRGB(uav_rgbnir_raster, r = 1, g = 2, b = 3, opacity = 0.8) #%>% # RGB + NIR (optional alpha) #%>%  # RGB + NIR, with alpha channel as NIR (optional)
      #fitBounds(lng1 = uav_extent$xmin, lat1 = uav_extent$ymin, 
      #          lng2 = uav_extent$xmax, lat2 = uav_extent$ymax)
      # Optionally add markers for the survey data
      #addMarkers(data = coords(), popup = ~paste("Household ID:", hse_svy$household_id))
      # addMarkers(data = coords())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
