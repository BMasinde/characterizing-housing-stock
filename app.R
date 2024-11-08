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
library(leafem)
library(terra)


# Define UI for application
ui <- fluidPage(
  titlePanel("'Do No Harm' Valorisation Workshop"),
  
  fluidRow(
    # Map output on the left
    column(width = 6,
           leafletOutput("mymap", width = "100%", height = "800px")
    ),
    
    # Image display on the right
    column(width = 6,
           uiOutput("image_display", height = "400px")  # Display the selected image
    )
  )
)

# path list for building materials
path_list_bm_points <- list("data/mwakaboko_bm_clipped_6.geojson") 

#hse_svy <- read.csv(path_mwakaboko2, header = T, sep = ";", dec = ",")

# List of input file paths for UAV images
path_list <- list("data/mwakaboko1_clipped2.tif", "data/mwakaboko1_clipped3.tif",
                  "data/mwakaboko1_clipped4.tif", 
                  "data/mwakaboko1_clipped5.tif",
                  "data/mwakaboko1_clipped6.tif")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # List to store output rasters and extents
  raster_list <- list()
  extent_list <- list()
  
  # Define target CRS
  target_crs <- "EPSG:4326"  # WGS84 projection
  
  # Load GeoJSON points from path_list_bm_points
  bm_points <- st_read(path_list_bm_points[[1]])
  
  # Loop over each file path
  for (i in seq_along(path_list)) {
    
    # Load the GeoTIFF file
    uav_tif <- rast(path_list[[i]])
    
    # Ensure that all bands have the same extent
    extents_match <- all(sapply(2:nlyr(uav_tif), function(j) ext(uav_tif[[j]]) == ext(uav_tif[[1]])))
    
    if (!extents_match) {
      stop("The bands do not have the same extent. You may need to resample or align them.")
    }
    
    # Check if the raster has at least 4 bands and select bands 1-4
    if (nlyr(uav_tif) >= 4) {
      uav_rgbnir <- uav_tif[[1:4]]  # Extracting all 4 bands
    } else {
      stop("The TIFF file does not have 4 bands.")
    }
    
    # Reproject to target CRS if necessary
    if (crs(uav_rgbnir) != target_crs) {
      uav_rgbnir <- terra::project(uav_rgbnir, target_crs)
    }
    
    # Resample to ensure all bands have the same resolution
    uav_rgbnir <- resample(uav_rgbnir, uav_rgbnir[[1]], method = "bilinear")
    
    # Convert terra::SpatRaster to raster::Raster and store in list
    raster_list[[i]] <- raster::stack(uav_rgbnir)
    
    # Get the extent of the raster and store in list
    extent_list[[i]] <- ext(uav_rgbnir)
  }
  
  # At this point, `raster_list` contains your processed rasters, and `extent_list` contains the extents.
  # For example, you can access the first raster with `raster_list[[1]]` and its extent with `extent_list[[1]]`.
  
  # Reactive coordinates for household survey points
  #coords <- eventReactive(input$recalc, {
  #  cbind(as.numeric(hse_svy$gps_longitude), as.numeric(hse_svy$gps_latitude))
  #}, ignoreNULL = FALSE)
  
  # Reactive variable to store the selected image path
  selected_image <- reactiveVal()
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      
      # Loop through raster_list to add each raster image
      {
        map <- .
        for (raster in raster_list) {
          map <- map %>% addRasterRGB(raster, r = 1, g = 2, b = 3, opacity = 0.8)
        }
        map
      } %>%
    
      # Add the GeoJSON points layer
      addCircleMarkers(data = bm_points, 
                       color = "blue", 
                       radius = 5, 
                       fillOpacity = 0.7,
                       layerId = ~image_key,  # Use imageKey as layer ID for easy retrieval
                       popup = ~paste("ID:", image_key))  # Show imageKey in popup
  })
  
  # Observe map clicks to get the clicked point’s imageKey
  observeEvent(input$mymap_marker_click, {
    click <- input$mymap_marker_click
    if (!is.null(click$id)) {  # Check if there’s a valid imageKey
      # Construct the path to the image using the imageKey
      image_path <- paste0("images/", click$id, ".png")
      selected_image(image_path)  # Update reactive value with image path
    }
  })
  
  # Display the selected image
  output$image_display <- renderUI({
    req(selected_image())  # Wait until selected_image has a value
    tags$img(src = selected_image(), width = "100%", height = "auto")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
