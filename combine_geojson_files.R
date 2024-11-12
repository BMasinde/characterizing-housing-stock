
# Script for appending (binding) clipped shapefiles (building materials)

library(sf)
library(dplyr)


# path to GeoJSON files
geojson1 <- st_read("data/mwakaboko_bm_clipped_6.geojson")
geojson2 <- st_read("data/mwakaboko_bm_clipped_5.geojson")
geojson3 <- st_read("data/mwakaboko_bm_clipped_7.geojson")
#geojson4 <- st_read("data/mwakaboko_bm_clipped_8.geojson")

# Ensure Consistent Coordiante Reference Systems (CRS)

# Append the GeoJSON Files
combined_geojson <- rbind(geojson1, geojson2, geojson3)

# Save the combined CeoJSON
st_write(combined_geojson, "data/combined_geojson_file.geojson", driver = "GeoJSON")
