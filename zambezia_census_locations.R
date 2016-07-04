# Libraries
library(rgdal)
library(raster)
library(sp)
library(leaflet)

# Source script
source('lib/get_zambezia.R')

# Read in zambezia data
get_zambezia(get_fresh = FALSE, save = FALSE)

# Get a more nicely named core census file
census <- CENSUS_MOPEIA_CORE

# Get more nicely named geographic coordinate column names
census$lat <- census$y <- census$latitude <- census$GPC_LAT
census$lon <- census$lng <- census$longitude <-census$x <- census$GPC_LNG

# Get a shapefile for Mozambique
moz <- raster::getData('GADM', country = 'MOZ', level = 3)

# Subset the shapefile just to Zambezia
zam <- moz[moz@data$NAME_1 == 'Zambezia',]

# Plot an interactive map
leaflet(zam) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("HERE.hybridDay") %>%
  # addProviderTiles("MapQuestOpen.Aerial")
  # addProviderTiles('OpenStreetMap.BlackAndWhite') %>%
  addCircleMarkers(lng = census$lng,
                   lat = census$lat,
                   color = 'blue',
                   fillColor = 'blue',
                   radius = 5,
                   opacity = 0,
                   fillOpacity = 0.5,
                   popup = census$META_INSTANCE_NAME) 