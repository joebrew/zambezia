library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(ggrepel)

# Read in the coordinates of health centers
us <- read.csv('unidades_sanitarias_cleaned.csv')
# Make spatial
us$x <- us$lng <- us$lon <- us$longitude <- us$gpc_lng
us$y <- us$lat <- us$latitude <- us$gpc_lat
coordinates(us) <- ~gpc_lng+gpc_lat

# Get a shapefile for Mozambique
moz <- raster::getData('GADM', country = 'MOZ', level = 3)
moz2 <- raster::getData('GADM', country = 'MOZ', level = 2)


# Subset the shapefile just to Zambezia
zam <- moz[moz@data$NAME_1 == 'Zambezia',]

# Subset further to just Mopeia
mop <- moz2[moz2@data$NAME_2 == 'Mopeia',]

# Save the original proj4string
original_proj4string <- proj4string(moz)

# Prepare ggmap-compatible objects
# zam_fortified <- fortify(zam, id = ID_3)
mop_fortified <- fortify(mop, id = ID_3)
mop_gg <- get_map(location = c(lon = mean(coordinates(mop)[,1]),
                               lat = mean(coordinates(mop)[,2])),
                  maptype = 'satellite',
                  zoom = 9,
                  language = 'PT')
mop_gg_road <- get_map(location = c(lon = mean(coordinates(mop)[,1]),
                                    lat = mean(coordinates(mop)[,2])),
                       maptype = 'roadmap',
                       zoom = 9,
                       language = 'PT')

# Switch to a projected format
# Project
mop <- spTransform(mop, CRS("+init=epsg:3347"))
# Later, we can switch back to lat/long:
mop_ll  <- spTransform(mop, CRS("+init=epsg:4326"))
# proj4string(mop_ll) <- proj4string(zam)

# Plot with health posts
plot(mop_ll)
points(us)

us@data$labels <- gsub('CENTRO DE SAUDE ',
                      '',
                      us@data$health_facility)
us@data$labels[5] <- 'NZANZA'
us@data$labels[c(1, 3:4, 6:12)] <- gsub('DE ',
                                        '',
                                        us@data$labels[c(1, 3:4, 6:12)])
  
# Get a ggmap object

ggmap(mop_gg) +
  geom_point(data = us@data,
             aes(x = x,
                 y = y),
             color = 'darkred',
             size = 10) +
  geom_polygon(data = mop_fortified,
               aes(x = long,
                   y = lat,
                   group = group),
               color = 'white',
               size = 5,
               fill = NA,
               alpha = 0.1) +
  # ggrepel::geom_label_repel(data = us@data,
  geom_label(data = us@data,
                            
                            aes(x = x + 0.02, 
                                y = y + 0.02,
                                label = labels),
                            size = 15,
                            alpha = 0.8) +
  xlab('Longitude') +
  ylab('Latitude')
ggsave('map1.pdf', width = 120, height = 120, units = 'cm')


ggplot() +
  geom_polygon(data = mop_fortified,
               aes(x = long,
                   y = lat,
                   group = group),
               color = 'black',
               fill = 'beige') +
  coord_map() +
  theme_bw() +
  geom_point(data = us@data,
             aes(x = x,
                 y = y), size = 10) +
  ggrepel::geom_label_repel(data = us@data,
                            aes(x = x, 
                                y = y,
                                label = labels),
                            size = 15,
                            alpha = 0.6) +
  xlab('Longitude') +
  ylab('Latitude')
ggsave('map2.pdf', width = 120, height = 120, units = 'cm')

library(ggthemes)
ggplot() +
  geom_polygon(data = mop_fortified,
               aes(x = long,
                   y = lat,
                   group = group),
               color = 'black',
               fill = 'beige') +
  coord_map() +
  theme_bw() +
  theme_map()
ggsave('map3.pdf')

pdf('map4.pdf')
plot(cism::mop2)
dev.off()
library(leaflet)
library(RColorBrewer)

cols <- colorRampPalette(brewer.pal(n = 9, name = 'Spectral'))(length(unique(us@data$health_facility)))
ll <- 
  leaflet() %>%
  # addProviderTiles("OpenStreetMap.Mapnik") %>%
  addProviderTiles("Esri.WorldImagery") %>%
  # addProviderTiles("CartoDB.PositronOnlyLabels") %>%
  # addProviderTiles("Stamen.Watercolor") %>% 
  # addProviderTiles("Stamen.TonerHybrid") %>%
  addProviderTiles('Stamen.TonerLabels') %>%
  addProviderTiles('OpenStreetMap.Mapnik') %>%
  addCircles(data = us@data,
             lng = ~lng,
             lat = ~lat,
             color = cols,
             opacity = 0.8) %>%
  addLegend('bottomright',
            colors = cols,
            labels = unique(us@data$health_facility),
            opacity = 0.8) %>%
  addPolylines(data = mop_ll, color = 'black',
               dashArray = '1,5,5,5,') %>%
  addLabelOnlyMarkers(~long, ~lat, label =  ~as.character(mag), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))
ll


ll <- 
  leaflet() %>%
  addProviderTiles('Esri.WorldStreetMap') %>%
  addCircles(data = us@data,
             lng = ~lng,
             lat = ~lat,
             color = 'black',
             opacity = 0.8) %>%
  # addMarkers(data = us@data,
  #            lng = ~lng,
  #            lat = ~lat,
  #            label = ~labels,
  #            labelOptions = labelOptions(noHide = T)) %>%
  addPolylines(data = mop_ll, color = 'black',
               dashArray = '1,5,5,5,') %>%
  addLabelOnlyMarkers(data = us@data,
                      lng = ~lng, 
                      lat = ~lat, 
                      label =  ~as.character(labels), 
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', textOnly = T))
ll

library(mapview)
mapshot(x = ll,
        file = 'map6.pdf')
