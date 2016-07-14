
```{r}

# Cluster around all points
# 
clusters_buffered <- gBuffer(SpatialPoints(census_spatial),
                             id = clusters$cluster,
                             byid = TRUE,
                             width = 1000)
proj4string(clusters_buffered) <- proj4string(census_spatial)


cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(max(clusters$cluster))
cols <- sample(cols, length(cols))
plot(mop)
plot(clusters_buffered, col = cols, add = TRUE, border = FALSE)

# Go over each point, and see which it falls into
# wheres <- over(census_spatial, clusters_buffered, returnList = TRUE)
wheres <- over(census_spatial, clusters_buffered)

cols_points <- cols[wheres]

plot(mop)
# Add points with no zone
points(census_spatial[is.na(wheres),], col = 'grey', pch = '.')
plot(clusters_buffered, col = adjustcolor(cols, alpha.f = 0.3), add = TRUE, border = FALSE)

points(census_spatial, col = adjustcolor(cols_points, alpha.f = 0.8), pch = '.')

# 
# # Create buffers around clusters (very slow)
# for (i in 1:max(clusters$cluster)){
#   message(paste0('Cluster ', i))
#   assign(paste0('cluster', i),
#          gBuffer(census_spatial[clusters$cluster == i,],
#                  width = 1000))
#     # Also get minimal cluster (ie, no buffer)
#   assign(paste0('minimal_cluster', i),
#          gBuffer(census_spatial[clusters$cluster == i,],
#                  width = 0.000001))
# }
# 
# plot(mop)
# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(max(clusters$cluster))
# cols <- sample(cols, length(cols))
# for (i in 1:max(clusters$cluster)){
#   obj <- get(paste0('cluster', i))
#   plot(obj, col = cols[i], add = TRUE, border = FALSE)
# }
# 
# # See which points are within the buffer zones of multiple polygons
# in_buffer <-
#   function(point){
#     ins <- c()
#     for (i in 1:n_clusters){
#       ins[i] <- over(point, get(paste0('cluster', i)))
#     }
#     return(length(which(!is.na(ins))))
#   }
# 
# # Really slow!
# census_spatial$zones <- FALSE
# for (i in 1:nrow(census_spatial)){
#   message(i)
#   census_spatial$zones[i] <-
#     in_buffer(census_spatial[i,])
# }
# hist(census_spatial$zones)

# ALTERNATIVE ! Just use the centroids of each cluster
# get the 1 km cluster, 
# and exclude all points not in it
# Create buffers around clusters (very slow)

centroids_buffered <- gBuffer(centroids, byid = TRUE, width = 2000)

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(max(clusters$cluster))
cols <- sample(cols, length(cols))
plot(mop)
plot(centroids_buffered, col = cols, add = TRUE, border = FALSE)

# Go over each point, and see which it falls into
wheres <- over(census_spatial, centroids_buffered, returnList = TRUE)

# How many are in only one polygon
only_one <- unlist(lapply(wheres, function(x){
  length(x) == 1
}))

# Subset to only keep those that are in only area
census_spatial_keep <- census_spatial[only_one,]
wheres <- unlist(wheres[only_one])
cols_points <- cols[wheres]

plot(mop)
# Add points with no zone
plot(centroids_buffered, col = adjustcolor(cols, alpha.f = 0.3), add = TRUE, border = FALSE)
points(census_spatial_keep, col = adjustcolor(cols_points, alpha.f = 0.8), pch = '.')


# for (i in 1:max(clusters$cluster)){
#   message(paste0('Cluster ', i))
#   assign(paste0('cluster', i),
#          gBuffer(centroids[i,],
#                  width = 1))
#   # Also get minimal cluster (ie, no buffer)
#   assign(paste0('minimal_cluster', i),
#          gBuffer(centroids[i,],
#                  width = 0.000001))
#   
# }
# plot(mop)
# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(max(clusters$cluster))
# cols <- sample(cols, length(cols))
# 
# for (i in 1:max(clusters$cluster)){
#   obj <- get(paste0('minimal_cluster', i))
#   plot(obj, col = cols[i], add = TRUE, border = FALSE)
#   obj <- get(paste0('cluster', i))
#   plot(obj, col = adjustcolor(cols[i], alpha.f = 0.3), add = TRUE, border = FALSE)
# }
# 
# # Combine
# 
# clusters_combined <- gUnion(cluster1, cluster2)
# for (i in 3:n_clusters){
#   message(i)
#   clusters_combined <- gUnion(clusters_combined,
#                               get(paste0('cluster', i)))
# }
# x <- gUnion(x, cluster8)
# 
# y <- rgeos::gIntersection(x, cluster8)

# Visualize



# See which points are within the buffer zones of multiple polygons
in_buffer <-
  function(point){
    ins <- c()
    for (i in 1:n_clusters){
      ins[i] <- over(point, get(paste0('cluster', i)))
    }
    return(length(which(!is.na(ins))))
  }
# Really slow!
census_spatial$zones <- FALSE
for (i in 1:nrow(census_spatial)){
  message(i)
  census_spatial$zones[i] <-
    in_buffer(census_spatial[i,])
}

#################################3

# Get a matrix of distances between all points
all_distances <-
  spDists(census_spatial)

# Set to NA the self-distance
for (i in 1:nrow(all_distances)){
  all_distances[i,i] <- NA
}

# Get a vector of minimum distances
minimum_distances <-
  apply(all_distances, 1, min, na.rm = TRUE)


####################################
# Create clusters
buffer_size <- 1 # in kilometers
the_data <- census_spatial
the_data$done <- FALSE
the_data$cluster_number <- NA
the_data$row_number <- 1:nrow(the_data)
cluster_number <- 1

while(length(which(the_data$done)) < length(the_data$done)){
  
  # Get a new start index
  possibles <- data.frame(the_data) %>% filter(!done)
  possible_minimum_distances <- minimum_distances[!the_data$done]
  start_index <- possibles$row_number[which.min(possible_minimum_distances)]
  if(the_data$done[start_index]){
    stop('Fix your start indexer')
  }
  
  # Get the start point
  start_point <- the_data[start_index,]
  
  # Mark the start point as in the cluster
  the_data$cluster_number[start_index] <- cluster_number
  the_data$done[start_index] <- TRUE
  
  # Get the nearest points that are within the buffer size
  close_indices <- which(all_distances[start_index,] <= buffer_size)
  
  if(length(close_indices) > 0){
    # For those that are close (and not yet part of another cluster)
    # assign to current cluster
    the_data$cluster_number[close_indices] <-
      ifelse(is.na(the_data$cluster_number[close_indices]), cluster_number,
             the_data$cluster_number[close_indices])
    the_data$done[close_indices] <- TRUE  
  }
  
  # Message
  message(paste0('Finished creating cluster ', cluster_number, '\n(',
                 length(close_indices) + 1, ' points assigned).\n',
                 length(which(!the_data$done)), ' points remaining.'))
  
  # Get a new cluster number
  cluster_number <- cluster_number + 1
}


# Plot
cols <- rainbow(max(the_data$cluster_number))
plot(mop)
points(the_data, col = cols)
```


# Visualizations


Here are all locations collected so far, overalid onto Esri's "World Imagery" satellite layer. Zoom in and out, move around, and click on any point to see the "META_INSTANCE_NAME" and village name.

```{r}
# Plot an interactive map
leaflet(zam) %>%
addProviderTiles("Esri.WorldImagery") %>%
addCircleMarkers(lng = census$lng,
lat = census$lat,
color = colors,
fillColor = 'blue',
radius = 2.5,
opacity = 0,
fillOpacity = 0.5,
popup = paste0(census$META_INSTANCE_NAME, '\nvillage: ',
census$LOCAL_VILLAGE_NAME))
```



```{r}
# # OTHER STRATEGIES
# 
# ## FPC
# library(fpc)
# lat<-census_spatial$latitude
# lon<-census_spatial$longitude
# 
# DBSCAN <- dbscan(cbind(lat, lon), eps = 0.01, MinPts = 3, showplot = TRUE)
# plot(lon, lat, col = DBSCAN$cluster, pch = 20)
# 
# ## FIELDS
# library(fields)
# threshold.in.km <- 1
# coors <- data.frame(lon,lat)
# 
# #distance matrix
# dist <- rdist.earth(coors,miles = F,R=6371)
# 
# #clustering
# fit <- hclust(as.dist(dist), method = "single")
# clusters <- cutree(fit,h = threshold.in.km)
# 
# plot(lon, lat, col = clusters, pch = 20)
# 
# # HCLUST
# library(fields)
# max_km <- 1
# lon <- census_spatial$lon
# lat <- census_spatial$lat
# coors <- data.frame(lon,lat)
# 
# #distance matrix
# dist <- rdist.earth(coors,miles = F,R=6371)
# 
# #clustering
# fit <- hclust(as.dist(dist), method = "single")
# clusters <- cutree(fit,h = max_km)
# 
# # Get centroids
# centroids <- data.frame(census_spatial)
# centroids$cluster <- clusters
# centroids <- centroids %>%
#   group_by(cluster) %>%
#   summarise(lon = mean(lon),
#             lat = mean(lat))
# coordinates(centroids) <- ~lon+lat
# proj4string(centroids) <- CRS(original_proj4string)
# centroids <- spTransform(centroids, CRS("+init=epsg:3347"))
# 
# plot(mop)
# points(centroids)
# 
# # Buffer around the centroids
# centroids_buffered <- gBuffer(SpatialPoints(centroids), byid = TRUE, width = 2000)
# 
# # # Buffer around the points
# # points_buffered <- 
# #   gBuffer(SpatialPoints(census_spatial[clusters == 1,]), 
# #           width = 1000)
# 
# 
# # Conversion of projections
# proj4string(points_buffered) <- proj4string(points_buffered)
# proj4string(centroids_buffered) <- proj4string(census_spatial)
# 
# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(max(clusters))
# cols <- sample(cols, length(cols))
# plot(mop)
# plot(points_buffered, col = cols, add = TRUE, border = FALSE)
# plot(centroids_buffered, col = cols, add = TRUE, border = FALSE)
# 
# # Go over each point, and see which it falls into
# wheres <- over(census_spatial, centroids_buffered, returnList = TRUE)
# 
# # How many are in only one polygon
# only_one <- unlist(lapply(wheres, function(x){
#   length(x) == 1
# }))
# 
# # Subset to only keep those that are in only area
# census_spatial_keep <- census_spatial[only_one,]
# sub_wheres <- unlist(wheres[only_one])
# cols_points <- cols[sub_wheres]
# 
# plot(mop)
# # Add points with no zone
# plot(centroids_buffered, col = adjustcolor(cols, alpha.f = 0.3), add = TRUE, border = FALSE)
# points(census_spatial_keep, col = adjustcolor(cols_points, alpha.f = 0.8), pch = '.',
#        cex = 0.3)

## K-means modified

We first built clusters using k-means, and then iteratively "trim" those clsuters until buffers are of suitable size.

## Custom approach

Since we're less concerned about number of clusters and proximity to cluster centers, and more concerned about distance _between_ clusters, we instead employ an approach in which clusters are formed sequentially (starting at the most isolated point), and each cluster's formation ends as soon as no points are within two kilometers.

Since over-continuity is an issue, we first remove all points that have fewer than 20 other locations within two kilometers.


```
