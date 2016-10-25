identify_problems <- function(){
  
  # Create placeholder vector of incorrect houses
  incorrect_houses <- c()
  
  for (i in 1:nrow(village_df)){
    message(i)
    # Get village name
    this_village <- village_df$village_number[i]
    if(!is.na(this_village)){
      # Get the points only
      sub_census <- 
        census_spatial_ll[which(census_spatial_ll$village_number == this_village),]
      # Get the border
      border <- gConvexHull(sub_census)
      # Calculate area
      # if(!is.null(border)){
      # this_area <- gArea(border)
      # Populate area in dataframe
      # village_df$area[i] <- this_area
      # }
      
      # Go through each household in the unit and calculate distance
      # to all other units
      # distances <- gDistance(sub_census, byid = TRUE)
      # distances <- spDists(sub_census, longlat = TRUE, diagonal = FALSE)
      distances <- geosphere::distm(sub_census, fun = distVincentySphere)
      median_distances <- apply(distances, 1, function(x){quantile(x, 0.75, na.rm = TRUE)})
      
      # Keep only the closest 50 percent
      close_cluster <- sub_census[which(median_distances <= median(median_distances, na.rm = TRUE)),]
      
      # Get the centroids of the close cluster
      centroids <- c(mean(close_cluster$lng, na.rm = TRUE), 
                     mean(close_cluster$lat, na.rm = TRUE))
      
      # Get distance from every household to those centroids
      distances_to_centroid <- geosphere::distm(x = sub_census, 
                                                y = centroids,
                                                fun = distVincentySphere)
      
      
      # From the vector of median distances, flag those which are abnormal
      threshold <- 2000
      incorrect_house_ids <- sub_census$house_number[which(distances_to_centroid > threshold)]
      # Also flag as incorrect any 1 household village ^^
      if(nrow(sub_census) == 1){
        incorrect_house_ids <- c(incorrect_house_ids, sub_census$house_number)
      }
      
      # Stick results into dataframe
      village_df$n_incorrect_houses[i] <- length(incorrect_house_ids)
      village_df$incorrect_houses[i] <- paste0(incorrect_house_ids, collapse = ', ')
      
      # Add to the vector of incorrect houses 
      incorrect_houses <- c(incorrect_houses, incorrect_house_ids)
      
      # file_name <- as.character(i)
      # while(nchar(file_name) < 3){
      #   file_name <- paste0(0, file_name)
      # }
      # file_name <- paste0('gifs/',file_name, '.png')
      # png(file_name)
      # plot(mop_ll)
      # 
      # points(sub_census,
      #      col = ifelse(sub_census$house_number %in% incorrect_house_ids, 
      #                   'red',
      #                   'blue'))
      # plot(border, add = T)
      # title(main = paste0('Village number ', village_df$village_number[i]))
      # dev.off()
    }
    
    
  }
  assign('incorrect_houses',
         incorrect_houses,
         envir = .GlobalEnv)
  
  village_df$problems <- village_df$incorrect_houses > 0
  assign('incorrect_villages',
         village_df[which(village_df$incorrect_houses > 0),],
         envir = .GlobalEnv
  )
  
}

make_village_df <- function(census){
  # Get the area and other metrics for each village
  unique_village_numbers <- sort(unique(census$village_number))
  village_df <- census %>%
    group_by(village_number) %>%
    summarise(n_children = sum(number_of_children),
              houses_with_any_children = length(which(number_of_children > 0)),
              houses = n(),
              area = NA,
              incorrect_houses = NA,
              n_incorrect_houses = NA,
              problems = NA)
  
  # MERGE WITH THE CLUSTER ASSIGNMENTS FROM AUGUST
  cluster_assignments <- readr::read_csv('outputs/cluster_assignments.csv')
  village_df <- 
    village_df %>%
    left_join(cluster_assignments)
  # Row number
  village_df$row_number <- 1:nrow(village_df)
  return(village_df)
}

# Like leaflet_village, but assumes all points are clean
leaflet_village_clean <- function(i){
  message(i)
  this_village_number <- village_df$village_number[i]
  this_spray_status <- village_df$status[i]
  if(!is.na(this_village_number)){
    # Get the points only
    sub_census <- census_spatial_ll[which(census_spatial_ll$village_number == this_village_number),]
    if(nrow(sub_census) > 0){
      sub_census_projected <- 
        census_spatial[which(census_spatial$village_number == this_village_number),]
      
      # Get the border
      border <- gConvexHull(sub_census)
      border_projected <- gConvexHull(sub_census_projected)
      
      the_data <- sub_census
      the_border <- border
      the_border_projected <- border_projected
      
      # # Get internal buffer
      # if(class(border)[[1]] != 'SpatialPoints'){
      #   internal <- rgeos::gBuffer(the_border_projected, width = -1000)
      #   # Make lat long
      #   if(!is.null(class(internal)) & class(internal) != 'NULL'){
      #     internal <- spTransform(internal, proj4string(border))
      #   }
      # }
      
      # Generate colors
      colors <- rep('darkblue', nrow(the_data))
      # colors[the_data$buffer] <- 'blue'
      
      # The village number
      the_village_number <- the_data$village_number[1]
      
      # The village df row
      the_village_df_row <- village_df %>% filter(village_number == the_village_number)
      
      # Get the maximum distance between two houses in kilometers
      max_d <- paste0(round(max(apply(geosphere::distm(the_data, fun = distVincentySphere), 
                                      2, 
                                      max, na.rm = TRUE), 
                                na.rm = TRUE) / 1000, digits = 1), 
                      ' K')
      
      #   # Legend title
      legend_title <- paste0('Village: ',
                             the_village_number,
                             '. Max distance: ',
                             max_d,
                             '. ',
                             ifelse(
                               ifelse(is.na(this_spray_status), 'No spray.',
                                            ifelse(this_spray_status,
                                    'SPRAY.',
                                    'No spray.'))))
      # '. ',
      # the_village_df_row$houses_with_any_children,
      # ' houses of ',
      # the_village_df_row$houses,
      # ' have children.')
      
      
      ll <- 
        leaflet() %>%
        # addProviderTiles("OpenStreetMap.Mapnik") %>%
        addProviderTiles("Esri.WorldImagery") %>%
        # addProviderTiles("CartoDB.PositronOnlyLabels") %>%
        # addProviderTiles("Stamen.Watercolor") %>% 
        # addProviderTiles("Stamen.TonerHybrid") %>%
        addProviderTiles('Stamen.TonerLabels') %>%
        addCircleMarkers(lng = the_data$lng,
                         lat = the_data$lat,
                         color = colors,
                         fillColor = colors,
                         radius = 2.5,
                         opacity = 0,
                         fillOpacity = 0.5,
                         popup = paste0('Household: ', the_data$house_number, ' Village number: ',
                                        the_data$village_number)) %>%
        addLegend("bottomright", 
                  colors = NA,
                  labels = ' ',
                  # colors = c('darkorange', 'blue'),
                  # labels = c('Within 2km of other village', 'xxx'),
                  title = legend_title,
                  opacity = 1 )
      
      if(!class(border)[[1]] %in% c('SpatialPoints', 'NULL')){
        ll <-
          ll %>%
          addPolylines(data = the_border, color = 'black',
                       dashArray = '1,5,5,5,')
      }
      # # BUFFER
      # if(!is.null(class(internal))){
      #   ll <- 
      #     ll %>%
      #     addPolylines(data = internal,
      #                  color = 'grey',
      #                  dashArray = '1,5')
      # }
      return(ll)
    }
  }
  
}


# CREATE FUNCTION FOR LEAFLET MAP OF VILLAGE
leaflet_village <- function(i){
  message(i)
  this_village_number <- village_df$village_number[i]
  if(!is.na(this_village_number)){
    # Get the points only
    sub_census <- census_spatial_ll[which(census_spatial_ll$village_number == this_village_number),]
    if(nrow(sub_census) > 0){
      # Get the border
      border <- gConvexHull(sub_census)
      
      the_data <- sub_census
      the_border <- border
      
      # Generate colors
      colors <- rep('blue', nrow(the_data))
      colors[the_data$house_number %in% incorrect_houses] <- 'red'
      
      # The village number
      the_village_number <- the_data$village_number[1]
      
      # The village df row
      the_village_df_row <- village_df %>% filter(village_number == the_village_number)
      
      #   # Legend title
      legend_title <- paste0('Village: ',
                             the_village_number,
                             '. ',
                             ifelse(the_village_df_row$problems,
                                    paste0(the_village_df_row$n_incorrect_houses, ' casas suspeitas', collapse = ''),
                                    'Nao ha problemas'))
      
      
      ll <- 
        leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addCircleMarkers(lng = the_data$lng,
                         lat = the_data$lat,
                         color = colors,
                         fillColor = colors,
                         radius = 2.5,
                         opacity = 0,
                         fillOpacity = 0.5,
                         popup = paste0('Household: ', the_data$house_number, ' Village number: ',
                                        the_data$village_number)) %>%
        addLegend("bottomright", 
                  colors = c('red', 'blue'),
                  labels = c(paste0('Suspected: ',
                                    the_village_df_row$n_incorrect_houses), 
                             paste0('Okay: ',
                                    the_village_df_row$houses - the_village_df_row$n_incorrect_houses)),
                  title = legend_title,
                  opacity = 1 )
      
      if(!class(border)[[1]] %in% c('SpatialPoints', 'NULL')){
        ll <-
          ll %>%
          addPolylines(data = the_border, color = 'black',
                       dashArray = '1,5')
      }
      return(ll)
    }
  } 
  
  
}

# Function to identify points which are within 2km of a point
# OUTSIDE of the village; in other words, remove those that might be problematic
identify_buffers <- 
  function(spatial_ll_census = census_spatial_ll,
           distances = distance_matrix){
    require(geosphere)
    # Get distances between all houses
    message('Calculating distances for everyone')
    spatial_ll_census$within_1k_not_village <- NA
    message(paste0('Done calculating distances : ', Sys.time()))
    # Sub function to go through each row, identify which are in the same village
    for (i in 1:nrow(spatial_ll_census)){
      message(i)
      this_village <- spatial_ll_census$village_number[i]
      these_distances <- distances[i,]
      # Remove other points in the village
      these_distances <- these_distances[spatial_ll_census$village_number != this_village]
      # See if there are any remaining less than 1k
      sub1 <- these_distances < 1000
      x <- any(sub1, na.rm = TRUE)
      spatial_ll_census$buffer[i] <- x
      spatial_ll_census$within_1k_not_village[i] <- length(which(sub1))
    }
    return(spatial_ll_census)
  }



# MAKE MASTER MAP
master_map <- function(){
  
  
  color_numbers <- as.numeric(factor(village_df$village_number))
  color_palette <- colorRampPalette(brewer.pal('Spectral', n = 9))(max(color_numbers, na.rm = TRUE))
  color_palette <- sample(color_palette, length(color_palette))
  the_colors <- color_palette[color_numbers]
  village_colors <- data_frame(village_number = village_df$village_number,
                               color = the_colors)
  colors <- left_join(census_spatial_ll@data,
                      village_colors,
                      by = 'village_number') %>%
    dplyr::select(color) %>%
    unlist %>%
    as.character()
  
  pdf(file = '~/Desktop/master_map.pdf',
      width = 30,
      height = 30)
  plot(mop_ll)
  
  for (i in 1:nrow(village_df)){
    message(i)
    this_village_number <- village_df$village_number[i]
    this_color <- village_colors$color[i]
    if(!is.na(this_village_number)){
      # Get the points only
      sub_census <- census_spatial_ll[which(census_spatial_ll$village_number == this_village_number),]
      if(nrow(sub_census) > 0){
        sub_census_projected <- 
          census_spatial[which(census_spatial$village_number == this_village_number),]
        
        # Get the border
        border <- gConvexHull(sub_census)
        border_projected <- gConvexHull(sub_census_projected)
        
        the_data <- sub_census
        the_border <- border
        the_border_projected <- border_projected
        
        # Get centroid
        centroids <- c(mean(the_data$lng, na.rm = TRUE),
                       mean(the_data$lat, na.rm = TRUE))
        
        # # Get internal buffer
        # if(!class(border)[[1]] %in% c('SpatialPoints', 'NULL')){
        #   internal <- rgeos::gBuffer(the_border_projected, width = -1000)
        #   # Make lat long
        #   if(!is.null(class(internal)) & class(internal) != 'NULL'){
        #     internal <- spTransform(internal, proj4string(border))
        #   }
        # }
        
        # The village number
        the_village_number <- the_data$village_number[1]
        
        # The village df row
        the_village_df_row <- village_df %>% filter(village_number == the_village_number)
        
        # Get the maximum distance between two houses in kilometers
        max_d <- max(apply(geosphere::distm(the_data, fun = distVincentySphere), 
                           2, 
                           max, na.rm = TRUE), 
                     na.rm = TRUE) / 1000
        
        # Plot the border
        plot(the_border,
             col = adjustcolor(this_color, alpha.f = 0.2), 
             add = TRUE,
             border = adjustcolor(this_color, alpha.f = 0.6),
             lty = 2)
        
        # Plot the points
        points(the_data,
               col = adjustcolor(this_color, alpha.f = 0.5),
               pch = 3,
               cex = 0.6)
        
        # Plot the centroid
        text(centroids[1],
             centroids[2],
             cex = ceiling(max_d / 25),
             labels = this_village_number,
             col = adjustcolor(this_color, alpha.f = 0.7))
        
        # # Add labels
        # text(the_data,
        #      labels = the_data$house_number,
        #      col = adjustcolor('black', alpha.f = 0.3),
        #      cex = 0.2)
      }
      
    }
    
  }
  dev.off()
}



# Like leaflet_village, but aall together
leaflet_village_master <- function(){
  
  color_numbers <- as.numeric(factor(village_df$village_number))
  color_palette <- colorRampPalette(brewer.pal('Spectral', n = 9))(max(color_numbers, na.rm = TRUE))
  color_palette <- sample(color_palette, length(color_palette))
  the_colors <- color_palette[color_numbers]
  village_colors <- data_frame(village_number = village_df$village_number,
                               color = the_colors)
  colors <- left_join(census_spatial_ll@data,
                      village_colors,
                      by = 'village_number') %>%
    dplyr::select(color) %>%
    unlist %>%
    as.character()
  
  
  ll <-  
    leaflet() %>%
    # addProviderTiles("OpenStreetMap.Mapnik") %>%
    # addProviderTiles("Esri.WorldImagery") %>%
    # addProviderTiles("CartoDB.PositronOnlyLabels") %>%
    # addProviderTiles("Stamen.Watercolor") %>% 
    addProviderTiles("Stamen.Toner") %>%
    addProviderTiles('Stamen.TonerLabels') 
  
  # Now loop through and add borders
  for (i in 1:nrow(village_df)){
    message(i)
    this_village_number <- village_df$village_number[i]
    if(!is.na(this_village_number)){
      # Get the points only
      sub_census <- census_spatial_ll[which(census_spatial_ll$village_number == this_village_number),]
      if(nrow(sub_census) > 0){
        sub_census_projected <- 
          census_spatial[which(census_spatial$village_number == this_village_number),]
        
        # Get the border
        border <- gConvexHull(sub_census)
        
        the_border <- border
        
        if(class(border)[[1]] != 'SpatialPoints'){
          this_color <- village_colors$color[i]
          ll <-
            ll %>%
            addPolylines(data = the_border, color = this_color,
                         # dashArray = '1,5',
                         opacity = 0.6,
                         popup = village_df$village_number[i]) %>%
            addPolygons(data = the_border,
                        color = this_color,
                        popup = village_df$village_number[i])
        }
        
      }
    }
  }
  
  # Add all points
  ll <-
    ll %>%
    addCircleMarkers(lng = census_spatial_ll$lng,
                     lat = census_spatial_ll$lat,
                     color = colors,
                     fillColor = colors,
                     radius = 2.5,
                     opacity = 0,
                     fillOpacity = 0.5,
                     popup = paste0('Household: ', census_spatial_ll$house_number, ' Village number: ',
                                    census_spatial_ll$village_number)) 
  
  
  return(ll)
}

# Get distance matrix
get_distance_matrix <- function(spatial_ll_census = census_spatial_ll){
  distances <- geosphere::distm(spatial_ll_census, fun = distVincentySphere)
  return(distances)
}

# Get the nearest neighbor of villages that aren't this village
nearest_neighbor <- function(census_spatial_ll = census_spatial_ll,
                             distance_matrix_old = distance_matrix_old,
                             n = 5){
  nn <- rep(NA, nrow(census_spatial_ll))
  for (i in 1:nrow(census_spatial_ll)){
    message(i)
    this_village <- census_spatial_ll$village_number[i]
    the_distances <- distance_matrix_old[i,]
    # Make a dataframe of villages and distances
    x <- data_frame(village_number = census_spatial_ll$village_number,
                    meters = the_distances) %>%
      group_by(village_number) %>%
      summarise(low = min(meters, na.rm = TRUE),
                mid = median(meters, na.rm = TRUE),
                avg = mean(meters, na.rm = TRUE)) %>%
      filter(village_number != this_village) %>%
      arrange(low)
    # Get the  nearest
    the_nearest <- x$village_number[1:n]
    # Paste together and return
    nn[i] <- paste0(the_nearest, collapse = ';')
  }
  return(nn)
}
