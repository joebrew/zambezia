# https://sap.manhica.net:4703/redcap/

library(tidyverse)
library(readr)
library(yaml)

# Read an old file just to get headers
headers <- read_csv('data/COSTMopeia_DATA_2017-01-31_1018.csv')[0,]

# See what recruitment data we already have
files <- dir('data/')
files <- files[grepl('NOHDRS', files)]
file_dates <- as.Date(substr(files, 24, 33))
file_times <- as.numeric(substr(files, 35, 38))

# Get most recent file
the_date <- max(file_dates)

# Retrieve recent data if old
# Make sure to be in the zambezia virtualenv first

if(the_date < Sys.Date()){
  system('python get_new_data.py')
  # Re-look at the files
  files <- dir('data/')
  files <- files[grepl('NOHDRS', files)]
  file_dates <- as.Date(substr(files, 24, 33))
  file_times <- as.numeric(substr(files, 35, 38))
}

the_file <- files[file_dates == the_date]

if(length(the_file) > 1){
  the_file_times <- file_times[file_dates == max(file_dates)]
  the_file <- the_file[the_file_times == max(the_file_times)]
}
the_file <- the_file[1]

# Read  
master <- read_csv(paste0('data/',
                          the_file),
                   col_names = FALSE)
names(master) <- names(headers); rm(headers)

# Read in geogrpahic and demographic data
master_table <- read_csv("~/Documents/zambezia/master_table_for_carlos.csv")

# Get spray status
ss <- master_table %>% group_by(village_number) %>%
  filter(!is.na(status)) %>%
  summarise(spray_status = dplyr::first(status)) %>%
  mutate(spray_status = ifelse(spray_status, 'Spray', 'No spray'))

# Join spray status to df
master <- master %>%
  left_join(ss,
            by = c('village' = 'village_number'))

# Clean up name
master <- master %>%
  mutate(malaria = ifelse(diagnostics == 1, TRUE, FALSE),
         rdt = ifelse(tdr == 1, TRUE, FALSE))

# Break into arms
arms <- sort(unique(master$redcap_event_name))
for (i in 1:length(arms)){
  df <- master %>% filter(redcap_event_name == arms[i])
  remove_these <- rep(NA, ncol(df))
  for (j in 1:ncol(df)){
    remove_these[j] <-
      all(is.na(df[,j]))
  }
  df <- df[,!remove_these]
  assign(gsub('_arm_1', '', arms[i]),
         df)
}
rm(df)

# Combine both acds
acd <- bind_rows(acd, 
             acd_children)

# Remove duplicates
acd <- acd[!duplicated(acd$perm_id),]
rm(acd_children)

