# https://sap.manhica.net:4703/redcap/

library(tidyverse)
library(readr)
library(yaml)

# Read in old data
master <- read_csv('data/cost-mopeia.csv')
# headers <- read_csv('data/COSTMopeia_DATA_2017-03-24_1021.csv')[0,]

# Read in new data
cost_acd <- read_csv('data/COST_ACD.csv')
cost_acd_followup <- read_csv('data/COST_ACD_FOLLOWUP.csv')
cost_acd_followup2 <- read_csv('data/COST_ACD_FOLLOWUPv062017.csv')
cost_pcd <- read_csv('data/COST_PCDV062017.csv')
all_names <- c(names(cost_acd),
               names(cost_acd_followup),
               names(cost_acd_followup2),
               names(cost_pcd))


# Read an old file just to get headers
# headers <- read_csv('data/COSTMopeia_DATA_2017-03-08_1751.csv')[0,]

# # See what recruitment data we already have
# files <- dir('data/')
# files <- files[grepl('NOHDRS', files)]
# file_dates <- as.Date(substr(files, 24, 33))
# file_times <- as.numeric(substr(files, 35, 38))
# 
# # Get most recent file
# the_date <- max(file_dates)
# 
# # Retrieve recent data if old
# # Make sure to be in the zambezia virtualenv first
# 
# if(the_date < Sys.Date()){
#   system('python get_new_data.py')
#   # Re-look at the files
#   files <- dir('data/')
#   files <- files[grepl('NOHDRS', files)]
#   file_dates <- as.Date(substr(files, 24, 33))
#   file_times <- as.numeric(substr(files, 35, 38))
# }
# 
# the_file <- files[file_dates == the_date]
# 
# if(length(the_file) > 1){
#   the_file_times <- file_times[file_dates == max(file_dates)]
#   the_file <- the_file[the_file_times == max(the_file_times)]
# }
# the_file <- the_file[1]
# 
# # Read  
# master <- read_csv(paste0('data/',
#                           the_file),
#                    col_names = FALSE)
# names(master)[1:ncol(headers)] <- names(headers); rm(headers)
no_name <- length(which(is.na(names(master))))

# Read in geogrpahic and demographic data
master_table <- read_csv("~/Documents/zambezia/data/master_table_for_carlos.csv")

# Get spray status
ss <- master_table %>% 
  dplyr::filter(!is.na(status)) %>%
  dplyr::group_by(village_number) %>%
  dplyr::summarise(spray_status = dplyr::first(status)) 
ss <- ss %>%
  dplyr::mutate(spray_status = ifelse(spray_status, 'Spray', 'No spray'))

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
# Remove acd_arm_1 (there's only 1)
# arms <- arms[arms != 'acd_arm_1']
for (i in 1:length(arms)){
  df <- master %>% filter(redcap_event_name == arms[i])
  remove_these <- rep(NA, ncol(df))
  for (j in 1:ncol(df)){
    remove_these[j] <-
      (length(which(is.na(df[,j]))) / nrow(df)) >= 0.99
  }
  df <- df[,!remove_these]
  assign(gsub('_arm_1', '', arms[i]),
         df)
}
rm(df)

# READ IN NEW DATA

# Get agregardos 
agregados <- master %>%
  filter(!is.na(village)) %>%
  dplyr::select(record_id, village) %>%
  filter(!duplicated(record_id))

# Get visit dates
right <- master %>%
  filter(!is.na(acd_visit_date))

# Combine both acds
acd <- full_join(acd, acd_children,
               by = c('record_id' = 'parent_uri'))
# acd <- bind_rows(acd,
#              acd_children)

# Remove duplicates
acd <- acd[!duplicated(acd$perm_id),]
rm(acd_children)

# # Get into acd the agregado
# acd <- left_join(acd,
#                  agregados,
#                  by = c('parent_uri' = 'record_id'))

