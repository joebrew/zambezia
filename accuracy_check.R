library(rmarkdown)
library(readxl)

# Read in Joe's tabulations
joe1 <- read_excel('Mopeia_Village Sumary and Alt Name Roster.xlsx', sheet = 1)
joe2 <- read_excel('Mopeia_Village Sumary and Alt Name Roster.xlsx', sheet = 2) 
# Fill in the blanks in joe2
for (i in 1:nrow(joe2)){
  message(i)
  
  # Village number final
  counter <- i
  village_number_final <- joe2$`Village Number Final`[i]
  while(is.na(village_number_final)){
    counter <- counter - 1
    village_number_final <- joe2$`Village Number Final`[counter]
  }
  joe2$`Village Number Final`[i] <- village_number_final
  
  # Status
  counter <- i
  status <- joe2$Status[i]
  while(is.na(status)){
    counter <- counter - 1
    status <- joe2$Status[counter]
  }
  joe2$Status[i] <- status
  
  # Cluster
  counter <- i
  cluster <- joe2$Cluster[i]
  while(is.na(cluster)){
    counter <- counter - 1
    cluster <- joe2$Cluster[counter]
  }
  joe2$Cluster[i] <- cluster
}

# Load the data from the final report
library(readr)
library(dplyr)
census <- read_csv('household_table.csv')
hh <- read_csv('chil')

# Ensure that Joe's table matches with spray statuses
check_this <-
  joe1 %>% 
  dplyr::select(`Village Number`, Status, Cluster) %>%
  left_join(
    census %>%
      group_by(village_number) %>%
      summarise(spray_status = dplyr::first(spray_status),
                cluster = dplyr::first(cluster)),
    by = c("Village Number" = 'village_number')
  )
ifelse(check_this$Status == 'No spray' &
             check_this$spray_status == 'NO SPRAY', TRUE,
           ifelse(check_this$Status == 'Spray' & 
                    check_this$spray_status == 'SPRAY', TRUE,
                  FALSE))
# Ensure it also matches with clusters
check_this$Cluster == check_this$cluster
potential_problems <- ifelse(check_this$Cluster == 'n/a' & !is.na(check_this$Cluster), 'problem', 'ok')
check_this[potential_problems == 'problem',]
# All ok ^


