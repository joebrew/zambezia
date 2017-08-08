library(tidyverse)
load('census.RData')


Peco a lista de agregados familires de mopeia de forma descriminada com as seguintes variaveis:
  
  Administrative post
locality
village_number
local_village_name
village_name
Housenofinal
head_name
keeper

x = census %>%
  dplyr::select(administrative_post,
                locality_Final,
                village,
                local_village_name,
                other_village,
                Housenofinal,
                head_name,
                keeper)
