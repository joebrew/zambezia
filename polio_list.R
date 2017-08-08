load('census.RData')

admin_dictionary <- data_frame(administrative_post = 1:3,
                               admin_post = c('Mopeia Sede',
                                              'Posto Campo',
                                      'Sambalendo'))
locality_dictionary <- data_frame(locality_Final = 1:12,
                                  location = c('Mopeia-Sede/Cua Cua',
                                               'Rovuma-Conho',
                                               'Sangalaza',
                                               'Mugrumba',
                                               'Catale',
                                               'Mungane',
                                               'Posto Campo Sede',
                                               'Lua Lua',
                                               'Sambalendo/Chimuara',
                                               'Nzanza',
                                               'Nzero',
                                               'Outro'))
census <- left_join(census, admin_dictionary)
census <- left_join(census, locality_dictionary)
x = census %>%
  dplyr::select(administrative_post,
                admin_post,
                locality_Final,
                location,
                village,
                local_village_name,
                house_number,
                head_name) %>%
  arrange(administrative_post,
          locality_Final,
          village,
          location)
write_csv(x, '~/Desktop/edgar.csv')

village_number_names <- 
  census %>%
  group_by(village_number, 
           village_name = local_village_name) %>%
  tally %>%
  arrange(desc(n)) %>%
  filter(!duplicated(village_number)) %>%
  dplyr::select(-n) %>%
  ungroup

x <- census %>%
  left_join(village_number_names) %>%
  left_join(admin_dictionary) %>%
  left_join(locality_dictionary) %>%
  group_by(admin_post,
           location,
           village_number) %>%
  summarise(village_name = first(village_name),
            latitude_centroid = mean(latitude),
            longitude_centroid = mean(longitude),
            n_children = sum(number_of_children),
            n_houses = sum(number_of_houses),
            n_residents = sum(number_of_residents))

write_csv(x, 'polio_list.csv')
