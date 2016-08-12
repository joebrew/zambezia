#' Get Zambezia census data from the database
#' 
#' Get Zambezia census data from the database
#' @param credentials_yaml The path to the credentials.yaml where CISM database 
#' connection credentials are stored
#' @param on_site Whether you are accessing the data from within CISM (defaults to \code{FALSE})
#' is to assign to the global environment
#' @param get_fresh Whether to get the data fresh; alternative is to use the 
#' most recent snapshot
#' @param save Whether to save an \code{.Rdata} for each table, after retrieivng it

get_zambezia <- function(credentials_yaml = 'credentials.yaml',
                         on_site = FALSE,
                         get_fresh = FALSE,
                         save = FALSE){
  
  #Libraries
  require(RMySQL)
  require(dplyr)
  require(DBI)
  require(yaml)

  if(get_fresh){
    # Get stuff from config
    connection_options <- yaml.load_file(credentials_yaml)
    
    # Modify the connection string if on site
    if(on_site){
      connection_options$port <- 3306 
    }
    
    # Open connection using dplyr
    con <- do.call('src_mysql', connection_options)
    
    # Tables to read
    tables <- src_tbls(con)
  } else {
    tables <- c("CENSUS_MOPEIA_CHILDREN_INFO",
                "CENSUS_MOPEIA_CORE" )
  }
  
  # Read them all in
  for (i in 1:length(tables)){
    # Define the name for this table
    table_name <- tables[i]
    # Define a file name for saving a snapshot
    file_name <- paste0('snapshots/',
                        Sys.Date(),
                        '_',
                        table_name,
                        '.RData')
    if(get_fresh){
      message(paste0('Reading from the ',
                     connection_options$dbname,
                     ' database: ', 
                     table_name))
      assign(tables[i],
             tbl(con,
                 tables[i]) %>%
               collect,
             envir = .GlobalEnv)
    } else {
      # Not getting fresh - just reading in newest snapshot
      snapshots <- dir('snapshots')
      snapshots <- snapshots[grepl(table_name, snapshots)]
      dates <- as.Date(substr(snapshots, 1, 10))
      if(length(dates) < 1){
        stop(paste0('No saved data in snapshots. Set get_fresh to TRUE.'))
      } else {
        message(paste0('Reading in data which was retrieved on ',
                       max(dates)))
      }
      read_these <- which(dates == max(dates))
      for (r in read_these){
        load(paste0('snapshots/', snapshots[r]),
             envir = .GlobalEnv)
      }
    }
    # Save daily snapshot
    if(save){
      save(list = table_name, 
           file = file_name)
    }
  }
}

  