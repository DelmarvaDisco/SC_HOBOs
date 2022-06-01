#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Hobo SC download
# Coder: James Maze with help from Nate Jone's waterLevel
# Date: 21 Jan 2021
# Purpose: Download lots of SC files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

download_hobo <- function(files){
  
  temp <- read_csv(paste0(files),
                   skip = 1,
                   col_names = TRUE) %>% 
    as_tibble() %>% 
    # Only select important cols, forget that coupler attached stuff. 
    #Also elminate Full Range measurements
    select(!contains(c("Coupler", "Host", "End of", "Stopped", "Full Range"))) %>% 
    #Add a column to delineate JL from BC
    mutate(Catchment = str_sub(files, 6, 7)) %>% 
    # Add a column for the file name; could be helpful for data mgmt.
    mutate(file = str_sub(files, 9)) %>% 
    # Get the Site_ID from the file name
    mutate(Site_ID = str_sub(file, 1, 5))
  
  #"Borrowed" this code from Nate Jones
  serial_number <- colnames(temp)[grep("LGR",colnames(temp))][1] %>% 
    str_extract(., pattern = "\\d{8,10}")
  
  time_zone <- colnames(temp)[grep("GMT",colnames(temp))]  %>% 
    str_sub(12, 20)
  
  #This is an ugly fix, but I couldn't just name columns bc files had different column numbers
  cols <- colnames(temp)
  colzzz <- str_trunc(cols, width = 7, side = "right", ellipsis = "_")
  colnames(temp) <- colzzz

  #Add columns to denote sn and tz
  temp <- temp %>% 
    add_column(serial_number) %>% 
    add_column(time_zone)
  
  
}
    
