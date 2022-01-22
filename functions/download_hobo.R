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
    as.tibble() %>% 
    # Only select important cols, forget that couple attached stuff
    select(!contains(c("Coupler", "Host", "End of", "Stopped"))) %>% 
    mutate(Catchment = str_sub(files, 6, 7)) %>% 
    # Add a column for the file name
    mutate(file = str_sub(files, 9)) %>% 
    # Get the site from the file name
    mutate(Site_ID = str_sub(file, 1, 5))
  
  #"Borrowed" this code from Nate Jones
  serial_number <- colnames(temp)[grep("LGR",colnames(temp))][1]
  time_zone <- colnames(temp)[grep("GMT",colnames(temp))] 
  
  #This is an ugly fix, but I couldn't just name columns bc files had different column numbers
  cols <- colnames(temp)
  colzzz <- str_trunc(cols, width = 7, side = "right", ellipsis = "_")
  colnames(temp) <- colzzz

  #Add columns to denote sn and tz
  temp <- temp %>% 
    add_column(serial_number) %>% 
    add_column(time_zone)
  
  
}
    
