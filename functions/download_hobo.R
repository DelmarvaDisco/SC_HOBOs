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
  
  #Pull serial number from column names
  serial_number <- colnames(temp)[grep("LGR",colnames(temp))][1] %>% 
    str_extract(., pattern = "\\d{8,10}")
  
  #Pull the timezone from column names
  time_zone <- colnames(temp)[grep("GMT",colnames(temp))]  %>% 
    str_sub(12, 20)
  
  #Specifiy the timezones with syntax for lubridate
  tz <- if_else(time_zone=="GMT-04:00",
                "America/New_York",
                if_else(time_zone=="GMT-05:00",
                        "America/Chicago",
                        "-9999"))
  
  #Since the Serial Number is included, each file has different column names. 
  # Here's a quick and dirty fix
  #1. Pull the original column names
  columns_original <- colnames(temp)
  #2. Truncate the column names, removing Serial Number. Makes them uniform across files.
  columns_output <- str_trunc(columns_original, width = 7, side = "right", ellipsis = "_")
  #3. Apply the truncated col names to temp
  colnames(temp) <- columns_output
  

  #Spit temp out into the world
  temp <- temp %>% 
    #Add columns
    add_column(serial_number) %>% 
    add_column(time_zone) %>% 
    #Convert Timestamp from char to datetime
    mutate(Timestamp = mdy_hms(`Date T_`, tz = tz)) %>% 
    select(-c(`Date T_`))
  
}
    
