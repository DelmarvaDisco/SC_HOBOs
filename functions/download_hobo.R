#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Hobo SC download
# Coder: James Maze with help from Nate Jone's waterLevel code
# Date: 21 Jan 2021
# Purpose: Download lots of SC files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

download_hobo <- function(files){
  
  temp <- read_csv(paste0(files),
                   skip = 1,
                   col_names = TRUE) %>% 
    as_tibble() %>% 
    # Only select important columns, forget the coupler attached column etc 
    #Also eliminate Full Range measurements. Our sites are all Low Range
    select(!contains(c("Coupler", "Host", "End", "Stopped", "Full Range"))) %>% 
    #Add a column to delineate JL from BC
    mutate(Catchment = str_sub(files, 6, 7)) %>% 
    # Add a column for the file name; could be helpful for data mgmt.
    mutate(file = str_sub(files, 9)) %>% 
    # Get the Site_ID from the file name
    # Use if_else because there's different numbers of characters for UW vs. SW 
    # sites
    mutate(Site_ID = if_else(str_detect(file, "UW"),
                             str_sub(file, 1, 6),
                             str_sub(file, 1, 5))) 
  
  #Pull the sensor serial number from column names
  serial_number <- colnames(temp)[grep("LGR",colnames(temp))][1] %>% 
    str_extract(., pattern = "\\d{8,10}")
  
  #Pull the timezone from column names
  time_zone <- colnames(temp)[grep("GMT",colnames(temp))]  %>% 
    str_sub(12, 20)
  
  #Specify the timezones with proper syntax for lubridate. 
  tz <- if_else(time_zone=="GMT-04:00",
                "America/New_York",
                if_else(time_zone=="GMT-05:00",
                        "America/Chicago",
                        "-9999"))
  
  #Since the Serial Number is included, each file has different column names. 
  # Here's a quick and dirty fix for binding the rows.
  #1. Pull the original column names
  columns_original <- colnames(temp)
  #2. Truncate the column names, removing Serial Number. Makes cols uniform across the different files.
  columns_output <- str_trunc(columns_original, width = 7, side = "right", ellipsis = "_")
  #3. Use the truncated col names to rename the data's columns
  colnames(temp) <- columns_output
  
  #Final reformatting to tidy up files
  temp <- temp %>% 
    #Add columns for SN and TZ
    add_column(serial_number) %>% 
    add_column(time_zone) %>% 
    #Convert Timestamp from char to datetime
    mutate(Timestamp = lubridate::mdy_hms(`Date T_`, tz = tz)) %>% 
    #Change the Site_ID to right format. Replace the underscore with a dash.
    mutate(Site_ID = str_replace(Site_ID, "_", "-")) %>% 
    select(-c(`Date T_`))
  
}
    
