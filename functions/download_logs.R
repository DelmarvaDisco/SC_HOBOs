#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Download Field Logs
#Coder: James Maze (jtmaze@umd.edu)
#Date: 6/7/2022
#Purpose: Quickly download field data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function ----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

download_logs <- function(Field_logs){
  
  temp <- read_excel(paste0(Field_logs),
                     na = "NA",
                     range = cell_cols("A:N"),
                     col_types = c("text", "list", "date", "text", "numeric",
                                   "numeric", "numeric", "numeric", "numeric",
                                   "text", "text", "text", "text", "text")) %>% 
    as_tibble() %>% 
    select(c("Site_ID", "Date", "Time", "Spec. Cond. (us/cm)", 
             "Temp (Deg. C)", "Field_Flag (0/1)", "Field_Flag_Notes")) %>%
    filter(!is.na(Date)) %>% 
    mutate(Time = str_sub(Time, 11, 19)) %>% 
    mutate(Year = str_sub(Date, 1, 4),
           Month = str_sub(Date, 5, 6), 
           Day = str_sub(Date, 7, 8)) %>% 
    mutate(Date = ymd(paste0(Year, "-", Month, "-", Day))) %>% 
    mutate(Timestamp = ymd_hms(paste0(Date, Time))) %>% 
    rename("SpC_field" = `Spec. Cond. (us/cm)`,
           "Temp_field" = `Temp (Deg. C)`) %>% 
    select(-c("Year", "Month", "Day", "Date", "Time"))

  (temp)
}
