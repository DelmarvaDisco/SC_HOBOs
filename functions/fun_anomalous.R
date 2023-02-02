#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Anomaly Remover SC data
#Coder: James Maze (jtmaze@umd.edu)
#Date: 3/3/2022
#Purpose: To remove unusual/low values from Hobo SCs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function ----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fun_anomalous <- function(temp, #timeseries data with anomalous values
                          min, #The minimum threshold for residuals from rolling median 
                          max #The maximum threshold for residuals from rolling median
){
  #load packages
  library(tidyverse)
  library(zoo)
  
  #Check values against the residuals of a rolling median. 
  temp <- temp %>% 
    #Create rolling median besed on the 11 nearest values. 5 on either side.
    mutate("rolling_median" = rollmedian(temp$Low_range_uScm,
                                         k = 11,
                                         fill = NA, 
                                         align = "center")) %>% 
    #Calculate the difference between observations and rolling median
    mutate("residuals" = Low_range_uScm - rolling_median) %>% 
    #Eliminate data with residuals above or below thresholds
    #!!! You may need to tinker with these values !!!
    filter(residuals > min) %>% 
    filter(residuals < max) %>% 
    select(-c(rolling_median, residuals))

  #return the data without anomalous values, which didn't meet residual thresholds
  
  return(temp)
  
}
