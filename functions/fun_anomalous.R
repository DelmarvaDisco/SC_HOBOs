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
    mutate("rolling_median" = rollmedian(temp$Low_range_uScm,
                                         k = 11,
                                         fill = NA, 
                                         align = "center")) %>% 
    
    mutate("residuals" = Low_range_uScm - rolling_median) %>% 
    filter(residuals > min) %>% 
    filter(residuals < max)

  
  #return the df without anomalous values
  return(temp)
}
