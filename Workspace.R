#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# Coder: James Maze
# Date: 21 Jan 2021
# Purpose: Plot and analyze Jackson Lane SC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Notes:
#     - For the Fall 2020 files, I launched sensors only to measure full range. 
#       Unfortunately low range is more appropriate for our sites. 

# 1. Libraries and workspace ----------------------------------------------

remove(list = ls())

library(xts)
library(dygraphs)
library(purrr)
library(lubridate)
library(stringr)
library(tidyverse)
library(stringr)

data_dir <- "data/JL/"


# 2. Read in the files ----------------------------------------------------

files <- list.files(paste0(data_dir), full.names = TRUE, pattern = ".csv")

#Download this mess
data <- files %>% 
  map(download_hobo) 

#Mush the tibbles together
data_comb <- data %>% 
  reduce(bind_rows)

#Rename cols to something manageable
colnames(data_comb) <- c("record_num", 
                         "Timestamp", 
                         "Full_range", 
                         "Temp_C", 
                         "file", 
                         "Site_ID", 
                         "serial_number",
                         "time_zone",
                         "Low_range")


# 3. Reformat the data ---------------------------------------------------------------------








