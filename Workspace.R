#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# Coder: James Maze
# Date: 21 Jan 2021
# Purpose: Plot and analyze Jackson Lane SC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Notes:
#     - For the Fall 2020 files, I launched sensors only to measure full range. 
#       Unfortunately low range is more appropriate for our sites. Even more Unfortunately,
#       the column names are all goofed up. Might just want to read those files separately

# 1. Libraries and workspace ----------------------------------------------

remove(list = ls())

library(xts)
library(dygraphs)
library(purrr)
library(lubridate)
library(stringr)
library(tidyverse)
library(stringr)

source("functions/download_hobo.R")


# 2. Read in the files ----------------------------------------------------


# 2a. Read the JL data ----------------------------------------------------

data_dir <- "data/JL/"

files <- list.files(paste0(data_dir), full.names = TRUE, pattern = ".csv")

#Download this mess
data_JL <- files %>% 
  map(download_hobo) 

#Mush the tibbles together
data_JL <- data_JL %>% 
  reduce(bind_rows)


# 2b. Read the BC data ----------------------------------------------------

data_dir <- "data/BC/"

files <- list.files(paste0(data_dir), full.names = TRUE, pattern = ".csv")

#Download
data_BC <- files %>% 
  map(download_hobo) 

#Mush the tibbles together
data_BC <- data_BC %>% 
  reduce(bind_rows)


# 3. Reformat the data ---------------------------------------------------------------------

#Rename cols to something manageable
colnames(data_JL) <- c("record_num", 
                    "Timestamp", 
                    "Full_range", 
                    "Temp_C", 
                    "Catchment",
                    "file", 
                    "Site_ID", 
                    "serial_number",
                    "time_zone",
                    "Low_range")

colnames(data_BC) <- c("record_num", 
                       "Timestamp", 
                       "Low_range",
                       "Full_range", 
                       "Temp_C", 
                       "Catchment",
                       "file", 
                       "Site_ID", 
                       "serial_number",
                       "time_zone")

data <- rbind(data_JL, data_BC) %>% 
  mutate(Timestamp = mdy_hms(Timestamp))

rm(data_BC, data_JL, data_dir)

# 4. Check out a big dygraph and make a raw csv --------------------------------------------------------------

data_xts <- data %>% 
  select(c(Timestamp, Site_ID, Full_range)) %>% 
  pivot_wider(names_from = Site_ID, values_from = Full_range)

data_xts <- xts(data_xts, order.by = data_xts$Timestamp)

(big_dygraph <- dygraph(data = data_xts, main = "All the SC data") %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
  dyRangeSelector())

data_dir <- "data/"

write_csv(data, file = paste0(data_dir,"sites_raw.csv"))

# 5. --------------------------------------------------------------------









