#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# Coder: James Maze
# Date: 21 Jan 2022
# Purpose: Plot and analyze Jackson Lane SC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Notes:
#     - Lots of cleaning up shitty values!
#     - Need to adjust time-zones

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
source("functions/prelim_plot.R")


# 2. Read in the files ----------------------------------------------------


# 2a. Read the JL data ----------------------------------------------------

data_dir <- "data/JL/"

files <- list.files(paste0(data_dir), full.names = TRUE, pattern = ".csv")

#Download this mess
data_JL <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

# 2b. Read the BC data ----------------------------------------------------

data_dir <- "data/BC/"

files <- list.files(paste0(data_dir), full.names = TRUE, pattern = ".csv")

#Download
data_BC <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

# 3. Reformat the data ---------------------------------------------------------------------

df <- rbind(data_JL, data_BC) 

#Rename columns
colnames(df) <- c("record_num", 
                   "Low_range_uScm", 
                   "Temp_C", 
                   "Catchment",
                   "file", 
                   "Site_ID", 
                   "serial_number",
                   "time_zone",
                   "Timestamp")

#Reformat some columns
df <- df %>% 
  mutate(Low_range_uScm = as.numeric(Low_range_uScm)) %>%
  #Do temperature conversion for SpC
  mutate("SpC_low_range" = Low_range_uScm/(1 - ((25 - Temp_C) * 0.021))) #%>% 
  #filter(Low_range_uScm >= 10)

rm(data_dir, data_BC, data_JL)

# 4. Cut the crappy values --------------------------------------------------------------------

# 4.1 DK_SW ---------------------------------------------------------------

temp <- df %>% 
  filter(Site_ID == "DK_SW")


prelim_plot(temp)


