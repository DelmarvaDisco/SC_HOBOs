#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# Coder: James Maze
# Date: 21 Jan 2022
# Purpose: Plot and analyze Jackson Lane SC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#   Notes:
#     - Lots of cleaning up shitty values!
#     - Problems with ND-SW in Fall 2021, too close to metal t-post??
#     - Screwed up launch of TS-SW in Spring 2021, no data
#     - Weird drops in CH and UW sites

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
source("functions/fun_anomalous.R")

data_dir <- "data/"

# 2. Read in the files ----------------------------------------------------


# 2a. Read the JL data ----------------------------------------------------

files <- list.files(paste0(data_dir, "JL/"), full.names = TRUE, pattern = ".csv")

#Download this mess
data_JL <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

# 2b. Read the BC data ----------------------------------------------------

files <- list.files(paste0(data_dir, "BC/"), full.names = TRUE, pattern = ".csv")

#Download
data_BC <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

# 3. Reformat the data ---------------------------------------------------------------------

df <- rbind(data_JL, data_BC) 

list_JL <- unique(data_JL$Site_ID)

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
  filter(!is.na(Temp_C)) %>% 
  filter(!is.na(Timestamp)) %>% 
  filter(!is.na(Low_range_uScm)) %>% 
  mutate("SpC_low_range" = Low_range_uScm/(1 - ((25 - Temp_C) * 0.021))) #%>% 
  #filter(Low_range_uScm >= 10)

rm(data_dir, data_BC, data_JL)

# 4. Cut the crappy values --------------------------------------------------------------------

# 4.1 DK_SW ---------------------------------------------------------------
Site <- "DK_SW"

temp <- df %>% 
  filter(Site_ID == Site)

#Look at plotted values
prelim_plot(temp)

#Filter out the values at the beginning & end of deployments
temp <- temp %>% 
  filter(Timestamp >= "2021-04-17 19:30:00") %>% 
  filter(Timestamp <= "2021-08-05 16:30:00" | Timestamp >= "2021-09-24 11:30:00") %>% 
  filter(Timestamp <= "2021-12-13 16:00:00" | Timestamp >= "2022-02-22 11:00:00") %>% 
  filter(Timestamp <= "2022-04-28 16:30:00")

#Filter out anomalous values
temp <- fun_anomalous(temp, min = -1, max = 1)

prelim_plot(temp)

output <- temp

rm(temp, Site)

# 4.2 ND_SW -------------------------------------------------------------------

Site <- "ND_SW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-05-21 16:00:00") %>% 
  filter(Timestamp <= "2021-08-05 15:30:00" | Timestamp >= "2021-09-24 10:45:00") %>%
  filter(Timestamp <= "2021-10-31 9:30:00" | Timestamp >= "2021-11-1 11:00:00") %>%
  filter(Timestamp <= "2021-12-14 7:00:00" | Timestamp >= "2022-02-22 10:00:00") %>% 
  filter(Timestamp <= "2022-04-30 8:00:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.3 TS_SW -------------------------------------------------------------------
Site <- "TS_SW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 10:00:00") %>% 
  filter(Timestamp <= "2021-10-20 7:00:00" | Timestamp >= "2021-10-26 13:00:00") %>%
  filter(Timestamp <= "2021-10-24 7:30:00" | Timestamp >= "2021-10-26 1:00:00") %>%
  filter(Timestamp <= "2021-10-29 16:30:00" | Timestamp >= "2021-10-29 21:00:00") %>%
  filter(Timestamp <= "2021-11-24 3:30:00" | Timestamp >= "2022-02-22 11:00:00") %>% 
  filter(Timestamp <= "2022-04-22 16:00:00")  

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)


# 4.4 BD_CH -------------------------------------------------------------------
Site <- "BD_CH"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 10:00:00")


# 4.5 TS_CH -------------------------------------------------------------------







