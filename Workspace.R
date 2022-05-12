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
#     -second try

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

data <- rbind(data_JL, data_BC) 

colnames(data) <- c("record_num", 
                   "Timestamp", 
                   "Low_range", 
                   "Temp_C", 
                   "Catchment",
                   "file", 
                   "Site_ID", 
                   "serial_number",
                   "time_zone")

data <- data %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>% 
  mutate(Low_range = as.numeric(Low_range)) %>% 
  mutate("SpC_low_range" = Low_range/(1 - ((25 - Temp_C) * 0.021))) %>% 
  #mutate("SpC_full_range" = Full_range/(1 - ((25 - Temp_C) * 0.021))) %>% 
  filter(Low_range >= 10)

rm(data_dir, data_BC, data_JL)

# 4. Check out a big dygraph and make a raw csv --------------------------------------------------------------

data_wide <- data %>% 
  #filter(Catchment == "BC") %>% 
  select(c(Timestamp, Site_ID, SpC_low_range)) %>% 
  pivot_wider(names_from = Site_ID, values_from = SpC_low_range)

data_xts <- xts(data_wide, order.by = data_wide$Timestamp)

#ALL the data on a dygraph
(big_dygraph <- dygraph(data = data_xts, main = "All Surface data") %>% 
  dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
  dyRangeSelector())


# 5. Cut the crappy values --------------------------------------------------------------------


# 5a. DK-SW Fall 2020 --------------------------------------------------------------------

#Only have the low range values

SiteName <- "DK_SW"

df <- data %>% 
  filter(Site_ID == SiteName) %>% 
  filter(Timestamp <= "2020-12-18 12:00:00") %>% 
  select(Timestamp, Temp_C, Full_range)

prelim_plot(df)



# 5b. TS-SW Fall 2020 ---------------------------------------------------------------------

#Only have the low range values


# 5c. ND-SW Fall 2020 ---------------------------------------------------------------

# I. Baltimore Corner SW Data on a dygraph -----------------------------------------------------------------------

data_wide <- data %>% 
  #filter(Catchment == "BC") %>% 
  select(c(Timestamp, Site_ID, SpC_low_range)) %>% 
  pivot_wider(names_from = Site_ID, values_from = SpC_low_range)

data_xts_BC_SW <- data_wide %>% 
  select(c("HB_SW", "MB_SW", "OB_SW", "XB_SW", "Timestamp")) 

data_xts_BC_SW <- xts(data_xts_BC_SW, order.by = data_xts_BC_SW$Timestamp)

(bc_dygraph <- dygraph(data = data_xts_BC_SW, main = "BC data") %>% 
    dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
    dyRangeSelector())

#ggplot version
data_BC_SW <- data %>% 
  filter(Site_ID %in% c("HB_SW", "MB_SW", "OB_SW", "XB_SW")) %>% 
  filter(SpC_low_range >= 22)

BC_SW_gg <- ggplot(data = data_BC_SW, 
                   mapping = aes(x = Timestamp, 
                                 y = SpC_low_range, 
                                 color = Site_ID)) + 
  geom_line() +
  theme_bw()

(BC_SW_gg)

rm(data_xts_BC_SW, bc_dygraph, data_BC_SW, BC_SW_gg, big_dygraph)

# II. XB-CH ----------------------------------------------------------------------

XB_CH <- data %>% 
  filter(Site_ID == "XB_CH") %>% 
  select(c(Timestamp, SpC_low_range))

XB_CH_xts <- xts(XB_CH, order.by = XB_CH$Timestamp)

(XB_CH_dygraph <- dygraph(data = XB_CH_xts, main = "XB CH SpC (uS/cm)") %>% 
    dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
    dyRangeSelector())


# III. Origin Bay ---------------------------------------------------------------------

OB_UW <- data %>% 
  filter(Site_ID == "OB_UW") %>% 
  select(c(Timestamp, SpC_low_range))

OB_UW_xts <- xts(OB_UW, order.by = OB_UW$Timestamp)

(OB_UW_dygraph <- dygraph(data = OB_UW_xts, main = "OB UW1 SpC (uS/cm)") %>% 
    dyOptions(drawPoints = TRUE, pointSize = 2) %>% 
    dyRangeSelector())






