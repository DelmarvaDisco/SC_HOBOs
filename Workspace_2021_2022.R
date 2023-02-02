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
library(readxl)
library(dygraphs)
library(tidyverse)

source("functions/download_hobo.R")
source("functions/prelim_plot.R")
source("functions/fun_anomalous.R")
source("functions/download_logs.R")
source("functions/comp_plot.R")

data_dir <- "data/"

# 2. Read in the files ----------------------------------------------------

# 2.1 Read the JL data ----------------------------------------------------

#Select JL files from data directory
files <- list.files(paste0(data_dir, "JL/"), full.names = TRUE, pattern = ".csv")

#Download and combine files
data_JL <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

#Clean up environment
rm(files)

# 2.2 Read the BC data ----------------------------------------------------

#Select BC files from data directory
files <- list.files(paste0(data_dir, "BC/"), full.names = TRUE, pattern = ".csv")

#Download and combine files
data_BC <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

#Clean up
rm(files)

# 3. Reformat the data ---------------------------------------------------------------------

#Combine the sepperate catchments
df <- rbind(data_JL, data_BC) 

#List the Site IDs, helpful later?
list_JL <- unique(data_JL$Site_ID)
list_BC <- unique(data_BC$Site_ID)

#Rename columns making them easier to work with. 
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
  #there's some wonky na's for Timestamp, Temp_C, and Low_rang_uScm
  filter(!is.na(Temp_C)) %>%
  filter(!is.na(Timestamp)) %>%
  filter(!is.na(Low_range_uScm)) %>%
  #Do temperature conversion to get SpC from Cond
  mutate("SpC_low_range" = Low_range_uScm/(1 - ((25 - Temp_C) * 0.021))) #%>% 
  #filter(Low_range_uScm >= 10)

rm(data_BC, data_JL)

# 4. Cut the crappy values for each site --------------------------------------------------------------------

# 4.1 DK-SW ---------------------------------------------------------------

Site <- "DK-SW"

raw <- df %>% 
  filter(Site_ID == Site)

#Look at plotted values
prelim_plot(raw)

#Filter out the values at the beginning & end of deployments
temp <- raw %>% 
  #Spring 2021 Deployment Start
  filter(Timestamp >= "2021-04-17 19:30:00") %>% 
  #Site dried out in summer 2021
  filter(Timestamp <= "2021-08-05 16:30:00" | Timestamp >= "2021-09-24 11:30:00") %>% 
  #Sensor pulled for winter 2022
  filter(Timestamp <= "2021-12-13 16:00:00" | Timestamp >= "2022-02-22 11:00:00") %>% 
  #Site dried out in summer 2022
  filter(Timestamp <= "2022-07-25 18:00:00" | Timestamp >= "2022-10-03 1:00:00") %>% 
  #Intermittently dry in Oct 2022
  filter(Timestamp <= "2022-10-19 2:15:00" | Timestamp >= "2022-10-24 6:15:00") %>% 
  #Intermittently dry in Nov 2022
  filter(Timestamp <= "2022-10-28 3:15:00" | Timestamp >= "2022-11-01 21:15:00") %>% 
  #Intermittently dry in Nov 2022 
  filter(Timestamp <= "2022-11-07 2:15:00" | Timestamp >= "2022-11-11 21:15:00") %>% 
  #Intermittently dry in Nov 2022 
  filter(Timestamp <= "2022-11-22 4:15:00" | Timestamp >= "2022-11-27 15:45:00") %>% 
  #End fall 2022 deployment
  filter(Timestamp <= "2022-12-16 3:30:00")

#Filter out anomalous values
temp <- fun_anomalous(temp, min = -1, max = 1)

#Plot again for another look
prelim_plot(temp)

#Compare the raw data to the processed data
comp_plot(raw, temp)

#Add flags to data with mucky sensor and low water levels. 
temp <- temp %>% 
  mutate(Flag = if_else(Timestamp >= "2022-10-03 1:00:00" & Timestamp <= "2022-11-17 20:15:00",
                        "1", 
                        "0"),
         Notes = if_else(Timestamp >= "2022-10-03 1:00:00" & Timestamp <= "2022-11-17 20:15:00",
                         "Site dry and sensor mucky, data quality low. SpC values too high.",
                         "NA"))

#Create output file for processed data. 
output <- temp

#Clean up environment
rm(temp, Site, raw)

# 4.2 ND-SW -------------------------------------------------------------------

#Same workflow, if functions or syntax is confusing see comments for DK-SW

Site <- "ND-SW"

raw <- df %>% 
  filter(Site_ID == Site)

prelim_plot(raw)

temp <- raw %>% 
  #Spring 2021 deployment start
  filter(Timestamp >= "2021-05-21 16:00:00") %>% 
  #Site dried out over Summer 2021
  filter(Timestamp <= "2021-08-05 15:30:00" | Timestamp >= "2021-09-24 10:45:00") %>%
  #A few days in Fall 2021 with bad data
  filter(Timestamp <= "2021-10-31 9:30:00" | Timestamp >= "2021-11-1 11:00:00") %>%
  #Sensor pulled for Winter 2022-2023
  filter(Timestamp <= "2021-12-14 7:00:00" | Timestamp >= "2022-02-22 10:00:00") %>% 
  #Dried in Summer 2022
  filter(Timestamp <= "2022-08-18 23:00:00" | Timestamp >= "2022-10-04 21:30:00") %>%
  #Intermittently dry Fall 2022
  filter(Timestamp <= "2022-10-08 18:30:00" | Timestamp >= "2022-10-10 14:30:00") %>% 
  ##Intermittently dry Fall 2022
  filter(Timestamp <= "2022-10-10 18:45:00" | Timestamp >= "2022-12-03 8:45:00") %>% 
  #filter(Ti)
  #Pulled for winter 2022-2023
  filter(Timestamp <= "2022-12-16 14:00:00")

#Anomalous values
temp <- fun_anomalous(temp, min = -1, max = 1)

#Another look at data
prelim_plot(temp)

#Compare the raw data to the processed data
comp_plot(raw, temp)

#Add flags
# temp <- temp %>% 
#   mutate(Flag = if_else(Timestamp >= "2022-08-01 1:00:00",
#                         "1", 
#                         "0"),
#          Notes = if_else(Timestamp >= "2022-08-01 1:00:00",
#                          "Site dry and sensor mucky, data quality low. SpC values too high.",
#                          "NA"))

#Not flagging data for now
temp <- temp %>%
  mutate(Flag = "0",
         Notes = "NA")

#Append data to output
output <- rbind(output, temp)

rm(temp, Site, raw)

# 4.3 TS-SW -------------------------------------------------------------------

Site <- "TS-SW"

raw <- df %>% 
  filter(Site_ID == Site)

prelim_plot(raw)

temp <- raw %>% 
  filter(Timestamp >= "2021-09-24 10:00:00") %>% 
  filter(Timestamp <= "2021-10-20 7:00:00" | Timestamp >= "2021-10-26 13:00:00") %>%
  filter(Timestamp <= "2021-10-24 7:30:00" | Timestamp >= "2021-10-26 1:00:00") %>%
  filter(Timestamp <= "2021-10-29 16:30:00" | Timestamp >= "2021-10-29 21:00:00") %>%
  filter(Timestamp <= "2021-11-24 3:30:00" | Timestamp >= "2022-02-22 11:00:00") %>% 
  filter(Timestamp <= "2022-06-13 10:00:00" | Timestamp >= "2022-12-15 21:15:00") %>% 
  filter(Timestamp <= "2022-12-16 13:30:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

#Another look at data
prelim_plot(temp)

#Compare the raw data to the processed data
comp_plot(raw, temp)

#No data to flag
temp <- temp %>%
  mutate(Flag = "0",
         Notes = "NA")

#Append to output file
output <- rbind(output, temp)

rm(temp, Site, raw)

# 4.4 BD-CH -------------------------------------------------------------------

Site <- "BD-CH"

raw <- df %>% 
  filter(Site_ID == Site)

prelim_plot(raw)

temp <- raw %>% 
  #Deployment started in Fall 2021
  filter(Timestamp >= "2021-09-24 10:00:00") %>% 
  #Data bad until well purge on Oct 18th 2021
  filter(Timestamp >= "2021-10-19 15:00:00") %>% 
  #Deployment ends Dec 2022
  filter(Timestamp <= "2022-12-16 12:30:00") %>% 
  #!!! A quick and easy way to eliminate low values when well was dry or sensor was removed. 
  filter(Low_range_uScm > 10)

temp <- fun_anomalous(temp, min = -1, max = 1)

#Another look at data
prelim_plot(temp)

#Compare the raw data to the processed data
comp_plot(raw, temp)

#Data is most likely worthless. 
temp <- temp %>%
  mutate(Flag = "2",
         Notes = "Data is likely unusable. Signal is obscured by dissolved stuff accumulating in the well between purges.")

output <- rbind(output, temp)

rm(temp, Site)

# 4.5 TS-CH -------------------------------------------------------------------

Site <- "TS-CH"

raw <- df %>% 
  filter(Site_ID == Site)

prelim_plot(raw)

temp <- raw %>% 
  #Sensor initially launched Fall 2021
  filter(Timestamp >= "2021-09-24 10:00:00") %>% 
  #Sensor initially pulled December 2022
  filter(Timestamp <= "2022-12-16 13:30:00") %>% 
  #!!! A quick and easy way to eliminate low values when well was dry or sensor was removed. 
  filter(Low_range_uScm > 10)

temp <- fun_anomalous(temp, min = -1, max = 1)

#Another look after cleaning
prelim_plot(temp)

#Compare the raw data to the processed data
comp_plot(raw, temp)

#Flagging all well data. Probably not usefull, except for specifc things. 
temp <- temp %>%
  mutate(Flag = "2",
         Notes = "Data is likely unusable. Signal is obscured by sediment and dissolved solutes accumulating in the well between purges.")

output <- rbind(output, temp)

rm(temp, Site)

# 4.6 OB-SW ---------------------------------------------------------------

Site <- "OB-SW"

raw <- df %>% 
  filter(Site_ID == Site)

prelim_plot(raw)

temp <- raw %>% 
  #Deployed fall 2021
  filter(Timestamp >= "2021-09-24 12:00:00") %>%
  #Intermittently dry
  filter(Timestamp <= "2021-10-09 10:00:00" | Timestamp >= "2021-10-10 18:00:00") %>%
  #Intermittently dry
  filter(Timestamp <= "2021-10-11 2:00:00" | Timestamp >= "2021-10-26 6:30:00") %>%
  #Intermittently dry
  filter(Timestamp <= "2021-10-27 13:30:00" | Timestamp >= "2021-10-29 23:00:00") %>%
  #Pulled durring winter 2021-2022
  filter(Timestamp <= "2021-12-03 9:15:00" | Timestamp >= "2022-02-23 9:30:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-06-19 17:00:00" | Timestamp >= "2022-06-23 20:00:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-06-25 7:15:00" | Timestamp >= "2022-07-07 10:15:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-07-08 2:45:00" | Timestamp >= "2022-07-09 20:15:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-07-14 14:15:00" | Timestamp >= "2022-07-16 21:15:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-07-17 18:15:00" | Timestamp >= "2022-07-19 20:15:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-07-20 10:15:00" | Timestamp >= "2022-07-21 12:15:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-07-21 8:15:00" | Timestamp >= "2022-07-22 13:15:00")  %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-08-02 14:15:00" | Timestamp >= "2022-08-05 12:15:00") %>% 
  #Intermittently dry
  filter(Timestamp <= "2022-08-07 15:30:30" | Timestamp >= "2022-12-06 14:00:00") %>% 
  #Pulled for winter 
  filter(Timestamp <= "2022-12-14 13:00:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

#Another look at the data 
prelim_plot(temp) 

#Compare the raw data to the processed data
comp_plot(raw, temp)

#No data to flag
temp <- temp %>%
  mutate(Flag = "0",
         Notes = "NA")

#Append to output file
output <- rbind(output, temp)

rm(temp, Site, raw)

# 4.7 OB-UW1 -------------------------------------------------------------------

Site <- "OB-UW1"

raw <- df %>% 
  filter(Site_ID == Site)

prelim_plot(raw)

temp <- raw %>% 
  #Deployed in Fall 2021
  filter(Timestamp >= "2021-09-24 12:00:00") %>% 
  #Intermitently dry in fall 2021
  filter(Timestamp <= "2021-09-27 18:00:00" | Timestamp >= "2021-10-30 8:30:00") %>%
  #Intermitently dry in fall 2021 - winter 2022
  filter(Timestamp <= "2021-11-06 6:45:00" | Timestamp >= "2022-01-08 15:30:00") %>% 
  #Dry until deployment in winter 2022
  filter(Timestamp <= "2022-06-05 4:00:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

#Another look at the data 
prelim_plot(temp) 

#Compare the raw data to the processed data
comp_plot(raw, temp)

#Flagging all well data. Probably not usefull, except for specifc things. 
temp <- temp %>%
  mutate(Flag = "2",
         Notes = "Data is likely unusable. Signal is obscured by sediment and dissolved solutes accumulating in the well between purges.")

#Append to output file
output <- rbind(output, temp)

rm(temp, Site)

# 4.8 XB-SW ---------------------------------------------------------------

Site <- "XB-SW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 12:00:00") %>% 
  filter(Timestamp <= "2021-10-08 20:45:00" | Timestamp >= "2021-10-10 18:00:00") %>%
  filter(Timestamp <= "2021-10-11 7:45:00" | Timestamp >= "2021-10-13 15:30:00") %>%
  filter(Timestamp <= "2021-10-14 18:00:00" | Timestamp >= "2021-10-17 11:30:00") %>%
  filter(Timestamp <= "2021-10-17 17:45:00" | Timestamp >= "2021-10-26 11:30:00") %>%
  filter(Timestamp <= "2021-12-09 7:00:00" | Timestamp >= "2021-12-11 19:00:00") %>%
  filter(Timestamp <= "2021-12-15 10:00:00" | Timestamp >= "2022-02-23 15:45:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.9 XB-CH ---------------------------------------------------------------

Site <- "XB-CH"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 12:30:00") %>% 
  filter(Timestamp <= "2021-10-18 13:15:00" | Timestamp >= "2021-10-18 15:15:00") %>%
  filter(Timestamp <= "2021-12-15 8:00:00" | Timestamp >= "2021-12-15 13:30:00") %>% 
  filter(Timestamp <= "2022-02-23 10:15:00" | Timestamp >= "2022-02-24 13:15:00") 

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.10 MB-SW -------------------------------------------------------------------

Site <- "MB-SW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 11:45:00" | Timestamp <= "2021-12-15 14:45:00") %>% 
  filter(Timestamp <= "2021-12-15 14:45:00" | Timestamp >= "2022-02-23 11:00:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.11 MB-UW --------------------------------------------------------------

Site <- "MB-UW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 12:30:00") %>% 
  filter(Timestamp <= "2021-09-28 16:45:00" | Timestamp >= "2021-10-30 4:15:00") %>%
  filter(Timestamp <= "2021-11-10 7:30:00" | Timestamp >= "2022-01-05 21:45:00") 

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.12 HB-SW --------------------------------------------------------------------

Site <- "HB-SW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 12:15:00") %>% 
  filter(Timestamp <= "2021-12-16 10:00:00" | Timestamp >= "2022-02-23 15:15:00") %>% 
  filter(Timestamp <= "2022-05-02 15:30:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 5. Plot the output ------------------------------------------------------

output_JL <- ggplot(data = output %>% filter(output$Catchment == "JL"),
                   mapping = aes(x = Timestamp, 
                                     y = SpC_low_range,
                                     color = Site_ID)) +
  geom_line () +
  theme_bw ()

(output_JL)

output_BC <- ggplot(data = output %>% filter(output$Catchment == "BC"),
                    mapping = aes(x = Timestamp, 
                                  y = SpC_low_range,
                                  color = Site_ID)) +
  geom_line () +
  theme_bw ()

(output_BC)

rm(output_JL, output_BC)


# 6. Check the output against YSI Field Measurements ----------------------

#List the field logs
Field_logs <- list.files(paste0(data_dir, "Field_logs"), 
                         full.names = TRUE, 
                         pattern = ".xlsx") 
#Eliminate 2020 field logs
Field_logs <- Field_logs[!str_detect(Field_logs, "2020")]

#Download and rbind
SpC_field <- Field_logs %>% 
  map(download_logs) %>% 
  reduce(bind_rows) 

SpC_field <- SpC_field %>% 
  mutate(Timestamp = base::round_date(Timestamp, "15 minute"))

#select columns to match with Hobos
output <- output %>% 
  select(Temp_C, Site_ID, Timestamp, SpC_low_range)  
 
#Join Hobo values to the field data
checks <- inner_join(output_f, SpC_field, by = c("Timestamp", "Site_ID"))

#Compare sensors to YSI
checks <- checks %>% 
  mutate(Hobo_YSI_diff = (`SpC_low_range` - `SpC_field`))

checks_plot <- ggplot(data = checks, 
                      mapping = aes(x = Timestamp, 
                                    y = Hobo_YSI_diff, 
                                    color = Site_ID)) +
  geom_point(size = 4) + 
  scale_y_continuous(limits = c(-50, 50)) +
  theme_bw()

(checks_plot)

# 7. Write the output -----------------------------------------------------

write_csv(output, file = paste0(data_dir,"SpC_output_2021_2022.csv"))


