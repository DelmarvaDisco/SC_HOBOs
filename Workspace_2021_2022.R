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
library(readr)
library(dygraphs)
library(purrr)
library(lubridate)
library(stringr)
library(tidyverse)
library(stringr)

source("functions/download_hobo.R")
source("functions/prelim_plot.R")
source("functions/fun_anomalous.R")
source("functions/download_logs.R")

data_dir <- "data/"

# 2. Read in the files ----------------------------------------------------

# 2.1 Read the JL data ----------------------------------------------------

files <- list.files(paste0(data_dir, "JL/"), full.names = TRUE, pattern = ".csv")

#Download this mess
data_JL <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

# 2.2 Read the BC data ----------------------------------------------------

files <- list.files(paste0(data_dir, "BC/"), full.names = TRUE, pattern = ".csv")

#Download
data_BC <- files %>% 
  map(download_hobo) %>% 
#Mush the tibbles together
  reduce(bind_rows)

# 3. Reformat the data ---------------------------------------------------------------------

df <- rbind(data_JL, data_BC) 

list_JL <- unique(data_JL$Site_ID)
list_BC <- unique(data_BC$Site_ID)

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

rm(data_BC, data_JL)

# 4. Cut the crappy values --------------------------------------------------------------------

# 4.1 DK-SW ---------------------------------------------------------------

Site <- "DK-SW"

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

# 4.2 ND-SW -------------------------------------------------------------------

Site <- "ND-SW"

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

# 4.3 TS-SW -------------------------------------------------------------------

Site <- "TS-SW"

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


# 4.4 BD-CH -------------------------------------------------------------------

Site <- "BD-CH"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 10:00:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.5 TS-CH -------------------------------------------------------------------

Site <- "TS-CH"


temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 10:00:00")

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.6 OB-SW ---------------------------------------------------------------

Site <- "OB-SW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 12:00:00") %>% 
  filter(Timestamp <= "2021-10-09 10:00:00" | Timestamp >= "2021-10-10 18:00:00") %>%
  filter(Timestamp <= "2021-10-11 2:00:00" | Timestamp >= "2021-10-26 6:30:00") %>%
  filter(Timestamp <= "2021-10-27 13:30:00" | Timestamp >= "2021-10-29 23:00:00") %>%
  filter(Timestamp <= "2021-12-03 9:15:00" | Timestamp >= "2022-02-23 9:30:00") 

temp <- fun_anomalous(temp, min = -1, max = 1)

output <- rbind(output, temp)

rm(temp, Site)

# 4.7 OB-UW -------------------------------------------------------------------

Site <- "OB-UW"

temp <- df %>% 
  filter(Site_ID == Site)

prelim_plot(temp)

temp <- temp %>% 
  filter(Timestamp >= "2021-09-24 12:00:00") %>% 
  filter(Timestamp <= "2021-09-27 18:00:00" | Timestamp >= "2021-10-30 8:30:00") %>%
  filter(Timestamp <= "2021-11-06 6:45:00" | Timestamp >= "2022-01-08 15:30:00") 

temp <- fun_anomalous(temp, min = -1, max = 1)

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
  mutate(Timestamp = round_date(Timestamp, "15 minute"))

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


