#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Raw to csv Jackson Lane
# Coder: James Maze
# Date: 21 Jan 2021
# Purpose: Plot and analyze Jackson Lane SC data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Libraries and workspace ----------------------------------------------

remove(list = ls())

library(xts)
library(dygraphs)
library(purrr)
library(lubridate)
library(tidyverse)
library(stringr)

data_dir <- "\data"


# 2. Read in the files ----------------------------------------------------

files <- list.files(path = paste0(data_dir), full.names = TRUE)






