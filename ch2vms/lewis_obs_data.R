#Load Packages
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)

#Install delta plot functions
devtools::install_github("peterkuriyama/ch2vms/ch2vms")
library(ch2vms)

#Load observer data
obs_data <- read.csv('all_data/obs_data.csv', stringsAsFactors = FALSE)

#get column names in the right format
names(obs_data) <- tolower(names(obs_data))
obs_data[, 118] <- NULL #Remove duplicated scientific name column
obs_data[, 100] <- NULL #Remove duplicated mt column

#Rename coolumns
obs_data <- plyr::rename(obs_data, c('lb' = 'hpounds', 
  'dmonth' = 'tow_month', 'dyear' = 'tow_year',
  'dday' = 'tow_day', 'set_depth' = 'depth1',
  'haul_duration' = 'duration'))
obs_data$apounds <- obs_data$hpounds
obs_data$species <- tolower(obs_data$species)

#Function to calculate values for delta plots
obs_deltas <- delta_plot(data = obs_data)


#Should look like this
load('output/survey_deltas.Rdata')


