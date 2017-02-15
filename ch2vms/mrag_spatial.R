#Spatial Pattern Analysis for MRAG

#---------------------------------------------------------------------------------
#To Do
setwd("/Users/peterkuriyama/School/Research/ch2vms")

#Load logbook data that has been preprocessed
library(ggplot2)
library(devtools)
library(plyr)
library(dplyr)
library(reshape2)
library(maps)
library(rworldmap)
library(sp)
library(rgdal)

devtools::install_github('peterkuriyama/ch2vms/ch2vms')
library(ch2vms)
# load_all()

#/Users/peterkuriyama/School/Research/ch2vms
#---------------------------------------------------------------------------------
#Load Data
wc <- load_wc_itq()

#Filter data based on speeds
filt_data <- wc %>% filter(speed >= 3 & speed <= 5)
filt_points <- filter_data(input = filt_data)

#Extract the points
pts <- filt_points[[2]]

#Plot stuff
#Use this to check that the filtering is working OK
plot(filt_points[[1]])
points(x = pts$lon, y = pts$lat, col = 'red', pch = '.')


#---------------------------------------------------------------------------------


library(rworldmap)

#---------------------------------------------------------------------------------
#Filter out points based on borders 









wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')


#West Coast Analysis
wc_bin <- bin_data(data = wc_filt, x_col = 'lon', y_col = 'lat')

wc_map + scale_x_continuous(limits = c(-128, -117)) + 
  scale_y_continuous(limits = c(31.5, 48)) + 
  geom_tile(data = wc_bin, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)


#Filter 

load("/Users/peterkuriyama/School/Research/ch2_vms/data/ne_vms.Rdata")
load("/Users/peterkuriyama/School/Research/ch2_vms/data/ne_vms.Rdata")


wc_bins <- ggplot(wc, aes(x = lon, y = lat, group = year)) + 
  stat_bin2d(binwidth = c(.0909, .11))


binned <- ggplot_build(wc_bins)$data[[1]]
yrz <- data.frame(group = 1:8, year = 2007:2014)
wc_binned <- left_join(binned, yrz, by = 'group')

#Plot this stuff










