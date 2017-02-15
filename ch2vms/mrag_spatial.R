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






pp <- vms_shape[, c('lon', 'lat', 'ov')]
pp <- as.data.frame(pp)

plot(wc_shape)

temp <- pp %>% filter(ov == 2)
points(temp,
 col = 'red', pch = '.')



alex <- subset(wc, vessel_name == "Alex (Faria)")
alex <- as.data.frame(alex)
row.names(alex) <- NULL

alex_coords <- alex[, c('lon', 'lat')]
alex_shape <- SpatialPointsDataFrame(coords = alex_coords,
  data = alex, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

alex_subset <- alex_shape[wcs1, ]


#Plot certain points
alex_temp <- alex_coords[which(is.na(alex_coords$ov)), ]
alex_temp <- 

alex_temp <- alex_coords %>% filter(ov == 2 & speed >= 3 & speed <= 5)
plot(wc_shape, xlim = c(-125, -124), ylim = c(40, 41.6))
points(alex_temp, col = 'red', pch = '.')

#add filtered points
points(alex_3, col = 'red', pch = '.')
# alex_4 <- alex_coords %>% filter(speed == 4)
# alex_5 <- alex_coords %>% filter(speed == 5)
# alex_6 <- alex_coords %>% filter(speed == 6)

png(width = 5.5, height = 8.3, file = 'figs/alex.png', res = 250, 
  units = 'in')

pdf(width = 5.5, height = 8.3, file = 'figs/alex.pdf')

plot(wc_shape, xlim = c(-125, -124), ylim = c(40, 43))
points(alex_3, col = 'red', pch = '.')
points(alex_4, col = 'blue', pch = '.')
points(alex_5, col = 'black', pch = '.')
points(alex_6, col = 'green', pch = '.')
dev.off()







#Define world maps
# map <- getMap()
# world_map <- map_data("world")

#Ok i think the projection doesn't really matter, points on land are in bays and stuff
usa <- subset(world_map, region == 'USA')
world_map <- spTransform(world_map, CRS("+proj=longlat +datum=WGS84"))


# ggplot(alex, aes(x = lon, y = lat, colour = speed)) + geom_point()

#Do this for some test data
newmap <- getMap(resolution = "high")
newmap <- spTransform(newmap, CRS("+proj=longlat + ellps=WGS84 +datum=WGS84"))

alex_speed <- alex %>% filter(speed <= 5 & speed >= 3)
plot(newmap, xlim = c(-125, -124), ylim = c(40, 47.6), asp = 1)
points(x = alex_speed$lon, y = alex_speed$lat, pch = '.', col = 'red')

alex1 <- alex
coordinates(alex1) <- c('lon', 'lat')
proj4string(alex1)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 

over(alex1, as(newmap, 'SpatialPolygons'))



#---------------------------------------------------------------------------------


library(rworldmap)

pdf(width = 3, height = 8.68, file = 'figs/alex_points.pdf')
plot(getMap(resolution = 'low'), xlim = c(-125, -124), ylim = c(40, 47.6), 
  mar = c(0, 0, 0, 0))
points(x = alex$lon, y = alex$lat, pch = '.', col = 'red')
dev.off()

#---------------------------------------------------------------------------------
#Filter out points based on borders 



wc %>% group_by(speed) %>% summarize(n()) %>% as.data.frame

#Filter between 3 and 8 knots
wc_filt <- wc %>% filter(speed >= 3 & speed < 8 & latitude < 49) 
wc_fil




wc_filt$l









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










