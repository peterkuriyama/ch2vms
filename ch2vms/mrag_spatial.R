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
wc_pts <- filt_points[[2]]

#Plot stuff
#Use this to check that the filtering is working OK
plot(filt_points[[1]])
points(x = wc_pts$lon, y = wc_pts$lat, col = 'red', pch = '.')


#---------------------------------------------------------------------------------
#Bin the WC pts
#Make sure to only keep those with ov value of 2
wc_pts <- wc_pts %>% filter(ov == 2)

wc_binned <- bin_data(data = wc_pts, x_col = "lon", y_col = "lat")

ggplot(data = wc_binned) + geom_tile(aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year) + theme_bw()

#Plots for comparison
wc_binned$when <- '999'
wc_binned[wc_binned$year < 2011, 'when'] <- "before"
wc_binned[wc_binned$year >= 2011, 'when'] <- "after"

#Calculate percentage of annual fishing effort in each location


#---------------------------------------------------------------------------------
#Fit linear model to see which ones had the largest slope changes
#Check to see that all years had more than one year
wc_binned <- wc_binned %>% group_by(unq) %>% mutate(nyears = length(year)) %>% as.data.frame

wc_binned <- wc_binned %>% arrange(year) 

wc_binned %>% filter(nyears > 1) %>%
  group_by(unq) %>% 
  do({
    mod <- lm(count ~ year, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
}) %>% as.data.frame -> wc_slopes


wc_slopes <- wc_slopes %>% group_by(unq) %>% mutate(mean = mean(count), stdev = sd(count),
  cv = stdev / mean) %>% as.data.frame 
unqs <- wc_slopes %>% group_by(unq) %>% summarize(slope = unique(slope), mean = unique(mean),
  cv = unique(cv)) %>% as.data.frame
hist(unqs$cv, breaks = 30)


#---------------------------------------------------------------------------------
#Final Plots

#Aggregated number of assumed fishing locations
wc_slopes %>% group_by(year) %>% summarize(count = sum(count)) %>% 
  ggplot() + geom_line(aes(x = year, y = count)) + 
  geom_point(aes(x = year, y = count)) + theme_bw()
#2012 is effed up

#Aggregated number of assumed fishing locations



ggplot(wc_top25) + geom_point(aes(x = mean, y = stdev, col = slope))




wc_top25 <- wc_slopes %>% filter(unq %in% bef_top25)




ggplot(wc_top25) + geom_point()


#Check that this makes sense
wc_top25 %>% filter(unq == '12 112') %>% ggplot(aes(x = year, y = count)) + geom_point() + 
  geom_line()



ggplot



#---------------------------------------------------------------------------------
#Find the most highly visited
#Most visited locations before
bef_most <- wc_binned %>% filter(when == 'before') %>% group_by(unq) %>% 
  summarize(count_avg = mean(count)) %>% arrange(desc(count_avg)) %>% as.data.frame
qs <- quantile(bef_most$count_avg)

#look at top 25% most fished locations
bef_top25 <- bef_most[which(bef_most$count_avg >= qs[4]), 'unq']

wc_top25 <- wc_binned %>% filter(unq %in% bef_top25)

ggplot(wc_top25) + geom_line(aes(x = year, y = count, group = unq, col = unq))



#Do a linear model to see which ones had the highest increase or decrease



wc_binned %>% group_by(x, y, year) %>% summarize()





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










