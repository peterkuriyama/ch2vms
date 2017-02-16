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


#Add slope quantiles to wc_slopes data frame
quants <- quantile(wc_slopes$slope)

#Process quantiles intervals
names1 <- names(quants)[findInterval(wc_slopes$slope, quants)]
names2 <- names(quants)[findInterval(wc_slopes$slope, quants) + 1]

wc_slopes$slope_quants <- paste0(names1, "-" ,names2)

#-----Difference Counts
#Add diff_ct quants
diff_quants <- quantile(wc_slopes$diff_ct)

#Process quantiles intervals
names1 <- names(diff_quants)[findInterval(wc_slopes$diff_ct, diff_quants)]
names2 <- names(diff_quants)[findInterval(wc_slopes$diff_ct, diff_quants) + 1]

wc_slopes$diff_quants <- paste0(names1, "-" ,names2)

#-----Mean Counts
#Add mean count quantiles
mean_quants <- quantile(wc_slopes$mean)

#Process quantiles intervals
names1 <- names(mean_quants)[findInterval(wc_slopes$mean, mean_quants)]
names2 <- names(mean_quants)[findInterval(wc_slopes$mean, mean_quants) + 1]

wc_slopes$mean_quants <- paste0(names1, "-" ,names2)

#-----Mean Change of Counts
mn_ct <- wc_slopes %>% group_by(unq, when) %>% summarize(avg_count = mean(count)) 
mn_ct1 <- dcast(mn_ct, unq ~ when)

mn_ct1[is.na(mn_ct1$after), 'after'] <- 0
mn_ct1[is.na(mn_ct1$before), 'before'] <- 0
mn_ct1$diff <- mn_ct1$after - mn_ct1$before
names(mn_ct1)[2:4] <- c("mean_after", "mean_before", "mn_itq_diff")

wc_slopes <- left_join(wc_slopes, mn_ct1, by = 'unq')

itq_diff_quants <- quantile(wc_slopes$mn_itq_diff)

names1 <- names(itq_diff_quants)[findInterval(wc_slopes$mn_itq_diff, itq_diff_quants)]
names2 <- names(itq_diff_quants)[findInterval(wc_slopes$mn_itq_diff, itq_diff_quants) + 1]
wc_slopes$itq_diff_quants <- paste0(names1, '-', names2)

hist(wc_slopes$mn_itq_diff, breaks = 30)

#---------------------------------------------------------------------------------
#Final Plots

#Aggregated number of assumed fishing locations
wc_slopes %>% group_by(year) %>% summarize(count = sum(count)) %>% 
  ggplot() + geom_line(aes(x = year, y = count)) + 
  geom_point(aes(x = year, y = count)) + theme_bw()
#2012 is effed up


#Plot Trends by quantiled slope, seems not to highlight the most fished areas
ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ slope_quants)

#Plot trends by those with the biggest differences
ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ diff_quants)

#Plot trends by those with different quantile means, like most fished locations,
  # least fished locations
ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ mean_quants)

#Plot trends by those with different quantile means, like most fished locations,
  # least fished locations
ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ itq_diff_quants)


#Overall plot of fishing location bins
  #Hard to make out the details
#Map Prep stuff
world_map <- map_data("world")
wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')

#Tile Map, hard to visualize
wc_map + scale_x_continuous(limits = c(-126, -117)) + 
  scale_y_continuous(limits = c(31.5, 48)) + 
  geom_tile(data = wc_binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)

#Show tile plots based on percent changes?
#Add slope quantile


#Quantify increases or decreases



ggplot(wc_top25) + geom_point(aes(x = mean, y = stdev, col = slope))

ggplot(data = wc_binned) + geom_tile(aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year) + theme_bw()



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










