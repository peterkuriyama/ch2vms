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

world_map <- map_data("world")
wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
      fill = NA, color = 'gray') +
    scale_x_continuous(limits = c(-126, -117)) + 
    scale_y_continuous(limits = c(33.385, 48)) 


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
nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"

wc_slopes$slope_quants <- factor(nmz)

#-----Difference Counts
#Add diff_ct quants
diff_quants <- quantile(wc_slopes$diff_ct)

#Process quantiles intervals
names1 <- names(diff_quants)[findInterval(wc_slopes$diff_ct, diff_quants)]
names2 <- names(diff_quants)[findInterval(wc_slopes$diff_ct, diff_quants) + 1]
nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"


wc_slopes$diff_quants <- factor(nmz)

#-----Mean Counts
#Add mean count quantiles
mean_quants <- quantile(wc_slopes$mean)

#Process quantiles intervals
names1 <- names(mean_quants)[findInterval(wc_slopes$mean, mean_quants)]
names2 <- names(mean_quants)[findInterval(wc_slopes$mean, mean_quants) + 1]
nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"
wc_slopes$mean_quants <- factor(nmz)

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

nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"
wc_slopes$itq_diff_quants <- factor(nmz)
