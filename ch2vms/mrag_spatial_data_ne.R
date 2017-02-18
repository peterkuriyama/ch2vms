#Do only this because everything else takes too long

load(file = 'output/ne_vms_overlap.Rdata')
ne_ne <- ne_vms_thing #give temporary name
ne_vms <- ldply(ne_ne)
# rm(ne_vms_list)
rm(ne_vms_thing)

ne_binned <- bin_data(data = ne_vms, x_col = 'lon', y_col = 'lat')

ne_binned$when <- '999'
ne_binned[ne_binned$year < 2010, 'when'] <- "before"
ne_binned[ne_binned$year >= 2010, 'when'] <- "after"

ne_binned <- ne_binned %>% group_by(unq) %>% mutate(nyears = length(year)) %>% as.data.frame

ne_binned <- ne_binned %>% arrange(year) 

ne_binned %>% filter(nyears > 1) %>%
  group_by(unq) %>% 
  do({
    mod <- lm(count ~ year, data = .)
    slope <- mod$coefficients[2]
    names(slope) <- NULL
    data.frame(., slope)
}) %>% as.data.frame -> ne_slopes

ne_slopes <- ne_slopes %>% group_by(unq) %>% mutate(mean = mean(count), stdev = sd(count),
  cv = stdev / mean) %>% as.data.frame 
unqs <- ne_slopes %>% group_by(unq) %>% summarize(slope = unique(slope), mean = unique(mean),
  cv = unique(cv)) %>% as.data.frame
# hist(unqs$cv, breaks = 30)


#Add slope quantiles to ne_slopes data frame
quants <- quantile(ne_slopes$slope)

#Process quantiles intervals
names1 <- names(quants)[findInterval(ne_slopes$slope, quants)]
names2 <- names(quants)[findInterval(ne_slopes$slope, quants) + 1]
nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"

ne_slopes$slope_quants <- factor(nmz)

#-----Difference Counts
#Add diff_ct quants
diff_quants <- quantile(ne_slopes$diff_ct)

#Process quantiles intervals
names1 <- names(diff_quants)[findInterval(ne_slopes$diff_ct, diff_quants)]
names2 <- names(diff_quants)[findInterval(ne_slopes$diff_ct, diff_quants) + 1]
nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"


ne_slopes$diff_quants <- factor(nmz)

#-----Mean Counts
#Add mean count quantiles
mean_quants <- quantile(ne_slopes$mean)

#Process quantiles intervals
names1 <- names(mean_quants)[findInterval(ne_slopes$mean, mean_quants)]
names2 <- names(mean_quants)[findInterval(ne_slopes$mean, mean_quants) + 1]
nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"
ne_slopes$mean_quants <- factor(nmz)

#-----Mean Change of Counts
mn_ct <- ne_slopes %>% group_by(unq, when) %>% summarize(avg_count = mean(count)) 
mn_ct1 <- dcast(mn_ct, unq ~ when)

mn_ct1[is.na(mn_ct1$after), 'after'] <- 0
mn_ct1[is.na(mn_ct1$before), 'before'] <- 0
mn_ct1$diff <- mn_ct1$after - mn_ct1$before
names(mn_ct1)[2:4] <- c("mean_after", "mean_before", "mn_itq_diff")

ne_slopes <- left_join(ne_slopes, mn_ct1, by = 'unq')

itq_diff_quants <- quantile(ne_slopes$mn_itq_diff)

names1 <- names(itq_diff_quants)[findInterval(ne_slopes$mn_itq_diff, itq_diff_quants)]
names2 <- names(itq_diff_quants)[findInterval(ne_slopes$mn_itq_diff, itq_diff_quants) + 1]

nmz <- paste0(names1, "-" ,names2)
nmz[grep('-NA', nmz)] <- "75%-100%"
ne_slopes$itq_diff_quants <- factor(nmz)

#Rename ne_slopes to be ne_data
ne <- ne_slopes
rm(ne)



# #Load NE Data
# load('all_data/ne_vms.Rdata')

# #Filter out many things initially
# ne_vms <- ne_vms %>% filter(region == 'WH')
# ne_vms <- ne_vms %>% filter(year >= 2009)
# ne_vms$speed <- as.numeric(ne_vms$speed)
# ne_vms <- ne_vms %>% filter(speed >= 3 & speed <= 5)

# #Filter out slashes in latitude and longitude
# ne_vms$latitude <- gsub("\302\260", "", ne_vms$latitude)
# ne_vms$longitude <- gsub("\302\260", "", ne_vms$longitude)

# #move decimal places
# ne_vms$lat1 <- gsub( "[.]", "", ne_vms$latitude)
# ne_vms$lon1 <- gsub( "[.]", "", ne_vms$longitude)


# ne_vms$lat1 <- paste0(substr(ne_vms$lat1, 1, 2), ".", substr(ne_vms$lat1, 3, 5))
# ne_vms$lon1 <- paste0(substr(ne_vms$lon1, 1, 3), ".", substr(ne_vms$lon1, 4, 6))

# ne_vms$lat <- as.numeric(ne_vms$lat1)
# ne_vms$lon <- as.numeric(ne_vms$lon1)

# ne_vms$lat1 <- NULL
# ne_vms$lon1 <- NULL

# #Filter NE Data based on shapefiles

# #Probably have to break the filtering up by years
# ne_vms_list <- vector('list', length = 6)
# yrz <- 2009:2014

# for(ii in 1:length(2009:2014)){
#   temp <- subset(ne_vms, year == yrz[ii])
#   ne_vms_list[[ii]] <- temp
# }



# #Do the filtering in parallel
# library(doParallel)
# cl <- makeCluster(6)
# registerDoParallel(cl)

# getDoParWorkers()

# shape_dir = 'all_data/ne_shapefile'
# file_nm = "NE_Multispecies_Broad_Stock_Areas"
# proj <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

# ne_shape <- readOGR(shape_dir, file_nm)
# ne_shape <- spTransform(ne_shape, CRS = CRS(proj))

# #Do this in lapply
# ne_vms_thing <- mclapply(ne_vms_list, mc.cores = 6, FUN = function(xx){
#       pts <- xx[, c('lon', 'lat')]

#       pts_proj <- SpatialPointsDataFrame(coords = pts, data = xx, 
#         proj4string = CRS(proj))

#       # Filter the data based on the shapefile
#       start_time <- Sys.time()
#       overlaps <- over(pts_proj, as(ne_shape, 'SpatialPolygons'))
#       end_time <- Sys.time()
#       print(end_time - start_time)

#       plottable <- as.data.frame(pts_proj)
#       plottable$lon.1 <- NULL
#       plottable$lat.1 <- NULL
        
#       plottable$ov <- overlaps
#       return(plottable)
#       # mean(pts$lon)
#     })

# save(ne_vms_thing, file = 'output/ne_vms_overlap.Rdata')


# stopCluster(cl)

# #Check that the map worked

# ne_vms1 <- subset(ne_vms, ov == 1)
# plot(ne_shape)
# points(ne_vms1$lon, ne_vms1$lat, col = 'red', pch = '.')

















