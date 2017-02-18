#Load NE Data
load('all_data/ne_vms.Rdata')

#Filter out many things initially
ne_vms <- ne_vms %>% filter(region == 'WH')
ne_vms <- ne_vms %>% filter(year >= 2009)
ne_vms$speed <- as.numeric(ne_vms$speed)
ne_vms <- ne_vms %>% filter(speed >= 3 & speed <= 5)

#Filter out slashes in latitude and longitude
ne_vms$latitude <- gsub("\302\260", "", ne_vms$latitude)
ne_vms$longitude <- gsub("\302\260", "", ne_vms$longitude)

#move decimal places
ne_vms$lat1 <- gsub( "[.]", "", ne_vms$latitude)
ne_vms$lon1 <- gsub( "[.]", "", ne_vms$longitude)


ne_vms$lat1 <- paste0(substr(ne_vms$lat1, 1, 2), ".", substr(ne_vms$lat1, 3, 5))
ne_vms$lon1 <- paste0(substr(ne_vms$lon1, 1, 3), ".", substr(ne_vms$lon1, 4, 6))

ne_vms$lat <- as.numeric(ne_vms$lat1)
ne_vms$lon <- as.numeric(ne_vms$lon1)

ne_vms$lat1 <- NULL
ne_vms$lon1 <- NULL

#Filter NE Data based on shapefiles

#Probably have to break the filtering up by years
ne_vms_list <- vector('list', length = 6)
yrz <- 2009:2014

for(ii in 1:length(2009:2014)){
  temp <- subset(ne_vms, year == yrz[ii])
  ne_vms_list[[ii]] <- temp
}



#Do the filtering in parallel
library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cl)

getDoParWorkers()

shape_dir = 'all_data/ne_shapefile'
file_nm = "NE_Multispecies_Broad_Stock_Areas"
proj <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

ne_shape <- readOGR(shape_dir, file_nm)
ne_shape <- spTransform(ne_shape, CRS = CRS(proj))

#Do this in lapply
ne_vms_thing <- mclapply(ne_vms_list, mc.cores = 6, FUN = function(xx){
      pts <- xx[, c('lon', 'lat')]

      pts_proj <- SpatialPointsDataFrame(coords = pts, data = xx, 
        proj4string = CRS(proj))

      # Filter the data based on the shapefile
      start_time <- Sys.time()
      overlaps <- over(pts_proj, as(ne_shape, 'SpatialPolygons'))
      end_time <- Sys.time()
      print(end_time - start_time)

      plottable <- as.data.frame(pts_proj)
      plottable$lon.1 <- NULL
      plottable$lat.1 <- NULL
        
      plottable$ov <- overlaps
      return(plottable)
      # mean(pts$lon)
    })

save(ne_vms_thing, file = 'output/ne_vms_overlap.Rdata')

stopCluster(cl)









