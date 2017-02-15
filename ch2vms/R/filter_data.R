#' Filter Data Over Polygons

#' Function to filter data points over shape file

#' @param input Points to filter, preferably subsetted by speed
#' @param shape_dir Directory of shapefile
#' @param file_nm Shapefile name
#' @param proj Projection characteristics

#' @export

#Filter the data based on shapefiles
filter_data <- function(input, shape_dir = 'all_data/wc_shapefile',
  file_nm = 'Strata_Final_dissAreaType', 
  proj = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'){

  #Read in shapefile
  shape <- readOGR(shape_dir, file_nm)
  #Make sure projections are consistent
  shape <- spTransform(shape, 
    CRS = CRS(proj))

  #Transform the input to right projection
  pts <- input[, c('lon', 'lat')]

  pts_proj <- SpatialPointsDataFrame(coords = pts, data = input, 
    proj4string = CRS(proj))

  #Filter the data based on the shapefile
  start_time <- Sys.time()
  overlaps <- over(vms_shape, as(wc_shape, 'SpatialPolygons'))
  end_time <- Sys.time()
  print(end_time - start_time)

  plottable <- as.data.frame(pts_proj)
  plottable$lon.1 <- NULL
  plottable$lat.1 <- NULL
    
  plottable$ov <- overlaps

  #Currently do not return the input as a shapefile format

  #Return shapefile and filtered data in plottable form
  return(list(shapefile = shape, pts = plottable))

}

# filtered <- wc %>% filter(speed >= 3 & speed <= 5)
# filter_data(input = filtered)


# #Convert the wc_vms data to latlon for the over function
# wc_coords <- wc_filt[, c('lon', 'lat')]
# vms_shape <- SpatialPointsDataFrame(coords = wc_coords,
#   data = wc_filt, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# #Break this into four chunks
# #Filter first by speed
# # vms_shape_filt <- vms_shape %>% filter(speed >= 3 & speed <= 5)
# start_time <- Sys.time()
# overlaps <- over(vms_shape, as(wc_shape, "SpatialPolygons"))
# save(overlaps, file = 'output/overlaps.Rdata')
# end_time <- Sys.time()
# end_time - start_time


