
#' Function to load west coast VMS data and subset for bottom trawl only
#' @param none  No parameters

#' @export
#' @examples
#' load_wc_itq()


load_wc_itq <- function(){
  #West Coast VMS Data
  load("/Users/peterkuriyama/School/Research/ch2vms/all_data/nw_vms.Rdata")
  
  #Change names and order of columns
  wc_vms <- nw.vms
  rm(nw.vms)
  names(wc_vms)[8] <- 'vessel_number'
  names(wc_vms)[9] <- 'filename'
  wc_vms$speed <- as.numeric(wc_vms$speed)
  
  #Adjust wc_vms latitudes and longitudes
  names(wc_vms)[4] <- 'lat_adj'
  names(wc_vms)[5] <- 'lon_adj'
  
  #Substitute out the symbol translations
  wc_vms$lat <- gsub("\\.", "", wc_vms$latitude)
  wc_vms$lon <- gsub("\\.", "", wc_vms$longitude)
  
  wc_vms$lat <- gsub("\\\260", "\\.", wc_vms$lat)
  wc_vms$lon <- gsub("\\\260", "\\.", wc_vms$lon)
   
  wc_vms$lat <- as.numeric(wc_vms$lat)
  wc_vms$lon <- as.numeric(wc_vms$lon)
   
  #Remove outlier values
  far <- wc_vms[which(wc_vms$lon >= 0), ]
  wc_vms <- subset(wc_vms, lon < 0)
  wc_vms <- subset(wc_vms, lat > 32)
  
  #Subset based on sector description
  
  itq <- subset(wc_vms, sector_desc == 'bottom trawl, shorebased IFQ, not including demersal trawl')

  return(itq)
}

