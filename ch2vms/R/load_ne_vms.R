#' Load Northeast VMS Data

#' Function to load northeast VMS Data

#' @param Parameter there aren't any

#' @export
#' @examples
#' load_ne_vms()

load_ne_vms <- function(){
  load('data/ne_vms.Rdata')

  #Rename lat and lon columns
  names(ne_vms)[9] <- 'trans_lat'
  names(ne_vms)[10] <- 'trans_lon'

  #Filter out NAs
  ne_vms_all <- ne_vms
  ne_vms <- ne_vms[which(is.na(ne_vms$speed) == FALSE), ]

  #Filter the zeroes out of the speeds
  ne_vms$speed <- as.numeric(ne_vms$speed)

  #Filter out zeroes
  ne_vms <- ne_vms %>% filter(speed != 0) %>% as.data.frame

  #Filtered out all zeroes and NAs
  return(ne_vms)
}