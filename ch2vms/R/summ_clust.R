#' Summarize Clusters

#' Function to summarize clusters, given a certain cut_point

#' @param dat Data with column for clusters

#' @export
#' @examples
#' Coming soon


summ_clust <- function(dat){
  #Cut the clusters
  # tree <- cutree(clust_input, h = cut_point)
  # dat$clust <- tree
# browser()
  
  #add in number of tows
  # dat <- dat %>% group_by(clust) %>% mutate(ntows = length(unique(haul_id)))

  #For each trip
  trips <- dat %>% group_by(drvid, trip_id, dyear) %>% mutate(trip_hpounds = sum(hpounds),
    trip_apounds = sum(apounds)) %>% group_by(drvid, trip_id, species, dyear) %>%
    summarize(trip_hpounds = unique(trip_hpounds), trip_apounds = unique(trip_apounds),
      trip_spp_hperc = sum(hpounds) / trip_hpounds, 
      trip_spp_aperc = sum(apounds) / trip_apounds) %>% as.data.frame

  #For each vessel
  vessel <- dat %>% group_by(drvid, dyear) %>% mutate(vess_hpounds = sum(hpounds),
    vess_apounds = sum(apounds)) %>% group_by(drvid, species, dyear) %>%
    summarize(vess_hpounds = unique(vess_hpounds), vess_apounds = unique(vess_apounds),
      vess_spp_hperc = sum(hpounds) / vess_hpounds,
      vess_spp_aperc = sum(apounds) / vess_apounds) %>% as.data.frame
  
  #For each cluster
  cluster <- dat %>% group_by(clust, dyear) %>% mutate(clust_hpounds = sum(hpounds),
    clust_apounds = sum(apounds)) %>% group_by(clust, dyear, species) %>%
    summarize(clust_hpounds = unique(clust_hpounds), clust_apounds = unique(clust_apounds),
      clust_spp_hperc = sum(hpounds) / clust_hpounds,
      clust_spp_aperc = sum(apounds) / clust_apounds) %>% as.data.frame
      
  
  out <- list(trips = trips, vessel = vessel, cluster = cluster)
  return(out)
}

  

#   #Summarize the data
#   summ_dat <- dat %>% group_by(drvid, trip_id, dyear) %>% mutate(trip_hpounds = sum(hpounds),
#     trip_apounds = sum(apounds)) %>% group_by(drvid, trip_id, species, dyear) %>%
#     mutate(trip_spp_hperc = sum(hpounds) / trip_hpounds, 
#            trip_spp_aperc = sum(apounds) / trip_apounds) %>%

#     #Vessel catch compositions
#     group_by(drvid, dyear) %>% mutate(vess_hpounds = sum(hpounds),
#       vess_apounds = sum(apounds)) %>% group_by(drvid, species, dyear) %>%
#       mutate(vess_spp_hperc = sum(hpounds) / vess_hpounds,
#              vess_spp_aperc = sum(apounds) / vess_apounds) %>%

#     #Cluster Catch Compositions
#     group_by(clust, dyear) %>% cumm(clust_hpounds = sum(hpounds), 
#       clust_apounds = sum(apounds)) %>% group_by(clust, dyear, species) %>%
#       mutate(clust_spp_hperc = sum(hpounds) / clust_hpounds,
#         clust_spp_aperc = sum(apounds) / clust_apounds) %>% as.data.frame

#   return(summ_dat)
# }


