#' Summarize Clusters

#' Function to summarize clusters, given a certain cut_point

#' @param dat Original data that was clustered
#' @param clust_input Output from clust_tows function
#' @param cut_point Cut point

#' @export
#' @examples
#' Coming soon


summ_clust <- function(dat, clust_input, cut_point){
  #Cut the clusters
  tree <- cutree(clust_input, h = cut_point)
  dat$clust <- tree
# browser()
  #add in number of tows
  dat <- dat %>% group_by(clust) %>% mutate(ntows = length(unique(haul_id)))

  #Summarize the data
  summ_dat <- dat %>% group_by(drvid, trip_id, dyear) %>% mutate(trip_hpounds = sum(hpounds),
    trip_apounds = sum(apounds)) %>% group_by(drvid, trip_id, species, dyear) %>%
    mutate(trip_spp_hperc = sum(hpounds) / trip_hpounds, 
           trip_spp_aperc = sum(apounds) / trip_apounds) %>%

    #Vessel catch compositions
    group_by(drvid, dyear) %>% mutate(vess_hpounds = sum(hpounds),
      vess_apounds = sum(apounds)) %>% group_by(drvid, species, dyear) %>%
      mutate(vess_spp_hperc = sum(hpounds) / vess_hpounds,
             vess_spp_aperc = sum(apounds) / vess_apounds) %>%

    #Cluster Catch Compositions
    group_by(clust, dyear) %>% mutate(clust_hpounds = sum(hpounds), 
      clust_apounds = sum(apounds)) %>% group_by(clust, dyear, species) %>%
      mutate(clust_spp_hperc = sum(hpounds) / clust_hpounds,
        clust_spp_aperc = sum(apounds) / clust_apounds) %>% as.data.frame

  return(summ_dat)
}


