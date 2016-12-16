#' Cluster the data

#' Funtion that returns cluster output. The purpose is to allow clustering to occur only once
#' while exploring effects of different cut points

#' @param dat Input data
#' @param port Port of interest. Function takes too long if done on all data

#' @export
#' @examples
#' clust_tows(dat = dat, port = 'ASTORIA')

#
clust_tows <- function(dat, port){
  temp <- subset(dat, dport_desc == port)

  distances <- dist(temp[, c('up_lat', 'set_lat', 'trans_set_long', 'trans_up_long')],
    method = 'euclidean')
  clusts <- hclust(dist_ast, method = 'average')
  return(clusts)

}