#' Resample years of logbook data

#' Function to permute catch data and evaluate significant declines/increases in clusters.

#' @param d_in Data input into the function
#' @param niters Number of iterations for each resampling
#' @param seed Seed value; defaults to 1

#' @export
#' @examples
#' resample_years(d_in = data, niters = 10)



resample_years <- function(d_in, niters, seed = 1){
# browser()
  
  #If dimensions are not right, quit
  # if(nrow(d_in) <= 1) stop("not enough rows")


  #Set Seed
  set.seed(seed)

  #calculate empirical difference between before and after catch shares
  emp <- d_in %>% group_by(clust, when, species) %>% 
    summarize(mean_aperc = mean(tow_aperc)) %>% as.data.frame
  emp <- dcast(emp, clust + species ~ when, value.var = 'mean_aperc')
  emp$diff <- emp$after - emp$before

  dyear_orig <- d_in$dyear

  #Resample the years without replacement

  diffs <- sapply(1:niters, FUN = function(x) {
    dyear_samp <- base::sample(dyear_orig, size = length(dyear_orig), replace = FALSE)

    d_in$dyear <- dyear_samp
    d_in[which(d_in$dyear < 2011), 'when'] <- 'before'
    d_in[which(d_in$dyear >= 2011), 'when'] <- 'after'

    dd1 <- d_in %>% group_by(clust, when, species) %>% summarize(mean_aperc = mean(tow_aperc),
        mean_hperc = mean(tow_hperc)) %>% as.data.frame
    dd1 <- dd1 %>% select(-mean_hperc)
    dd1 <- dcast(dd1, clust + species ~ when, value.var = 'mean_aperc')
    dd1$diff <- dd1$after - dd1$before
    return(dd1$diff)
    }
  )

  #Return the pvaluess
  gthan <- sum(diffs >= emp$diff) / length(diffs)
  lthan <- sum(diffs < emp$diff) / length(diffs)

  outs <- list(pval = data.frame(emp = emp$diff, greater = gthan, less = lthan, ndata = nrow(d_in)), 
               diffs = diffs)

  return(outs)
}
