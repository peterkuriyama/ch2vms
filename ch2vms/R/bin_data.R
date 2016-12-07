#' Bin data into grid

#' Function to count number of tows in bins. Uses ggplot stat_bin2d functions.

#' @param data Data frame to bin
#' @param x_col name of longitude column
#' @param y_col name of latitude column
#' @export
#' @examples
#' bin_data(data = )

bin_data <- function(data, x_col = 'trans_lon', y_col = 'trans_lat', group = 'year',
  grid_size = c(0.0909, 0.11)){

  bin <- ggplot(data, aes_string(x = x_col, y = y_col, group = group)) + 
    stat_bin2d(binwidth = grid_size)
  binned <- ggplot_build(bin)$data[[1]]

  #Add column for each unique site
  binned$unq <- paste(binned$xbin, binned$ybin)

  #Look at site-specific trends, to see which ones had highs and lows
  binned %>% group_by(unq) %>% mutate(min_ct = min(count), max_ct = max(count), diff_ct = max_ct - min_ct) %>%
    as.data.frame -> binned

  #Add in unique group category
  unq_grp <- data.frame(year = unique(data[, group]), group = 1:length(unique(data[, group])))
  binned <- inner_join(binned, unq_grp, by = 'group')

  return(binned)
}





