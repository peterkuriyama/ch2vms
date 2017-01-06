#' Create delta plots

#' Function to generate values relevant for delta plots. Delta plots come from Gillis et al. 
#' (2008)

#' @param data Data frame used to calculate delta plots
#' @param spps Vector of species to calculate delta plot values for
#' @param year_col Name of column containing year information, defaults to tow_year which
#' corresponds to wc_data
#' @export
#' @examples
#' delta_plot()

delta_plot <- function(data = wc_data, spps = c('Dover Sole', 'Arrowtooth Flounder', 'Sablefish', 'Petrale Sole', 'Longspine Thornyhead',
    'Shortspine Thornyhead', 'Chilipepper Rockfish', 'Lingcod', 'Yellowtail Rockfish', 
    'Darkblotched Rockfish', 'Pacific Ocean Perch', 'Bank Rockfish', 'Widow Rockfish'), 
    year_col = 'tow_year'){
  
  #Define range of years given data
  yrz <- as.integer(range(data$tow_year))
  yrz <- yrz[1]:yrz[2]
  
  #Apply the delta plot function across vector of years
  out_list <- lapply(yrz, FUN = function(x){
    temp <- subset(wc_data, tow_year == x)
    calc_deltas_spp(data = temp, spps = spps, focus = 'hpounds')
  })

  names(out_list) <- yrz
  out_list <- ldply(out_list)
  names(out_list)[[1]] <- 'year'

  out_list$species <- as.character(out_list$species)

  ss <- unique(out_list$species)
  abbrevs <- lapply(strsplit(ss, " "), function(x) sapply(x, FUN = function(y) paste0(substr(y, 1, 1), collapse = '')))

  ss_names <- ldply(lapply(abbrevs, FUN = function(x) paste0(x, collapse = '')))
  ss_names <- data.frame(species = ss, abbrev = ss_names[, 1])
  ss_names$species <- as.character(ss_names$species)
  ss_names$abbrev <- as.character(ss_names$abbrev)
  ss_names <- ss_names[order(ss_names$species), ]

  ss_names$short <- c("Arr", 'Bnk', 'Chl', 'Drk', 'Dvr', 'Lng', 'Lon', 
    'POP', 'Pet', 'Sbl', 'Shr', 'Wid', 'Ylt')

  dps <- left_join(out_list, ss_names, by = 'species')

  return(dps)
}

