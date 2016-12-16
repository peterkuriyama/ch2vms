#' Create delta plots

#' Function to generate values relevant for delta plots. Delta plots come from Gillis et al. 
#' (2008)

#' @param none There are no parameters
#' @export
#' @examples
#' delta_plot()

delta_plot <- function(){
  #Identify top species
  wc_data %>% group_by(species) %>% summarize(apounds = sum(apounds, na.rm = TRUE)) %>%
    arrange(desc(apounds)) %>% head(n = 30) %>% as.data.frame

  #Select species of interest
  spps <- c('Dover Sole', 'Arrowtooth Flounder', 'Sablefish', 'Petrale Sole', 'Longspine Thornyhead',
    'Shortspine Thornyhead', 'Chilipepper Rockfish', 'Lingcod', 'Yellowtail Rockfish', 
    'Darkblotched Rockfish', 'Pacific Ocean Perch', 'Bank Rockfish', 'Widow Rockfish' )
  wc_data %>% filter(species %in% spps) -> of_int_data

  ##Use hpounds only
  #hpounds of all data
  delta_plots <- of_int_data %>% 
    group_by(species, tow_year) %>% 
    do(data.frame(prop_zero = calc_delta_plot(data = ., spp = unique(.[, 'species']), focus = 'hpound')[1],
      skew = calc_delta_plot(data = ., spp = unique(.[, 'species']), focus = 'hpound')[2])) %>%
    as.data.frame 

  ss <- unique(delta_plots$species)
  abbrevs <- lapply(strsplit(ss, " "), function(x) sapply(x, FUN = function(y) paste0(substr(y, 1, 1), collapse = '')))

  ss_names <- ldply(lapply(abbrevs, FUN = function(x) paste0(x, collapse = '')))
  ss_names <- data.frame(species = ss, abbrev = ss_names[, 1])
  ss_names$species <- as.character(ss_names$species)
  ss_names$abbrev <- as.character(ss_names$abbrev)
  ss_names$short <- c("Arr", 'Bnk', 'Chl', 'Drk', 'Dvr', 'Lng', 'Lon', 
    'POP', 'Pet', 'Sbl', 'Shr', 'Wid', 'Ylt')

  dps <- left_join(delta_plots, ss_names, by = 'species')

  #Final Delta Plot
  ggplot(dps) + geom_text(aes(x = prop_zero + .05, y = skew, label = short)) + 
    geom_point(aes(x = prop_zero, y = skew)) + 
    facet_wrap(~ tow_year) + theme_bw() + geom_hline(aes(yintercept = 0), linetype = 2)
}

