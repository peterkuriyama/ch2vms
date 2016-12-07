#' Compare metrics between two species

#' Function to count number of tows in bins. Uses ggplot stat_bin2d functions.

#' @param data Defaults to wc_data
#' @param species1 Species 1 to look at
#' @param species2 Species 2 to look at
#' @param focus Column of interest, Options are hperc, aperc, hpounds, apounds

#' @export
#' @examples
#' compare_two_spp(data = wc_data, species1 = "Dover Sole", species2 = "Sablefish", focus = 'hperc')


#Possible things to look at
#hperc, aperc, hpounds, apounds

compare_two_spp <- function(data = wc_data, species1, species2, focus = 'hperc'){

  #Create data frame with unique tows
  unq_hauls <- unique(data$haul_id)
  unq_hauls <- data.frame(unq_hauls)
  names(unq_hauls) <- 'haul_id'
  unq_hauls$haul_id <- as.character(unq_hauls$haul_id)

  #Pull species 1 info
  to_add1 <- data %>% filter(species == species1) %>% select(haul_id, tow_hperc, tow_aperc,
    duration, set_lat, set_long, up_lat, up_long, depth1, hpounds, apounds, tow_day,
    tow_month, tow_year)
  to_add1$species <- species1

  focus_column <- grep(focus, names(to_add1))
  hauls1 <- left_join(unq_hauls, to_add1[, c(1, focus_column)], by = 'haul_id')
  names(hauls1)[2] <- paste0(names(hauls1)[2], '_spp1')
  hauls2 <- left_join(unq_hauls, to_add2[, c(1, focus_column)], by = 'haul_id')
  names(hauls2)[2] <- paste0(names(hauls2)[2], '_spp2')

  hauls <- left_join(hauls1, hauls2, by = 'haul_id')
  hauls[, 2] <- na_to_zero(hauls[, 2])
  hauls[, 3] <- na_to_zero(hauls[, 3])

  # plot(hauls[, 2], hauls[, 3], pch = 19)

  wc_data %>% select(haul_id, set_lat, set_long, up_lat, up_long, duration, depth1, tow_day,
    tow_month, tow_year) %>% distinct -> unq_hauls

  hauls <- left_join(hauls, unq_hauls, by = 'haul_id')
  return(hauls)

}






#Convert NAs to zeros
  # to_add1$tow_hperc <- na_to_zero(to_add1$tow_hperc)
  # to_add1$tow_aperc <- na_to_zero(to_add1$tow_aperc)
  # to_add1$hpounds <- na_to_zero(to_add1$hpounds)
  # to_add1$apounds <- na_to_zero(to_add1$apounds)
  



  # #Pull Species 2 info
  # to_add2 <- data %>% filter(species == species2) %>% select(haul_id, tow_hperc, tow_aperc,
  #   duration, set_lat, set_long, up_lat, up_long, depth1, hpounds, apounds, tow_day,
  #   tow_month, tow_year)
  # to_add2$species <- species2

  # #Convert NAs to zeros
  # to_add2$tow_hperc <- na_to_zero(to_add2$tow_hperc)
  # to_add2$tow_aperc <- na_to_zero(to_add2$tow_aperc)
  # to_add2$hpounds <- na_to_zero(to_add2$hpounds)
  # to_add2$apounds <- na_to_zero(to_add2$apounds)


  # #the_n
  
  # out <- left_join(to_add1[, c(1, focus_column)], to_add2[, c(1, focus_column)],
  #   by = 'haul_id')
  
  # hauls <- left_join(unq_hauls, to_add, by = 'haul_id')
  # names(hauls)[c(2, 3)] <- c('dover_hperc', 'dover_aperc')

  # #Fill in NAs with 0
  # hauls[which(is.na(hauls[, 2])), 2] <- 0
  # hauls[which(is.na(hauls[, 3])), 3] <- 0

  # #Add in other species
  # second_add <- wc_data %>% filter(species == 'Sablefish') %>% select(haul_id, tow_hperc, 
  #   tow_aperc)


