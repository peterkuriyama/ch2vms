###VMS Stuff
#--------------------------------------------------------------------------------
#To Do
#Expand tow footprints to get a sense of overall effort rather than set/up points
#Remove points with only 

#Explore correlations between two species
#Calculate skew of catch distributions
#Zero 

#NE ALSO
#Temporal Distribution of effort different before and after?
#Wher and how are they catching species in certain areas?
#Shift in effort for each vessel, directional shift maybe?
#Track median of distribution or something 
#k means clustering

# library(doParallel)
# cl <- makeCluster(6)
# registerDoParallel(cl)

#--------------------------------------------------------------------------------
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)

#Start
setwd("/Users/peterkuriyama/School/Research/ch2vms")

#Load package
install_github("peterkuriyama/ch2vms/ch2vms")
library(ch2vms)

# load_all()

#--------------------------------------------------------------------------------
#Load West Coast Logbook Data
wc_data <- load_wc_logbook()
wc_data <- wc_data[-grep("\\.", row.names(wc_data)), ]

#--------------------------------------------------------------------------------
#Delta plots for subset of species, specified by Gillis et al. (2008)
wc_data %>% group_by(species) %>% summarize(apounds = sum(apounds, na.rm = TRUE)) %>%
  arrange(desc(apounds)) %>% head(n = 30) %>% as.data.frame

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

#Plot
ggplot(dps) + geom_text(aes(x = prop_zero, y = skew, label = short)) + 
  facet_wrap(~ tow_year) + 

#Test of significance from Gillis 2008? 

#--------------------------------------------------------------------------------
#Modify wc data to incorporate delta things, clustering, rjinsdorp things

#Gillis and Rjinsdorp 


#Remove certain columns
wc_data <- wc_data %>% select(-c(rpcid, dpcid, spid, dday, dmonth, dyear, dport, rday, rmonth, ryear,
  bimon_period, rport, area, arid_psmfc, block, msec_lat, msec_long, up_msec_lat, up_msec_long,
  apounds_calculated, apounds_wdfw, ft_match_flag, ftid2, ftid3, ftid4, ftid5, gr_endor_1,
  gr_endor_2, gr_endor_3, gr_endor_4, gr_endor_5, sable_tier_1, sable_tier_2, sable_tier_3,
  sable_tier_4, sable_tier_5, len_endor_3, len_endor_4, len_endor_5, avglat, avglong, avglat.gis,
  group.north, group.south, gf, ns, rebuilding, up_date, set_date_full, up_date_full, duration_hour,
  set_lat_r, set_long_r, up_lat_r, up_long_r, dist_slc_km, dist_hf_km, dist_slc_mi, lat, long,
  permid_2, permid_3, permid_4, permid_5, gr_sector, agid)) 

#Calculate percentage compositions for each tow
wc_data %>% group_by(haul_id) %>% mutate(tow_hpounds = sum(hpounds, na.rm = TRUE), 
    tow_apounds = sum(apounds, na.rm = TRUE),
    tow_hperc = hpounds / tow_hpounds, tow_aperc = apounds / tow_apounds, 
    tow_diff = tow_aperc - tow_hperc) %>% as.data.frame %>% 
select(-c(tow_hpounds, tow_apounds)) -> wc_data

#Calculate percentage compositions for each trip?



#Create data frame with unique tows
unq_hauls <- unique(wc_data$haul_id)
unq_hauls <- data.frame(unq_hauls)
names(unq_hauls) <- 'haul_id'
unq_hauls$haul_id <- as.character(unq_hauls$haul_id)

to_add <- wc_data %>% filter(species == 'Dover Sole') %>% select(haul_id, tow_hperc, tow_aperc,
  duration, set_lat, set_long, up_lat, up_long, depth1, hpounds, apounds, tow_day,
  tow_month, tow_year)

hauls <- left_join(unq_hauls, to_add, by = 'haul_id')
names(hauls)[c(2, 3)] <- c('dover_hperc', 'dover_aperc')

#Fill in NAs with 0
hauls[which(is.na(hauls[, 2])), 2] <- 0
hauls[which(is.na(hauls[, 3])), 3] <- 0

#Add in other species
second_add <- wc_data %>% filter(species == 'Sablefish') %>% select(haul_id, tow_hperc, 
  tow_aperc)


hauls


#Compare two species
#Dover sole and




#how to filter for two species, to look at specific changes in compositions
#Effect of depth


wc_data %>% filter(species == "Dover Sole") %>% group_by(up_long) %>% 
  summarize(avg_perc = mean(tow_hperc, na.rm = TRUE), nvals = length(tow_hperc)) %>% 
  ggplot() + geom_point(aes(x = up_long, y = avg_perc, size = nvals))

wc_data %>% filter(species == 'Dover Sole' | species == 'Sablefish') 




wc_data %>% filter(species == 'Dover Sole') %>% ggplot() + 
  geom_histogram(aes(tow_aperc)) + facet_wrap(~ rport_desc)

#Delta









catch_comps %>% filter(species == 'Dover Sole') %>% ggplot() + geom_histogram(aes(tow_hperc))

geom_bar(stat = 'identity',
  aes(x = factor(species), y = tow_hperc))


geom_hist(

  stat = 'identity', aes(x = factor(species), y = clust_hperc)) +
     facet_grid(~ clust) + theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(catch_comps )




ggplot()






#Look at a single vessel
one_vess <- subset(wc_data, drvid == '578282')

#Most caught species
one_vess %>% group_by(species) %>% summarize(tot_hpounds = sum(hpounds, na.rm = TRUE),
  tot_apounds = sum(apounds, na.rm = TRUE)) %>% arrange(desc(tot_apounds))

#What else did tows that caught Dover catch?
dover_hauls <- one_vess %>% filter(species == 'Dover Sole') %>% distinct(haul_id) 

#Filter out only dover hauls and see what else was caught
dover_hauls <- one_vess[one_vess$haul_id %in% dover_hauls$haul_id, ]
dover_hauls %>% group_by(haul_id) %>% mutate

dover_hauls %>% group_by(species) %>% summarize(tot_hpounds = sum(hpounds, na.rm = TRUE),
  tot_apounds = sum(apounds, na.rm = TRUE)) %>% arrange(desc(tot_apounds))


one_vess %>% group_by()


one_vess %>% filter(haul_id %in% dover_hauls)



unique(one_vess$species)



#Add midpoint of each tow to 

#Filter data to keep relevant columns

#Look only


#Remove expanded tows




#--------------------------------------------------------------------------------
#Merge VMS data to include declarations
# load("data/ne_vms_wdeclarations.Rdata")
# ne_vms_dec <- ne.vms
# rm(ne.vms)
# names(ne_vms_dec) <- tolower(names(ne_vms_dec))

# #Remove values that don't have the same number of characters
# ne_vms_dec <- ne_vms_dec[-which(nchar(ne_vms_dec$declarations) != 14), ]

# #Parse out the Northeast VMS Descriptions
# descs <- ldply(strsplit(ne_vms_dec$declarations, "-"))
# names(descs)[1:2] <- c("plan", "program")
# descs$area <- substr(descs[, 3], 1, 2)
# descs$daysatsea <- substr(descs[, 3], 3, 3)
# descs$gear <- substr(descs[, 3], 4, 4)
# descs$trip_mod <- substr(descs[, 3], 5, 5)
# descs$stock_area <- substr(descs[, 3], 6, 6)
# descs$plan <- NULL
# descs$V3 <- NULL

# ne_vms_dec <- cbind(ne_vms_dec, descs)

#Format ne_vms data with declarations
# to_add <- ldply(strsplit(ne_vms_dec$filename_short, "_"))
# names(to_add) <- c('year', 'month', 'region')

# to_add$year <- substr(to_add$year, 6, 10)
# to_add$year <- as.integer(to_add$year)
# to_add$month <- as.numeric(to_add$month)

# ne_vms_dec <- cbind(ne_vms_dec, to_add)
# ne_vms_dec$unq <- paste(ne_vms_dec$year, ne_vms_dec$month, ne_vms_dec$region,
#   ne_vms_dec$latitude, ne_vms_dec$longitude)

#Format the NE VMS data with speeds
load('data/ne_vms.Rdata')
ne_vms$latitude <- gsub("\302", "", ne_vms$latitude)
ne_vms$longitude <- gsub("\302", "", ne_vms$longitude)
ne_vms <- ne_vms %>% filter(year >= 2009)
ne_vms$speed <- as.numeric(ne_vms$speed)



#Look at gillnet point locations over years
years <- 2009:2014

for(yr in years){
  filename <- 'figs/gillnet'
  filename <- paste0(filename, yr)
  filename <- paste0(filename, '.png')

  png(width = 14.57, height = 12.13, file = filename, res = 200, units = 'in')

  ne_vms %>% filter(year == yr & region == 'G') -> test

  #Remove the outlier speeds
  test %>% filter(speed < 20) -> test

  #Plot everything
  zz <- wc_map + geom_point(data = test, aes(x =  lon, y = lat, colour = speed)) + 
          scale_x_continuous(limits = c(-71.2, -68.5)) + 
          scale_y_continuous(limits = c(40, 44)) + facet_wrap(~ month)
  print(zz)

  dev.off()

  print(yr)
}

# Look at gillnet points across month in each year
months <- 1:12

for(mt in months){
  filename <- 'figs/gillnet'
  filename <- paste0(filename, mt)
  filename <- paste0(filename, '.png')

  png(width = 14.57, height = 12.13, file = filename, res = 200, units = 'in')

  ne_vms %>% filter(month == mt & region == 'G') -> test

  #Remove the outlier speeds
  test %>% filter(speed < 20) -> test

  #Plot everything
  zz <- wc_map + geom_point(data = test, aes(x =  lon, y = lat, colour = speed)) + 
          scale_x_continuous(limits = c(-71.2, -68.5)) + 
          scale_y_continuous(limits = c(40, 44)) + facet_wrap(~ year)
  print(zz)

  dev.off()

  print(mt)
}


#Bin 
source("R/bin_data.R")

#only the zeroes bro
zeroes <- ne_vms %>% filter(speed == 0 & region == 'G')
zero_bin <- bin_data(zeroes, x_col = 'lon', y_col = 'lat')

#Incorporate distance from port also?

wc_map + scale_x_continuous(limits = c(-71, -66)) + scale_y_continuous(limits = c(41, 42.7)) + 
  geom_tile(data = zero_bin, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)


#Cluster the tows by speed and location, aggregat
#Locations of ports?







#Filter out gillnet rows
# ne_vms <- ne_vms[grep("_WH.csv", ne_vms$filename), ]


# rm(to_add)
# ne_vms_dec <- ne_vms_dec %>% select(latitude, longitude, 
#     declarations, filename_short, year, month, region)

ne_vms_dec %>% filter(year >= 2009) -> ne_vms_dec

#Add in a column of unique values to compare the two datasets
ne_vms$unq <- paste(ne_vms$year, ne_vms$month, ne_vms$region, ne_vms$latitude, 
  ne_vms$longitude)

ne_vms_dec$unq <- paste(ne_vms_dec$year, ne_vms_dec$month, ne_vms_dec$region, 
  ne_vms_dec$latitude, ne_vms_dec$longitude)

#Filter out gillnet tows from each
# ne_vms %>% filter(region != "G") -> ne_vms
# ne_vms_dec %>% filter(region != 'G') -> ne_vms_dec


#--------------------------------------------------------------------------------
#SPATIAL ANALYSES

#Load Map
world_map <- map_data("world")

wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = NA, color = 'gray')

# wc_map + scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(34, 46.5))

#--------------------------------------------------------------------------------
#Spatially Analyze Northeast VMS Data

#Northeast VMS Data
ne_vms <- load_ne_vms()

#Assign speed categories to ne_vms
ne_vms$category <- 'nada'
ne_vms$speed <- as.numeric(ne_vms$speed)

ne_vms[which(ne_vms$speed < 2), 'category'] <- 'stopped - <2knots' 
ne_vms[which(ne_vms$speed >= 2 & ne_vms$speed < 6), 'category'] <- 'fishing - between 2 and 6 knots' 
ne_vms[which(ne_vms$speed >= 6 & ne_vms$speed <= 10), 'category'] <- 'steaming - between 6 and 10 knots' 
ne_vms[which(ne_vms$speed > 10), 'category'] <- 'errors - greater than 10 knots' 

#Look at month of data
ne_vms %>% filter(year == 2009 & month == 2 & region == 'WH') -> one_month
unique(one_month$category)

png(width = 12, height = 9.15, units = 'in', res = 200, file = 'figs/speed_cats_ne.png')
wc_map + geom_point(data = one_month, aes(x = lon, y = lat)) + 
  scale_x_continuous(limits = c(-73, -68)) + 
  scale_y_continuous(limits = c(40, 43.9)) + facet_wrap(~ category)
dev.off()

#Try binning one month
binned <- bin_data(one_month)

#Back calculate



#Function to identify rows assigned to specific grids

found <- vector('list', length = nrow(binned))

for(ii in 1:length(found)){
  if(ii %% 100 == 0) print(ii)
  temp <- binned[ii, ]

  find_these <- which(-wc_data_orig$long <= temp$xmax & -wc_data_orig$long >= temp$xmin &
    wc_data_orig$lat <= temp$ymax & wc_data_orig$lat >= temp$ymin & 
    wc_data_orig$tow_year == temp$year) 

  ifelse(length(find_these) == temp$count, found[[ii]] <- find_these, stop(print(ii)))

}




#Areas that have diffs greater than 1000
really_high <- subset(binned, diff_ct >= 1000)

#Find Year of maximum tows
really_high %>% group_by(unq) %>% arrange(year) %>% 
  mutate(max_year = year[which(count == max_ct)]) %>% 
  as.data.frame -> really_high

#Find the number of maximum
really_high %>% group_by(unq) %>% summarize(max_year = unique(max_year)) %>% 
  group_by(max_year) %>% summarize(nsites = length(unique(unq)))

#Plot all the really high sites
png(width = 12.3, height = 7, units = 'in', res = 200, file = 'figs/really_high.png')
wc_map + scale_x_continuous(limits = c(-71, -66)) + scale_y_continuous(limits = c(41, 42.7)) + 
  geom_tile(data = really_high, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)
dev.off()

#2009
temp <- really_high %>% filter(max_year == 2009)

png(width = 16.5, height = 4.5, units = 'in', res = 200, file = 'figs/max_2009.png')
wc_map + scale_x_continuous(limits = c(-71, -66)) + scale_y_continuous(limits = c(41, 42.7)) + 
  geom_tile(data = temp, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_grid(~ year)
dev.off()

#2010
temp <- really_high %>% filter(max_year == 2010)

png(width = 16.5, height = 4.5, units = 'in', res = 200, file = 'figs/max_2010.png')
wc_map + scale_x_continuous(limits = c(-71, -66)) + scale_y_continuous(limits = c(40.7, 42.3)) + 
  geom_tile(data = temp, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_grid(~ year)
dev.off()



#--------------------------------------------------------------------------------
#Logbook data with expanded tows
wc_data <- load_wc_logbook() #Expanded tows

#Logbook data with unique tows
wc_unique_tows <- wc_data[-grep("\\.", rownames(wc_data)), ]


#----------------------------------------------------------------------------------------------------
#Look at Sites that have had the greatest change in number of tows


#----------------------------------------------------------------------------------------------------
#Map of all 6 years of data
png(width = 18, height = 12, file = 'figs/ne_years.png', units = 'in', res = 200)
wc_map + scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(34, 46.5)) + 
  geom_tile(data = binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year) 
dev.off()




#Look at sites by range of diff values
wc_map + scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(40, 44)) + 
  geom_tile(data = binned, aes(x = x, y = y, fill = diff_ct)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') 


#Plot these things by individual
#Identify new areas?

#Need to fill in missing values for nyears
#Look at nyear == 1
binned %>% group_by(year) %>% summarize(nsites = length(unique(unq))) %>% 
  ggplot(aes(x = year, y = nsites)) + geom_line(size = 1.4) + theme_bw() + 
  xlab('Year') + ylab("Number of Unique Sites")

#Number of tows
binned %>% group_by(year) %>% summarize(npings = sum(count)) %>% ggplot(aes(x = year, 
  y = npings)) + geom_line(size = 1.4) + theme_bw() + xlab("Year") + ylab("Number of Pings")

#Look at the Overall pattern across all 6 years
wc_map + scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(34, 46.5)) + 
  geom_tile(data = binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)

head(wc_data_orig)
wc_data_orig %>% group_by(drvid, tow_year) %>% summarize(ntows = length(unique(haul_id))) %>% 
  arrange(desc(ntows)) %>% as.data.frame 


binned %>% filter(nyears == 1) %>% group_by(year) %>% summarize(nsites = length(unique(unq)))






binned %>% select(unq, year, )



range(really_high$x)
range(really_high$y)

#Look at top 25% of tows
quantile(binned$diff_ct)
binned %>% group_by(unq) %>% mutate(nyears = length(unique(group))) %>% as.data.frame -> binned

#really high


#Find Areas where the 

ggplot(really_high) + geom_line(aes(x = group, y = count, group = unq)) + theme_bw()


quantile(tops$diff_ct)
unique(tops$diff_ct)




ggplot(binned) + geom_line(aes(x = group, y = count, group = unq))




binned %>% filter(count >= 1000)


wc_map + scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(34, 46.5)) + 
  geom_tile(data = binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') 



ggplot() + geom_tile(data = binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + 
  scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(34, 46.5)) 

#Plot Northeast Map
wc_map + scale_x_continuous(limits = c(-77, -62)) + scale_y_continuous(limits = c(34, 46.5)) +
  geom_tile(data = binned, aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ group)



ggplot(data = binned) + geom_tile(aes(x = x, y = y, fill = count)) + 
  scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ group)



geom_tile(data = subset(diff_plot_ba, legal == 'yes'), aes(x = x,



# # bin <- ggplot(wc_data, aes(x = -long, y = lat, group = tow_year)) + 
# #   stat_bin2d(binwidth = c(.0909, .11))
# # 
# # binned <- ggplot_build(bin)$data[[1]]
# # 
# # #create year column in binned
# # yrz <- data.frame(group = 1:6, year = as.character(2008:2013))
# # yrz$year <- as.character(yrz$year)
# # binned <- merge(binned, yrz, by = 'group')



#Now adjust the latitudes and longitueds
lons <- ldply(strsplit(ne_vms$longitude[1:5], "\302\260"))
lons$V1 <- as.numeric(lons$V1)
lons$V2 <- as.numeric(lons$V2)

lats <- ldply(strsplit(ne_vms$latitude[1:5], "\302\260"))
lats$V1 <- as.numeric(lats$V1)
lats$V2 <- as.numeric(lats$V2)

lons$V1[1] - lons$V2[1] / 60
lats$V1[1] + lats$V2[1] / 60




lats$V1 + lats$v2 / 60

as.numeric(lats$V2) / 60

#Try converting degrees minutes seconds to decimals 
37.7 / 60




head(ne_vms)




ne_vms %>% filter(speed != 0)


new_ne_vms <- ne_vms[(is.na(ne_vms$speed) == FALSE), ]




new_ne_vms %>% group_by(year) %>% summarize(nonzeros = length(speed != 0)) %>%
  sum(., nonzeros)


#Filter so that all of thes values have speeds
ne_vms %>% filter(is.na(speed) == FALSE) -> new_ne_vms


#Play with transforming
strsplit(ne_vms[1:5, 'latitude'], '\302\260')


strsplit(ne_vms$lati)

ne_vms %>% 






 





########################################################################################################
#SCRAPS



# ####################################################
# mb <- subset(wc_data_orig, dport_desc == 'MORRO BAY' & dyear == 2012)
# mb <- mb[-grep('\\.', rownames(mb)), ]


# mb %>% group_by(species) %>% summarize(toth = sum(hpounds, na.rm = TRUE),  
#   tota = sum(apounds, na.rm = TRUE)) %>% as.data.frame


# unique(wc_data_orig)





# # wc_data <- subset(wc_data, ha_ratio >= 0.6 & ha_ratio <= 1.1)


# ####################################################
# #Check that every

# #Add in ratio of apounds to hpounds
# #ratio should be between 0.6-1.1 for acceptable rows, Lee and Sampson
# # wc_data$ha_ratio <- wc_data$hpounds / wc_data$apounds

# # hist(subset(wc_data, ha_ratio < 2)$ha_ratio, breaks = 30) #histogram looks fairly normal
# # length(which(wc_data$ha_ratio <= 1.2 & 
# #   wc_data$ha_ratio >= 0.6)) / nrow(wc_data) #45% of tows seem to 










# #--------------------------------------------------------------------------------
# #Source Functions to plot things
# funcs <- list.files('R')

# source('R/ch2_load_and_format.r')

# #Load Functions
# for(ii in 1:length(funcs)){
#   source(paste0("R/", funcs[ii]))
# }

# sapply(funcs, FUN = function(x) source(paste0('R/', x)))

# #Takes 45 seconds to run
# # wc_data <- ch2_load_and_format()
# #load wc_data that has already been run
# # load('output/wc_data.Rdata')

# #--------------------------------------------------------------------------------
# #
# wc_data[1, c('set_lat', 'up_lat', 'avglat')]

# #Filter data so that hpounds and apounds ratios are between 0.6 and 1.1
# wc_data_filt <- subset(wc_data, ha_ratio >= 0.6 & ha_ratio <= 1.1)

# #make sure everything is a unique row
# wc_data_filt <- wc_data_filt[-(grep("\\.", rownames(wc_data_filt))), ]

# #avglat and avglon are the midpoints of each tow, good starting point
# #cut down number of columsn because it's too big right now
# which(wc_data_filt$dyear != wc_data_filt$ryear)

# #change all the longitudes to negative
# wc_data_filt$avglong <- -wc_data_filt$avglong

# #Cents for centroids (medians)
# wc_data_filt %>% group_by(dyear, drvid) %>% summarise(mean_lat = mean(avglat, na.rm = TRUE),
#   mean_lon = mean(avglong, na.rm = TRUE))  %>% group_by(drvid) %>%
#   do({
#     mod_lon <- lm(mean_lon ~ dyear, data = .)
#     slope_lon <- coef(mod_lon)[2]
#     names(slope_lon) <- NULL

#     mod_lat <- lm(mean_lat ~ dyear, data = .)
#     slope_lat <- coef(mod_lat)[2]
#     names(slope_lat) <- NULL
#     data.frame(., slope_lon, slope_lat)
#   }) %>% as.data.frame -> cents

# #Plot the changes in slopes
# cc <- cents[, c('drvid', 'slope_lon', 'slope_lat')] %>% distinct()
# plot(cc$slope_lon, cc$slope_lat, pch = 19, col = "#4D4D4D20")
# abline(h = 0)
# abline(v = 0)

# #normalize lat and lon
# cents %>% group_by(drvid) %>% mutate(std_lat = (mean_lat - mean(mean_lat)) / sd(mean_lat),
#   std_lon = (mean_lon - mean(mean_lon)) / sd(mean_lon)) %>% as.data.frame -> cents
# cents %>% group_by(dyear) %>% mutate(avg_std_lat = mean(std_lat, na.rm = TRUE), 
#   avg_std_lon = mean(std_lon, na.rm = TRUE)) %>% as.data.frame -> cents




# ggplot() + geom_line(data = cents, aes(x = dyear, y = std_lon, group = drvid, 
#   col = "#4D4D4D60")) + theme_bw() + geom_line(data = cents,
#   aes(x = dyear, y = avg_std_lon))

# ggplot(data = cents, aes(x = dyear, y = std_lon, group = drvid)) + 
#   geom_line(colour = "#4D4D4D60") + theme_bw() + geom_line(data = cents_a,
#     aes(x = dyear, y = avg_std_lon))

# #--------------------------------------------------------------------------------
# #Individual vessel things
# #Very few vessels are fishing with multiple gear types


# qlon <- quantile(cents$slope_lon, na.rm = TRUE)
# qlat <- quantile(cents$slope_lat, na.rm = TRUE)

# subset(cents, slope_lon >= qlon[4] & slope_lat >= qlat[4])


# #Try plotting with a random drvid
# # temp <- subset(wc_data_filt, drvid == '546053')
# # temp1 <- subset(cents, drvid == '1037785')

# wc_map + geom_point(data = temp, aes(x = -long, y = lat, colour = hpounds)) + 
#   scale_x_continuous(limits = c(-125, -123.5)) + scale_y_continuous(limits = c(44.5, 48)) +
#   facet_wrap(~ when + target)

# #Look at the individual catches for each vessel

# #Find species that has the highest average catch value







# #Find most populous clusters
# euc %>% group_by(cluster) %>% summarise(n = length(cluster)) %>% as.data.frame -> nclust
# nclust <- nclust[order(nclust$n, decreasing = TRUE), ]


# # ggdata <- subset(euc, cluster == 1 | cluster == 9 | cluster == 32)
# ggdata <- euc[euc$cluster %in% nclust[nclust$n >= 10, 'cluster'], ]

# ggplot() + geom_segment(data = ggdata, aes(x = -set_long, xend = -up_long,
#   y = set_lat, yend = up_lat, group = cluster, colour = cluster), 
# arrow = arrow(length = unit(0.1, 'cm'))) +  theme_bw()

# facet_wrap(~ cluster) +

# #--------------------------------------------------------------------------------
# #Create plots of aggregated fleet effort
# source('R/aggregated_effort.r')

# #--------------------------------------------------------------------------------
# #Estimate kernel densities for each year

# #Smoothed Densities, takes the most time to plot
# # dens <- ggplot(wc_data, aes(x = -set_long, y = set_lat, group = tow_year)) + 
# #   stat_density2d(aes(fill = ..level..), geom = 'polygon') 

# # dens + facet_wrap(~ tow_year)

# #--------------------------------------------------------------------------------
# #Expand tow points between start and end points
# #specify distance between expansion points

# expanded <- vector('list', length = nrow(wc_data))

# for(ii in 1:nrow(wc_data)){
# # for(ii in 1:100){
#   # if(sum(is.na(wc_data[ii, c('up_lat', 'up_long', 
#   #   'set_lat', 'set_long')])) != 4) next
#   tt <- wc_data[ii, c('up_lat', 'set_lat', 'up_long', 'set_long', 'dist_slc_km')]
  
#   if(is.na(tt$dist_slc_km)) next
#   if(sum(is.na(tt[, c('up_lat', 'up_long', 'set_lat', 'set_long')])) > 0) next
  
#   expanded[[ii]] <- expand_tow_track(tt)
#   if(ii %% 10000 == 0) print(ii)
# # if(ii %% 10 == 0) print(ii)
# }

# #check to see that expanded tows are within the ranges of each
# #row 5
# expanded[[5]]
# wc_data[5, c('set_lat', 'set_long', 'up_lat', 'up_long')]

# #row 300000
# expanded[[300000]]
# wc_data[300000, c('set_lat', 'set_long', 'up_lat', 'up_long')]

# #row 500000
# expanded[[200000]]
# wc_data[200000, c('set_lat', 'set_long', 'up_lat', 'up_long')]

# names(expanded) <- 1:length(expanded)
# expanded_df <- plyr::ldply(expanded)

# #Expand wc data and add tow points
# expanded_df$.id <- as.integer(expanded_df$.id)
# idea <- wc_data[expanded_df$.id, ]

# idea$lat <- expanded_df$lat
# idea$long <- expanded_df$long

# #This is the expanded data now...
# wc_data <- idea
# save(wc_data, file = 'output/wc_data_expanded_tows.Rdata')

# #--------------------------------------------------------------------------------

# #Try to calculate this in a different way


# # extract data from ggplot
# binned <- ggplot_build(bh)$data[[1]]

# # #create year column in binned
# yrz <- data.frame(group = 1:6, year = as.character(2008:2013))
# yrz$year <- as.character(yrz$year)
# binned <- merge(binned, yrz, by = 'group')

# ##### 
# count_vess <- function(zz){
#   wc_data_unique %>% filter(tow_year == zz$year & -long >= zz$xmin &
#                        -long <= zz$xmax & lat >= zz$ymin & lat <= zz$ymax) %>% 
#     summarise(nvess = length(unique(drvid))) -> out
#   return(out$nvess)
# }

# #
# nvess <- rep(999, nrow(binned))

# strt <- Sys.time()
# #Takes like a minute
# for(jj in 1:nrow(binned)){
#   #loop over rows in binned
#   zz <- binned[jj, ]
#   nvess[jj] <- count_vess(binned[jj, ]) #store number of vessels
  
#   if(jj %% 500 == 0) cat('index= ', jj, 'time= ', Sys.time() - strt, '\n')
# }


# #----------------------------------------------------------
# #Add the counts to binned
# binned$nvess <- nvess
# binned$legal <- 'no'
# binned[which(binned$nvess >= 3), 'legal'] <- 'yes'
# binned %>% group_by(group) %>% mutate(tot_tows = sum(count), perc = count / tot_tows) %>%
#   as.data.frame -> binned

# #Average before and after
# binned$when <- sapply(binned$year, FUN = function(x) ifelse(x >= 2011, 'After', 'Before'))
# binned %>% group_by(x, y, xbin, ybin, when) %>% mutate(avg_perc = mean(perc)) %>% 
#   as.data.frame -> binned_avg_when
# binned_avg_when %>% group_by(x, y, xbin, ybin, when) %>% filter(row_number() == 1) %>% 
#   as.data.frame -> binned_avg_when

# diff_plot <- reshape2::dcast(binned_avg_when, x + y + xbin + ybin ~ when, value.var = 'avg_perc')
# diff_plot$diff <- diff_plot$After - diff_plot$Before

# #Merge in diff with binned
# diff_plot <- merge(diff_plot, binned[, c('xbin', 'ybin', 'legal')], by = c('xbin', 'ybin'))

# binned$when <- factor(binned$when, levels = c('Before', 'After'))

# nyears_fished <- binned %>% group_by(x, y) %>% 
#   summarise(nyrs_fished = length(is.na(perc)))
# nyears_fished$area_id <- 1:nrow(nyears_fished)

# binned <- merge(nyears_fished, binned, by = c('x', 'y'), all = FALSE)
# binned %>% group_by(x, y, when) %>% mutate(avg_perc = mean(perc, na.rm = TRUE)) %>%
#   as.data.frame -> binned
# binned$when <- as.factor(binned$when)
# reshape2::dcast(binned_avg_when, x + y + xbin + ybin ~ when, value.var = "avg_perc")

# #----------------------------------------------------------
# #Plot the legal spatial changes on maps
# legals <- binned %>% filter(legal == 'yes')

# #calculate summary statistics
# by_year <- reshape2::dcast(binned, x + y + xbin + ybin ~ year, value.var = 'avg_perc')
# by_when <- reshape2::dcast(binned_avg_when, xbin + ybin ~ when, value.var = 'avg_perc')

# by_when$status <- 1

# by_when[which(is.na(by_when$Before)), 'status'] <- 'new_area'
# by_when[which(is.na(by_when$After)), 'status'] <- 'old_area'
# by_when[which(by_when$After > by_when$Before), 'status'] <- 'increased'
# by_when[which(by_when$After < by_when$Before), 'status'] <- 'decreased'

# #Merge statuses into binned data frame
# names(by_when)[1:2] <- c('x', 'y')
# binned <- merge(binned, by_when[, c('x', 'y', 'status')], by = c('x', 'y'))

# #---------------------------------------------------------
# #All the Maps
# wc_plot <- ggplot() + geom_map(data = wc_map, map = wc_map, aes(x = long, y = lat, 
#     map_id = region), fill = 'gray') + 
#     geom_polygon(data = wc_map, aes(x = long, y = lat), fill = NA, color = 'gray') + 
#     coord_cartesian(xlim = c(-125, -117.3)) 

# blanks <- theme(panel.border = element_blank(),
#       panel.grid.major = element_blank(), panel.grid.minor = element_blank())      )      
#     #   , axis.title.x = element_blank(), axis.title.y = element_blank(),
#     #   axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
#     #   axis.text = element_blank())
    
#     # + theme(panel.border = element_blank(),
#     #   panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#     #   rect = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
#     #   axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
#     #   axis.text = element_blank())

# #Plot differences instead    
# reshape2::dcast(binned, x + y + xbin + ybin ~ when, value.var = 'avg_perc')

# #Plot old area figures
# png(width = 3.9, height = 7, units = 'in', res = 200,
#   file = 'figs/west_coast.png')
# wc_plot 
# dev.off()


# ##Diff plot with all values
# subset(diff_plot, legal == 'yes')

# #Remove NAs
# diff_plot_ba <- diff_plot[which(is.na(diff_plot$diff) == FALSE), ]

# subset(diff_plot_ba, legal == 'yes')

# png(width = 5, height = 7, units = 'in', res = 200, file = 'figs/diff.png')
# wc_plot + geom_tile(data = subset(diff_plot_ba, legal == 'yes'), aes(x = x,
#   y = y, fill = diff)) + scale_fill_gradient2(low = 'blue', high = 'red') + 
#   theme(panel.border = element_blank(),
#       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#       panel.background = element_rect(fill = 'white'))      
# dev.off()

# png(width = 4, height = 7, units = 'in', res = 200, file = 'figs/wc_diff.png')
# ggplot() + geom_tile(data = legals, aes(x = xbin, y = ybin, fill = avg_perc)) + 
#   facet_wrap(~ when) + theme_bw() + scale_fill_gradient(high = 'red', low = 'white') + 
#   theme(panel.border = element_blank(),
#       panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#       panel.background = element_rect(fill = 'gray'))      

# dev.off()

# wc_plot + geom_tile(data = subset(legals, status == 'old_area'),
#   aes(x = x, y = y, fill = avg_perc)) + facet_wrap(~ when) + 
#   scale_fill_gradient(high = 'red', low = 'white') + blanks

# binned %>% filter(status == 'old_area', legal == 'yes') %>% ggplot(aes(x = xbin,
#   y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
#   scale_fill_gradient(high = 'red', low = 'white')




# #Can't show any of the new areas
# binned %>% filter(status == 'new_area', legal == 'no') %>% ggplot(aes(x = xbin,
#   y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
# scale_fill_gradient(high = 'red', low = 'white')

# #Areas that Increased
# binned %>% filter(status == 'increased', legal == 'yes') %>% ggplot(aes(x = xbin,
#   y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
#   scale_fill_gradient(high = 'red', low = 'white')

# binned %>% filter(status == 'decreased', legal == 'yes') %>% ggplot(aes(x = xbin,
#   y = ybin)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
#   scale_fill_gradient(high = 'red', low = 'white')


# ggplot(binned, aes(x = xbin, y = ybin))


# by_when %>% group_by(status) %>% summarise(n_stats = n(), 
#   perc = round(n_stats / 1182 * 100, 2))
# # 34% of locations decreased
# # 33% of locations had increased percentage of average effort
# # 8% of locations were new
# # 25% of locations were not revisited




# #Find which sites were visited before and after catch shares
# sites <- data.frame(before = rowSums(is.na(by_year[, c('2008', '2009', '2010')]) == FALSE), 
#                     after = rowSums(is.na(by_year[, c('2011', '2012', '2013')]) == FALSE))
# sites$x <- by_year$x
# sites$y <- by_year$y
# sites$status <- 'poop'

# sites[which(sites$before & sites$after != 0), 'status'] <- ''

# apply(sites, MAR = 1, FUN = function(x) (which(x == 0)))

# sites <- cbind(rowSums(is.na(by_year[, c('2008', '2009', '2010')]) == FALSE),
#                rowSums(is.na(by_year[, c('2011', '2012', '2013')]) == FALSE))



# #--------------------------------------------------------------------------------
# #Estimate percentage of effort in each area annually
# ####Binned number of sets in each area

# # bin <- ggplot(wc_data, aes(x = -long, y = lat, group = tow_year)) + 
# #   stat_bin2d(binwidth = c(.0909, .11))
# # 
# # binned <- ggplot_build(bin)$data[[1]]
# # 
# # #create year column in binned
# # yrz <- data.frame(group = 1:6, year = as.character(2008:2013))
# # yrz$year <- as.character(yrz$year)
# # binned <- merge(binned, yrz, by = 'group')
# # 
# # #Calculate number of unique vessels in each bin...
# # nvess <- vector(length = nrow(binned))
# # start_time <- Sys.time()
# # 
# # for(ii in 1:nrow(binned)){
# # # for(ii in 1:20){
# #   rrow <- binned[ii,  ]
# # 
# #   temp <- subset(wc_data, tow_year == rrow$year)
# #   # wc_data %>% filter(tow_year == rrow$year) -> xx
# #   
# #   keeps <- which(-temp$long >= rrow$xmin & -temp$long <= rrow$xmax &
# #              temp$lat >= rrow$ymin & temp$lat <= rrow$ymax)
# #   # if(length(keeps) == 0) next
# #   if(ii %% 5 == 0) cat('time=',Sys.time() - start_time, 'ii=',ii, '\n')
# #     nvess[ii] <- length(unique(temp[keeps, 'drvid']))
# # }
# # 
# # save(nvess, file = 'output/nvess_in_each_bin.Rdata')
# # load(file = 'output/nvess_in_each_bin.Rdata')






# # 
# strt <- Sys.time()
# for(jj in 1:nrow(bbn)){
#   zz <- bbn[jj, ]
#   nnn[jj] <- count_vess(bbn[jj, ])
#   wc_data %>% filter(tow_year == zz$year & -long >= zz$xmin &
#     -long <= zz$xmax & lat >= zz$ymin & lat <= zz$ymax) %>% 
#     summarise(nvess = length(unique(drvid))) -> out
#   nnn[jj] <- out$nvess
#   if(jj %% 200 == 0) print(jj)
# }
# edn <- Sys.time()
# edn - strt

# bbn$nvess <- nnn

# bbn$legal <- 'no'
# bbn[which(bbn$nvess >= 3), 'legal'] <- 'yes'

# bbn %>% group_by(group) %>% mutate(tot_tows = sum(count), perc = count / tot_tows) %>%
#   as.data.frame -> bbn

# #Average before and after
# bbn$when <- sapply(bbn$year, FUN = function(x) ifelse(x >= 2011, 'after', 'before'))
# bbn %>% group_by(x, y, when) %>% summarise(avg_perc = mean(perc)) -> bbn_avg_when
# bbn$when <- factor(bbn$when, levels = c('before', 'after'))

# nyears_fished <- bbn %>% group_by(x, y) %>% 
#   summarise(nyrs_fished = length(is.na(perc)))
# nyears_fished$area_id <- 1:nrow(nyears_fished)

# bbn <- merge(nyears_fished, bbn, by = c('x', 'y'), all = FALSE)
# bbn %>% group_by(x, y, when) %>% mutate(avg_perc = mean(perc, na.rm = TRUE)) %>%
#   as.data.frame -> bbn
# bbn$when <- as.factor(bbn$when)
# cc <- dcast(bbn, x + y ~ when, value.var = 'avg_perc')




# #Plot percentages spatially
# bbn %>% filter(nyrs_fished == 6 & legal == 'yes') %>% ggplot(aes(x = xbin, y = y)) + 
#   geom_tile(aes(fill = perc)) + facet_wrap(~ year)

# #Plot percentage changes by area
# bbn %>% filter(nyrs_fished == 6 & legal == 'yes') %>% 
#   ggplot(aes(x = year, y = perc, group = area_id)) + geom_line()
  
#   geom_tile(aes(fill = perc)) + facet_wrap(~ year)




# #
# change_by_year <- dcast(bbn[, c('x', 'y', 'perc', 'year')], x + y ~ year, value.var = 'perc')


# #Calculate number of locations with increases, decreases new locations 
# # or gone locations by BEFORE or AFTER
# change_period <- dcast(bbn_when, x + y ~ when, value.var = "avg_perc")
# change_period$status <- 1
# change_period[which(is.na(change_period$before)), 'status'] <- 'new_area'
# change_period[which(is.na(change_period$after)), 'status'] <- 'old_area'
# change_period[which(change_period$after > change_period$before), 'status'] <- 'increased'
# change_period[which(change_period$after < change_period$before), 'status'] <- 'decreased'




# bbn %>% filter(year == 2008) %>% ggplot(aes(x = xbin, y = ybin)) + 
#   geom_tile(aes(fill = count)) + facet_wrap(~ legal)







# #copy over count column and make rows with less than 3 vessels
# binned$nvess <- nvess
# binned$can_plot <- 'no'
# binned[which(binned$nvess >= 3), 'can_plot'] <- 'yes'


# binned %>% group_by(group) %>% mutate(tot_tows = sum(count), perc = count / tot_tows) %>%
#   as.data.frame -> binned

# #add years to binned
# # binned <- merge(data.frame(group = 1:6, year = 2008:2013), binned, all = TRUE)

# #Plot Stylized figures without lat and long
# ggplot(binned, aes(x = xbin, y = y, group = can_plot)) + 
#   geom_tile(aes(fill = perc)) + facet_wrap(~ can_plot)

# #Plot on maps
# wc_plot + geom_tile(data = binned, aes(x = x, y = y, fill = perc)) + 
#   facet_wrap(~ year)




# #plots of areas with highest percentage of effort
# pdf(width = 12, height = 10, file = 'figs/effort_by_year.pdf')
# ggplot(binned, aes(x = x, y = y)) + 
#   geom_tile(aes(fill = perc)) + facet_wrap(~ year) + theme_bw()
# dev.off()

# #Average before and after
# binned$when <- sapply(binned$year, FUN = function(x) ifelse(x >= 2011, 'after', 'before'))
# binned %>% group_by(x, y, when) %>% summarise(avg_perc = mean(perc)) -> ba_binned
# # ba_binned$when <- as.factor(ba_binned$when, levels = c('before', 'after'))
# ba_binned$when <- factor(ba_binned$when, levels = c('before', 'after'))

# #Averaged before and after plots 
# pdf(width = 7, height = 7, file = 'figs/avg_effort_bef_aft.pdf')
# ggplot(ba_binned, aes(x = x, y = y)) + geom_tile(aes(fill = avg_perc)) + facet_wrap(~ when) + 
#   theme_bw()
# dev.off()

# #Calculate number of locations with increases, decreases new locations or gone locations
# sum_table <- dcast(ba_binned, x + y ~ when, value.var = "avg_perc")
# sum_table$status <- 1
# sum_table[which(is.na(sum_table$before)), 'status'] <- 'new_area'
# sum_table[which(is.na(sum_table$after)), 'status'] <- 'old_area'
# sum_table[which(sum_table$after > sum_table$before), 'status'] <- 'increased'
# sum_table[which(sum_table$after < sum_table$before), 'status'] <- 'decreased'



# sum_table %>% group_by(status) %>% summarise(n_stats = n(), perc = round(n_stats / 448 * 100, 2))
# # 40% of locations decreased
# # 30% of locations had increased percentage of average effort
# # 7% of locations were new
# # 24% of locations were not revisited

# sum_table$x <- round(sum_table$x, digits = 4)
# long_vals <- unique(sum_table$x)[order(unique(sum_table$x), decreasing = TRUE)]

# ids <- data.frame(x = long_vals, id = 1:length(unique(sum_table$x)))
# ids$id <- -ids$id

# sum_table <- merge(sum_table, ids, by = 'x')

# #Make plots of each category
# plot_by_status <- function(stat1){
#   y_range <- range(sum_table$y)
#   x_range <- range(sum_table$id)

#   inc <- subset(sum_table, status == stat1)
#   inc$status <- NULL
#   xx <- melt(inc, id = c('x', 'y', 'id'), variable.name = 'when')

#   # xx$x <- round(xx$x, digits = 4)
#   # ids <- data.frame(x = unique(xx$x), id = 1:length(unique(xx$x)))
#   # ids$id <- -ids$id

#   # xx <- merge(xx, ids, by = 'x')
  
#   pp <- ggplot(xx, aes(x = id, y = y)) + geom_tile(aes(fill = value)) + facet_wrap(~ when) +   
#           scale_fill_gradient(low = 'white', high = 'red') + ylim(y_range[1], y_range[2]) + 
#           xlim(x_range[1], x_range[2])

#   #Increased locations
#   # filename <- paste0('figs/', stat1, '.pdf')
#   # pdf(width = 12, height = 10, file = filename)
#   # pp <- wc_plot + geom_tile(data = xx, aes(x = x, y = y, fill = value)) + facet_wrap(~ when) + theme_bw() + 
#   #   scale_fill_gradient(low = 'white', high = 'red')

#    # ggplot(xx, aes(x = x, y = y)) + geom_tile(aes(fill = value)) + 
#    #  facet_wrap(~ when) + theme_bw()
#   print(pp)
#   # dev.off()
# }

# plot_by_status(stat1 = 'old_area')
# plot_by_status(stat1 = 'new_area')
# plot_by_status(stat1 = 'increased')
# plot_by_status(stat1 = 'decreased')


# #Start HERE
# #--------------------------------------------------------------------------------

# #--------------------------------------------------------------------------------
# #Map vessel average paths

# #Correct wc_data that has year 2022 to 2002
# wc_data[which(wc_data$tow_year == 2022), 'tow_year'] <- 2002

# #Remove duplicated tow rows, 
# wc_data %>% distinct(haul_id) -> unq_tows

# #Add in column indicating before/after catch share implementation
# unq_tows$when <- ifelse(unq_tows$tow_year <= 2010, 'bef', 'aft')

# #add incolumn indicating if number of years is balanced before/after
# unq_tows$balanced <- FALSE
# unq_tows[unq_tows$tow_year >= 2007, 'balanced'] <- TRUE

# #Calculate annual average locations for all vessels
# unq_tows %>% group_by(drvid, tow_year) %>%
#   summarise(avg_set_lat = mean(set_lat, na.rm = T),
#             avg_set_long = mean(set_long, na.rm = T), avg_up_lat = mean(up_lat, na.rm = T), 
#             avg_up_long = mean(up_long, na.rm = T), ntows = n()) -> avg_loc

# #For loop to create vessel maps
# ids <- unique(unq_tows$drvid)

# pdf(width = 7, height = 7,
#   file = 'figs/vess_tows.pdf')

# for(ii in 1:length(ids)){
#   unq_tows %>% filter(drvid == ids[ii]) -> unq1
#   unq1$when <- ifelse(unq1$tow_year <= 2010, 'bef', 'aft')
#   unq1$when <- factor(unq1$when, levels = c("bef", 'aft')) #so that order of plots is bef aft

#   # unq1_bef <- unq1 %>% filter(tow_year <= 2010 & tow_year >= 2008)
#   # unq1_aft <- unq1 %>% filter(tow_year > 2010)

#   avg_loc %>% filter(drvid == '1037785') -> avg1

#   long_range <- range(c(-unq1$set_long, -unq1$up_long), na.rm = TRUE)
#   long_range[1] <- long_range[1] - .5
#   long_range[2] <- long_range[2] + .5

#   lat_range <- range(unq1$set_lat, unq1$up_lat, na.rm =TRUE)
#   lat_range[1] <- lat_range[1] - .5
#   lat_range[2] <- lat_range[2] + .5
  
#   if(length(unique(unq1$when)) == 1){
#     xx <- wc_plot + coord_map(xlim = long_range, ylim = lat_range) + 
#                   geom_segment(data = unq1, 
#                     aes(x = -set_long, y = set_lat, xend = -up_long, 
#                     yend = up_lat, colour = tow_year), 
#                     arrow = arrow(length = unit(.03, 'npc'))) + 
#                   labs(title = paste(unique(unq1$drvid), as.character(unique(unq1$when))))  
#   }
#   if(length(unique(unq1$when)) == 2){
#     xx <- wc_plot + coord_map(xlim = long_range, ylim = lat_range) + 
#                       geom_segment(data = unq1 %>% filter(balanced == TRUE), 
#                         aes(x = -set_long, y = set_lat, xend = -up_long, 
#                         yend = up_lat, colour = tow_year), 
#                         arrow = arrow(length = unit(.03, 'npc'))) + facet_wrap(~ when) + 
#                       labs(title = unique(unq1$drvid))  
#   }
#   print(xx)
#   print(ii)
# }

# dev.off()



#           # geom_segment(data = unq1_aft, aes(x = -set_long, y = set_lat, xend = -up_long, 
#           #   yend = up_lat, colour = 'blue'), arrow = arrow(length = unit(.04, 'npc')))
            

    

#   filter(drvid == '1037785') %>% ggplot(aes(x = -avg_set_long,
#     y = avg_set_lat)) + geom_point(aes(colour = ifelse(tow_year >= 2011, 'red', 'blue')))


# wc_data %>% filter(drvid == '1037785') -> xx
# xx %>% group_by(tow_year) %>% summarise(length(unique(haul_id)))
# unique(xx$haul_id)




# tt <- subset()

# #--------------------------------------------------------------------------------
# #convert longitudes to be in latitude units (Branch et al. 2005)
# which(is.na(wc_data$set_long))
# test_set_long <- wc_data$set_long * cos((wc_data$set_lat * pi) / 180)
# test_up_long <- wc_data$up_long * cos((wc_data$up_lat * pi) / 180)


# hist(log(wc_data$duration_min))




# #----------------------------------------
# #calculate miles per hour for towing and filter out ones that are too high
# #First convert lon units with lat unit conversion from eq. 1 of Branch et al 2005
# temp$set_long




# #----------------------------------------
# #Plot Maps
# states_map <- map_data("state")
# wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington'), ]

# #Map with points for the highest 
# ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-125.5, -123),
#   ylim = c(41, 46)) + geom_segment(data = temp, aes(x = -set_long, xend = -up_long,
#     y = set_lat, yend = up_lat, group = tow_month, colour = tow_month), 
#   arrow = arrow(length = unit(0.1, 'cm'))) + facet_wrap(~ tow_year, ncol = 5) 

# #Looks like there are some really long tows









# temp %>% group_by(ryear) %>% summarise(nstate = length(unique(agid))) %>%




















# # #----------------------------------------
# # #Scraps with old wc_data which only goes to 2012
# # #----------------------------------------
# # # wc_data <- ch2_load_data()

# # load('data/wc_data.Rdata')

# # #rename unq column as tow
# # wc.data <- plyr::rename(wc.data, c('unq' = 'tow', 'noncon_vid' = 'vesselid',
# #   'columns..agid' = 'state'))
# # wc_data <- wc.data #rename with underscore

# # #find the rows and remove them from wc_data
# # #Filter out:
# # #sturgeon
# # #cucumber
# # #prawn
# # #whiting
# # #dogfish
# # #grenadier
# # #squid
# # #wrymouth
# # wc_data <- wc_data[-grep(paste("sturgeon", 'cucumber', 'prawn', 'whiting', 'dogfish', 'grenadier',
# #   'squid', "wrymouth", 'halibut', sep='|'), wc_data$target.desc), ]

# # #Filter rows where tow_date is missing
# # wc_data <- wc_data[-which(nchar(wc_data$tow_date) == 0), ]

# # #Parse out tow dates
# # wc_data$tow_month <- substr(wc_data$tow_date, 4, 6)
# # wc_data$tow_day <- substr(wc_data$tow_date, 1, 2)
# # wc_data$tow_year <- substr(wc_data$tow_date, 8, 9)

# # wc_data$state_port <- paste(wc_data$state, wc_data$dport)
# # #----------------------------------------

# # wc_data %>% group_by(vesselid) %>% summarise(nstate = length(unique(state))) %>% as.data.frame

# # #Merge Port Codes with wc data

# # #Load Port data and rename
# # port_codes <- read.csv("data/port_codes.csv", stringsAsFactors = FALSE)
# # port_codes <- plyr::rename(port_codes, c('Pcid' = 'text_id', 'Agid' = 'state',
# #   'Agency' = 'number_id', 'Port.Agency.Description' = 'description'))

# # #identify missing ports
# # #Add in ports that I know are missing
# # added_ports <- data.frame(text_id = c("", "", "", "", "", "", "", "", "", "", "", ""),
# #                    state = c("O", "O", "W", "W", "W", "W", "W", "W", "W", "W", "W", "W"),
# #                    number_id = c("02", "46", "WES", "ORE", "BEL", "N.B", "SEA", "BLA",
# #                                  "P.A", "ILW", "ANA", "CAL"),
# #                    description = c("ASTORIA", "dont know", "WESTPORT", "dont know ORE", 
# #                                    "BELLINGHAM", "NORTH BEND", "SEATTLE", "BLAINE", 
# #                                    "PORT ANGELES", "ILWACO", "ANACORTES", "dont know CAL"))
# # port_codes <- rbind(port_codes, added_ports)
# # port_codes$state_port <- paste(port_codes$state, port_codes$number_id)

# # unq_port_codes <- unique(wc_data$state_port)

# # wc_data_merged <- merge(wc_data, port_codes[, c('state_port', 'description')], by = 'state_port')
# # wc_data <- wc_data_merged

# # #----------------------------------------
# # #Load Vessel data
# # permits <- read.csv('data/permits.csv', stringsAsFactors = FALSE)
# # trawl_permits <- subset(permits, TrawlGear == 'Yes')

# # #pull first character of state columns
# # trawl_permits$PermitOwnerState <- substr(trawl_permits$PermitOwnerState, 1, 1)
# # trawl_permits$VesselOwnerState <- substr(trawl_permits$VesselOwnerState, 1, 1)

# # trawl_permits[which(trawl_permits$PermitOwnerState != trawl_permits$VesselOwnerState), 
# # c("PermitOwnerState", "VesselOwnerState")]

# # trawl_permits$state_length <- paste(trawl_permits$)

# # wc_data %>% group_by(description) %>% summarise(nvess = length(unique(vesselid))) %>% 
# #   as.data.frame



# # #Merge port codes 
# # unique(wc_data$dport)

# # head(port_codes)



# # aa <- unique(wc_data$state_port)
# # aa[which(aa %in% unique(port_codes$state_port) == FALSE)]


# # bb <- wc_data[which(wc_data$state_port == 'O 46'), ]
# # max(bb$hpounds, na.rm = TRUE)
# # bb[which(bb$hpounds == 15000), ]


# # #----------------------------------------
# # #Match port codes to vessels
# # #then match with vessel accounts in each year




# # #Merge the data
# # wc_data_merged <- merge(wc_data, port_codes[, c('Port.Agency.Description', 'state_port')],
# #   all = TRUE, by = "state_port")




# # #Find which dports are characters
# # #Westport

# # head(subset(wc_data, dport == "WES"))


# # dim(subset(wc_data, dport == "ORE"))

# # dim(subset(wc_data, dport == "BEL"))#Bellingham

# # dim(subset(wc_data, dport == "SEA"))#Seattle
# # head(subset(wc_data, dport == "SEA"))

# # dim(subset(wc_data, dport == "BLA"))#Blaine
# # head(subset(wc_data, dport == "BLA"))

# # dim(subset(wc_data, dport == "N.B"))#Neah Bay
# # head(subset(wc_data, dport == "N.B"))

# # head(subset(wc_data, dport == "ILW")) #ilwaco

# # head(subset(wc_data, dport == "ANA")) #anacortes

# # dim(subset(wc_data, dport == "CAL")) #Centralia?
# # head(subset(wc_data, dport == "CAL"))
# # wc_data$dport == "BEL"




# # #Select one vessel and see where it goes and how that changes through time
# # #Number of rows per vessel and years
# # # wc_data %>% group_by(vesselid) %>% summarise(min_year = min(year), max_year = max(year),
# # #   nrows = length(year)) %>% filter(max_year >= 2012) %>% arrange(desc(nrows))

# # #----------------------------------------
# # #Group by port...


# # #----------------------------------------
# # #Look at H4033, the vessel with most rows
# # highest <- subset(wc_data, vesselid == 'H4033')

# # #Try mapping
# # #Load map data
# # states_map <- map_data("state")
# # wc_map <- states_map[states_map$region %in% c('california', 'oregon', 'washington'), ]

# # #Map with points for the highest 
# # ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-127, -123),
# #   ylim = c(40, 49)) + geom_point(data = highest, aes(x = mid_long, y = mid_lat), colour = 'red') + 
# #   facet_wrap(~ year)

# # #Look at map in one particular year
# # ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-127, -123),
# #   ylim = c(40, 49)) + geom_point(data = subset(highest, tow_year == '08'), 
# #   aes(x = mid_long, y = mid_lat), colour = 'red') + 
# #   facet_wrap(~ tow_month)



# # #look at histogram of tow durations during the year
# # highest %>% group_by(year) %>% 

# # ggplot(data = highest, aes(duration)) + geom_histogram() + facet_wrap(~ year) + 




# # ggplot(wc_map, aes(x = long, y = lat)) + geom_polygon() + coord_map(xlim = c(-127, -123),
# #   ylim = c(40, 49)) + geom_point(data = highest, aes(x = mid_long, y = mid_lat), colour = 'red') + 
# #   facet_wrap(~ year)






# # ggplot(highest, aes(x = mid_long, y = mid_lat)) + geom_point() + facet_wrap(~ year)


# # #



# # #Check
# # # wc_data %>% group_by(target.desc) %>% summarise(nrowz = length(year)) %>% arrange(desc(nrowz)) %>% 
# # #   as.data.frame

# # # ggplot(wc_data, aes(x = length, y = depth1)) + geom_point()

# # #Plot histograms of depth by year
# # ggplot(wc_data, aes(depth1)) + geom_bar() + facet_wrap(~ year)

# # #Plot heatmaps of location choice by year
# # ggplot(wc_data, aes(x = mid_long, y = mid_lat)) + 
# #   geom_bin2d(binwidth = (c(.15, .15))) + facet_wrap(~ year)








# # #SCRAPPS


# # hist(wc_data$mid_lat)






# # #Filter CHLB, 
# # #pacific whiting

# # wc.data[is.na(wc.data$target), 'state.target']

# # wc.data %>% filter(target == '') -> zz

# # zz %>% group_by(spcode.desc) %>% summarise(ntows = length(hpounds)) %>% arrange(desc(ntows)) %>% 
# #   as.data.frame
# # unique(zz$spcode.desc)

# # unique(zz$columns..agid)



# # wc.data %>% filter(target != "CHLB") %>% group_by(target.desc) %>% mutate(ntrips = length(trip), 
# #   perc_trips = ntrips / sum(ntrips)) %>%
# #   arrange(desc(ntrips)) 





# #  as.data.frame %>% 



# # unique(wc.data$net_type)

# # subset(wc.data, net_type == 'D')

# # unique(wc.data$target.desc)
# # #Filter logbook data to remove NAs and nongroundfish species

# # (wc.data[is.na(wc.data$target.desc), ])

# # nrow(wc.data) - nrow(wc.data[is.na(wc.data$target.desc), ])


# # nas <- subset(wc.data, target.desc == NA)















# # unique(wc.data$target.desc)

# # nw.vms1 <- load_and_process_data()

# # dat <- nw.vms1


# # ggplot(dat, aes(x = rounded.lon, y = rounded.lat)) + geom_point() + facet_wrap(~ year)


# # dat %>% group_by(vessel_name, year) %>% summarise()

# # #Look at n trips per vessel

# # nw.vms1 %>% group_by(vessel_name) %>% 










# # #Reclassify
# # nw.vms$speed <- as.numeric(nw.vms$speed)
# # nw.vms$year <- as.numeric(nw.vms$year)
# # nw.vms$month <- as.numeric(nw.vms$month)

# # #Remove values with NA for speed
# # nw.vms <- nw.vms[is.na(nw.vms$speed) == FALSE, ]
# # #Remove values with 0 for speed
# # nw.vms <- subset(nw.vms, speed != 0)

# # #Assumed trawl speed is between 2 and 4, this is arbitrary
# # nw.vms <- subset(nw.vms, speed >= 2 & speed <= 4)
# # nw.vms$rounded.lat <- round(nw.vms$lat, digits = 2)
# # nw.vms$rounded.lon <- round(nw.vms$lon, digits = 2)

# # #Filter out nonsensical values
# # nw.vms <- subset(nw.vms, lon < 0)

# # #
# # states_map <- map_data('world')
# # # wc <- subset(states_map, region %in% c('california', 'oregon', 'washington'))

# # ggplot(states_map, aes(x = long, y = lat)) + geom_polygon()


# # ggplot(nw.vms, aes(x = lon, y = lat)) + stat_bin2d(bins = 100) + facet_wrap(~ year)


# # subset(nw.vms)

# # temp <- nw.vms %>% group_by(month, year) %>% summarise(nvess = length(unique(vessel_name)), 
# #   npoints = length(lat))






# # ggplot(temp, aes(x = month, y = nvess)) + geom_point() + facet_wrap(~ year)

# # ggplot(temp, aes(x = month, y = npoints)) + geom_point() + facet_wrap(~ year)





# # ggplot(nw.vms aes(x = ))



# # #
# # first.50 <- head(nw.vms, n = 50)
# # first.50[is.na(first.50$speed) == FALSE, ]



# # binned$unq <- paste(binned$x, binned$y)

# # binned$perc <- binned$perc * 100
# # binned$perc <- round(binned$perc, digits = 4 )

# # #Cutoffs
# # #data must have values from before and after catch shares
# # #Fit linear model and look at slopes of 

# # binned <- merge(data.frame(group = 1:12, year = 2002:2013),
# #   binned, all = TRUE)

# # #before, after binned
# # ba_binned <- subset(binned, year >= 2008)
# # ba_binned$when <- binned$year

# # ba_binned$when <- sapply(ba_binned$year, 
# #   FUN = function(x) ifelse(x >= 2011, 'after', 'before'))

# # ba_binned %>% group_by(unq) %>% summarise(min_year = min(year), max_year = max(year))

# # nunq <- unique(ba_binned$unq)
# # keeps <- rep(999, length(nunq))

# # for(zz in 1:length(nunq)){
# #   temp <- subset(ba_binned, unq == nunq[zz])
# #   if(length(unique(temp$when)) == 2) keeps[zz] <- 1
# # }

# # full <- nunq[which(keeps == 1)]
# # outs <- vector(length = length(full), 'list')

# # for(ff in 1:length(full)){
# #   temp <- subset(ba_binned, unq == full[ff])
# #   outs[[ff]] <- lm(perc ~ year, data = temp)
# # }


# # #slopes
# # slopes <- sapply(outs, FUN = function(x) coef(x)[2])
# # names(slopes) <- NULL

# # pos <- subset(ba_binned, unq %in% full[which(slopes > 0)])
# # plot(pos$year, pos$perc, type = 'n')
# # punq <- unique(pos$unq)

# # for(pp in 1:length(punq)){
# #   temp <- subset(pos, unq == punq[pp])
# #   lines(temp$year, temp$perc, col = 'gray')
# # }


# # ba_binned[which(slopes > 0)]

# # pos <- subset(ba_binned)which(slopes > 0)


# # coef(outs[[1]])


# # zz <- 
# # length(unique(temp$when)) == 2


# # plot(binned$group, binned$perc, type = 'n')
# # nunq <- unique(binned$unq)

# # for(ii in 1:length(nunq)){
# #   temp <- subset(binned, unq == nunq[ii])

# #   lines(temp$group, temp$perc, col = "#80808050")
# # }


# # rgb(0, 1, 0)
# # rgb(190, 190, 190, alpha = .9, maxColorVal = 250)

# # ggplot(binned, aes(x = group, y = perc))

# # binned$group <- as.factor(binned$group)

# # ggplot(binned, aes(x = group, y = std_count)) + geom_boxplot()

# # #look at individual lines
# # ggplot(binned, aes(x = group, y = std_count, group = unq)) + 
# #   geom_line(colour = 'gray') + theme_bw()
  

# #   geom_line(col = rgb(red = 190, green = 190, blue =  190, alpha = .1))


# # ggplot(binned, aes(x = group, y = count, group = unq)) + geom_boxplot() 


# # binned %>% group_by(group) %>% summarise(tot_tows = sum(count))


# ##Number of vessels and minutes per year
# # wc_data %>% distinct(drvid, tow_month, tow_day, tow_year, mph) %>%
# #   group_by(tow_year) %>% 
# #   summarise(tot_min = sum(duration_min, na.rm = TRUE), 
# #             nvess = length(unique(drvid))) -> sum_effort
# # sum_effort <- subset(sum_effort, tow_year >= 2002 & tow_year < 2015)

# # #2014 is really low with only 40 vessels, check that this is right
# # #load 2014 data
# # vess14 <- read.csv('data/vessels2014.csv', stringsAsFactors = FALSE)
# # catch14 <- read.csv('data/catch14.csv', stringsAsFactors = FALSE)

# # #Format catch14
# # names(catch14) <- c('year', 'species', 'sector_lb', 'carryover_lb',
# #   'catch', 'remaining')
# # catch14$species == unique(vess14$species)

# # #Format vess14
# # names(vess14) <- c('year', 'vessel', 'owner', 'species', 'balance', 'limit')
# # vess14$limit <- as.numeric(gsub(',', '', vess14$limit))
# # vess14$balance <- as.numeric(gsub(',', '', vess14$balance))
# # vess14$vessel <- tolower(vess14$vessel)
# # vess14$owner <- tolower(vess14$owner)

# # vess14 %>% group_by(vessel) %>% summarise(tot_bal = sum(balance)) %>% 
# #   arrange(desc(tot_bal)) %>% as.data.frame
# # vess14 %>% group_by(species) %>% summarise(tot_bal = sum(balance), 
# #   tot_lim = sum(limit), rat = tot_bal / tot_lim) %>% as.data.frame -> check

# # vv <- data.frame(x = catch14$sector_lb + catch14$carryover_lb , y = catch14$catch)

# # catch14[grep('of', catch14$species), 'species']
# # catch14$rat <- vv$y / vv$x


# # data.frame('species' = check$species, 'ifq_catch' = check$tot_bal, 'bal_catch' = catch14$catch)

# # length(unique(vess14$vessel))



# #--------------------------------------------------------------------------------

# #Try this for january, 2009 only
# ne_vms %>% filter(year == 2009 & month == 1 & region == "WH") -> test
# ne_vms_dec %>% filter(year == 2009 & month == 1 & region == "WH") -> test_dec

# #For each unique value, find the average speed, number of things by gear type
# test %>% group_by(unq) %>% summarize(nvals = length(latitude), mean_speed = mean(speed, na.rm = TRUE),
#   speed_0 = quantile(speed, na.rm = TRUE)[1],
#   speed_25 = quantile(speed, na.rm = TRUE)[2], 
#   speed_50 = quantile(speed, na.rm = TRUE)[3], 
#   speed_75 = quantile(speed, na.rm = TRUE)[4], 
#   speed_100 = quantile(speed, na.rm = TRUE)[5]) -> test_speeds
# test_speeds %>% as.data.frame %>% arrange(desc(nvals)) -> test_speeds

# #Find the ports for some of the highest locations


# most_unq <- head(test_speeds$unq, n = 30)




# head(test_speeds, n = 30)


# #Look at maximum
# focus_ind <- which(test_speeds$nvals == max(test_speeds$nvals))
# test_speeds[focus_ind, ]

# test %>% filter(unq == test_speeds[focus_ind, 'unq']) -> ttspeeds

# test_dec %>% filter(unq == test_speeds[focus_ind, 'unq']) -> ttdecs

# test$dec$unq == 

# head(test_dec)




# #Try looking at which rows will merge, look for duplicates
# # test$unq <- paste(test$year, test$month, test$region, test$latitude, test$longitude)
# # test_dec$unq <- paste(test_dec$year, test_dec$month, test_dec$region, 
# #   test_dec$latitude, test_dec$longitude)

# #Find unique values 



# unique(test$unq)[1]

# test_dec %>% filter(unq == unique(test$unq)[1]) -> tt1
# test %>% filter(unq == unique(test$unq)[1]) -> tt2

# #Certain spots will have similar speeds


# test %>% group_by(unq) %>% summarize(nvals = length(unq))



# test_dec %>% filter(unq == test$unq[1])


# test_dec %>% group_by(unq) %>% summarize(nvals = length(unique))


# subset(test, unq == test$unq[1]) %>% dim
# tt <- subset(test_dec, unq == test$unq[1]) 



# unique(tt$declarations)




# there <- which(test$unq %in% test_dec$unq)
# not_there <- which(test$unq %in% test_dec$unq == FALSE)

# merged <- left_join(test, test_dec, by = c('unq'))
# test



# merged_test <- left_join(test, test_dec, by = c('latitude', 'longitude',
#   'year', 'month', 'region'))

# not_merged_test <- anti_join(test, test_dec, by = c('latitude', 'longitude',
#   'year', 'month', 'region'))


# merged_test <- inner_join(test, test_dec, by = c('latitude', 'longitude',
#   'year', 'month', 'region'))

# merged <- inner_join(ne_vms, ne_vms_dec, by = c('latitude', 'longitude',
#   'year', 'month', 'region'))