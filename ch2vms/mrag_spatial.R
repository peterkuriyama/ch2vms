#---------------------------------------------------------------------------------
#Load West Coast Data
#From: 
# source("mrag_spatial_data_wc.R") and
# source('mrag_spatial_data_ne.R')


#---------------------------------------------------------------------------------
#Check the northeast plots
world_map <- map_data("world")

ne_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
      fill = NA, color = 'gray') +
    scale_x_continuous(limits = c(-76, -62.5)) + 
    scale_y_continuous(limits = c(34.266, 45.55)) 

wc_map <- ggplot() + geom_map(data = world_map, map = world_map, aes(x = long, y = lat, 
    map_id = region), fill = 'gray') + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
      fill = NA, color = 'gray') +
    scale_x_continuous(limits = c(-126, -117)) + 
    scale_y_continuous(limits = c(33.385, 48)) 

#---------------------------------------------------------------------------------
#Final Plots, and specify conditions
if(nrow(the_data) == 4763){
  vert_line <- 2009.5
  tit <- 'mrag_ne'
  this_map <- ne_map
  main_lab <- 'Northeast'
} 

if(nrow(the_data) == 1689){
  vert_line <- 2010.5
  tit <- 'mrag_wc'
  this_map <- wc_map
  main_lab <- 'West Coast'
} 

fig_name <- paste0('figs/', tit)
#---------------------------------------------------------------------------------
#Aggregated number of assumed fishing locations
chg <- the_data %>% group_by(year) %>% summarize(count = sum(count))

png(width = 7, height = 7, units = 'in', res = 200, 
  file = paste0(fig_name, '_agg_effort.png'))
ggplot(data = chg) + geom_line(aes(x = year, y = count)) + 
  geom_point(aes(x = year, y = count)) + theme_bw() + ylim(0, max(chg$count)) + 
  geom_vline(xintercept = vert_line, linetype = 'dashed') + ylab('# VMS Points') +
  labs(title = paste0("Trend in ", main_lab, " Fishing"))
dev.off()

#Def some data problems

#------------------------------------
#Histogram of the mn_itq_differences
png(width = 7, height = 7, units = 'in', res = 200, 
  file = paste0(fig_name, '_diffct_bef_aft.png'))
hist(unique(the_data$mn_itq_diff), breaks = 30, xlab = "Mean Change before and after ITQ",
  main = paste0(main_lab, " Change in Average Fishing Effort"))
dev.off()

#------------------------------------
#Plot all the tile plots
#Everything
png(width = 7, height = 7, units = 'in', res = 200,
  file = paste0(fig_name, "_diffct_spatial.png"))
this_map + geom_tile(data = the_data, aes(x = x, y = y, fill = mn_itq_diff)) + 
  scale_fill_gradient2(low = 'blue', high = 'red')  + labs(fill = "Difference \nin #Tows")
dev.off()

#------------------------------------
#Did the most visited sites have the most consistent visitation?
#Do people concentrate in well sampled sites?
cvs_sites <- the_data %>% group_by(unq) %>% summarize(stdev = unique(stdev),
  cv = unique(cv), mean_quants = unique(mean_quants), mn = unique(mean)) %>%
  group_by(mean_quants) %>% mutate(med_cv = median(cv))
  
quants <- quantile(cvs_sites$mn)

#Print this
cat("Mean Count Quantiles", '\n')
print(quants)

qs <- vector(length = length(quants) - 1)
quants <- round(quants, digits = 0)

for(jj in 1:length(quants) - 1){
  qs[jj] <- paste(quants[jj], quants[jj + 1], sep = '-')
}

thing <- data.frame(mean_quants = unique(cvs_sites$mean_quants)[order(unique(cvs_sites$mean_quants))],
    quants = qs)
cvs_sites <- left_join(cvs_sites, thing, by = 'mean_quants')  
cvs_sites$labb <- paste(cvs_sites$mean_quants, '\n', cvs_sites$quants)

png(width = 5.22, height = 7, units = 'in', res = 200, 
  file = paste0(fig_name, "_cvs_by_ntows_site.png"))
ggplot(cvs_sites) + geom_histogram(aes(x = cv)) + 
facet_grid(labb ~ .) +
  geom_vline(aes(xintercept = med_cv), linetype = 'dashed') + labs(title = main_lab)
dev.off()





the_data %>% group_by(unq) %>% summarize(mn_itq_diff = unique(mn_itq_diff)) %>% 
  group_by(mn_itq_diff) %>% summarize(nloc = length(unique(unq))) %>% as.data.frame



#Which latitudes had the biggest increases?
#Generate histogram of
chng <- the_data %>% group_by(y, itq_diff_quants) %>% summarize(mean_diff = 
  mean(mn_itq_diff), nvals = n())

chng$abs_mean_diff <- abs(chng$mean_diff)
ylims <- range(chng$y)

#------------------------------------
#Doesn't seem to be a latitudinal pattern
# p1 <- ggplot(chng, aes(x = abs_mean_diff, y = y, colour = mean_diff,
#   size = nvals)) + geom_point() + scale_x_reverse() + 
#   scale_colour_gradient2(low = 'blue', high = 'red')

if(tit == 'mrag_ne'){
  p1 <- ggplot(chng, aes(x = abs_mean_diff, y = y, colour = mean_diff,
    size = nvals)) + geom_point() + 
    scale_colour_gradient2(low = 'blue', high = 'red') + 
    labs(x = "Absolute Value of Mean Difference", y = 'Latitude')
  
  png(width = 7.9, height = 5.47, units = 'in', res = 200,
    file = paste0(fig_name, "_means_by_lat.png"))
  multiplot(this_map, p1, cols = 2)
  dev.off()

}

if(tit == 'mrag_wc'){
  p1 <- ggplot(chng, aes(x = abs_mean_diff, y = y, colour = mean_diff,
    size = nvals)) + geom_point() + scale_x_reverse() + 
    scale_colour_gradient2(low = 'blue', high = 'red') + ylim(33.385, 48)
  png(width = 7.9, height = 5.47, units = 'in', res = 200,
    file = paste0(fig_name, "_means_by_lat.png"))
  multiplot(p1, this_map, cols = 2)
  dev.off()
}
