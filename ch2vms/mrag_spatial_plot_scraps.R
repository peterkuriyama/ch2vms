#Things that seem to not be too important

#------------------------------------
#Count trends by year

#Plot Trends by quantiled slope, seems not to highlight the most fished areas
ggplot(the_data, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ slope_quants)

#Plot trends by those with the biggest differences
ggplot(the_data, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ diff_quants)

#Plot trends by those with different quantile means, like most fished locations,
  # least fished locations
ggplot(the_data, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ mean_quants)

#Plot trends by those with different quantile differences before and after catch shares 
ggplot(the_data, aes(x = year, y  = count, group = unq)) + geom_line() +
  facet_wrap(~ itq_diff_quants)


#Hmm no pattern really here
the_data %>% group_by(unq) %>% summarize(cv = unique(cv), quant = unique(mean_quants)) %>%
  ggplot() + geom_histogram(aes(x = cv)) + facet_wrap(~ quant)


#Facet by itq_diff_quants
this_map + geom_tile(data = the_data, aes(x = x, y = y, fill = mn_itq_diff)) + 
  scale_fill_gradient2(low = 'blue', high = 'red')  + labs(fill = "Difference \nin #Tows") + 
  facet_wrap(~ itq_diff_quants)

#Look at the locations of places that had the biggest decreases and 
#increases avg tows after itqs
big_change <- the_data %>% filter(itq_diff_quants == "75%-100%" | itq_diff_quants == "0%-25%") 
range(big_change$mn_itq_diff)

#*#*#*#*#
#Might be worth bootstrapping this to assign p-values to changes we see. 
this_map + geom_tile(data = big_change, aes(x = x, y = y, fill = mn_itq_diff)) + 
  scale_fill_gradient2(low = 'blue', high = 'red')  + labs(fill = "Difference \nin #Tows")
#Keeper

#Plot CV of tows
this_map + geom_point(data = the_data, aes(x = x, y = y, colour = cv)) + 
  scale_colour_gradient(low = 'white', high = 'red') + facet_wrap(~ mean_quants)

#Which of the biggest areas had the highest increases?
quantile(the_data$mean)
sites_most <- the_data %>% filter(mean_quants == '75%-100%')

#Plot 2??
this_map + geom_tile(data = sites_most, aes(x = x, y = y, fill = count)) + 
  facet_wrap(~ itq_diff_quants)

the_data %>% filter(mean_quants == "75%-100%") %>% this_map +
  geom_tile(aes(x = x, y = y, count = count)) + facet_wrap(~ itq_diff_quants)



# #No latitudinal pattern

# #Obviously it seems like WC effort declined

# #------------------------------------
# #Look at locations with biggest declines
# big_decrease <- the_data %>% filter(itq_diff_quants == "0%-25%")
# big_increase <- the_data %>% filter(itq_diff_quants == "75%-100%")

# this_map + geom_tile(data = rbind(big_decrease, big_increase), aes(x = x, y = y, fill = mn_itq_diff)) + 
#   scale_fill_gradient2(low = 'blue', high = 'red') 

# ggplot(rbind(big_decrease, big_increase)) 

# ggplot(big_decrease) + geom_line(aes(x = year, y = count, group = unq)) 





# wc_map + scale_x_continuous(limits = c(-126, -117)) + 
#   scale_y_continuous(limits = c(31.5, 48)) + 
#   geom_tile(data = wc_binned, aes(x = x, y = y, fill = count)) + 
#   scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)




# #Count trends by year
# hist(unique(the_data$mn_itq_diff), breaks = 30)

# ggplot(the_data, aes(x = mean, y  = cv)) + geom_point() +
#   facet_wrap(~ slope_quants)

# ggplot(the_data, aes(x = mean, y  = cv)) + geom_point() +
#   facet_wrap(~ slope_quants)

# ggplot(the_data, aes(x = mean, y  = cv)) + geom_point() +
#   facet_wrap(~ itq_diff_quants)










# #Aggregated number of assumed fishing locations
# wc_slopes %>% group_by(year) %>% summarize(count = sum(count)) %>% 
#   ggplot() + geom_line(aes(x = year, y = count)) + 
#   geom_point(aes(x = year, y = count)) + theme_bw()
# #2012 is effed up

# #------------------------------------
# #Count trends by year

# #Plot Trends by quantiled slope, seems not to highlight the most fished areas
# ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
#   facet_wrap(~ slope_quants)



# #Plot trends by those with the biggest differences
# ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
#   facet_wrap(~ diff_quants)

# #Plot trends by those with different quantile means, like most fished locations,
#   # least fished locations
# ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
#   facet_wrap(~ mean_quants)

# #Plot trends by those with different quantile differences before and after catch shares 
# ggplot(wc_slopes, aes(x = year, y  = count, group = unq)) + geom_line() +
#   facet_wrap(~ itq_diff_quants)

# #------------------------------------
# #Look at the locations of places that had the biggest decreases and 
# #increases avg tows after itqs
# big_change <- wc_slopes %>% filter(itq_diff_quants == "75%-100%" | itq_diff_quants == "0%-25%") 
# range(big_change$mn_itq_diff)

# #*#*#*#*#
# #Might be worth bootstrapping this to assign p-values to changes we see. 
# wc_map + geom_tile(data = big_change, aes(x = x, y = y, fill = mn_itq_diff)) + 
#   scale_fill_gradient2(low = 'blue', high = 'red')  + labs(fill = "Difference \nin #Tows")

# #Plot CV of tows
# wc_map + scale_x_continuous(limits = c(-126, -117)) + 
#   scale_y_continuous(limits = c(33.385, 48)) +
#   geom_point(data = wc_slopes, aes(x = x, y = y, colour = cv)) + 
#   facet_wrap(~ mean_quants)

# #Hmm no pattern really here
# wc_slopes %>% group_by(unq) %>% summarize(cv = unique(cv), quant = unique(mean_quants)) %>%
#   ggplot() + geom_histogram(aes(x = cv)) + facet_wrap(~ quant)






# # ggplot(big_change) + geom_line(aes(x = year, y = count, group = unq)) + 
# #   facet_wrap(~ itq_diff_quants)

# #Which latitudes had the biggest increases?
# #Generate histogram of
# chng <- big_change %>% group_by(y, itq_diff_quants) %>% summarize(mean_diff = 
#   mean(mn_itq_diff), nvals = n())
# chng$abs_mean_diff <- abs(chng$mean_diff1)
# ylims <- range(chng$y)

# p1 <- ggplot(chng, aes(x = abs_mean_diff, y = y, colour = mean_diff,
#   size = nvals)) + geom_point() + scale_x_reverse() + 
#   scale_colour_gradient2(low = 'blue', high = 'red')

# p2 <- wc_map + scale_y_continuous(limits = c(33.385, 48)) + 
#   scale_x_continuous(limits = c(-126, -117))

# multiplot(p1, p2, cols = 2)
# #No latitudinal pattern

# #Obviously it seems like WC effort declined

# #------------------------------------
# #Look at locations with biggest declines
# big_decrease <- wc_slopes %>% filter(itq_diff_quants == "0%-25%")
# ggplot(rbind(big_decrease, big_increase)) + geom_tile(aes(x = x, y = y, fill = mn_itq_diff)) + 
#   scale_fill_gradient2(low = 'blue', high = 'red') 
# ggplot(big_decrease) + geom_line(aes(x = year, y = count, group = unq)) 





# wc_map + scale_x_continuous(limits = c(-126, -117)) + 
#   scale_y_continuous(limits = c(31.5, 48)) + 
#   geom_tile(data = wc_binned, aes(x = x, y = y, fill = count)) + 
#   scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)




# #Count trends by year
# hist(unique(wc_slopes$mn_itq_diff), breaks = 30)

# ggplot(wc_slopes, aes(x = mean, y  = cv)) + geom_point() +
#   facet_wrap(~ slope_quants)

# ggplot(wc_slopes, aes(x = mean, y  = cv)) + geom_point() +
#   facet_wrap(~ slope_quants)

# ggplot(wc_slopes, aes(x = mean, y  = cv)) + geom_point() +
#   facet_wrap(~ itq_diff_quants)



# #Overall plot of fishing location bins
#   #Hard to make out the details
# #Map Prep stuff

# #Tile Map, hard to visualize
# wc_map + scale_x_continuous(limits = c(-126, -117)) + 
#   scale_y_continuous(limits = c(31.5, 48)) + 
#   geom_tile(data = wc_binned, aes(x = x, y = y, fill = count)) + 
#   scale_fill_gradient2(low = 'blue', high = 'red') + facet_wrap(~ year)
















