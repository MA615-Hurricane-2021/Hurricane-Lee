# Author: Handing Zhang

source("download_data.R")
source("Wrangling_Danny.R")
library(ggplot2)
library(ggridges)
# buoy_by_six$time <- 1:20
# buoy_by_six
# 
# ggplot(buoy_by_six, aes(x = time, y = WSPD_avg_six)) +
#   geom_line()




# 
# 
# # I want to create a Date column for buoy_by_six.
# # ?????? My Date value messed up when being assigned.
# #------------------------------------------------------------------------------------------------------
# buoy_by_six
# 
# a_null <- data.frame(rep(NA, nrow(buoy_by_six)))
# a_null
# start_Lee = ymd_h(2011090200)
# start_Lee
# class(start_Lee)
# a_null[1] <- start_Lee
# a_null[1]
# # class(as.Date(a_null[1]))
# # class((a_null[1] <- start_Lee + hours(12)))
# (a_null[3] <- start_Lee + hours(16))
# a_null[3]
# 
# for (i in 1:20) {
#   a_null[i] <- start_Lee + hours((i - 1) * 6)
# }
# atest <- a_null[1,]
# length(atest)
# data.frame(atest)
# buoy_by_six$Date <- atest
# buoy_by_six
# atest1 <- atest[,-1]
# length(atest1)
# 
# (a_null[1] <- as.Date(start_Lee + hours((2 - 1) * 6)))
# a_null
# 
# str(a_null)
# 
# 
# 
# a_null[1] <-  start_Lee
# a_null[1]
# a_null
# buoy_by_six$Date <- a_null
# 
# (a_null[1] <- start_Lee + hours(6 *5))
# typeof(start_Lee)
# class(start_Lee)
# as.Date(start_Lee)
# class(as.Date(start_Lee))
# a_null[5] <- as.Date(start_Lee)
# a_null
#------------------------------------------------------------------------------------------------------



#------------------------------------------------------------------------------------------------------

plot_buoy <- function(){
  #Average Wind Speed by Hour Time Series Plot (Hour)
  WSPD_ts <- ggplot(buoy_by_hour, aes(x = YMDH_Date, y = WSPD_avg)) +
  geom_line() +
  xlab("Time line") + 
  ylab("Average Wind Speed by Hour")
  
  #Average Wind Speed by Hour Time Series Plot (Hour)
  GST_ts <- ggplot(buoy_by_hour, aes(x = YMDH_Date, y = GST_avg)) +
    geom_line() +
    xlab("Time line") + 
    ylab("Average Peak Gust Speed by Hour")
  
  # Average Sea Sea level pressure by Hour
  PRES_ts <- ggplot(buoy_by_hour, aes(x = YMDH_Date, y = PRES_avg)) +
    geom_line() +
    xlab("Time line") + 
    ylab("Average Sea Sea level pressure by Hour")
  
  
  return(ggarrange(WSPD_ts, GST_ts, PRES_ts, nrow = 2, ncol = 2))
}

# plot_buoy()





hurr_tracks_Lee1
class(as.Date(hurr_tracks_Lee1$`Date-Time`))
#------------------------------------------------------------------------------------------------------
# wind: 1-minute maximum sustained surface wind speed, measured at 10 meters above the ground, 
# in knots (values are rounded to the nearest 5-knot value)
wind_track_ts <- ggplot(hurr_tracks_Lee1, aes(x = `Date-Time`, y = wind )) +
  #scale_x_date(date_breaks = "six hour") +
  geom_line()


#------------------------------------------------------------------------------------------------------


# ggarrange(GST_ts, WSPD_ts, PRES_ts,  wind_track_ts, ncol = 2, nrow = 2)





#-------------------------------------------------------------------------------------------------------------
buoy_by_six
wind_buoy <- ggplot(buoy_by_six, aes(x = Date_Time, y = WSPD_avg_six)) +
  geom_line()
# ggarrange(wind_buoy, wind_track_ts)
#-------------------------------------------------------------------------------------------------------------







# 
# # plot1: Cumulative rainfall of each county from one day before the hurricane arrives and two days after.
# #-------------------------------------------------------------------------------------------------------------
# 
# map_counties(storm = "Lee-2011", metric = "rainfall", days_included = -1:2) +
#   ggtitle("Rain during Lee (2011) for one day before and two days after closest approach")
# #-------------------------------------------------------------------------------------------------------------
# 


# 
# 
# # Avg windspeed 
# #-------------------------------------------------------------------------------------------------------------
# 
# ggplot(
#   buoy_by_hour, 
#   aes(x = WSPD_avg, y = dt, fill = stat(x))
# ) +
#   geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
#   scale_fill_viridis_c(name = "Wind Speed", option = "D") +
#   labs(title = 'Average Wind Speed Density During Lee') 
# #-------------------------------------------------------------------------------------------------------------
# 




plot1 <- function(){
  ggplot(
    buoy_by_hour, 
    aes(x = WSPD_avg, y = dt, fill = stat(x))
  ) +
    geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Wind Speed", option = "D") +
    labs(title = 'Density of Average Wind Speed Per Hour During Lee') +
    geom_vline(xintercept = 8)
  
}

plot2 <- function(){
  ggplot(
    buoy_by_hour, 
    aes(x = GST_avg, y = dt, fill = stat(x))
  ) +
    geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Wind Speed", option = "D") +
    labs(title = 'Density of Average Peak Gust Speed Per Hour During Lee') +
    geom_vline(xintercept = 12.5, color = "blue")
  
}


plot3 <- function(){
  ggplot(
    buoy_by_hour, 
    aes(x = PRES_avg, y = dt, fill = stat(x))
  ) +
    geom_density_ridges_gradient(scale = 3, size = 0.3, rel_min_height = 0.01) +
    scale_fill_viridis_c(name = "Wind Speed", option = "C") +
    labs(title = 'Density of Average See Pressure Per Hour During Lee') +
    geom_vline(xintercept = 1000, color = "blue")
}


