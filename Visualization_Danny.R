source("download_data.R")
source("Wrangling_Danny.R")
library(ggplot2)

buoy_by_six$time <- 1:20
buoy_by_six

ggplot(buoy_by_six, aes(x = time, y = WSPD_avg_six)) +
  geom_line()



buoy_by_hour$hour <- 1:nrow(buoy_by_hour)

# Add a column for date in buoy_by_hour in order to make the time series plot later.
#------------------------------------------------------------------------------------------------------
buoy_by_hour %<>%
  unite("MDH", MM:hh, sep = "", remove = F) %>%
  mutate(Year = 2011, .before = MDH) %>%
  unite("YMDH", Year:MDH, sep = "", remove = F) %>%
  mutate(YMDH_Date = ymd_h(YMDH), .before = YMDH)
# #------------------------------------------------------------------------------------------------------
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




#Average Wind Speed by Hour Time Series Plot (Hour)
#------------------------------------------------------------------------------------------------------
WSPD_ts <- ggplot(buoy_by_hour, aes(x = YMDH_Date, y = WSPD_avg)) +
  geom_line() +
  xlab("Time line") + 
  ylab("Average Wind Speed by Hour")
#------------------------------------------------------------------------------------------------------

#Average Wind Speed by Hour Time Series Plot (Hour)
#------------------------------------------------------------------------------------------------------
GST_ts <- ggplot(buoy_by_hour, aes(x = YMDH_Date, y = GST_avg)) +
  geom_line() +
  xlab("Time line") + 
  ylab("Average Peak Gust Speed by Hour")
#------------------------------------------------------------------------------------------------------

# Average Sea Sea level pressure by Hour
#------------------------------------------------------------------------------------------------------
PRES_ts <- ggplot(buoy_by_hour, aes(x = YMDH_Date, y = PRES_avg)) +
  geom_line() +
  xlab("Time line") + 
  ylab("Average Sea Sea level pressure by Hour")

#------------------------------------------------------------------------------------------------------
ggarrange(GST_ts, WSPD_ts, PRES_ts, ncol = 2, nrow = 2)



hurr_tracks_Lee1
class(as.Date(hurr_tracks_Lee1$`Date-Time`))
#------------------------------------------------------------------------------------------------------
# wind: 1-minute maximum sustained surface wind speed, measured at 10 meters above the ground, 
# in knots (values are rounded to the nearest 5-knot value)
wind_track_ts <- ggplot(hurr_tracks_Lee1, aes(x = `Date-Time`, y = wind )) +
  #scale_x_date(date_breaks = "six hour") +
  geom_line()
#------------------------------------------------------------------------------------------------------


ggarrange(GST_ts, WSPD_ts, PRES_ts,  wind_track_ts, ncol = 2, nrow = 2)






#-------------------------------------------------------------------------------------------------------------
buoy_by_six
ggplot(buoy_by_six, aes(x = Date_Time, y = WSPD_avg_six)) +
  geom_line()

#-------------------------------------------------------------------------------------------------------------

































