
# Import necessary packages here.
#-------------------------------------------------------------------------------------------------------------
library(drat)
addRepo("geanders")
pacman::p_load(tidyverse, dplyr, hurricaneexposuredata, hurricaneexposure, lubridate, ggpubr)
#-------------------------------------------------------------------------------------------------------------

# Loads all datasets available from the package.
#-------------------------------------------------------------------------------------------------------------
data("hurr_tracks") 
data("county_centers")
data("rain")
data("closest_dist")
data("storm_winds")
data("storm_events")
data("ext_tracks_wind") 
#-------------------------------------------------------------------------------------------------------------
head(closest_dist)
head(ext_tracks_wind)
head(storm_winds)
head(rain)


# Slice hurr_tracks by filtering for Hurricane Lee.
#-------------------------------------------------------------------------------------------------------------
hurr_tracks_Lee <- hurr_tracks %>% 
  filter(storm_id == "Lee-2011")
#-------------------------------------------------------------------------------------------------------------


df1 <- download_data()
# Wrangle buoy to have columns containing Date, Date-Time for convenience for later use.
#-------------------------------------------------------------------------------------------------------------
buoy <- df1 %>% 
  unite("Date", `#YY`:DD, sep = "-", remove = F)

buoy$Date <- as.Date(buoy$Date)
# Lets also make a column recording time in standard format of type "time".

buoy <- buoy %>% 
  unite("Time", hh:mm, sep = ":", remove = F)
# We get a character column Time by hour:minutes recording time.

buoy <- buoy %>% 
  unite("Date-Time", c(Date, Time), sep = " ", remove = F)

buoy$`Date-Time`
buoy$`Date-Time` <- ymd_hm(buoy$`Date-Time`)


# Now we have a column with Date.
# a column with Time.
# a column with Date and time.

# head(buoy)
# Slicing by Date-Time Example
# buoy$`Date-Time` >= ymd_hm("2020-11-09 02:00:12 UTC")
#-------------------------------------------------------------------------------------------------------------


# Wrangle hurr_tracks_Lee to have a "Date-Time" column
#-------------------------------------------------------------------------------------------------------------
hurr_tracks_Lee1 <- hurr_tracks_Lee %>% 
  separate(col = date, into = c("YY", "MM", "DD", "hh", "mm"), sep = c(4, 6, 8, 10)) %>% 
  unite(col = "Date", YY:DD, sep = "-", remove = F) %>% 
  unite(col = "Time", hh:mm, sep = ":", remove = F) %>% 
  unite(col = "Date-Time", c(Date, Time), sep = " ", remove = F)

head(hurr_tracks_Lee1)
hurr_tracks_Lee1$`Date-Time` <- ymd_hm(hurr_tracks_Lee1$`Date-Time`)

class(hurr_tracks_Lee1$`Date-Time`)

hurr_tracks_Lee1$`Date-Time`[1]

# ymd_hm("2011-09-02 12:00")
hurr_tracks_Lee <- hurr_tracks_Lee1
#-------------------------------------------------------------------------------------------------------------





# Pruning bouy with Hurricane Lee's Data in terms of time for sake of aligning them later.
#-------------------------------------------------------------------------------------------------------------
buoy <- buoy %>% 
  filter(`Date-Time` <= ymd_hm("2011-09-06 18:00"))
# The hurricane ends at 0902 18:00.
range(buoy$`Date-Time`)
range(hurr_tracks_Lee$`Date-Time`)
#-------------------------------------------------------------------------------------------------------------



# For sake of aligning this data to our data of hurricane package, let's aggregate the relatively dense data points
# by grouping them by one hour and six hours separatesly
#-------------------------------------------------------------------------------------------------------------


buoy$WSPD <- as.numeric(buoy$WSPD)
buoy[, 9:16] <- apply(buoy[, 9:16], MARGIN = 2, FUN = as.numeric)
apply(buoy[, 9:16], MARGIN = 2, FUN = as.numeric)

buoy_by_hour <- buoy %>% 
  group_by(MM, DD, hh) %>% 
  summarise(count = n(),
            WSPD_avg = mean(WSPD), 
            GST_avg = mean(GST), 
            WVHT_avg = mean(WVHT),
            PRES_avg  = mean(PRES))

#-------------------------------------------------------------------------------------------------------------


# Now let's slice buoy_by_hour into 19 pieces in order to aggregate it by 6 hours (take mean.)
#-------------------------------------------------------------------------------------------------------------

# b1 <- buoy_by_hour[1:6,]
# b2 <- buoy_by_hour[7:12,]
# b3 <- buoy_by_hour[13:18,]
# b4 <- buoy_by_hour[19:24,]
# b5 <- buoy_by_hour[25:30,]
# b6 <- buoy_by_hour[31:36,]
# b7 <- buoy_by_hour[37:42,]
# b8 <- buoy_by_hour[43:48,]
# b9 <- buoy_by_hour[49:54,]
# b10 <- buoy_by_hour[55:60,]
# b11 <- buoy_by_hour[61:66,]
# b12 <- buoy_by_hour[67:72,]
# b13 <- buoy_by_hour[73:78,]
# b14 <- buoy_by_hour[79:84,]
# b15 <- buoy_by_hour[85:90,]
# b16 <- buoy_by_hour[91:96,]
# b17 <- buoy_by_hour[97:102,]
# b18 <- buoy_by_hour[103:108,]
# b19 <- buoy_by_hour[109:114,]
# 
# buoy_by_hour
# b1
# class(b1)
# typeof(b1)
# b_list = list(b1,b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19)
# 
# a_null <- rep(NA, 19)
# 
# a_list_null <- list(rep("x", 19))
# vec_null <- rep(NA, 19)


# for (i in 1:19) {
#   a_list_null[i] <- apply(data.frame(b_list[i]), MARGIN = 2, as.numeric)
# }
# 
# b_list[1] <- apply(data.frame(b_list[1]), MARGIN = 2, as.numeric)
# 
# 
# for (i in 1:19) {
#   vec_null[i] <- apply(data.frame(b_list[i]), MARGIN = 2, as.numeric)
# }
# 
# 
# 
# a_list_null[1]<- apply(data.frame(b_list[1]), MARGIN = 2, as.numeric)
# 
# for (i in 1:19) {
#   a[i] <- apply(b_list[i], MARGIN = 2, FUN =  mean)
# }

 
buoy_by_hour

a <-  buoy_by_hour %>% 
  mutate(g = floor(as.numeric(hh) / 6))
a

# for (i in 1:length(a$g)) {
#   if (a$g[i] ==0)
#     a$g[i] <- 4
# }





# Add a Date buoy_by_six
#-------------------------------------------------------------------------------------------------------------

a

buoy_by_six <- a %>% 
  group_by(DD, g) %>% 
  summarise(WSPD_avg_six = mean(WSPD_avg), 
            GST_avg_six = mean(GST_avg),
            WVHT_avg_six = mean(WVHT_avg), 
            PRES_avg_six = mean(PRES_avg))

index <- seq(from = 1, to = 115, by = 6)


buoy_by_six$hh <- a$hh[index]
buoy_by_six$YYMM <- rep(201109, nrow(buoy_by_six))
buoy_by_six %<>% 
  unite("Date_Time", c(YYMM, DD, hh), sep = "", remove = F)

buoy_by_six$Date_Time <- ymd_h(buoy_by_six$Date_Time)
#-------------------------------------------------------------------------------------------------------------



# b_list[1]
# apply(data.frame(b_list[1]), MARGIN = 2, FUN =  mean)
# 

#-------------------------------------------------------------------------------------------------------------





# Now let's do some visualizations, starting with single line time series plots to explore some trends.
#-------------------------------------------------------------------------------------------------------------
# Wind Speed.
# WSPD	Wind speed (m/s) averaged over an eight-minute period for buoys and a two-minute period for land stations.
# Thus each day in Date we would have s

p <- ggplot(buoy, aes(x = Date, y = WSPD)) +
  geom_line() + 
  xlab("")
p
head(buoy)
#-------------------------------------------------------------------------------------------------------------
