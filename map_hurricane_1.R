library(hurricaneexposure)
library(drat)
library(tidyverse)
library(stats)


#-------------------------------------------------------------------------------------------------------------
# Use hurricanexposuredata package to do visualizations.

addRepo("geanders")

#install.packages("hurricaneexposuredata")

library(hurricaneexposuredata)

data("hurr_tracks")

data("rain")

head(hurr_tracks)

map_counties(storm = "Lee-2011", metric = "rainfall")
map1 <- map_counties(storm = "Lee-2011", metric = "rainfall", days_included = -5:10)
map1

map_counties(storm = "Lee-2011", metric = "wind", wind_var = "sust_dur")
map_counties(storm = "Lee-2011", metric = "wind", wind_source = "ext_tracks")

map_counties(storm = "Lee-2011", metric = "distance")

map_rain_exposure(storm = "Lee-2011", rain_limit = 175, dist_limit = 500,
                  days_included = -5:3)

map_tracks(storms = "Lee-2011",
           alpha = 0.5, plot_points = TRUE, color = "blue")

floyd_map <- map_event_exposure(storm = "Lee-2011", event_type = "flood")
map_tracks(storms = "Lee-2011", plot_object = floyd_map, plot_points = TRUE, 
           color = "darkgray")

tornado <- map_event_exposure(storm = "Lee-2011", event_type = "tornado")

map_tracks(storms = "Lee-2011", plot_object = tornado, plot_points = TRUE, 
           color = "darkgray")

hur1 <- hurr_tracks %>% 
  filter(storm_id == "Lee-2011")


# data("wind_storms")
data("storm_winds")
head(storm_winds)
storm_winds %>% 
  filter(storm_id == "Lee-2011")




