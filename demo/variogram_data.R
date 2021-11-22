library(hurricaneexposure)
library(drat)
library(hurricaneexposuredata)
library(dplyr)

getVario_Data <- function() {
  
  data("ext_tracks_wind")
  data("storm_winds")
  data("rain")
  
  rainf <- rain %>% filter(storm_id =="Lee-2011")
  wind_storm <- storm_winds %>% filter(storm_id == "Lee-2011")
  wind_ext <- ext_tracks_wind %>% filter(storm_id == "Lee-2011")
  
  df1 <- inner_join(rainf, wind_storm, by = "fips") %>% 
    select(-c(3,11,12)) %>% 
    rename(M_vmax_gust = "vmax_gust") %>% 
    rename(M_vmax_sust = "vmax_sust") %>% 
    rename(M_gust_dur = "gust_dur") %>% 
    rename(M_sust_dur = "sust_dur")
  
  F_df <- inner_join(df1, wind_ext, by = "fips") %>% 
    select(-13) %>% 
    rename(E_vmax_gust = "vmax_gust") %>%
    rename(E_vmax_sust = "vmax_sust") %>% 
    rename(E_sust_dur = "sust_dur") %>% 
    rename(storm_id = "storm_id.x")
  
  return(F_df)
}

# Demo
# df <- getVario_Data()

# Note
# This data fram combine with three datasets, the column with M_ such as M_vmax_gust means Modeled county wind speeds 
# the column with E_ eg. E_vmax_gust means Estimated county wind speeds