library(hurricaneexposure)
library(drat)
library(hurricaneexposuredata)
library(dplyr)
library(gstat)
library(fields)
# Function version, easy for further use. 
varg_Clean <- function() {
  data("ext_tracks_wind")
  data("storm_winds")
  data("rain")
  data("county_centers")
  rainf <- rain %>% filter(storm_id =="Lee-2011")
  wind_storm <- storm_winds %>% filter(storm_id == "Lee-2011")
  
  # inner_join two data set by fips number
  df1 <- inner_join(rainf, wind_storm, by = "fips")
  F_df <- inner_join(df1, county_centers, by = "fips")%>%
    select(-c(3,4,11,12,13,14,15))
  
  # get the average number for each level
  forvariogram<- F_df%>%
    group_by(fips)%>%
    summarise(precip = mean(precip),vmax_sust = mean(vmax_sust),
              lati= mean(latitude), longi=mean(longitude))
  
  return(forvariogram)
}


varg_Plot <- function(cod, mod) {
  
  forvariogram <- varg_Clean()
  variogram_precip<-variogram(precip~1,~lati+longi,data = forvariogram)
  variogram_wind<-variogram(vmax_sust~1,~lati+longi,data = forvariogram)
  variogram_precip_em<-vgram(loc=cbind(forvariogram$lati,forvariogram$longi),
                             y=forvariogram$precip)
  variogram_wind_em<-vgram(loc=cbind(forvariogram$lati,forvariogram$longi),
                             y=forvariogram$vmax_sust)
  
  
  #fit variogram model
  #Using Spherical model to fit variogram of precip, 
  #Using Gaussian model to fit variogram of wind 
  variogram_precip.fit = fit.variogram(variogram_precip, vgm( "Sph"))
  variogram_wind.fit = fit.variogram(variogram_wind, vgm("Gau"))
  
  
    # add with the fits 
  if(cod == "precip" & mod == 1) {
    p <- plot(variogram_precip,variogram_precip.fit,
        main = NULL,
        xlab="distance between counties",
        ylab="semivariance",pch=1,col="black")
    return(p)
  }
  if(cod == "wind" & mod == 1) {
    p <- plot(variogram_wind,variogram_wind.fit,
         main = NULL,
         xlab="distance between counties",
         ylab="semivariance",pch=16,col="black")
    return(p)
  }

  #plot variogram for precip
  if(cod == "precip" & mod == 2) {
    plot(variogram_precip_em, main = NULL)

  }
  
  #plot variogram for wind
  if(cod == "wind" & mod == 2) {
     plot(variogram_wind_em, main = NULL)
    
  }
}

# Demo
varg_Plot("wind", 2)




#------------------------------------------------------------------------------------


# 
# data("ext_tracks_wind")
# data("storm_winds")
# data("rain")
# data("county_centers")
# rainf <- rain %>% filter(storm_id =="Lee-2011")
# wind_storm <- storm_winds %>% filter(storm_id == "Lee-2011")
# 
# 
# df1 <- inner_join(rainf, wind_storm, by = "fips")
# F_df <- inner_join(df1, county_centers, by = "fips")%>%
#   select(-c(3,4,11,12,13,14,15))
# 
# forvariogram<- F_df%>%
#   group_by(fips)%>%
#   summarise(precip = mean(precip),vmax_sust = mean(vmax_sust),
#             lati= mean(latitude), longi=mean(longitude))
# #plot variogram for precip and wind
# variogram_precip<-variogram(precip~1,~lati+longi,data = forvariogram)
# plot(variogram_precip)
# 
# variogram_wind<-variogram(vmax_sust~1,~lati+longi,data = forvariogram)
# plot(variogram_wind)
# 
# #fit variogram model
# #Using Spherical model to fit variogram of precip, 
# #Using Gaussian model to fit variogram of wind 
# variogram_precip.fit = fit.variogram(variogram_precip,
#                                      vgm( "Sph"))
# variogram_wind.fit = fit.variogram(variogram_wind, vgm("Gau"))
# 
# plot(variogram_precip,variogram_precip.fit,
#      main="Variogram of precip and fitted line",
#      xlab="distance between counties",
#      ylab="semivariance",pch=1,col="black")
# 
# 
# plot(variogram_wind,variogram_wind.fit,    
#      main="Variogram of sustained wind and fitted line",
#      xlab="distance between counties",
#      ylab="semivariance",pch=16,col="black")
# 
# 
# 




