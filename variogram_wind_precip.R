library(hurricaneexposure)
library(drat)
library(hurricaneexposuredata)
library(dplyr)

data("ext_tracks_wind")
data("storm_winds")
data("rain")
data("county_centers")
rainf <- rain %>% filter(storm_id =="Lee-2011")
wind_storm <- storm_winds %>% filter(storm_id == "Lee-2011")


df1 <- inner_join(rainf, wind_storm, by = "fips")
F_df <- inner_join(df1, county_centers, by = "fips")%>%
  select(-c(3,4,11,12,13,14,15))

forvariogram<- F_df%>%
  group_by(fips)%>%
  summarise(precip = mean(precip),vmax_sust = mean(vmax_sust),
            lati= mean(latitude), longi=mean(longitude))
#plot variogram for precip and wind
variogram_precip<-variogram(precip~1,~lati+longi,data = forvariogram)
plot(variogram_precip)

variogram_wind<-variogram(vmax_sust~1,~lati+longi,data = forvariogram)
plot(variogram_wind)

#fit variogram model
#Using Spherical model to fit variogram of precip, 
#Using Gaussian model to fit variogram of wind 
variogram_precip.fit = fit.variogram(variogram_precip,
                                     vgm( "Sph"))
variogram_wind.fit = fit.variogram(variogram_wind, vgm("Gau"))

plot(variogram_precip,variogram_precip.fit,
     main="Variogram of precip and fitted line",
     xlab="distance between counties",
     ylab="semivariance",pch=1,col="black")


plot(variogram_wind,variogram_wind.fit,    
     main="Variogram of sustained wind and fitted line",
     xlab="distance between counties",
     ylab="semivariance",pch=16,col="black")



