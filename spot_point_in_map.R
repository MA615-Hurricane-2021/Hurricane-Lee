library(hurricaneexposure)
library(hurricaneexposuredata)
library(ggplot2)

plot_buoy_with_tracks <- function(){
  # get the data for plot
  data("hurr_tracks")
  data("rain")
  a <- head(hurr_tracks)
  # plot the track with the buoy station point
  map1 <- map_tracks(storms = "Lee-2011",
             alpha = 0.5, plot_points = TRUE, color = "blue") + 
    geom_point(aes(x=c(-91.338),y=c(29.450),fill = "AMRL1"),color="darkorange")+scale_size(range=c(1,1)) +
    labs(fill = "Buoy Station") 
    # element_text()
    # guides(size = "buoy station")
  return(map1)
}

