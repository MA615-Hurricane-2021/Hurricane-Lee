library(tidyverse)
library(magrittr)
library(ggplot2)
library(rlang)


# There is only one function to create the chart, bouyData(input, name), for instance bouyData(WSPD, "WSPD")

# source the function from download_data.R and combine the column
buoyCbind <- function(){
  source("download_data.R") #if you cannot source this file, open it can run download_data() only!!
  df1 <- download_data()
  data_update <- df1 %>% 
    unite("Date", "MM": "DD", sep = "-") %>%
    unite("Time", "hh":"mm", sep = ":") %>% 
    unite("DT", "Date":"Time", sep = " ")
  data_update <- data_update[,-c(6,7,8,9,13,14,15)] # select the column we use for next function
  data_update[,3:8] <- apply(data_update[,3:8], 2, as.numeric) # change the column into numerical
  return(data_update)
}


buoyData <- function(input, name){
  dta_1 <- buoyCbind()
  sq1 <- seq(1, length(dta_1$DT), 60) # set the sequence for every six hour in x-axis
  x_label <- as.list(dta_1[sq1,"DT"])
  pt <- ggplot(dta_1, aes(x = DT)) +  #{{}} curly-curly, ensure plot the correct column
    geom_line(aes(y = {{input}}), group = 1, color = "light blue") +
    scale_x_discrete(breaks = x_label[["DT"]]) +
    labs(x = "Timeline") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 90), 
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"))
 
   # Add title for different plot
  if(name == "WSPD") {
    j <- pt + ggtitle("Wind Speed (WSPD)", subtitle = "Unit: m/s")
    return(j)
  }
  if(name == "WDIR") {
    j <- pt + ggtitle("Wind Direction (WDIR)", subtitle = "Unit: °T")
    return(j)
  }
  if(name == "GST") {
    j <- pt+ ggtitle("Wind Gust (GST)", subtitle = "Unit: m/s")
    return(j)
  }
  if(name == "PRES") {
    j <- pt + ggtitle("Atmospheric Pressure (PRES)", subtitle = "Unit: hPa")
    return(j)
  }
  if(name == "ATMP") {
    j <- pt + ggtitle("Air Temperature (ATMP)", subtitle = "Unit: °C")
    return(j)
  }
  if(name == "WTMP") {
    j <- pt + ggtitle("Water Temperature (WTMP)", subtitle = "Unit: °C")
    return(j)
  }
}


# Demo
# buoyData(WDIR,"WDIR")






# Test
# library(lubridate)
# 
# data_update <- df1 %>% 
#   unite("Date", "#YY": "DD", sep = "-") %>%
#   unite("Time", "hh":"mm", sep = ":")
# data_update$Date <- paste(data_update$Date, data_update$Time, sep = " ")
# data_update$Date <- ymd_hm(data_update$Date)
# 
# 
# bouyCbind1 <- function(){
#   source("download_data.R") #if you cannot source this file, open it can run download_data() only!!
#   df1 <- download_data()
#   data_update <- df1 %>% 
#     unite("Date", "mm": "DD", sep = "-") %>%
#     unite("Time", "hh":"mm", sep = ":")
#   data_update$Date <- paste(data_update$Date, data_update$Time, sep = " ")
#   data_update$Date <- ymd_hm(data_update$Date)
#   data_update <- data_update[,-c(2,6,7,8,9,13,14,15)] # select the column we use for next function
#   data_update[,3:8] <- apply(data_update[,2:7], 2, as.numeric) # change the column into numerical
#   return(data_update)
# }
# 
# bouyData1 <- function(input){
#   dta_1 <- bouyCbind1()
#   # sq1 <- seq(1, length(dta_1$DT), 60) # set the sequence for every six hour in x-axis
#   # x_label <- as.list(dta_1[sq1,"DT"])
#   pt <- ggplot(dta_1, aes(x = Date)) +  #{{}} curly-curly, ensure plot the correct column
#     geom_line(aes(y = {{input}}), group = 1, color = "light blue") +
#     scale_x_datetime(breaks = "day") +
#     labs(x = "Timeline") + theme_bw() + 
#     theme(axis.text.x = element_text(angle = 90), 
#           panel.border = element_blank(),
#           panel.grid = element_blank(),
#           axis.line = element_line(colour = "black"))
#   return(pt)
# }
# 
# bouyData1(ATMP)
