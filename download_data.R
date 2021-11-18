library(dplyr)
library(readr)
library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(eply)



str(unquote("x"))
{{"x"}}
download_data <- function(
  url = "https://www.ndbc.noaa.gov/view_text_file.php?filename=amrl1h2011.txt.gz&dir=data/historical/stdmet/",
  select_date = T){
  # download the buoy data from the website 
  ### url---str, the link you want to pull your data from
  ### select_date--T/F, turn on to select the date when hurricane Lee occured
  suppressMessages(  ###  This stops the annoying messages on your screen.
    file3 <- read_table(url, col_names = TRUE)
  )
  if (select_date) {
    data_new <-  file3 %>% filter(MM == "09" & (DD =="06" | DD =="05" | DD =="04" | DD =="03" | DD =="02") )   
    return(data_new)
  }
  else {
    return(file3)
  }
  
}
# demo
# df1 <- download_data()
# #
# view(hurr_tracks)
# 


buoy_cdate <- function(df){
  # combine the month and date into a new parameter named "Date"
  df_1 <- df %>% 
    unite("Date", "MM": "DD", sep = "/")
  return(df_1)
}




group_buoy <- function(i) {
  j <- ifelse(i %in% c("00","01","02","03","04","05"), 1, 
              ifelse(i %in% c("06","07","08","09","10","11"), 2, 
                     ifelse(i %in% c("12","13","14","15","16","17"), 3, 4)))
  return(j)
}


# we can also directly 
# df2$timegroup <- ifelse(df2$hh %in% c("00","01","02","03","04","05"), "group1", 
#                         ifelse(df2$hh %in% c("06","07","08","09","10","11"), "group2","group3"))

# acquire the date
## method 1: to extract the date from the group value
group_to_date <- function(i) {
  if (((i %% 10 - 1)*6) %/% 10 == 0){
    ## if the value of hours is only one digit, we add 0 to the value of hours and turn it into a character
    ## if I use ymd_hm or as.POSIXct, the character type will change into numeric type, why?
    (paste0("2011","-09-0",toString(floor(i / 10))," 0", toString((i %% 10 - 1) * 6), ":00"))
  }
  else
    (paste0("2011","-09-0",toString(floor(i / 10))," ", toString((i %% 10 - 1) * 6), ":00"))
}
# 
# df2_stat$Date <- df2_stat$group %>% sapply(FUN = group_to_date)

## method 2: extract from the original data and use seq to get every 6 hours' data

calculate_stat_per6h <- function(method = "median"){
  df2 <- download_data()
  df2 <- df2 %>% buoy_cdate()
  # group the data by every 6 hours
  df2$timegroup <- sapply(df2$hh, group_buoy)
  # group the data by date and every 6 hours
  df2$group <- as.numeric(str_sub(df2$Date, -1, -1))*10 + df2$timegroup
  
  # delete the columns without data
  df2 <- df2 %>% subset(select = -c(WVHT, DPD, APD, MWD, DEWP, VIS, TIDE))
  
  # mutate the type of data from column 4 to the last column
  df2 %<>% mutate_at(4:ncol(df2), as.numeric)
  
  # get the column we want to calculate the median
  col_1 <- colnames(df2)[-1:-4][-14:-15]
  
  # calculate the median of each column
  df2_stat <- df2 %>%
    group_by(group) %>%
    summarise_at(vars(col_1), list(mid_6h = {{method}}))
  
  # aquire the time of each row in df2_stat
  ## method_1
  df2_stat$Date <- df2_stat$group %>% sapply(FUN = group_to_date)
  
  # aquire the time of each row in df2_stat
  # method_2
  # df3 <- df2 %>% 
  #   unite("Time", "hh": "mm", sep = ":") %>% 
  #   unite("D", "Date": "Time", sep = " ") %>% 
  #   select("D")
  # df3_time <- df3[seq(1,nrow(df3),60),]
  
  # add the timestamp in df2_stat
  # df2_stat$Date <- df3_time
  return(df2_stat)
  
}
# demo
df1 <- calculate_mid_per6h()
