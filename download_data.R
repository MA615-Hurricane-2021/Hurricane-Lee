library(dplyr)
library(readr)
library(tidyverse)
library(magrittr)

download_data <- function(
  url = "https://www.ndbc.noaa.gov/view_text_file.php?filename=amrl1h2011.txt.gz&dir=data/historical/stdmet/",
  select_date = T){
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
df1 <- download_data()
# 


buoy_cdate <- function(df){
  # combine the month and date into a new parameter named "Date"
  df_1 <- df %>% 
    unite("Date", "MM": "DD", sep = "/")
  return(df_1)
}

df2 <- download_data()
df2 <- df2 %>% buoy_cdate()
print(df2)


group_buoy <- function(i) {
  j <- ifelse(i %in% c("00","01","02","03","04","05"), 1, 
              ifelse(i %in% c("06","07","08","09","10","11"), 2, 
                     ifelse(i %in% c("12","13","14","15","16","17"), 3, 4)))
  return(j)
}

group_buoy_2 <- function(){
  
}

df2$timegroup <- sapply(df2$hh, group_buoy)

# we can also directly 
# df2$timegroup <- ifelse(df2$hh %in% c("00","01","02","03","04","05"), "group1", 
#                         ifelse(df2$hh %in% c("06","07","08","09","10","11"), "group2","group3"))

#### test #################


## str_extract(string = ,pattern = )
## extract the right 2 characters of date, combine them with timegroup so we get new class grouped by date and hour

df2$group <- as.numeric(str_sub(df2$Date, -1, -1))*10 + df2$timegroup

group_buoy_date <- function(i) {
  j <- ifelse(i, 1, 
              ifelse(i %in% c("06","07","08","09","10","11"), 2, 
                     ifelse()))
  return(j)
}



colnames(df2)


# how to get the exact column by index R

structure(df2)
ncol(df2)

df2 %<>% mutate_at(4:ncol(df2), as.numeric)

df2 %>%
  group_by(group) %>%
  summarise_at(vars("WSPD", "WDIR"), list(name = n()))

df2 %>%
  group_by(group) %>%
  summarise(count = n())


aggregate(x = c(df2$WDIR, df$WSPD),     
          
          # Specify group indicator
          by = list(as.numeric(df2$group)),      
          
          # Specify function (i.e. mean)
          FUN = mean)



my_data <- data.frame(x1 = 1:5,            # Create example data
                      x2 = 2:6,
                      x3 = 3)

my_data$sum <- my_data %>% apply(1, sum)
