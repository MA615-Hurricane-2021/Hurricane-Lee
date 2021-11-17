library(tidyverse)
library(magrittr)
library(ggplot2)
library(rlang)


# source the function from download_data.R and combine the column
bouyCbind <- function(){
  source("download_data.R") #if you cannot source this file, open the file and run download_data() only!!!!
  df1 <- download_data()
  data_update <- df1 %>% 
    unite("Date", "MM": "DD", sep = "-") %>%
    unite("Time", "hh":"mm", sep = ":") %>% 
    unite("DT", "Date":"Time", sep = " ")
  data_update <- data_update[,-c(6,7,8,9,13,14,15)] # select the column we use for next function
  data_update[,3:8] <- apply(data_update[,3:8], 2, as.numeric)
}


bouyData <- function(input){
  dta_1 <- data_update
  sq1 <- seq(1, length(dta_1$DT), 60)
  x_label <- as.list(dta_1[sq1,"DT"])
  pt <- ggplot(dta_1, aes(x = DT)) +  #{{}} curly-curly, ensure plot the correct column
    geom_line(aes(y = {{input}}), group = 1, color = "maroon") +
    scale_x_discrete(breaks = x_label[["DT"]]) +
    labs( x = "Date") + theme_bw() + 
    theme(axis.text.x = element_text(angle = 90), 
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"))
  return(pt)
}


# Demo
# bouyData(ATMP)

