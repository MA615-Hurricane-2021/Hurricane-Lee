library(tidyverse)
library(magrittr)
library(ggplot2)
library(rlang)

bouyCbind <- function(){
  source("download_data.R")
  df1 <- download_data()
  data_update <- df1 %>% 
    unite("Date", "MM": "DD", sep = "-") %>%
    unite("Time", "hh":"mm", sep = ":") %>% 
    unite("DT", "Date":"Time", sep = " ")
  data_update <- data_update[,-c(6,7,8,9,13,14,15)]
}


bouyData <- function(input){
  dta_1 <- bouyCbind()
  sq1 <- seq(1, length(dta_1$DT),60)
  x_label <- as.list(dta_1[sq1,"DT"])
  pt <- ggplot(dta_1, aes(x = DT)) +  #{{}} curly-curly, ensure plot the correct column
    geom_line(aes(y = as.numeric({{input}})), group = 1, color = "maroon") +
    scale_x_discrete(breaks = x_label[["DT"]]) +
    xlab("Date") + theme_bw() +
    theme(axis.text.x = element_text(angle = 90), 
          panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_line(colour = "black"))
  return(pt)
}


# Demo
# bouyData(WSPD)