source("download_data.R")
library(tidyverse)
library(ggplot2)


bouyData <- function(input){
  df1.1 <- df1 %>% 
    unite("Date", "#YY": "DD", sep = "/") %>%
    unite("Time", "hh":"mm", sep = ":") %>% 
    unite("DT", "Date":"Time", sep = " ")
  pt <- ggplot(df1.1, aes(DT, {{input}})) +  #{{}} curly-curly, ensure plot the correct column
    geom_line(group = 1) 
    # theme(axis.text.x = element_text(angle = 45)) not recommend at this time 
    return(pt)
}


# Debugging
bouyData(ATMP)


