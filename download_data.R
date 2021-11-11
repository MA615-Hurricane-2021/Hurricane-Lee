library(dplyr)

download_data <- function(url = 
           "https://www.ndbc.noaa.gov/view_text_file.php?filename=amrl1h2011.txt.gz&dir=data/historical/stdmet/"){
  ### new url1
  suppressMessages(  ###  This stops the annoying messages on your screen.
    file3 <- read_table(url3, col_names = TRUE)
  )
  
  data_new <-  file3 %>% filter(MM == "09" & (DD =="06" | DD =="05" | DD =="04" | DD =="03" | DD =="02") )   
  return(data_new)
}
# demo
# df1 <- download_data()
#