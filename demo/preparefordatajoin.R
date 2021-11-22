#install.packages("gstat")
library(gstat)  
#install.packages("fields")
library(fields)
#install.packages("sp")
library(sp)
library(magrittr)
library(lubridate)
library(dplyr)
source("map_hurricane_1.R")
source("bouy_plot.R")

lee <- hurr_tracks %>% filter(hurr_tracks$storm_id == "Lee-2011")

#lee$date <- ymd_hm(lee$date)


df1<-df1%>%
  mutate(date=paste(`#YY`,MM,DD,hh,sep = ""))

df1.2<- df1%>% 
  group_by(date)%>%
  summarise(WDIR=mean(as.numeric(WDIR)),WSPD=mean(as.numeric(WSPD)),
            GST=mean(as.numeric(GST)),WVHT=mean(as.numeric(WVHT)),
            DPD=mean(as.numeric(DPD)),APD=mean(as.numeric(APD)),
            MWD=mean(as.numeric(MWD)),PRES=mean(as.numeric(PRES)),
            ATMP=mean(as.numeric(ATMP)),WTMP=mean(as.numeric(WTMP)),
            DEWP=mean(as.numeric(DEWP)),VIS=mean(as.numeric(VIS)),
            TIDE=mean(as.numeric(TIDE)))
df1.3<- df1.2[1:20,]
  for (i in 0:19){
    df1.3.1<-filter(df1.2[1+6*i,])
    df1.3[i+1,]<-df1.3.1
  }

lee1.3<- lee

lee1.3$date<-substr(lee$date,1,10)

sixhours<- inner_join(lee1.3,df1.3,by="date")


vgram<-vgram(loc=cbind(sixhours$latitude,sixhours$longitude),y=sixhours$WDIR)
plot(vgram)
 