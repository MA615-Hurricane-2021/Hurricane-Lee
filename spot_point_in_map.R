library(ggplot2)
library(ggmap)
library(sp)
library(maptools)
library(maps)

mydata <- data.frame(longitude = c(29, 37, -5), latitude = c(-3, 55, 54))
visit.x<-mydata$longitude

visit.y<-mydata$latitude #数据准备

mp<-NULL #定义一个空的地图
mapworld<-borders("world",colour = "gray50",fill="white") #绘制基本地图

mp<-ggplot()+mapworld+ylim(-60,90)

#利用ggplot呈现，同时地图纵坐标范围从-60到90

mp2<-mp+geom_point(aes(x=visit.x,y=visit.y,size=1),color="darkorange")+scale_size(range=c(1,1))

#绘制带点的地图，geom_point是在地图上绘制点，x轴为经度信息，y轴为纬度信息，size是将点的大小按照收集的个数确定，color为暗桔色，scale_size是将点变大一些

mp3<-mp2+theme(legend.position = "none") #将图例去掉

mp3 #将地图呈现出来

library(hurricaneexposure)
library(hurricaneexposuredata)

data("hurr_tracks")

data("rain")

head(hurr_tracks)
map_tracks(storms = "Lee-2011",
           alpha = 0.5, plot_points = TRUE, color = "blue") + 
  geom_point(aes(x=c(-91.338),y=c(29.450),size=2),color="darkorange")+scale_size(range=c(1,1)) +
  labs(size = "buoy station") +
  # guides(color = )

