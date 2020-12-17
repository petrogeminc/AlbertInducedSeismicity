library(magick)
library(maptools)
library(rgdal)
library(ggplot2)
library(gganimate)
library(ggmap)
library(scales) # for function date_format
library(tidyverse)

setwd("C:/Users/has89/OneDrive/Desktop/Alberta Induced Seismicity")

##########Graphing
sdf<-read.csv("./csv files/prepared-sdf.csv")


#Set your API Key
ggmap::register_google(key = "AIzaSyD5e-QpVDdW5sU21RYFFStK97kT0fnfjO0")
has_google_key()


sdf$DATETIME_<-as.Date(sdf$DATETIME_)

sdf$year_month<-as.Date(sdf$year_month)


#converting magnitude to traffic light system
sdf$traffic_light_kaush<-cut(sdf$MAG, breaks = c(-Inf, 1,2,3,4, Inf), labels=c("LightGreen","DarkGreen","Yellow", "Orange","Red"), include.lowest = TRUE)



#Alberta Map

Alberta <- get_map(location = c(-120.5,48.5,-109.5,60.5), source = 'google', maptype =  "terrain",zoom=6)


count_by_year<-ggplot(data= sdf, aes(x =DATETIME_,  ..count..,fill=factor(sdf$traffic_light_kaush,levels=c("Red","Orange", "Yellow", "DarkGreen","LightGreen"))))+
  geom_histogram( color="black")+
  xlab("Year") + ylab("Count") +
  scale_color_brewer(palette = "Spectral") +
  scale_x_date(breaks = date_breaks("2 year"),labels = date_format("%Y")) +theme_bw()+
  scale_fill_manual(values = c("LightGreen" = "green","DarkGreen"="darkgreen", "Yellow" = "yellow","Orange" = "orange","Red" = "red"),name = "Traffic Light System:    ")+
  theme(legend.position="none")

count_by_year

count_by_year_anim<-count_by_year +transition_time(sdf$year_month)+shadow_mark() +labs(title = "Year: {substr(frame_time,1,7)}")
#count_by_year_anim<-count_by_year +transition_states(sdf$year_month, transition_length = .1, state_length = .1) +labs(title = "Year: {closest_state}")+shadow_mark()


count_by_year_anim

anim1=animate(count_by_year_anim,nframes=length(unique(sdf$year_month)), fps=4)

anim1


mag_by_year<-ggplot(data= sdf, aes(x = MAG ,  ..count..,fill=factor(sdf$traffic_light_kaush,levels=c("Red","Orange", "Yellow", "DarkGreen","LightGreen"))))+
  geom_histogram(colour='black',binwidth = 0.1)+
  xlab("Magnitude") + ylab("Count") +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_manual(values = c("LightGreen" = "green","DarkGreen"="darkgreen", "Yellow" = "yellow","Orange" = "orange","Red" = "red"),name = "Traffic Light System:    ")+
  theme_bw()+theme(legend.position="none")

mag_by_year

mag_by_year_anim<-mag_by_year +transition_time(sdf$year_month) +labs(title = "Year: {substr(frame_time,1,7)}")

mag_by_year_anim

anim2=animate(mag_by_year_anim,nframes=length(unique(sdf$year_month)), fps=4)

anim2


LL.seismicity.map <- ggmap(Alberta)+
  geom_point(data=sdf,aes(x=sdf$LON, y=sdf$LAT, color=factor(sdf$traffic_light_kaush,levels=c("Red","Orange", "Yellow", "DarkGreen","LightGreen"))))+
  labs(title="Seismicity Map", x="Longitude", y="Latitude")+
  scale_color_manual(values = c("LightGreen" = "green","DarkGreen"="darkgreen", "Yellow" = "yellow","Orange" = "orange","Red" = "red"),name = "Traffic Light System:    ")+
  theme_bw()+theme(legend.position="none")#+
  #geom_point(aes(x=-117.5,y=54.5),size = 20, pch = 1)+#almost Fox Creek Lat/Long
  #geom_point(aes(x=-115,y=52.5),size = 20, pch = 1)#almost Rocky MOuntain House Lat/Long
  #geom_point(aes(x=-116.8089,y=54.4022),size = 20, pch = 1)+#exact Fox Creek Lat/Long
  #geom_point(aes(x=-114.9183,y=52.3793),size = 20, pch = 1)#exact Rocky MOuntain House Lat/Long



LL.seismicity.map

LL.seismicity.anim<-LL.seismicity.map +  
  transition_time(as.Date(sdf$year_month)) +
  labs(title = "Year: {substr(frame_time,1,7)}")+
  ease_aes('linear')  

LL.seismicity.anim

anim3=animate(LL.seismicity.anim,nframes=length(unique(as.Date(sdf$year_month))), fps=4)


anim3



depth_lat_mag <- ggplot()+
  geom_point(data=sdf,aes(x=sdf$LAT, y=sdf$DEPTH, color=factor(sdf$traffic_light_kaush,levels=c("Red","Orange", "Yellow", "DarkGreen","LightGreen"))))+
  labs(x="Latitude", y="Depth")+
  scale_color_manual(values = c("LightGreen" = "green","DarkGreen"="darkgreen", "Yellow" = "yellow","Orange" = "orange","Red" = "red"),name = "Traffic Light System:    ")+
  theme_bw()+theme(legend.position="none")+
  scale_y_continuous(trans = "reverse")#+
  #geom_vline(xintercept = 52.5, linetype="solid", 
            # color = "blue", size=20, alpha=0.1)+
  #geom_vline(xintercept = 54.5, linetype="solid", 
            # color = "blue", size=20, alpha=0.1)



#geom_point(aes(x=-116.8089,y=54.4022),size = 20, pch = 1)+#Fox Creek Lat/Long
#geom_point(aes(x=-114.9183,y=52.3793),size = 20, pch = 1)#Rocky MOuntain House Lat/Long



depth_lat_mag

depth_lat_mag.anim<-depth_lat_mag +  
  transition_time(sdf$DATETIME_) +transition_time(sdf$year_month) +labs(title = "Year: {substr(frame_time,1,7)}")

anim4=animate(depth_lat_mag.anim,nframes=length(unique(sdf$year_month)), fps=4)

anim4


depth_long_mag <- ggplot()+
  geom_point(data=sdf,aes(x=sdf$LON, y=sdf$DEPTH, color=factor(sdf$traffic_light_kaush,levels=c("Red","Orange", "Yellow", "DarkGreen","LightGreen"))))+
  labs( x="Longitude", y="Depth")+
  scale_color_manual(values = c("LightGreen" = "green","DarkGreen"="darkgreen", "Yellow" = "yellow","Orange" = "orange","Red" = "red"),name = "Traffic Light System:    ")+
  theme_bw()+theme(legend.position="none")+
  scale_y_continuous(trans = "reverse")#+
  #geom_vline(xintercept = -117.5, linetype="solid", 
            # color = "blue", size=20, alpha=0.1)+
  #geom_vline(xintercept = -115, linetype="solid", 
           #  color = "blue", size=20, alpha=0.1)

  
  #geom_point(aes(x=-116.8089,y=54.4022),size = 20, pch = 1)+#Fox Creek Lat/Long
  #geom_point(aes(x=-114.9183,y=52.3793),size = 20, pch = 1)#Rocky MOuntain House Lat/Long
  

depth_long_mag

depth_long_mag.anim<-depth_long_mag +  
  transition_time(sdf$DATETIME_) +transition_time(sdf$year_month) +labs(title = "Year: {substr(frame_time,1,7)}")

anim5=animate(depth_long_mag.anim,nframes=length(unique(sdf$year_month)), fps=4)

anim5


depth_by_year<-ggplot(data= sdf, aes(x = factor(YEAR_) ,  y=DEPTH, alpha=.5))+
  geom_boxplot(color='blue',fill='gold',outlier.alpha = 0.05)+geom_jitter(color='black', alpha=0.1)+
scale_y_continuous(trans = "reverse")+
  xlab("Year") + ylab("Depth (km)") +
  scale_color_brewer(palette = "Spectral")+
  theme_bw()+theme(legend.position="none")

depth_by_year

mag_by_year_anim<-mag_by_year +transition_time(sdf$year_month) +labs(title = "Year: {substr(frame_time,1,7)}")

mag_by_year_anim

anim2=animate(mag_by_year_anim,nframes=length(unique(sdf$year_month)), fps=4)

anim2








a_gif<-animate(count_by_year_anim, width = 480, height = 240)
b_gif<-animate(mag_by_year_anim, width = 480, height = 240)
c_gif<-animate(LL.seismicity.anim, width = 240, height = 480)
d1_gif<-animate(depth_lat_mag.anim, width = 480, height = 240)
d2_gif<-animate(depth_long_mag.anim, width = 480, height =240)




anim_save("a_gif.gif",a_gif)
anim_save("b_gif.gif",b_gif)

anim_save("c_gif.gif",c_gif)

anim_save("d1_gif.gif",d1_gif)
anim_save("d2_gif.gif",d2_gif)





