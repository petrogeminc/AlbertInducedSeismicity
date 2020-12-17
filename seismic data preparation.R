
#if(!requireNamespace("devtools")) install.packages("devtools")
#devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)


library(maptools)
library(rgdal)
library(ggplot2)
library(gganimate)
library(ggmap)
library("scales") # for function date_format
library(magick)

#########Importing and completeing seisimcity map

sdf<-read.csv('Copy of Recent_AB_Earthquakes.csv')

source("LongLatToUTM-modified.r")
#finding UTM coordinates

utm<-mapply(LongLatToUTM,sdf$LON,sdf$LAT)

UTM<- data.frame(matrix(unlist(utm), nrow=2225, byrow=T),stringsAsFactors=FALSE)
UTM[2]<-NULL
colnames(UTM)[1]="Zone"
colnames(UTM)[3]="Northing"
colnames(UTM)[2]="Easting"

sdf<-cbind(sdf,UTM)
sdf$Easting=sdf$Easting/1000
sdf$Northing=sdf$Northing/1000





#converting date
sdf$DATETIME_ <- as.Date(sdf$DATETIME_,"%m/%d/%Y")
#sdf$Date_num <- as.numeric(sdf$DATETIME_ )
#collecting all the dates of each month in one month 

sdf$year_month=paste(as.character(sdf$YEAR_),"/",as.character(sdf$MONTH_),"/1",sep="") #"/",toString(sdf$MONTH_),"/","1")
sdf$year_month<- as.Date(sdf$year_month,"%Y/%m/%d")


#converting magnitude to traffic light system
sdf$traffic_light<-cut(sdf$MAG, breaks = c(-Inf, 2, 4, Inf), labels=c("Green", "Yellow", "Red"),include.lowest = TRUE)

sdf$ID <- seq.int(nrow(sdf))

write.csv(sdf,"prepared-sdf.csv")
