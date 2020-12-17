leducreefs <- readShapePoly("./shape files/WCSB_ATLAS_1994_LEDUC_REEF_OUTLINE.shp")
leducreefs.points <- fortify(leducreefs)
#source("Lat&Long2UTM.R")

UTM2<-LongLatToUTM(leducreefs.points$long,leducreefs.points$lat)


#UTM2<-LongLatToUTM(leducreefs.points$lat,leducreefs.points$long)
# Adding the Easting and Northing to the datbase

leducreefs.points$Easting<-UTM2[[2]]/1000
leducreefs.points$Northing<-UTM2[[3]]/1000

#swanhillsreefs.points <- fortify(swanhillsreefs)

UTM.base.map<- geom_polygon(aes(x = Easting,
                   y = Northing,
                   group = group),
               data = leducreefs.points,
               color = "lightblue",
               fill = "lightblue",
               alpha = 0.3) 







