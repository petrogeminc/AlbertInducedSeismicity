
#Function
LongLatToUTM<-function(x,y){
  zone <- 31+(x%/%6)
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=NAD83")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=NAD83",sep='')))
  results<-merge(zone,as.data.frame(res))
  return(results)
}

