`Induced seismicity` refers to typically minor earthquakes and tremors
that are caused by human activity that alters the stresses and strains
on the Earth’s crust. Most induced seismicity is of a low magnitude
(**wikipedia**). For more details visit
[here](https://ags.aer.ca/activities/induced-seismicity).

This package includes the codes and raw data to evaluate Earthquakes in
Alberta. Raw data has been gathered from public data available in [AER
website](https://www.aer.ca/).

Data Prepration
---------------

We need to install below libraries for this analysis:

    ## Loading required package: maptools

    ## Loading required package: sp

    ## Checking rgeos availability: FALSE
    ##      Note: when rgeos is not available, polygon geometry     computations in maptools depend on gpclib,
    ##      which has a restricted licence. It is disabled by default;
    ##      to enable gpclib, type gpclibPermit()

    ## Loading required package: rgdal

    ## rgdal: version: 1.5-18, (SVN revision 1082)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.0.4, released 2020/01/28
    ## Path to GDAL shared files: D:/OneDrive/OneDrive - University of Calgary/Documents/R/win-library/4.0/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 6.3.1, February 10th, 2020, [PJ_VERSION: 631]
    ## Path to PROJ shared files: D:/OneDrive/OneDrive - University of Calgary/Documents/R/win-library/4.0/rgdal/proj
    ## Linking to sp version:1.4-4
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading rgdal.

    ## Loading required package: ggplot2

    ## Loading required package: gganimate

    ## Loading required package: ggmap

    ## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.

    ## Please cite ggmap if you use it! See citation("ggmap") for details.

    ## Loading required package: scales

    ## Loading required package: magick

    ## Linking to ImageMagick 6.9.11.34
    ## Enabled features: cairo, freetype, fftw, ghostscript, lcms, pango, rsvg, webp
    ## Disabled features: fontconfig, x11

    ## Loading required package: tidyverse

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.0
    ## v purrr   0.3.4

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x readr::col_factor() masks scales::col_factor()
    ## x purrr::discard()    masks scales::discard()
    ## x dplyr::filter()     masks stats::filter()
    ## x dplyr::lag()        masks stats::lag()

first we are Loading Alberta Earthquakes Data prepared in `csv` format.

      sdf <- read.csv('./csv files/Recent_AB_Earthquakes.csv')

Co-ordination has provided in **geographic coordinate system** consists
of latitude and longitude. we need to convert them to **UTM** (Universal
Transverse Mercator) system. This function provide this conversion.

      LongLatToUTM <- function(x,y)
        {
          require(sp)
          zone <- 31+(x%/%6)
          xy <- data.frame(ID = 1:length(x), X = x, Y = y)
          coordinates(xy) <- c("X", "Y")
          proj4string(xy) <- CRS("+proj=longlat +datum=NAD83")
          res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=NAD83",sep='')))
          results <- merge(zone,as.data.frame(res))
          return(results)
        }

Apply this conversion on loaded data

      utm <- mapply(LongLatToUTM,sdf$LON,sdf$LAT)

      UTM <- data.frame(matrix(unlist(utm), nrow=2225, byrow=T),stringsAsFactors=FALSE)
      UTM[2] <- NULL
      colnames(UTM)[1] = "Zone"
      colnames(UTM)[3] = "Northing"
      colnames(UTM)[2] = "Easting"
      
      sdf<-cbind(sdf,UTM)
      sdf$Easting=sdf$Easting/1000
      sdf$Northing=sdf$Northing/1000

here we need to convert date column

      #converting date
      sdf$DATETIME_ <- as.Date(sdf$DATETIME_,"%m/%d/%Y")
      
      sdf$year_month=paste(as.character(sdf$YEAR_),"/",as.character(sdf$MONTH_),"/1",sep="") #"/",toString(sdf$MONTH_),"/","1")
      sdf$year_month<- as.Date(sdf$year_month,"%Y/%m/%d")

converting magnitude to traffic light system

      sdf$traffic_light<-cut(sdf$MAG, breaks = c(-Inf, 2, 4, Inf), labels=c("Green", "Yellow", "Red"),include.lowest = TRUE)
      
      sdf$ID <- seq.int(nrow(sdf))
      
      write.csv(sdf,"'./csv files/prepared-sdf.csv")

here we are generating UTM base shape files

      leducreefs <- readShapePoly("./shape files/WCSB_ATLAS_1994_LEDUC_REEF_OUTLINE.shp")
      leducreefs.points <- fortify(leducreefs)

      UTM2<-LongLatToUTM(leducreefs.points$long,leducreefs.points$lat)

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

Analysis and Animation
----------------------

      sdf<-read.csv("./csv files/prepared-sdf.csv")

      #Set your API Key
      ggmap::register_google(key = "AIzaSyD5e-QpVDdW5sU21RYFFStK97kT0fnfjO0")
      has_google_key()

    ## [1] TRUE

      sdf$DATETIME_<-as.Date(sdf$DATETIME_)

      sdf$year_month<-as.Date(sdf$year_month)


      #converting magnitude to traffic light system
      sdf$traffic_light_kaush<-cut(sdf$MAG, breaks = c(-Inf, 1,2,3,4, Inf), labels=c("LightGreen","DarkGreen","Yellow", "Orange","Red"), include.lowest = TRUE)



      #Alberta Map

      Alberta <- get_map(location = c(-120.5,48.5,-109.5,60.5), source = 'google', maptype =  "terrain",zoom=6)

    ## Source : http://tile.stamen.com/terrain/6/10/18.png

    ## Source : http://tile.stamen.com/terrain/6/11/18.png

    ## Source : http://tile.stamen.com/terrain/6/12/18.png

    ## Source : http://tile.stamen.com/terrain/6/10/19.png

    ## Source : http://tile.stamen.com/terrain/6/11/19.png

    ## Source : http://tile.stamen.com/terrain/6/12/19.png

    ## Source : http://tile.stamen.com/terrain/6/10/20.png

    ## Source : http://tile.stamen.com/terrain/6/11/20.png

    ## Source : http://tile.stamen.com/terrain/6/12/20.png

    ## Source : http://tile.stamen.com/terrain/6/10/21.png

    ## Source : http://tile.stamen.com/terrain/6/11/21.png

    ## Source : http://tile.stamen.com/terrain/6/12/21.png

    ## Source : http://tile.stamen.com/terrain/6/10/22.png

    ## Source : http://tile.stamen.com/terrain/6/11/22.png

    ## Source : http://tile.stamen.com/terrain/6/12/22.png

      count_by_year<-ggplot(data= sdf, aes(x =DATETIME_,  ..count..,fill=factor(sdf$traffic_light_kaush,levels=c("Red","Orange", "Yellow", "DarkGreen","LightGreen"))))+
      geom_histogram( color="black")+
      xlab("Year") + ylab("Count") +
      scale_color_brewer(palette = "Spectral") +
      scale_x_date(breaks = date_breaks("2 year"),labels = date_format("%Y")) +theme_bw()+
      scale_fill_manual(values = c("LightGreen" = "green","DarkGreen"="darkgreen", "Yellow" = "yellow","Orange" = "orange","Red" = "red"),name = "Traffic Light System:    ")+
      theme(legend.position="none")

      count_by_year

    ## Warning: Use of `sdf$traffic_light_kaush` is discouraged. Use
    ## `traffic_light_kaush` instead.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](README_files/figure-markdown_strict/unnamed-chunk-8-2.png)

      count_by_year_anim<-count_by_year +transition_time(sdf$year_month)+shadow_mark() +labs(title = "Year: {substr(frame_time,1,7)}")
      #count_by_year_anim<-count_by_year +transition_states(sdf$year_month, transition_length = .1, state_length = .1) +labs(title = "Year:     {closest_state}")+shadow_mark()


      #count_by_year_anim

      anim1=animate(count_by_year_anim,nframes=length(unique(sdf$year_month)), fps=4)

    ## Warning: Use of `sdf$traffic_light_kaush` is discouraged. Use
    ## `traffic_light_kaush` instead.

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

      anim1

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.gif)
