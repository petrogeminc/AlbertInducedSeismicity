# Alberta Induced Seismicity

`Induced seismicity` refers to typically minor earthquakes and tremors that are caused by human activity that alters the stresses and strains on the Earth's crust. Most induced seismicity is of a low magnitude (**wikipedia**). For more details visit [here](https://ags.aer.ca/activities/induced-seismicity).

This packeage includes the codes and raw data to evaluate Earthquakes in Alberta. Raw data has been gathered from public data available in [AER website](https://www.aer.ca/).

## Data Prepration

here is required libraries for running the codes:

``` r
library(maptools)
library(rgdal)
library(ggplot2)
library(gganimate)
library(ggmap)
library("scales") # for function date_format
library(magick)
``` 

### Loading Alberta Earthquakes Data
```r
  sdf <- read.csv('./csv files/Recent_AB_Earthquakes.csv')
```


