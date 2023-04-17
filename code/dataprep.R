### data preparation ###

# load required libraries
library(sf)
library(raster)
library(terra)
library(rgdal)
library(gdalUtils)





# plot AOI outlines: Hohe Tauern Nationalpark

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data")
aoi <- st_read("npht_agrenze_wgs84.shp")
plot(aoi[1])

# load summer Landsat data from 2013 and 2022 as multi-band images (B2, B3, B4, B6)

  # 2001

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data/landsat/2013")
data_list2013 <- list.files(pattern = '.TIF', all.files=FALSE, full.names=TRUE)
lc08_2013 <- terra::rast(data_list2013)
plotRGB(lc08_2013, r=3, g=2, b=1, scale= 40000)

  # 2022

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data/landsat/2022")
data_list2022 <- list.files(pattern = '.TIF', all.files=FALSE, full.names=TRUE)
lc08_2022 <- terra::rast(data_list2022)
plotRGB(lc08_2022, r=3, g=2, b=1, scale= 50000)

# image classification: mapping of ice surfaces







# load SRTM DEM data




# terrain analysis




