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
plot(aoi)

# load annual summer Landsat data (B2, B3, B4, B6)




  # 2000

  # 2005

  # 2010

  # 2015

  # 2020



# image classification: mapping of ice surfaces







# load SRTM DEM data




# terrain analysis




