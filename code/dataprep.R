### data preparation ###

### load required packages
library(sf)
library(raster)
library(terra)
library(viridis)


### plot AOI outlines: Hohe Tauern Nationalpark (NPHT)

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data")
aoi <- st_read("npht_agrenze_wgs84.shp")
plot(aoi[1])

### load summer Landsat data from 2013 and 2022 as multi-band images (B2, B3, B4, B6)

  # 2013

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data/landsat/2013")
data_list2013 <- list.files(pattern = '.TIF', all.files=FALSE, full.names=TRUE)
lc08_2013 <- terra::rast(data_list2013)

  # 2022

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data/landsat/2022")
data_list2022 <- list.files(pattern = '.TIF', all.files=FALSE, full.names=TRUE)
lc08_2022 <- terra::rast(data_list2022)

# clip satellite images to AOI extent

# change crs of AOI
aoi <- st_transform(aoi, crs = crs(lc08_2013))

l8_2013_aoi <- terra::trim(mask(lc08_2013, aoi))
plotRGB(l8_2013_aoi, r=3, g=2, b=1, scale= 50000)

l8_2022_aoi <- terra::trim(mask(lc08_2022, aoi))
plotRGB(l8_2022_aoi, r=3, g=2, b=1, scale= 50000)


### mapping of ice surfaces

# calculate the Normalized Difference Snow Index (NDSI)

landsat_ndsi <- function(x,y) {(x-y)/(x+y)}

l8_2013_aoi_ndsi <- landsat_ndsi(l8_2013_aoi$LC08_L2SP_192027_20130803_20200912_02_T1_SR_B3,
                                 l8_2013_aoi$LC08_L2SP_192027_20130803_20200912_02_T1_SR_B6)
names(l8_2013_aoi_ndsi) <- "NDSI"
l8_2022_aoi_ndsi <- landsat_ndsi(l8_2022_aoi$LC09_L2SP_192027_20220719_20230407_02_T1_SR_B3,
                                 l8_2022_aoi$LC09_L2SP_192027_20220719_20230407_02_T1_SR_B6)
names(l8_2022_aoi_ndsi) <- "NDSI"
plot(l8_2013_aoi_ndsi)
plot(l8_2022_aoi_ndsi)

## extract ice surfaces by applying a NDSI threshold to each image

# define NDSI threshold by calculating average NDSI within regional RGI glacier outlines

setwd("/Volumes/satdata/uni_wuerzburg/Programming/topoglacier_data")
rgi_npht_glacier <- st_read("rgi_npht.shp")
rgi_npht_glacier <- st_transform(rgi_npht_glacier, crs = crs(lc08_2013))

npht_glacier_ndsi2013 <- terra::extract(l8_2013_aoi_ndsi, rgi_npht_glacier, fun = mean, na.rm = TRUE, df = TRUE)
print(paste0("Average NDSI for RGI outlines: ", mean(npht_glacier_ndsi2013$NDSI))) # threshold of 0.4 is chosen, comparable to literature (see https://doi.org/10.5194/tc-12-1629-2018)

ndsithreshold <- function(x) {
  m <-  c(-1, 0.4, 0,
          0.4, 1, 1)
  
  rmat <- matrix(m, ncol=3, byrow=TRUE)
  
  icesurface_mask <- terra::classify(x, rmat)
}

surfaceice2013_mask <- ndsithreshold(l8_2013_aoi_ndsi)
names(surfaceice2013_mask) <- "binary_icecover"
plot(surfaceice2013_mask)

surfaceice2022_mask <- ndsithreshold(l8_2022_aoi_ndsi)
names(surfaceice2022_mask) <- "binary_icecover"
plot(surfaceice2022_mask)

## validate surface ice mask with RGI data on glacier extent

# validation area value

print(paste0("RGI glacier extent: ", sum(rgi_npht_glacier$AREA))) # unit = sqkm

# NDSI-based area value

print(paste0("NDSI-based glacier extent: ", (sum(values(surfaceice2013_mask$binary_icecover == 1), na.rm = TRUE)*900) / 1000000))


### terrain analysis

#load 10m resolution DEM data for NPHT (Digitales Geländemodell Österreich)

npht_dem <- terra::rast("dem10.tif")
plot(npht_dem, col = terrain.colors(255))

# slope gradient

npht_slope <- terrain(npht_dem, v = "slope", unit = "degrees")
plot(npht_slope, col = viridis(255))

# slope aspect

npht_aspect <- terrain(npht_dem, v = "aspect", unit = "degrees")
plot(npht_aspect, col = rainbow(8))







