### glacier change analysis ###


### data sets of NPHT

# glaciated areas 2013
# glaciated areas 2022

# elevation
# slope gradient
# slope aspect


### load required packages



### change detection - mapping of areal ice loss

iceloss <- surfaceice2022_mask - surfaceice2013_mask
iceloss[iceloss == 1] <- 0
iceloss[iceloss == -1] <- 1
plot(iceloss)

# quantify ice loss in sqkm

(sum(values(iceloss == 1), na.rm = TRUE)*900) / 1000000

# vectorize areas of ice loss

iceloss[iceloss == 0] <- NA
iceloss_vector <- terra::as.polygons(iceloss, dissolve = TRUE)
plot(iceloss_vector)


### topographic control on ice loss

## clip terrain indices to ice loss mask

# elevation

iceloss_elevation <- terra::trim(mask(npht_dem, iceloss_vector))
plot(iceloss_elevation, col = magma(255))

# slope gradient

iceloss_slope <- terra::trim(mask(npht_slope, iceloss_vector))
plot(iceloss_slope, col = magma(255))


# slope aspect

iceloss_aspect <- terra::trim(mask(npht_aspect, iceloss_vector))
plot(iceloss_aspect, col = rainbow(8))


## extract and visualize spatial statistics of terrain indices for ice loss areas

# zonal statistics

iceloss_elevation <- terra::resample(iceloss_elevation, iceloss_slope)
terrain_metrics <- terra::rast(list(iceloss_elevation, iceloss_slope, iceloss_aspect))

for (i in 1:length(terrain_metrics)) {
  
  ex_mean <- terra::extract(terrain_metrics, iceloss_vector, fun = mean, na.rm = TRUE)
  ex_min <- terra::extract(terrain_metrics, iceloss_vector, fun = min, na.rm = TRUE)
  ex_max <- terra::extract(terrain_metrics, iceloss_vector, fun = max, na.rm = TRUE)
  ex_sd <- terra::extract(terrain_metrics, iceloss_vector, fun = sd, na.rm = TRUE)
  ex_stats <- rbind(ex_mean, ex_min, ex_max, ex_sd)
  df_terrain_iceloss <- data.frame(ex_stats)
  row.names(df_terrain_iceloss) <- c("mean", "min", "max", "sd")
  colnames(df_terrain_iceloss) <- c("id","elevation", "slope", "aspect")
  df_terrain_iceloss <- df_terrain_iceloss[, -1]

}

df_terrain_iceloss


# histogram

hist(iceloss_elevation,
     main = "distribution of elevation (m a.s.l.) across areas of glacier ice loss", breaks = 100, col = "darkgreen")

hist(iceloss_slope,
     main = "distribution of slope gradient (degrees) across areas of glacier ice loss", breaks = 100, col = "darkgreen")

hist(iceloss_aspect,
     main = "distribution of slope aspect (degrees) across areas of glacier ice loss", breaks = 100, col = "darkgreen")


## Logistic Regression Model 











