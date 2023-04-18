### glacier change analysis ###


### data sets of NPHT

# glaciated areas 2013
# glaciated areas 2022

# elevation
# slope gradient
# slope aspect


### load required packages

library(readr)
library(caret)
library(ggplot2)

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

iceloss_elevation <- terra::resample(iceloss_elevation, iceloss_slope) # adjust extent of elevation raster
terrain_metrics <- terra::rast(list(iceloss_elevation, iceloss_slope, iceloss_aspect)) # create stack of terrain metrics

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

} # extract statistics for terrain metrics within ice loss areas

df_terrain_iceloss


# histogram

hist(iceloss_elevation,
     main = "distribution of elevation (m a.s.l.) across areas of glacier ice loss", breaks = 100, col = "darkgreen")

hist(iceloss_slope,
     main = "distribution of slope gradient (degrees) across areas of glacier ice loss", breaks = 100, col = "darkgreen")

hist(iceloss_aspect,
     main = "distribution of slope aspect (degrees) across areas of glacier ice loss", breaks = 100, col = "darkgreen")




### Logistic Regression Model 

## create grid of random sample points

creategrid <- function(cellsize) {
  
  # the grid is created by producing a raster layer that matches the extent of the AOI. The resolution of 
  # the raster layer determines the cell size of the grid and can be set with the function.
  # the glaciated area from 2013 is used as an extent for the grid, so that both ice loss and persistent ice areas are sampled
  
  r <- raster(ncol = 3339, nrow = 1428, xmn = 278925, xmx = 379095, ymn = 5194005, ymx = 5236845) # AOI extent
  res(r) <- cellsize
  crs(r) <- crs(iceloss_vector)
  values(r) <- 1:ncell(r)
  r_terra <- terra::rast(r)
  surfaceice2013_mask[surfaceice2013_mask == 0] <- NA 
  icemask2013_vec <- terra::as.polygons(surfaceice2013_mask, dissolve = TRUE)
  r_masked <- crop(r_terra, icemask2013_vec, mask = T)
  grid_polys <- terra::as.polygons(r_masked, dissolve = FALSE)
  grid_polys_sf <- st_as_sf(grid_polys)
  centroids <- st_centroid(grid_polys_sf)
  
}

grid <- creategrid(500) # apply the grid function by setting cell size

## extract topographic parameters (explanatory variable) and binary ice cover loss value (response variable)

iceloss[is.na(iceloss[])] <- 0 

sampling <- function(x) {
  extract(x, grid, method = "simple", buffer = NULL)
} # function to extract raster values for grid points

sample_iceloss <- sampling(iceloss) # sample response variable
sample_elevation <- sampling(npht_dem) # sample explanatory variables
sample_slope <- sampling(npht_slope)
sample_aspect <- sampling(npht_aspect)

samples <- cbind(sample_iceloss,
                 sample_elevation,
                 sample_slope,
                 sample_aspect)
samples <- samples[, -c(3,5,7)]
colnames(samples) <- c("id","ice_loss", "elevation", "slope", "aspect")
samples <- na.omit(samples) # final samples for logistic regression model


## perform logistic regression model

# create plot that illustrates the relationship between response and explanatory variables

plot <- ggplot(data = samples, aes(x = elevation, y = slope, col = as.factor(ice_loss))) +
  geom_point(size = 1) +
  xlab("elevation (m a.s.l.") + ylab("slope (degrees)") +
  scale_color_discrete(name = "ice loss")
plot

# create a training and testing dataset out of the samples

inTrain <- createDataPartition(y = samples$ice_loss, p = .60, list = FALSE)
training <- samples[inTrain,]
testing <- samples[-inTrain,]

dim(training)
dim(testing)

# Fit the logistic regression model with the training data

samples.fit = glm(ice_loss ~ elevation + slope + aspect, data=training, family=binomial)
summary(samples.fit) # gives overview of model fit (predictor significance: high z-value, low p-value)

# test model performance against testing data

samples.prob = predict(samples.fit, testing, type="response")
samples.pred = rep("0", dim(training)[1])
samples.pred[samples.prob > .5] = "1"
table(samples.pred, training$ice_loss) # confusion matrix

mean(samples.pred == training$ice_loss) # prediction success rate
1 - mean(samples.pred == training$ice_loss) # error rate

# apply model to new data

predict(samples.fit, newdata=data.frame(
  elevation = c(3492, 2940), slope = c(24, 7), aspect = c(244, 180)), type="response")


samples.fit = glm(ice_loss ~ elevation, data=training, family=binomial) 
summary(samples.fit) # model fit

# compute confusion matrix of new model, should be similar to original model

samples.prob = predict(samples.fit, testing, type="response")
samples.pred = rep("0", dim(training)[1])
samples.pred[samples.prob > .5] = "1"
table(samples.pred, training$ice_loss)

mean(samples.pred == training$ice_loss) # success rate

# apply new model with only one predictor shows whether the others disturb model performance depending on deviation between different models 

predict(samples.fit, newdata=data.frame(elevation=c(3492, 2940)), type="response")




### Examine ice loss for individual glaciers and determine the potential role of glacier size

## calculate individual glacier area loss

rgi_npht_glacier <- rgi_npht_glacier[, -c(3,4,5,8,9,11,12,13,14)] # exclude redundant columns

for (i in 1:length(rgi_npht_glacier)) {
  iceloss_sum <- terra::extract(iceloss, rgi_npht_glacier, fun = sum, na.rm = TRUE)
} # extract number of pixels within RGI glaciers that show ice loss between 2013 and 2022

iceloss_sum <- as.data.frame(iceloss_sum)
iceloss_sum$arealoss <- (iceloss_sum$binary_icecover * 900) / 1000000 # convert number of pixels to area

rgi_glacier_iceloss <- cbind(rgi_npht_glacier, iceloss_sum$arealoss) # add ice loss to RGI spatial polygon data
rgi_glacier_iceloss$lossperc <- rgi_glacier_iceloss$iceloss_sum.arealoss / rgi_glacier_iceloss$AREA

# plot spatial polygons colored by area loss

rgi_glacier_iceloss_sf <- st_as_sf(rgi_glacier_iceloss)
plot(rgi_glacier_iceloss_sf[8])

# plot ice loss against glacier size

plot2 <- ggplot(rgi_glacier_iceloss, aes(x = AREA, y = lossperc)) +
  geom_point(size = 1, col = "lightblue") +
  labs(title = "Relationship between glacier ice loss (2013-2022) and glacier size (2003)",
       subtitle = "Nationalpark Hohe Tauern, Austria",
       x="glacier size (sqkm)",
       y="ice loss (sqkm) ") + 
  theme(axis.title.x = element_text(margin = margin(t = 20))) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  xlim(0,8) + ylim(0,1) + 
  geom_smooth(method = 'lm', formula = y~x, col = "black", linewidth = 0.1)
plot2

# calculate rsq for glacier size and ice loss

rsq <- function (x, y) cor(x, y) ^ 2 # function with rsq formula
print(paste0("R2 glacier size, ice loss: ", rsq(rgi_glacier_iceloss$AREA, rgi_glacier_iceloss$lossperc)))

