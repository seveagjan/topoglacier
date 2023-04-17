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









