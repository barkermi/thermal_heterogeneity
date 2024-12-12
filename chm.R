library(raster)
library(lidR)

setwd("E:\\sfmr_08272019")

las19 <- readLAS("point_clouds\\sfmr_ph1_08262019_cloud.las", select = "xyz")
summary(las19)

dsm19 <- raster("sfmr_ph1_08262019_dsm.tif")

# classify ground
las19 <- classify_ground(las19, algorithm = csf())
dtm19 <- rasterize_terrain(las19, res(dsm19)[1], knnidw())
# writeRaster(dtm19, 'dtm19.tif')
dtm19 <- raster('dtm19.tif')

plot(dtm19)
plot(dsm19)

raster(dtm19)
dsm19
dsm19test <- crop(dsm19, dtm19)
extent(dsm19test) <- extent(dtm19)
dsm19test <- mask(dsm19test, dtm19)
plot(dsm19test)
plot(dsm19test)

# plot(dtm, col = gray(1:50/50))
res(dsm19)[1]
dtm19
dsm19
chm19 <- dsm19test - dtm19
plot(chm19)

writeRaster(chm19, 'chm19.tif')
# ortho19 <- stack('sfmr_phase1_08262019.tif')
# extent(ortho19) <- extent(dtm19)
# ortho19 <- mask(ortho19, dtm19)
# ortho19resampled <- projectRaster(ortho19,dtm19,method = 'ngb')
# outstack19 <- stack(ortho19resampled,dsm19test, dtm19)
# Create chm


# canopy_stack19 <- stack()

las21 <- readLAS("point_clouds\\sfmr_phase1_07152021_cloud.las", select = 'xyz')
# classify ground
las21 <- classify_ground(las21, algorithm = csf())
chm19 <- raster("chm19.tif")
# clip_roi(las21, chm19)

dsm21 <- raster("sfmr_phase1_07152021_dsm.tif")
dtm21 <- rasterize_terrain(las21, res(chm19)[1], knnidw())
plot(dtm21)
writeRaster(dtm21, 'dtm21.tif')
dtm21 <- raster('dtm21.tif')


# extent(dsm21test) <- extent(dtm21)
dsm21test <- resample(dsm21,dtm21,method='bilinear')
plot(dsm21test)
# dsm21test <- crop(dsm21, extent(dtm21))
dsm21test <- mask(dsm21test, dtm21)
plot(dtm21)
plot(dsm21test)
dsm21test
dtm21
chm21 <- dsm21test-dtm21
writeRaster(chm21, "chm21.tif")

# make a stack for 2021 ndvi and chm
red21 <- raster("F:\\thermal\\rasters\\sfmr_phase1_07152021_conv.tif", band = 3)
nir21 <- raster("F:\\thermal\\rasters\\sfmr_phase1_07152021_conv.tif", band = 5)
ndvi21 <- (nir21-red21)/(nir21+red21)
ndvi21resample <-  resample(ndvi21,dtm21,method='bilinear')
plot(ndvi21resample)
ndvi21resample <- mask(ndvi21resample, dtm21)
ndvi_chmSTACK21 <- stack(ndvi21resample, chm21)
writeRaster(brick(ndvi_chmSTACK21), "ndvi_chm21_2.tif")

# make a stack for 2019
setwd("E:\\sfmr_08272019")
chm19 <- raster("chm19.tif")
red19 <- raster("E:\\sfmr_08272019\\sfmr_phase1_08262019.tif", band = 3)
nir19 <- raster("E:\\sfmr_08272019\\sfmr_phase1_08262019.tif", band = 5)
ndvi19 <- (nir19-red19)/(nir19+red19)
ndvi19resample <-  resample(ndvi19,chm19,method='bilinear')
plot(ndvi19resample)
ndvi19resample <- mask(ndvi19resample, chm19)
ndvi_chmSTACK19 <- stack(ndvi19resample, chm19)
writeRaster(brick(ndvi_chmSTACK19), "ndvi_chm19.tif")

canopy_stack21 <- stack()

# reclassify function
# rc <- function(x1, x2) {
#   ifelse(x1>0.05 & x2>3, 1, 0)
# }
# 
# r19.class <- raster::overlay(ndvi_chmSTACK19, fun = rc)
library(terra)

terra19 <- terra::rast("E:\\sfmr_08272019\\ndvi_chm19.tif")
r19.class <- terra::ifel(terra19$ndvi_chm19_1 > 0.05 & terra19$ndvi_chm19_2 > 3 , 1, 0)
plot(r19.class)

terra21 <- terra::rast("E:\\sfmr_08272019\\ndvi_chm21_2.tif")
terra21$ndvi_chm21_2_1
r21.class <- terra::ifel(terra21$ndvi_chm21_2_1 > 0.05 & terra21$ndvi_chm21_2_2 > 3 , 1, 0)
plot(r21.class)

terra::writeRaster(r19.class, "E:\\sfmr_08272019\\canopy_cov19.tif")
terra::writeRaster(r21.class, "E:\\sfmr_08272019\\canopy_cov21.tif")
