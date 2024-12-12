# thermal raster analysis

library(raster)
# tir2009 <- raster('E:\\sfmr_08272019\\raster_products\\TIR2009_mask.tif')/10
# tir2009 <- raster("E:\\sfmr_08272019\\raster_products\\masks20230831\\tir09intMask.tif")
# tir2009newnew <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20090822_mask.tif")
tir2009_adj_filt <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20240106\\tir20090822_adj_filt.tif")
min(raster::values(tir2009_adj_filt), na.rm = TRUE)

# tir2019new <- raster("E:\\sfmr_08272019\\raster_products\\TIR2019_maskN.tif")
# tir2019new <- raster("E:\\sfmr_08272019\\raster_products\\masks20230831\\tir19intMask.tif")
# tir2019newnew <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20190826_cor.tif")
tir2019_cor_filt <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20240106\\tir20190826_cor_filt.tif")
min(raster::values(tir2019_cor_filt), na.rm = TRUE)
# NAvalue(tir2019new) <- min(raster::values(tir2019new), na.rm = TRUE) # specify the NA

# tir2021new <- raster("E:\\sfmr_08272019\\raster_products\\TIR2021_maskN.tif")
# tir2021new <- raster("E:\\sfmr_08272019\\raster_products\\masks20230831\\tir21intMask.tif")
# tir2021newnew <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20210715_cor.tif")
tir2021_cor_adj_filt <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20240106\\tir20210715_cor_adj_filt.tif")
min(raster::values(tir2021_cor_adj_filt), na.rm = TRUE)
# NAvalue(tir2021new) <- 0 # specify NA



sd(raster::values(tir2009_adj_filt), na.rm = TRUE) #0.23 min
sd(raster::values(tir2019_cor_filt), na.rm = TRUE) #1.28, max
sd(raster::values(tir2021_cor_adj_filt), na.rm = TRUE) #0.80

# can only run below when I have intersected wetted areas and matching res, revisit
# t.test(raster::values(tir2009), raster::values(tir2019new), na.rm = TRUE)

scaleFactor <- sd(raster::values(tir2019_cor_filt), na.rm = TRUE) # 1.28

hist(raster::values(tir2009_adj_filt), breaks = 32)
hist(raster::values(tir2019_cor_filt), breaks = 32)
hist(raster::values(tir2021_cor_adj_filt), breaks = 32)


# center the rasters 0s relative to their own mean (subtract mean from vals)
mean09 <- mean(raster::values(tir2009_adj_filt), na.rm = TRUE)
mean19 <- mean(raster::values(tir2019_cor_filt), na.rm = TRUE)
mean21 <- mean(raster::values(tir2021_cor_adj_filt), na.rm = TRUE)

tir2009cent <- tir2009_adj_filt-mean09
tir2019cent <- tir2019_cor_filt-mean19
tir2021cent <- tir2021_cor_adj_filt - mean21

library(R.utils)
isZero(mean(raster::values(tir2009cent), na.rm=TRUE))
isZero(mean(raster::values(tir2019cent), na.rm=TRUE))
isZero(mean(raster::values(tir2021cent), na.rm=TRUE))

# then scale (divide) them relative to the scene with sd from most varying scene (2019)
tir2009scaled <- tir2009cent/scaleFactor
min(raster::values(tir2009scaled), na.rm = TRUE)
tir2019scaled <- tir2019cent/scaleFactor
min(raster::values(tir2019scaled), na.rm = TRUE)
tir2021scaled <- tir2021cent/scaleFactor
min(raster::values(tir2021scaled), na.rm = TRUE)

hist(raster::values(tir2009scaled),breaks = 16)
hist(raster::values(tir2019scaled),breaks = 16)
hist(raster::values(tir2021scaled),breaks = 16)

#write rasters

NAvalue(tir2009_adj_filt)
NAvalue(tir2019_cor_filt)
NAvalue(tir2021_cor_adj_filt)

writeRaster(tir2009scaled, filename = "D:\\sfmr_08272019\\raster_products\\centeredScaled20240106\\tir09cs.tif")
writeRaster(tir2019scaled, filename = "D:\\sfmr_08272019\\raster_products\\centeredScaled20240106\\tir19cs.tif")
writeRaster(tir2021scaled, filename = "D:\\sfmr_08272019\\raster_products\\centeredScaled20240106\\tir21cs.tif")

# center and scale rasters for respective wetted areas
tir2009wet <- raster("E:\\sfmr_08272019\\raster_products\\masks20230831\\tir09wetMask.tif")
tir2019wet <- raster("E:\\sfmr_08272019\\raster_products\\masks20230831\\tir19wetMaskM.tif") # M denotes we removed southern part of site that was captured on different day in 2019
tir2021wet <- raster("E:\\sfmr_08272019\\raster_products\\masks20230831\\tir21wetMaskM.tif")

# replace thermal < 12 w NA for 2019
plot(tir2019wet)
tir2019wet <- reclassify(tir2019wet, cbind(-Inf, 12, NA), right=FALSE)
plot(tir2019wet)

# center
tir2009wet_cent <- tir2009wet-mean(raster::values(tir2009wet), na.rm = TRUE)
tir2019wet_cent <- tir2019wet-mean(raster::values(tir2019wet), na.rm = TRUE)
tir2021wet_cent <- tir2021wet-mean(raster::values(tir2021wet), na.rm = TRUE)

# scalefactor is 1.845, highest sd from 2019
scaleFactor_wet <- max(cbind(sd(raster::values(tir2009wet), na.rm = TRUE),
                             sd(raster::values(tir2019wet), na.rm = TRUE),
                             sd(raster::values(tir2021wet), na.rm = TRUE)))

tir2009wet_scaled <- tir2009wet_cent/scaleFactor_wet
tir2019wet_scaled <- tir2019wet_cent/scaleFactor_wet
tir2021wet_scaled <- tir2021wet_cent/scaleFactor_wet

plot(tir2009wet_scaled)
plot(tir2019wet_scaled)
plot(tir2021wet_scaled)

writeRaster(tir2009wet_scaled, filename = "E:\\sfmr_08272019\\raster_products\\centeredScaled\\tir09cs_wet.tif")
writeRaster(tir2019wet_scaled, filename = "E:\\sfmr_08272019\\raster_products\\centeredScaled\\tir19cs_wet.tif")
writeRaster(tir2021wet_scaled, filename = "E:\\sfmr_08272019\\raster_products\\centeredScaled\\tir21cs_wet.tif")
