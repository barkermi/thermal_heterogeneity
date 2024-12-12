# apply correction to orthos
library(raster)
# load in orthos with their respective wetted areas
tir2009 <- raster("D:\\sfmr_08272019\\raster_products\\masks20230831\\tir09wetMask.tif")
# tir2009_1 <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20090822_mask.tif") # these are the same rasters

tir20190826 <- raster("D:\\sfmr_08272019\\raster_products\\masks20230831\\tir19wetMaskM.tif")
tir20210715 <- raster("D:\\sfmr_08272019\\raster_products\\masks20230831\\tir21wetMaskM.tif")

tir2009_raw_vals <- raster::values(tir2009)
tir2021_raw_vals <- raster::values(tir20210715)

# range(raster::values(tir20190826), na.rm = TRUE)
tir20190826_reclass <- reclassify(tir20190826, cbind(-Inf, 12, NA), right=FALSE)
tir20190826_reclass_vals <- raster::values(tir20190826_reclass)

ncell(tir2009)-freq(tir2009, value = NA) # count non-NA cells
range(tir2009_raw_vals, na.rm = TRUE)
summary(tir2009_raw_vals, na.rm = TRUE)
quantile(tir2009_raw_vals, 0.01, na.rm = TRUE)
quantile(tir2009_raw_vals, 0.99, na.rm = TRUE)
mean(tir2009_raw_vals, na.rm = TRUE)
sd(tir2009_raw_vals, na.rm = TRUE)
plot(tir2009)

ncell(tir20190826_reclass)-freq(tir20190826_reclass, value = NA)
range(tir20190826_reclass_vals, na.rm = TRUE)
quantile(tir20190826_reclass_vals, 0.01, na.rm = TRUE)
quantile(tir20190826_reclass_vals, 0.99, na.rm = TRUE)
mean(tir20190826_reclass_vals, na.rm = TRUE)
sd(tir20190826_reclass_vals, na.rm = TRUE)
plot(tir20190826_reclass)

plot(tir20210715)
ncell(tir20210715)-freq(tir20210715, value = NA)
range(tir2021_raw_vals, na.rm = TRUE)
quantile(tir2021_raw_vals, 0.01, na.rm = TRUE)
quantile(tir2021_raw_vals, 0.99, na.rm = TRUE)
mean(tir2021_raw_vals, na.rm = TRUE)
sd(tir2021_raw_vals, na.rm = TRUE)
plot(tir20190826_reclass)
summary(raster::values(tir20210715), na.rm = TRUE)

# read in the model creted with the cooler_analysis20231010 script
correction_lm <- readRDS("D:\\thermal_test20231010\\cooler4tests_lm.rda")
correction_lm

# tir20190826_cor <- correction_lm$coefficients[1]+tir20190826_reclass*correction_lm$coefficients[2]
tir20190826_cor <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20190826_cor.tif")
plot(tir20190826_cor)
range(raster::values(tir20190826_cor), na.rm = TRUE)

# tir20210715_cor <- correction_lm$coefficients[1]+tir20210715*correction_lm$coefficients[2]
tir20210715_cor <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20210715_cor.tif")
plot(tir20210715_cor)
range(raster::values(tir20210715_cor), na.rm = TRUE)

correction_lm$coefficients[1]+38.35*correction_lm$coefficients[2]

# write out the corrected masked rasters
# writeRaster(tir20190826_cor, "D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20190826_cor.tif")
# writeRaster(tir20210715_cor, "D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20210715_cor.tif")
