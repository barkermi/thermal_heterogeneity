# tir raster filtering out less than 1st percentile and greater than 99th percentile

library(raster)

tir2009 <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20090822_mask.tif")
tir2019_cor <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20190826_cor.tif")
tir2021_cor <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20210715_cor.tif")

# adjust 2009 and 2021 to the 2019 gage
tir2009_adj <- tir2009 - 0.95
tir2021_cor_adj <- tir2021_cor - 1.15

# adjusted raster are intermediate product, no need to write
# writeRaster(tir2009_adj, )
# writeRaster(tir2021_adj, )

raster2009_vals <- data.frame(raster::values(tir2009_adj)) %>% drop_na()
raster2019_vals <- data.frame(raster::values(tir2019_cor)) %>% drop_na()
raster2021_vals <- data.frame(raster::values(tir2021_cor_adj)) %>% drop_na()
names(raster2009_vals) <- "temp_c"
names(raster2019_vals) <- "temp_c"
names(raster2021_vals) <- "temp_c"

# calculate percentiles and filter
quant99_2009 <- quantile(raster2009_vals$temp_c, 0.99)
quant1_2009 <- quantile(raster2009_vals$temp_c, 0.01)

quant99_2019 <- quantile(raster2019_vals$temp_c, 0.99)
quant1_2019 <- quantile(raster2019_vals$temp_c, 0.01)

quant99_2021 <- quantile(raster2021_vals$temp_c, 0.99)
quant1_2021 <- quantile(raster2021_vals$temp_c, 0.01)

m2009 <- c(-Inf, quant1_2009, NA,  quant99_2009, Inf, NA)
rclmat2009 <- matrix(m2009, ncol=3, byrow=TRUE)
tir2009_adj_filt <- raster::reclassify(tir2009_adj, rclmat2009, right = NA, include.lowest = FALSE)

range(raster::values(tir2009_adj_filt), na.rm = TRUE)
ncell(tir2009_adj_filt)-freq(tir2009_adj_filt, value=NA)
cellStats(tir2009_adj_filt, stat = "mean")

m2019 <- c(-Inf, quant1_2019, NA,  quant99_2019, Inf, NA)
rclmat2019 <- matrix(m2019, ncol=3, byrow=TRUE)
tir2019_cor_filt <- raster::reclassify(tir2019_cor, rclmat2019, right = NA, include.lowest = FALSE)

range(raster::values(tir2019_cor_filt), na.rm = TRUE)
ncell(tir2019_cor_filt)-freq(tir2019_cor_filt, value=NA)
cellStats(tir2019_cor_filt, stat = "mean")
cellStats(tir2019_cor_filt, stat = "sd")


m2021 <- c(-Inf, quant1_2021, NA,  quant99_2021, Inf, NA)
rclmat2021 <- matrix(m2021, ncol=3, byrow=TRUE)
tir2021_cor_adj_filt <- raster::reclassify(tir2021_cor_adj, rclmat2021, right = NA, include.lowest = FALSE)

range(raster::values(tir2021_cor_adj_filt), na.rm = TRUE)
ncell(tir2021_cor_adj_filt)-freq(tir2021_cor_adj_filt, value=NA)
cellStats(tir2021_adj_filt, stat = "mean")
cellStats(tir2021_adj_filt, stat = "sd")

# write out the corrected, adjusted, filtered rasters
writeRaster(tir2009_adj_filt, "D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20240106\\tir20090822_adj_filt.tif")
writeRaster(tir2019_cor_filt, "D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20240106\\tir20190826_cor_filt.tif")
writeRaster(tir2021_cor_adj_filt, "D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20240106\\tir20210715_cor_adj_filt.tif")
