# violin plot, three eyars, corrected and gage adjusted vals
library(raster)
library(dplyr)
library(tidyverse)

tir2009 <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20090822_mask.tif")
tir2019_cor <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20190826_cor.tif")
tir2021_cor <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20210715_cor.tif")

tir2009_adj <- tir2009 - 0.95
tir2021_adj <- tir2021_cor - 1.15

raster2009_violin <- data.frame(raster::values(tir2009_adj))
names(raster2009_violin) <- "temp_c"
raster2009_violin$year <- 2009

raster2019_violin <- data.frame(raster::values(tir2019_cor))
names(raster2019_violin) <- "temp_c"
raster2019_violin$year <- 2019

raster2021_violin <- data.frame(raster::values(tir2021_adj))
names(raster2021_violin) <- "temp_c"
raster2021_violin$year <- 2021

# drop na cells
raster2009_violin <- raster2009_violin %>% drop_na()
raster2019_violin <- raster2019_violin %>% drop_na()
raster2021_violin <- raster2021_violin %>% drop_na()

#drop highest temps in highest 1% and lowest 1%
quant99_2009 <- quantile(raster2009_violin$temp_c, 0.99)
quant1_2009 <- quantile(raster2009_violin$temp_c, 0.01)

quant99_2019 <- quantile(raster2019_violin$temp_c, 0.99)
quant1_2019 <- quantile(raster2019_violin$temp_c, 0.01)

quant99_2021 <- quantile(raster2021_violin$temp_c, 0.99)
quant1_2021 <- quantile(raster2021_violin$temp_c, 0.01)

raster2009_violin_filt <- raster2009_violin %>% filter(temp_c < quant99_2009 & temp_c > quant1_2009)
raster2019_violin_filt <- raster2019_violin %>% filter(temp_c < quant99_2019 & temp_c > quant1_2019)
raster2021_violin_filt <- raster2021_violin %>% filter(temp_c < quant99_2021 & temp_c > quant1_2021)


mad(raster2009_violin_filt$temp_c, na.rm = TRUE) #0.30
mad(raster2019_violin_filt$temp_c, na.rm = TRUE) #0.85
mad(raster2021_violin_filt$temp_c, na.rm = TRUE) #0.79

# sample taking 10%? violin plot won't work for full dataset
raster2009_sub <- sample_n(raster2009_violin_filt, round(nrow(raster2009_violin_filt)/10))
raster2019_sub <- sample_n(raster2019_violin_filt, round(nrow(raster2019_violin_filt)/10))
raster2021_sub <- sample_n(raster2021_violin_filt, round(nrow(raster2021_violin_filt)/10))

rasterAll_violin <- rbind(raster2009_sub, raster2019_sub, raster2021_sub)

names(rasterAll_violin)

p <- ggplot(rasterAll_violin, aes(x=factor(year), y=temp_c, fill = factor(year))) + 
  geom_violin(trim=FALSE, scale = "width")+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of LWIR Temp by Year\nRaster Cells for All Years",x="Year", y = "LWIR Temp C")

p + scale_fill_brewer(palette="Blues") + theme_classic()+ theme(legend.position = "none")
ggsave(filename = "C:\\Users\\deadf\\Box\\thermal_manuscript2024\\figures\\fig7.png", width = 4.24, height = 3.08, units = "in", 
       device='png', dpi=600)
t.test(raster2019_violin_filt$temp_c, raster2009_violin_filt$temp_c, paired = FALSE)
t.test(raster2019_violin_filt$temp_c, raster2021_violin_filt$temp_c, paired = FALSE)

ks.test(raster2009_violin_filt$temp_c, raster2019_violin_filt$temp_c)
ks.test(raster2021_violin_filt$temp_c, raster2019_violin_filt$temp_c)


summary(raster2009_violin_filt$temp_c)
count(raster2009_violin_filt$temp_c)
range(raster2009_violin_filt$temp_c)
quantile(raster2009_violin_filt$temp_c,.01)
quantile(raster2009_violin_filt$temp_c,.99)
mean(raster2009_violin_filt$temp_c)
sd(raster2009_violin_filt$temp_c)

summary(raster2019_violin_filt$temp_c)
range(raster2019_violin_filt$temp_c)
quantile(raster2019_violin_filt$temp_c,.01)
quantile(raster2019_violin_filt$temp_c,.99)
mean(raster2019_violin_filt$temp_c)
sd(raster2019_violin_filt$temp_c)

summary(raster2021_violin_filt$temp_c)
range(raster2021_violin_filt$temp_c)
quantile(raster2021_violin_filt$temp_c,.01)
quantile(raster2021_violin_filt$temp_c,.99)
mean(raster2021_violin_filt$temp_c)
sd(raster2021_violin_filt$temp_c)
