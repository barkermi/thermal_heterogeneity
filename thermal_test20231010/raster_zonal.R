# zonal statistcs multiple input rasters

library(terra) 
library(icosa)
library(spatstat)
setwd("D:\\thermal_test20231010")
ff <- list.files("photos_20231010", "\\.tif$",full=TRUE)
s <- rast(ff)
terra::plot(s, 1)

# poly <- vect("zone.shp")

cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
polys <- SpatialPolygons(cds1, cds2)
terra::plot(polys)
clickpoly(add = TRUE)

v <- extract(s, clickpoly(add = TRUE), fun = 'mean')

ex <- terra::extract(s, poly, fun='mean', na.rm=TRUE)

terra::crs(poly) <- terra::crs(s)

terra::crs(s) <- terra::crs(poly)
terra::crs(poly)
terra::plot(s, 1)
terra::plot(poly)
#or  ex <- extract(s, poly, fun='mean', na.rm=TRUE, exact=TRUE)

#import required libraries
library(maptools)
library(raster)

#list files (in this case raster TIFFs)
grids <- list.files("photos_20231010", pattern = "*.tif$")

#check the number of files in the raster list (grids)
length <- length(grids)

#read-in the polygon shapefile
library(rgdal)
poly <- readOGR("zone.shp")
terra::plot(poly)
#create a raster stack
paths = paste0("D:\\thermal_test20231010\\photos_20231010\\", grids)
s <- raster::stack(paths)
# paste0("D:\\thermal_test20231010\\photos_20231010\\", grids)
#extract raster cell count (sum) within each polygon area (poly)
for (i in 1:length(grids)){
  ex <- extract(s, poly, fun=mean, na.rm=TRUE, df=TRUE)
}

#write to a data frame
df <- data.frame(ex)

#write to a CSV file
write.csv(df, file = "./path/to/data/CSV.csv")

library(SpatialEpi)
plot(s, 1)
test = clickpoly(add =TRUE)
coord.system <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 "
coord.system <- paste(coord.system, "+ellps=WGS84 +datum=WGS84 +units=m +no_defs", sep = "")

polygon2spatial_polygon(test, coord.system)
crs(s)
?polygon2spatial_polygon
