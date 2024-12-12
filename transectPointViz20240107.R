# temp profile plots
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd("D:\\sfmr_08272019")

points <- read.csv("tables\\transect_vals20240107.csv")
# points$DIST_UPSTREAM_M <- rev(points$DIST_M)

# points2009 <- read.csv("tables\\transect2009.csv")
# 
# points2009$DIST_M <- points2009$ORIG_FID*2
# points2009$DIST_UPSTREAM_M <- rev(points2009$DIST_M)
# p2009 <- ggplot(points2009, aes(x = DIST_M, y = MEAN))
# p2009 + geom_smooth(aes(size = 1))

# rename mean to reflect all points along transect
# points2009 <- rename(points2009, MEAN09All = MEAN)
# 
# 
# plot(points2009$DIST_UPSTREAM_M, points2009$MEAN09All)
# 
# plot(points$DIST_M, points$MEAN09All)
library(ggplot2)
# p <- ggplot(mtcars, aes(wt, mpg))
# p +geom_point(size = 4)

# pivot longer
# points_new <- points %>% dplyr::select(ORIG_FID, MEAN09, MEAN19, MEAN21) %>% 
#   pivot_longer(!ORIG_FID, names_to = "YEAR", values_to = "MEAN")
# points_new$DIST_M <- points_new$ORIG_FID*2
# points_new$DIST_UPSTREAM_M <- rev(points_new$DIST_M)
# 
# points2009_new <- points2009 %>% dplyr::select(ORIG_FID, MEAN09All) %>% 
#   pivot_longer(!ORIG_FID, names_to = "YEAR", values_to = "MEAN")
# points2009_new$DIST_M <- points2009_new$ORIG_FID*2
# points2009_new$DIST_UPSTREAM_M <- rev(points2009_new$DIST_M)
# 
# # center and scale to 2021 (these vals come from scaleThermal code)
# points2009_new$MEAN <- (points2009_new$MEAN - mean09)/scaleFactor
# 
# new_df <- rbind(points2009_new, points_new)
# 
# # adjust the temps
# points$temp[points$year == 2009] <- points$temp[points$year == 2009] - 0.95
# points$temp[points$year == 2021] <- points$temp[points$year == 2021] - 1.15

p <- ggplot(points, aes(x = dist_up, y = tir_cor_adj_filt, color = factor(year)))
p+geom_point()
p + geom_smooth(aes(colour = factor(year)), method = 'loess', size = 1) + 
  guides(color=guide_legend(title="Year")) + 
  xlab("Distance Upstream (m)") + 
  ylab("Temperature from Corrected/Adjusted TIR (C)")
ggsave(filename = "C:\\Users\\deadf\\Box\\thermal_manuscript2024\\figures\\fig9.png", width = 5.44, height = 3.96, units = "in", 
       device='png', dpi=600)

# statistical analysis
points2019 <- points %>% filter(year == 2019)
points2009 <- points %>% filter(year == 2009)
points2021 <- points %>% filter(year == 2021)

# t tests
t.test(points2019$tir_cor_adj_filt, points2009$tir_cor_adj_filt, paired = TRUE)
t.test(points2021$tir_cor_adj_filt, points2009$tir_cor_adj_filt, paired = TRUE)

t.test(points2019$tir_cor_adj_filt, points2021$tir_cor_adj_filt, paired = TRUE)

ks.test(points2019$tir_cor_adj_filt, points2009$tir_cor_adj_filt, paired = TRUE)
ks.test(points2021$tir_cor_adj_filt, points2009$tir_cor_adj_filt, paired = TRUE)

ks.test(points2019$tir_cor_adj_filt, points2021$tir_cor_adj_filt, paired = TRUE)
p+geom_point(aes(colour = factor(year)))

p+geom_line(aes(colour = factor(year)))


plot(points,main='Simple Moving Average (SMA)',ylab='Temperature C')
lines(rollmean(points,5),col='blue')
lines(rollmean(points,40),col='red')

data <- AirPassengers

ggplot(data=points_new, mapping=aes(x=DIST_M, y=MEAN, colour=YEAR)) +
  geom_point() + geom_smooth(aes(group=1))

zoo::rollapplyr(points$temp, width = 10L, FUN = median)
points$temp

## rolling mean
# z <- zoo(11:15, as.Date(31:35))
# rollapply(z, 2, mean)

points %>% filter(year == 2019) %>% arrange(dist_origin) %>% 
  group_by(group10 = rep(row_number(), each=10, length.out = n())) %>%
  summarise(median_temp = median(temp, na.rm = TRUE), min_dist = min(dist_origin, na.rm = TRUE)) %>%
  ggplot(aes(x = min_dist, y = median_temp)) + geom_line() + ggtitle("2019")

points %>% filter(year == 2009) %>% arrange(dist_origin) %>% 
  group_by(group10 = rep(row_number(), each=10, length.out = n())) %>%
  summarise(median_temp = median(temp, na.rm = TRUE), min_dist = min(dist_origin, na.rm = TRUE)) %>%
  ggplot(aes(x = min_dist, y = median_temp)) + geom_line() + ggtitle("2009")

points %>% filter(year == 2021) %>% arrange(dist_origin) %>% 
  group_by(group10 = rep(row_number(), each=10, length.out = n())) %>%
  summarise(median_temp = median(temp, na.rm = TRUE), min_dist = min(dist_origin, na.rm = TRUE)) %>%
  ggplot(aes(x = min_dist, y = median_temp)) + geom_line() + ggtitle("2021")

points2019 <- points %>% filter(year == 2019)


mod10