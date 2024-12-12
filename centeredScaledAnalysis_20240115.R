# calcluate area for each deviaiton class
library(raster)
library(dplyer)
library(viridis)

cs_rast09 <- raster('D:\\sfmr_08272019\\raster_products\\centeredScaled20240106\\tir09cs.tif')
cs_rast19 <- raster('D:\\sfmr_08272019\\raster_products\\centeredScaled20240106\\tir19cs.tif')
cs_rast21 <- raster('D:\\sfmr_08272019\\raster_products\\centeredScaled20240106\\tir21cs.tif')

m <- c(-2,-1,-1, -1,0,0,  0,1,1,  1,2,2,  2,3,3,  3,4,4,  4,5,5,  5,6,6)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

cs_rast09_rc <- reclassify(cs_rast09, rclmat, right = NA, include.lowest = FALSE)
cs_rast19_rc <- reclassify(cs_rast19, rclmat, right = NA, include.lowest = FALSE)
cs_rast21_rc <- reclassify(cs_rast21, rclmat, right = NA, include.lowest = FALSE)


# for values >= 0 (instead of > 0), do
# cs_rast09_rc <- reclassify(cs_rast09_rc, rclmat, include.lowest=TRUE)

names(cs_rast09_rc) <- "tir_mad"
names(cs_rast19_rc) <- "tir_mad"
names(cs_rast21_rc) <- "tir_mad"

result09 <- as.data.frame(cs_rast09_rc) %>%
  group_by(tir_mad) %>% 
  summarise(pixels = n()) %>% 
  mutate(`area m^2` = pixels * 0.6 * 0.6, #multiply by pixel area
         `area km^2` = round(`area m^2`/1e6,2)) 
result09
result09_noNA <- result09 %>% filter(row_number() <= n()-1) 
result09_fin <- result09_noNA%>% mutate(pct_area = `area m^2`/sum(`area m^2`)*100) %>% mutate(condition = "Pre-Restoration")

result19 <- as.data.frame(cs_rast19_rc) %>%
  group_by(tir_mad) %>% 
  summarise(pixels = n()) %>% 
  mutate(`area m^2` = pixels * 0.0611606 * 0.0611606, #multiply by pixel area
         `area km^2` = round(`area m^2`/1e6,2)) 
result19
result19_noNA <- result19 %>% filter(row_number() <= n()-1) 
result19_fin <- result19_noNA%>% mutate(pct_area = `area m^2`/sum(`area m^2`)*100) %>% mutate(condition = "Post-Restoration")


result21 <- as.data.frame(cs_rast21_rc) %>%
  group_by(tir_mad) %>% 
  summarise(pixels = n()) %>% 
  mutate(`area m^2` = pixels * 0.071808 * 0.071808, #multiply by pixel area
         `area km^2` = round(`area m^2`/1e6,2)) 
result21
result21_noNA <- result21 %>% filter(row_number() <= n()-1) 
result21_fin <- result21_noNA%>% mutate(pct_area = `area m^2`/sum(`area m^2`)*100) %>% mutate(condition = "Post-Fire")

cs_results <- bind_rows(result09_fin, result19_fin, result21_fin, )
cs_results
ggplot(cs_results, aes(fill=condition, y=pct_area, x=tir_mad)) + 
  geom_bar(position = 'stack', stat = 'count') +
  scale_fill_viridis(discrete = T) + 
  xlab("Deviation") +
  ylab("Percent Area")
?geom_bar

ggplot( cs_results, aes( x = tir_mad, y = pct_area, fill = condition ) ) +
  geom_col( ) +
  scale_fill_viridis(discrete = T) +
  xlab( "MAD" ) + 
  ylab( "Percent Area" ) +
  labs( title = "Proportion of Cell Types" )

ggplot(cs_results, aes(x = tir_mad, y = pct_area)) +
  geom_col(aes(fill = condition), position = position_fill(reverse = TRUE)) +
  theme(legend.position = "top")

library(viridisLite)
ggplot(cs_results, aes(x=factor(tir_mad), y=pct_area, fill=factor(condition))) +
  geom_bar(stat="identity", position = position_dodge(width = .5)) +
  xlab("MAD Temperature Group") +
  ylab("Percent Area") + 
  guides(fill=guide_legend(title="Condition")) +
  # scale_fill_viridis(option = "E", discrete = T) +
  scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  theme(text = element_text(family = "Times New Roman")) +
  theme_bw()
