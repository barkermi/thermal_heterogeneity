# orthophotos analysis
library(ggplot2)
library(tidyr)
library(dplyr)
# orthophoto_pts2019 <- read.csv("E:\\sfmr_08272019\\tables\\rand_pts20190827.csv")
orthophoto_pts2019 <- read.csv("D:\\sfmr_08272019\\tables\\points20190826.csv") # data on samsung external


# min, max, stdev, mean
#
# break out mosaic values for later
orthophoto_pts2019 <- rename(orthophoto_pts2019, point_id = ï..OID_)

# ortho_tir <- orthophoto_pts2019 %>% dplyr::select(c(point_id, TIR20190827, TIR20190827_avg, TIR20190827_dis))
ortho_tir <- orthophoto_pts2019 %>% dplyr::select(c(point_id, sfmr_phase1_08262019_mos, sfmr_phase1_08262019_avg, sfmr_phase1_08262019_dis))

# photo_tir <- orthophoto_pts2019 %>% dplyr::select(-c(356:358))
photo_tir <- orthophoto_pts2019 %>% dplyr::select(-c(605:607))

# photo_tir %>% group_by(point_id) %>% summarise(mean = mean())
test <- photo_tir %>% 
  pivot_longer(!point_id, names_to = "photo", values_to = "temp") #%>% 
  # group_by(point_id) %>% summarise(mean = mean(temp, na.rm = TRUE))

  # replace 0s with nan
test["temp"][test["temp"] == 0] <- NA
# install.packages('ggridges')
library(ggridges)
test$point_id <- factor(test$point_id)
# test$temp_cor <- test$temp * 0.7183615 + 7.9884541

ortho_tir$point_id <- factor(ortho_tir$point_id)
test <- left_join(test, ortho_tir, by = "point_id")
# test$diff <- test$TIR20190827 - test$temp
# test_sub <- test[1:50,]
# test_sub <- test[sample(nrow(test), 20), ]
# sample a subset of points using point_id
set.seed(5678)
my_sample <- sample(unique(test$point_id), 15)

# subset below contains points with most image views (ranging from 24 - 19 respectively)
test_sub <- test %>% filter(point_id %in% c('33', '40', '53', '18', '34', '44', '49', '10', '78', '73'))
my_ridgeplot <- ggplot(test_sub, aes(x = temp, y = point_id, fill = stat(x))) +   geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01, jittered_points = FALSE) +
  geom_point(aes(x = sfmr_phase1_08262019_mos, y = point_id), shape = 21, fill = "lightgray",
             color = "black", size = 4) +
  geom_point(aes(x = sfmr_phase1_08262019_dis, y = point_id), shape = 21, fill = "red",
             color = "black", size = 3) +
  geom_point(aes(x = sfmr_phase1_08262019_avg, y = point_id), shape = 21, fill = "yellow",
             color = "black", size = 3)   + 
  theme(legend.position = "none") + 
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
  xlab("Temperature (C)") + 
  ylab("Point ID")


get_only_legend <- function(plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(plot)) 
  
  # Mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box") 
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend) 
}
x <- c(1, 2, 3)
y <- c(1, 2, 3)
group_name <- c("Mosaic", "Disabled", "Average")
df <- data.frame(x,y, group_name)
names(df) <- c("x", "y", "BlendingMode")

legend_plot <- ggplot(df, aes(x = x, y = y, fill = BlendingMode)) + geom_point(shape = 21, size = 3, stroke = 1) +
  scale_fill_manual(values = c(Mosaic = "lightgray", Disabled = "red", Average = "yellow")) +
  theme(legend.direction = "horizontal")

legend <- get_only_legend(legend_plot)

library(grid)
library(gridExtra)
grid.arrange(my_ridgeplot, legend, nrow = 2, heights = c(10,1))
g1 <- ggplotGrob(my_ridgeplot)
p <- arrangeGrob(g1, legend, nrow  = 2)
p
grid.draw(p)
# ggsave("test_fig.jpg", p)
point <- test %>% filter(point_id == "30")
hist(point$temp)
# calculate mean absolute error for the mosaic val - the stacked orthoimage mean
# want to look at mean of the differences of the pairs
point_summary <- left_join(test, ortho_tir, by = "point_id")
point_summary2 <- point_summary %>% group_by(point_id) %>% summarise(mean = mean(temp, na.rm = TRUE), median = median(temp, na.rm = TRUE), 
                                                                     mae_mos = mean(abs(sfmr_phase1_08262019_mos.x-temp), na.rm = TRUE),
                                                                     mae_dis = mean(abs(sfmr_phase1_08262019_dis.x-temp), na.rm = TRUE),
                                                                     mae_avg = mean(abs(sfmr_phase1_08262019_avg.x-temp), na.rm = TRUE)) #pull counts of views on a pixel , count = count(temp, na.rm = TRUE)

# get number of photos containing the sampled points
point_count <- test %>% drop_na(temp) %>% count(point_id)
point_summary <- left_join(point_summary, ortho_tir, by = "point_id")
point_summary <- left_join(point_summary, point_count, by = "point_id")

# mosaic - median, then do mean of those
point_summary$diff_mos <- abs(point_summary$sfmr_phase1_08262019_mos - point_summary$mean)
point_summary$diff_dis <- abs(point_summary$sfmr_phase1_08262019_dis - point_summary$mean)
point_summary$diff_avg <- abs(point_summary$sfmr_phase1_08262019_avg - point_summary$mean)

point_summary$diff_mos_median <- point_summary$sfmr_phase1_08262019_mos - point_summary$median
point_summary$diff_dis_median <- point_summary$sfmr_phase1_08262019_dis - point_summary$median
point_summary$diff_avg_median <- point_summary$sfmr_phase1_08262019_avg - point_summary$median
# report these
mean(point_summary2$mae_mos)
mean(point_summary2$mae_dis)
mean(point_summary2$mae_avg)

mean(point_summary$diff_mos_median)
mean(point_summary$diff_dis_median)
mean(point_summary$diff_avg_median)

# and these, to show mosaicing captures central tendency
sd(point_summary$diff_mos)
sd(point_summary$diff_dis)
sd(point_summary$diff_avg)

hist(point_summary$diff_mos, breaks = 30)

shapiro.test(point_summary$diff_dis)
mean(point_summary$diff_avg)

summary(lm(mean~sfmr_phase1_08262019_mos, data = point_summary))
summary(lm(mean~sfmr_phase1_08262019_avg, data = point_summary))
summary(lm(mean~sfmr_phase1_08262019_dis, data = point_summary))
plot(point_summary$sfmr_phase1_08262019_mos, point_summary$mean)
plot(point_summary$sfmr_phase1_08262019_dis, point_summary$mean)
plot(point_summary$sfmr_phase1_08262019_avg, point_summary$mean)


summary(lm(median~sfmr_phase1_08262019_mos, data = point_summary))
summary(lm(median~sfmr_phase1_08262019_avg, data = point_summary))
summary(lm(median~sfmr_phase1_08262019_dis, data = point_summary))

p <- ggplot(point_summary, aes(x=factor(point_id), y=mean, fill = point_id)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of LWIR Temp by Measurement",x="Measuremnet", y = "Temperature C")

p + scale_fill_brewer(palette="Blues") + theme_classic()


ggplot(test, aes(x = temp_cor, y = point_id, fill = stat(x))) +   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, jittered_points = TRUE) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C")
ggplot(test, aes(x = diff, y = point_id, fill = stat(x), alpha = .5)) +   geom_density_ridges_gradient(scale = 4, rel_min_height = 0.01, jittered_points = TRUE, alpha = 1) +
  scale_fill_viridis_c(name = "Temp. [F]", option = "C")
names(test)
hist(test$diff)

str(test)
str(ortho_tir)
