# comparing pre post fire stream temps
library(raster)

setwd("D:\\sfmr_08272019")
temps <- read.csv("tables\\randPts1921.csv")

# new data
# temps_new <- read.csv("C:\\Users\\barkmatt\\Box\\thermal_projects\\sfork\\tables_figures\\rand_points1921new.csv")

#### violin plot for temps_new ####
temps$temp_21conv <- temps$temp21-1.15

violinDataFire <- temps %>% rename("2019" = temp19, "2021 (Gage-adjusted)" = temp_21conv)%>% pivot_longer(cols=c('2019', '2021 (Gage-adjusted)'),
                                                                                                                              names_to='year',
                                                                                                                              values_to='LWIRTempC')



pFire <- ggplot(violinDataFire, aes(x=factor(year), y=LWIRTempC, fill = year)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of LWIR Temp by Year\nPre vs. Post-Fire",x="Year", y = "LWIR Temp C")

pFire + scale_fill_brewer(palette="Blues") + theme_classic()

# old
names(temps)
plot(temps$temp19, temps$temp21)
hist(temps$temp19, breaks = 15)
hist(temps$temp21, breaks = 15)

#new
# names(temps_new)
# plot(temps_new$TIR08262019Masknew_1, temps_new$TIR07152021Masknew_1)
# hist(temps_new$TIR08262019Masknew_1, breaks = 30)
# hist(temps_new$TIR07152021Masknew_1, breaks = 15)

# old
# non-normal cant do ttest
# shapiro.test(temps$temp19)
# shapiro.test(temps$temp21)

# new
# non-normal cant do ttest
# shapiro.test(temps_new$TIR08262019Masknew_1)
# shapiro.test(temps_new$TIR07152021Masknew_1)

t.test(temps$temp19, temps$temp21-1.15, paired = TRUE)

# old
# do wilcox
wilcox.test(temps$temp19, temps$temp21-1.15, paired = TRUE) #p = .008

# new
# wilcox
# not significant when we adjust for difference in water tepm observed at dam
# wilcox.test(temps_new$TIR08262019Masknew_1, temps_new$TIR07152021Masknew_1-1.15, paired = TRUE) #p = .14

summary(temps$temp19)
summary(temps$temp21-1.15)

boxplot(temps$temp19, temps$temp21)
sd(temps$temp2019)
sd(temps$temp2021-1.15)

median(temps$temp19)
median(temps$temp21-1.15)

# old ks test
# not significant
ks.test(temps$temp19, temps$temp21-1.15, paired = TRUE) # .017


# new ks test
# ks.test(temps_new$TIR08262019Masknew_1, temps_new$TIR07152021Masknew_1-1.15, paired = TRUE) # .01 D =0.074
prePostFireMod <- lm(temps$temp21~temps$temp19)
summary(prePostFireMod)

# MADs
mad(temps$temp19) # 0.72
mad(temps$temp21-1.15) # 0.83

# new MADs
# mad(temps_new$TIR08262019Masknew_1) # 0.74
# mad(temps_new$TIR07152021 -1.15) # 0.87

# updated with temperature corrected data 11/22/2023
# to compare across all wetted cells
tir2019 <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20190826_cor.tif")
tir2021 <- raster("D:\\sfmr_08272019\\raster_products\\corrected_tir_rasters20231119\\tir20210715_cor.tif")

tir2019
tir2021

# tir2019new <- raster("E:\\sfmr_08272019\\raster_products\\TIR2019_maskN.tif")
# NAvalue(tir2019new) <- min(raster::values(tir2019new), na.rm = TRUE) # specify the NA
# 
# tir2021new <- raster("E:\\sfmr_08272019\\raster_products\\TIR2021_maskN.tif")
# NAvalue(tir2021new) <- 0 # specify NA


#old
ks.test(raster::values(tir2019), raster::values(tir2021)-1.15)
sum(!is.na(raster::values(tir2019)))
sum(!is.na(raster::values(tir2021)))

#new
# ks.test(raster::values(tir2019new), raster::values(tir2021new)-1.15)
# 
# tir2021new
# tir2019new

#old
# median absolute deviation
# mad(temps$temp2019) #1.47
# mad(temps$temp2021-1.15) # 1.33

# old
mad(raster::values(tir2019), na.rm = TRUE)
mad(raster::values(tir2021), na.rm = TRUE)

t.test(raster::values(tir2019), raster::values(tir2021)-1.15, paired = FALSE)
# new
# mad(raster::values(tir2019new), na.rm = TRUE) #0.98
# mad(raster::values(tir2021new), na.rm = TRUE) #0.87

# get the at the dam temperatures to correct the ttest above
# central tendency of stream surface resilient to canopy loss associated w

# sd form both years and whether KS was significant