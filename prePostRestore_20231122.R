# comparing pre post restoration temps

setwd("D:\\sfmr_08272019")
# prePost1 <- read.csv("tables\\temps_2009_2019.csv")
prePost1 <- read.csv("tables\\randPts0919.csv")

# prePost <- read.csv("C:\\Users\\barkmatt\\Box\\thermal_projects\\sfork\\tables_figures\\rand_points0919new.csv")
names(prePost1)

#old data
summary(prePost1$temp09)
summary(prePost1$temp19)

# new
# summary(prePost$TIR2009)
# summary(prePost$TIR08262019Masknew_1)

#old
sd(prePost1$temp09) #.24
sd(prePost1$temp19) #.61
# prePost1$temp09c[1:50]

#new
# sd(prePost$TIR2009)
# sd(prePost$TIR08262019Masknew_1)
# prePost$TIR2009[1:50]

#old
# ks.test(prePost1$temp09, prePost1$temp19+0.95, paired = TRUE)
# boxplot(prePost1$temp09, prePost1$temp19)
# t.test(prePost1$temp09c, prePost1$temp19, paired = TRUE)

#new
ks.test(prePost1$temp19, prePost1$temp09-0.95, paired = TRUE) # p < 0.001, D = .99
boxplot(prePost1$temp09-0.95, prePost1$temp19)

# ks.test(prePost$TIR2009, prePost$TIR08262019Masknew_1, paired = TRUE) # p < 0.001, D = 0.643

# figure in paper
boxplot(prePost1$temp09-0.95, prePost1$temp19, prePost1$temp19, names = c("August 2009", "August 2019", "August 2019\nCorrected"),ylab = "LWIR Measured Temperature (C)", xlab = "Flight")
t.test(prePost1$temp09, prePost1$temp19 + 0.95, paired = TRUE)

#### violin plot
# wide to long
library(tidyr)
prePost1$temp_09conv <- prePost1$temp09-0.95
violinData <- prePost1 %>% rename("2009" = temp09, "2019" = temp19, "2009 (Gage-adjusted)" = temp_09conv)%>% pivot_longer(cols=c('2009 (Gage-adjusted)', '2019'),
                    names_to='year',
                    values_to='LWIRTempC')



p <- ggplot(violinData, aes(x=factor(year), y=LWIRTempC, fill = year)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of LWIR Temp by Year\nPre vs. Post-Restoration",x="Year", y = "LWIR Temp C")

p + scale_fill_brewer(palette="Blues") + theme_classic()+ theme(legend.position = "none")

p<-ggplot(violinData, aes(x=year, y=LWIRTempC, fill=year)) +
  geom_violin(trim=FALSE)


p+ stat_summary(fun.y=mean, geom="point", shape=23, size=2)

#old
hist(log10(prePost1$temp09c), breaks = 15)
hist(log10(prePost1$temp19), breaks = 15)

#new
hist(log10(prePost1$temp09), breaks = 15)
hist(log10(prePost1$temp19+0.95), breaks = 15)
?hist

#old
shapiro.test(prePost1$temp09) # reject null, not normal
shapiro.test(prePost1$temp19)
#new
# shapiro.test(prePost$TIR2009) # reject null, not normal
# shapiro.test(prePost$TIR08262019Masknew_1) # reject null, not normal

#old
# do non parametric, violating normailty (wilcox)
wilcox.test(prePost1$temp09-0.95, prePost1$temp19, paired = TRUE) # p<0.001, V 199188
t.test(prePost1$temp09-0.95, prePost1$temp19, paired = TRUE) # p<0.001, est diff -2.15

?wilcox.test

#new
# wilcox.test(prePost$TIR2009, prePost$TIR08262019Masknew_1, paired = TRUE) # p<0.001, V 431998
# wilcox.test(prePost$TIR2009, prePost$TIR08262019Masknew_1 + 0.95, paired = TRUE) # p<0.001, V 124937


# old
mad(prePost1$temp09-0.95, na.rm = TRUE) # 0.22
mad(prePost1$temp19, na.rm = TRUE) #0.57

mean(prePost1$temp09-0.95, na.rm = TRUE) # 0.22
mean(prePost1$temp19, na.rm = TRUE) #0.57

shapiro.test(prePost1$temp09-0.95)
# new
# mad(prePost$TIR2009, na.rm = TRUE) # 0.30
# mad(prePost$TIR08262019Masknew_1, na.rm = TRUE) #0.68

# correct bias at release
# wilcox.test(prePost$TIR2009, prePost$TIR08262019Masknew_1+0.95, paired =TRUE) # p < 0.001 W 124937


# report diff in median
# cite Christian for surface temp vs measurements in dam water column
# why these temps are higher

# new rasters anlaysis
tir2019new <- raster("E:\\sfmr_08272019\\raster_products\\TIR2019_maskN.tif")
# NAvalue(tir2019new) <- min(raster::values(tir2019new), na.rm = TRUE) # specify the NA

mad(raster::values(tir2019new), na.rm = TRUE) #0.98
sum(!is.na(raster::values(tir2019new)))
hist(raster::values(tir2019new), breaks = 100)

ks.test(raster::values(tir2009), raster::values(tir2019new)+0.95, paired = TRUE) # p < 0.001, D = 0.643


# compare raster cells in ks test, mask them using the wetted area
library(raster)
tir2009 <- raster('raster_products\\TIR2009_mask.tif')/10
tir2019 <- raster('raster_products\\TIR2019_mask.tif')

# tir2009 <- tir2009/10
tir2009
tir2019

sum(!is.na(raster::values(tir2009)))


hist(raster::values(tir2009), breaks = 100)
hist(raster::values(tir2019), breaks = 100)
ks.test(raster::values(tir2009), raster::values(tir2019))
ks.test(raster::values(tir2009)-0.95, raster::values(tir2019))

mad(raster::values(tir2009), na.rm = TRUE) #.30
mad(raster::values(tir2019), na.rm = TRUE) #2.46

# add 0.95 to correct for temp bias observed at release
wilcox.test(raster::values(tir2009), raster::values(tir2019+0.95))
?mad

# count cells that are not NA
v2009 <- raster::values(tir2009)
v2019 <- raster::values(tir2019)

length(v2009, na.rm = TRUE)
length(v2009) - sum(is.na(v2009)) #81844
length(v2019) - sum(is.na(v2019)) #16919927
