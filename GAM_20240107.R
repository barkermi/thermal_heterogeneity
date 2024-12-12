# GAM
# updated 12/9/2023

library(mgcv)
library(nlme)

setwd("D:\\sfmr_08272019")
points <- read.csv("tables\\transect_vals20240107.csv")

summary(points$tir_cor_adj_filt)
tapply(points$tir_cor_adj_filt, points$year, summary)
tapply(points$tir_cor_adj_filt, points$year, mad)
tapply(points$tir_cor_adj_filt, points$year, count)
# variables: gage temp, discharge, canopy cover, phase 2 implementation, year,
# lat, lon, dist2head
# make year factor, ordered 09, 19, 21
# phase 2 is a factor yes no
# canopy cov continuous
#discharge one #/year
# year will be fixed effect
# 
myGAM <- gam(tir_cor_adj_filt~s(year, bs = "re") + s(dist_up, bs = "tp")  + discharge + factor(restored)+ factor(fire)+temp_at_release,
             data = points, method = "REML")

summary(myGAM) # adj r2 .87, 87.3% deviance explained

# add air temp and canopy cov +ti(dist_origin, POINT_X, POINT_Y) # cannot allocate vector
# removed  + s(dist_origin, bs = "tp")+ s(POINT_Y, POINT_X) # cannot allocate vector
# removed s(year, bs = "re")
myGAM2 <- gam(tir_cor_adj_filt~ +s(dist_up) + s(POINT_X, POINT_Y)
              + discharge +air_temp_C +canopy_cov+ factor(restored)+ factor(fire)+temp_at_release,
             data = points, method = "REML", select = TRUE)

summary(myGAM2) #adj r2 .878, 88.3% dev explained

myGAM3 <- gam(tir_cor_adj_filt~ s(year, bs = "re") + s(dist_up) + s(POINT_Y, POINT_X) + discharge + canopy_cov + factor(restored),
             data = points,
             # correlation = corSpher(form = ~lon +lat) +
             #   corArma(form = ~year,p=1, q=1), 
             method = "REML")

summary(myGAM3) # .878, 88.2%

# GAM with tensor interaction for x and dist_origin
# + factor(restored) # rename this var to stage0

# if stage 0 is included, canopy is dropped
myGAM4 <- gam(tir_cor_adj_filt~ s(year, bs = "re") + s(dist_up) + s(POINT_Y, POINT_X) + 
                ti(POINT_X, POINT_Y, dist_up) +discharge + canopy_cov,
             data = points,
             # correlation = corSpher(form = ~lon +lat) +
             #   corArma(form = ~year,p=1, q=1), 
             method = "REML")

summary(myGAM4) # 0.877, 88%

# within year is larger than between year variatoin, 
# add interactio for canopy and discharge, canopy and stage0 factor
myGAM5 <- gam(tir_cor_adj_filt~ s(year, bs = "re") + s(dist_up) + s(POINT_Y, POINT_X) + 
                ti(POINT_X, POINT_Y, dist_up) +discharge + canopy_cov + discharge:canopy_cov,
              data = points,
              # correlation = corSpher(form = ~lon +lat) +
              #   corArma(form = ~year,p=1, q=1), 
              method = "REML")

summary(myGAM5) #0.877, 88%

myGAM6 <- gam(tir_cor_adj_filt~ s(year, bs = "re") + s(dist_up) + s(POINT_Y, POINT_X) + 
                ti(POINT_X, POINT_Y, dist_up) +discharge  +canopy_cov + air_temp_C,
              data = points,
              # correlation = corSpher(form = ~lon +lat) +
              #   corArma(form = ~year,p=1, q=1), 
              method = "REML")

summary(myGAM6) # 0.877, 88%
plot(myGAM6)

myGAM7 <- gam(tir_cor_adj_filt~ s(year, bs = "re") + s(dist_up) + s(POINT_Y, POINT_X) + 
                ti(POINT_X, POINT_Y, dist_up) +discharge  +canopy_cov +  temp_at_release+air_temp_C,
              data = points,
              # correlation = corSpher(form = ~lon +lat) +
              #   corArma(form = ~year,p=1, q=1), 
              method = "REML")

summary(myGAM7) #0.877, 88%

# to reflect restoration, slopes between points (email steve), could bring in wood (hold off on these maybe for future work)
# avg temp/hex, avg slope/hex, avg wood (or non-water using ndwi first)/hex
# double-check air temp for 2019

# diff between pre restor and unburned is a function of the restoration, but we are able to parse out the effect of canopy, talk about min, median, and stdev
plot(points$canopy_cov, points$tir_cor_adj_filt, col = points$year)
# linear model
myLM <- lm(tir_cor_adj_filt~ dist_up + POINT_Y * POINT_X+discharge + factor(restored)+temp_at_release, data = points)

summary(myLM)
summary(lm(tir_cor_adj_filt~factor(restored) + dist_up + factor(fire)+canopy_cov, data = points))
# summary(lm(temp[1:280]~dist_origin[1:280], data = points))
summary(lm(tir_cor_adj_filt~dist_up +discharge, data = points))

points2009 <- points %>% filter(year == 2009)
points2019 <- points %>% filter(year == 2019)
points2021 <- points %>% filter(year == 2021)

plot(points2009$dist_up, points2009$tir_cor_adj_filt)
plot(points2019$dist_up, points2019$tir_cor_adj_filt)
plot(points2021$dist_up, points2021$tir_cor_adj_filt)


# hist(points$temp[561:840], breaks = 15)

plot(points$dist_up, points$tir_cor_adj_filt, col = factor(points$year))

# plot(points$POINT_X, points$temp, col = factor(points$year))
# plot(points$POINT_Y, points$temp, col = factor(points$year))
# ask paco about using gam as contrast
#is temp diff between year a function of the other covariates
# remove splines to run as GLM
# https://stats.stackexchange.com/questions/376237/correcting-for-multiple-pairwise-comparisons-with-gam-objects-mgcv-in-r

# two evalutations
# one contrast pre and post restoration 09 vs 19

names(temps)
library(data.table)
tempsDT <- data.table(temps)
names(tempsDT) <- c()
melted <- melt(tempsDT, "ï..OID_", c("temp2019", "temp2021"))

glm.mod <- glm(value~factor(variable), data = melted)
summary(glm.mod)
names(melted)
?melt()
temps
# gives output similar to glm  
# use lat, lon to acocunt for spat component
# lme4 pkg also work with glht(), does p-value adjustments for multiple pairwise contrasts

# ask paco if you have factor variable like year can they be considered indpendent
# is temp different even when youve considered all these factors
# and will tell us about the importance of the covariates
# may also consider interactions

# also try this
# maybe drop out year, use it as a random effect, 
# fixed effects could be pre post burn and pre restore post restore
# maybe include airtemp in addition to discharge temp
# this way you dont have to apply adjusters

# another potential variable
# years since variation? 2009 gets 0, 2019 gets 1, 2021 3
# captures response n changing processes geomorphic over time

# if you do all three together set the reference on the 2019

# interesting contrasts are just ref vs fire, ref vs pre restore
# could do dummy temps 50, 100, 150 to make sure its contrasting to reference not doing additive

