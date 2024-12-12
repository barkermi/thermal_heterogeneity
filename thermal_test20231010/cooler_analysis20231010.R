# cooler test 2023-10-10
# updated 2023 - 01 - 11
# fired camera at 5 second intervals beginning at 13:35, ending at 16:00
# follow this to try to handle the autocorrelation: https://cran.r-project.org/web/packages/itsadug/vignettes/acf.html

library(readxl)
library(dplyr)
library(lubridate)
library(exifr)
library(lmtest)
library(Metrics)
library(tidyverse)
library(caret)

setwd("D:\\thermal_test20231010")
cooler_temps_logger <- read_xlsx(path = "cooler_temps20231010.xlsx", skip = 1, sheet = 'Data')

cooler_temps_logger$Time <- as.POSIXlt(cooler_temps_logger$Time)
cooler_temps_logger <- cooler_temps_logger %>% filter(Time >= ymd_hms('2023-10-10 13:35:00')) %>% filter(Time <= ymd_hms('2023-10-10 16:00:00'))

# par(mar = c(0,0,0,0))
dev.off()
par(mfrow=c(1,1))
plot(cooler_temps_logger$Time, cooler_temps_logger$Temperature)

cooler_temps_logger$id = seq(0:1740)

camera_temps <- read.csv('temps.csv')
camera_temps$id <- seq(0:1740)
# ?merge

# merge logger with camera measured temps
merged_df <- merge(cooler_temps_logger, camera_temps, by = 'id')
plot(merged_df$temp_c, merged_df$Temperature)
plot(merged_df$Time, merged_df$temp_c - merged_df$Temperature)

# print the base model
summary(lm(Temperature~temp_c, data = merged_df))

# calc differences in camera and logger
merged_df$diff = merged_df$temp_c - merged_df$Temperature
summary(lm(diff~id, data = merged_df))

sd(merged_df$Temperature)
sd(merged_df$temp_c)

summary(lm(Temperature~temp_c:id + temp_c, data = merged_df))

plot(((merged_df$temp_c - mean(merged_df$temp_c))/sd(merged_df$temp_c)), ((merged_df$Temperature-mean(merged_df$Temperature))/sd(merged_df$Temperature)))

# is bias change just showsing us cam warming up
# plot diff vs exif camera temp internal
merged_df$sec <- seconds(merged_df$Time)
merged_df$sec <- merged_df$sec-min(merged_df$sec)

# get the exif things
camera_exif <- read_exif("D:\\thermal_test20231010\\photos_20231010", recursive = TRUE)
camera_exif$id = seq(0:1740)

exif_sub <- camera_exif %>% select(id, SceneTemperature, ImagerTemperatureC)

merged_df <- merge(merged_df, exif_sub, by = 'id')

# durbin watson test diffs against time
mdl.test <- lm(diff~sec, data= merged_df)
summary(mdl.test)

mdl.test2 <- lm(diff~ImagerTemperatureC, data = merged_df)
summary(mdl.test2)

mdl.test3 <- lm(diff~SceneTemperature, data = merged_df)
summary(mdl.test3)

plot(merged_df$sec, merged_df$diff)
dwtest(mdl.test)

#### build models w repeated kfold ####
# repeated k fold cross validation for base model
# Define training control
set.seed(123)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 100)
# Train the recorded temperature model
model <- train(Temperature ~temp_c, data = merged_df, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
model$finalModel

# Train the time model
model_sec <- train(Temperature ~sec, data = merged_df, method = "lm",
               trControl = train.control)
# Summarize the results
print(model_sec)
summary(model_sec$finalModel)

# bring in the other cooler tests and rename vars ot match

master <- read_excel("C:\\Users\\deadf\\imageprocessing\\data\\cooler_tests_master.xlsx")

# combine previous three cooler tests and predict with oct 2023 model
master_new <- master %>% 
  select(C, NIST_temp_C, sec, treatment) %>% rename(temp_c = C, Temperature = NIST_temp_C)

predictions_all <- model %>% predict(master_new)
data.frame( R2 = R2(predictions_all, master_new$Temperature),
            RMSE = RMSE(predictions_all, master_new$Temperature),
            MAE = MAE(predictions_all, master_new$Temperature))

model_all <- train(Temperature ~temp_c, data = master_new, method = "lm",
                   trControl = train.control)

print(model_all)
model_all$finalModel

predicitons_model_all <- model_all %>% predict(master_new)
# beofre correction
data.frame( R2 = R2(master_new$temp_c, master_new$Temperature),
            RMSE = RMSE(master_new$temp_c, master_new$Temperature),
            MAE = MAE(master_new$temp_c, master_new$Temperature))
# after correction
data.frame( R2 = R2(predicitons_model_all, master_new$Temperature),
            RMSE = RMSE(predicitons_model_all, master_new$Temperature),
            MAE = MAE(predicitons_model_all, master_new$Temperature))

# show the ablines for model specific and the cooler2023 test
plot(master_new$temp_c, master_new$Temperature)
abline(26.58981, 0.05065, col = 'green') # fan test
abline(25.94974, -0.04698, col = 'black') # shade test
abline(25.17290, 0.08466, col = 'orange') # sun test

abline(7.9885, 0.7184, col = 'blue') # three cooler tests
abline(2.9168, 0.9284, col = 'red', lty = 2) # oct 2023 cooler test
abline(2.984696, 0.922275, col = 'orange', lty = 2) # all data, 4 tests

summary(lm(Temperature~temp_c, data = master_new))
summary(lm(Temperature~temp_c, data = merged_df)) # this is just the range test

# combine all 4 cooler tests and make a lm
merged_df$treatment <- "Range"

cooler20231010 <- merged_df %>% select(temp_c, Temperature, sec, treatment)
cooler20231010$sec[1]
typeof(master_new$sec) <- typeof(cooler20231010)
class(cooler20231010$sec) <- class(master_new$sec) # convert col from lubridate to numeric
cooler_tests_all <- rbind(cooler20231010, master_new)
cooler_tests_all$dif2 <- cooler_tests_all$temp_c - cooler_tests_all$Temperature
summary(cooler_tests_all$temp_c)
# do a repeated cv for all the data
# Train the recorded temperature model
model_4tests <- train(Temperature ~temp_c, data = cooler_tests_all, method = "lm",
               trControl = train.control)
# Summarize the results
print(model_4tests)
model_4tests$finalModel

# plot everything with ggplot, add colors for tests and the final model regression line
ggplot(cooler_tests_all, aes(x=temp_c, y=Temperature, color=factor(treatment), )) +
  geom_point(size = 2) +
  xlab("TIR Temperature (C)") +
  ylab("NIST Temperature (C)") + 
  geom_smooth(method='lm', color = "blue") +
  guides(color=guide_legend(title="Cooler Test")) +
  scale_color_viridis() +
  scale_color_hue(labels = c("Fan", "Range", "Shade", "Sun")) +
  theme_bw() + annotate(geom="text", x=10, y=30, label="b0 = 2.9847\nb1 = 0.9223\nR^2 = 0.99",
                        color="black")
  # scale_y_continuous(breaks = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  # theme_bw()


# linear model for all 4 cooler tests data
cooler_tests_all_lm <- lm(Temperature~temp_c, data = cooler_tests_all)
summary(cooler_tests_all_lm)

# autocorrelation stuff
# https://stats.stackexchange.com/questions/181257/correcting-for-autocorrelation-in-simple-linear-regressions-in-r
# https://www.econometrics-with-r.org/15.4-hac-standard-errors.html
library(sandwich) # use the HAC to widen the error since we are autocor
m <- m <- floor(0.75 * nrow(cooler_tests_all)^(1/3))
NW_VCOV <- NeweyWest(cooler_tests_all_lm, 
                     lag = m - 1, prewhite = F, 
                     adjust = T)

# new standard erro
sqrt(diag(NW_VCOV))[2]

coeftest(cooler_tests_all_lm, vcov = NW_VCOV)
# save the model with all cooler data to recalc orthos
# saveRDS(cooler_tests_all_lm, 'cooler4tests_lm.rda')
# cooler test 4 to predict all three previous tests

# visualize the 4 cooler tests error vs time

shade_temps <- cooler_tests_all %>% filter(treatment == "shade")
sun_temps <- cooler_tests_all %>% filter(treatment == "sun")
fan_temps <- cooler_tests_all %>% filter(treatment == "fan")
range_temps <- cooler_tests_all %>% filter(treatment == "Range")

shade_temps$diff2 <- shade_temps$temp_c - shade_temps$Temperature
sun_temps$diff2 <- sun_temps$temp_c - sun_temps$Temperature
fan_temps$diff2 <- fan_temps$temp_c - fan_temps$Temperature
range_temps$diff2 <- range_temps$temp_c - range_temps$Temperature


# create labels for cooler test plots
xlabel <- "Elapsed Time (s)"
ylabel <- "TIR (C) - NIST (C)"

par(mfrow=c(4,1))
plot(shade_temps$sec, shade_temps$diff2, main = "Shade", xlab = xlabel, ylab = ylabel)
abline(h = 0, lty = 2)
sqrt((sum(shade_temps$diff^2))/length((shade_temps$diff)))
mean(shade_temps$diff2)

plot(sun_temps$sec, sun_temps$diff2, main = "Full Sun", xlab = xlabel, ylab = ylabel)
abline(h = 0, lty = 2)
sqrt((sum(sun_temps$diff^2))/length((sun_temps$diff)))
mean(sun_temps$diff2)
range(sun_temps$diff2)

plot(fan_temps$sec, fan_temps$diff2, main = "Fan", xlab = xlabel, ylab = ylabel)
abline(h = 0, lty = 2)
sqrt((sum(fan_temps$diff^2))/length((fan_temps$diff)))
mean(fan_temps$diff2)
rmse(fan_temps$NIST_temp_C, fan_temps$C)

plot(range_temps$sec, range_temps$diff2, main = "Range", xlab = xlabel, ylab = ylabel)
abline(h = 0, lty = 2)
sqrt((sum(range_temps$diff^2))/length((range_temps$diff)))
mean(range_temps$diff2)
rmse(range_temps$NIST_temp_C, range_temps$C)

predictions_3tests <- model %>% predict(master_new)
predictions_4tests <- cooler_tests_all_lm %>% predict(master_new)


plot(cooler_tests_all$temp_c, cooler_tests_all$Temperature)
abline(2.984696, 0.922275)

# demonstrate shade before and after correction
# before correcton
data.frame( R2 = R2(shade_temps$temp_c, shade_temps$Temperature),
            RMSE = RMSE(shade_temps$temp_c, shade_temps$Temperature),
            MAE = MAE(shade_temps$temp_c, shade_temps$Temperature))
#after correction
data.frame( R2 = R2(predict(cooler_tests_all_lm, shade_temps), shade_temps$Temperature),
            RMSE = RMSE(predict(cooler_tests_all_lm, shade_temps), shade_temps$Temperature),
            MAE = MAE(predict(cooler_tests_all_lm, shade_temps), shade_temps$Temperature))

# demonstrate sun before and after correction
# before correcton
data.frame( R2 = R2(sun_temps$temp_c, sun_temps$Temperature),
            RMSE = RMSE(sun_temps$temp_c, sun_temps$Temperature),
            MAE = MAE(sun_temps$temp_c, sun_temps$Temperature))
#after correction
data.frame( R2 = R2(predict(cooler_tests_all_lm, sun_temps), sun_temps$Temperature),
            RMSE = RMSE(predict(cooler_tests_all_lm, sun_temps), sun_temps$Temperature),
            MAE = MAE(predict(cooler_tests_all_lm, sun_temps), sun_temps$Temperature))

# demonstrate fan before and after correction
# before correcton
data.frame( R2 = R2(fan_temps$temp_c, fan_temps$Temperature),
            RMSE = RMSE(fan_temps$temp_c, fan_temps$Temperature),
            MAE = MAE(fan_temps$temp_c, fan_temps$Temperature))
#after correction
data.frame( R2 = R2(predict(cooler_tests_all_lm, fan_temps), fan_temps$Temperature),
            RMSE = RMSE(predict(cooler_tests_all_lm, fan_temps), fan_temps$Temperature),
            MAE = MAE(predict(cooler_tests_all_lm, fan_temps), fan_temps$Temperature))

# range test before and after correction
#before
data.frame( R2 = R2(range_temps$temp_c, range_temps$Temperature),
            RMSE = RMSE(range_temps$temp_c, range_temps$Temperature),
            MAE = MAE(range_temps$temp_c, range_temps$Temperature))
#after
data.frame( R2 = R2(predict(cooler_tests_all_lm, range_temps), range_temps$Temperature),
            RMSE = RMSE(predict(cooler_tests_all_lm, range_temps), range_temps$Temperature),
            MAE = MAE(predict(cooler_tests_all_lm, range_temps), range_temps$Temperature))

# demonstrate all data before and after correction
# before correcton
data.frame( R2 = R2(cooler_tests_all$temp_c, cooler_tests_all$Temperature),
            RMSE = RMSE(cooler_tests_all$temp_c, cooler_tests_all$Temperature),
            MAE = MAE(cooler_tests_all$temp_c, cooler_tests_all$Temperature))
#after correction
data.frame( R2 = R2(cooler_tests_all_lm$fitted.values, cooler_tests_all$Temperature),
            RMSE = RMSE(cooler_tests_all_lm$fitted.values, cooler_tests_all$Temperature),
            MAE = MAE(cooler_tests_all_lm$fitted.values, cooler_tests_all$Temperature))


#after correction using cooler test 4 data to predict to three previous cooler tests
data.frame( R2 = R2(predictions_3tests, master_new$Temperature),
            RMSE = RMSE(predictions_3tests, master_new$Temperature),
            MAE = MAE(predictions_3tests, master_new$Temperature))
#after correction using cooler test 4 data to predict to three previous cooler tests
data.frame( R2 = R2(predictions_4tests, master_new$Temperature),
            RMSE = RMSE(predictions_4tests, master_new$Temperature),
            MAE = MAE(predictions_4tests, master_new$Temperature))

# means for all tests after correcting
mean(predict(cooler_tests_all_lm, shade_temps))
mean(predict(cooler_tests_all_lm, sun_temps))
mean(predict(cooler_tests_all_lm, fan_temps))
mean(predict(cooler_tests_all_lm, range_temps))
mean(predict(cooler_tests_all_lm, cooler_tests_all))

# break out individual treatments
# sun_temps <- master %>% filter(treatment == "sun")
# shade_temps <- master %>% filter(treatment == "shade")
# fan_temps <- master %>% filter(treatment == "fan")
# 
# sun_temps_new <- sun_temps %>% 
#   select(C, NIST_temp_C) %>% rename(temp_c = C, Temperature = NIST_temp_C)
# 
# fan_temps_new <- fan_temps %>% 
#   select(C, NIST_temp_C) %>% rename(temp_c = C, Temperature = NIST_temp_C)
# 
# shade_temps_new <- shade_temps %>% 
#   select(C, NIST_temp_C) %>% rename(temp_c = C, Temperature = NIST_temp_C)

# predict using the best cv model, check rmse, r2, mae


# sun stuff
# range from 27.1 - 27.8
predictions_sun <- model %>% predict(sun_temps)
data.frame( R2 = R2(predictions_sun, sun_temps$Temperature),
            RMSE = RMSE(predictions_sun, sun_temps$Temperature),
            MAE = MAE(predictions_sun, sun_temps$Temperature))
# compare to cv model using data from this test
model_sun <- train(Temperature ~temp_c, data = sun_temps, method = "lm",
               trControl = train.control)

print(model_sun)
model_sun$finalModel

predictions_model_sun <- model_sun %>% predict(sun_temps)

# predict with data from four tests
predictions_all_sun <- cooler_tests_all_lm %>% predict(sun_temps)

# before correction
data.frame( R2 = R2(sun_temps$temp_c, sun_temps$Temperature),
            RMSE = RMSE(sun_temps$temp_c, sun_temps$Temperature),
            MAE = MAE(sun_temps$temp_c, sun_temps$Temperature))
#after correction sun data
data.frame( R2 = R2(predictions_model_sun, sun_temps$Temperature),
            RMSE = RMSE(predictions_model_sun, sun_temps$Temperature),
            MAE = MAE(predictions_model_sun, sun_temps$Temperature))
#after correction range cooler data
data.frame( R2 = R2(predictions_sun, sun_temps$Temperature),
            RMSE = RMSE(predictions_sun, sun_temps$Temperature),
            MAE = MAE(predictions_sun, sun_temps$Temperature))
#after correction all data
data.frame( R2 = R2(predictions_all_sun, sun_temps$Temperature),
            RMSE = RMSE(predictions_all_sun, sun_temps$Temperature),
            MAE = MAE(predictions_all_sun, sun_temps$Temperature))

# show the ablines for sun model specific and the cooler2023 test
plot(sun_temps_new$temp_c, sun_temps_new$Temperature)
abline(25.17290, 0.08466, col = 'blue')
abline(2.9168, 0.9284, col = 'red', lty = 2)

# shade stuff
# range actual logger temp was 24.7 - 25.0, likely explains high intercept ocmpared to cooler
predictions_shade <- model %>% predict(shade_temps_new)
data.frame( R2 = R2(predictions_shade, shade_temps_new$Temperature),
            RMSE = RMSE(predictions_shade, shade_temps_new$Temperature),
            MAE = MAE(predictions_shade, shade_temps_new$Temperature))

model_shade <- train(Temperature ~temp_c, data = shade_temps_new, method = "lm",
                   trControl = train.control)

print(model_shade)
model_shade$finalModel

predicitons_model_shade <- model_shade %>% predict(shade_temps_new)

# predict with data from four tests
predictions_all_shade<- cooler_tests_all_lm %>% predict(shade_temps_new)

# before correction
data.frame( R2 = R2(shade_temps_new$temp_c, shade_temps_new$Temperature),
            RMSE = RMSE(shade_temps_new$temp_c, shade_temps_new$Temperature),
            MAE = MAE(shade_temps_new$temp_c, shade_temps_new$Temperature))
#after correction using shade only data
data.frame( R2 = R2(predicitons_model_shade, shade_temps_new$Temperature),
            RMSE = RMSE(predicitons_model_shade, shade_temps_new$Temperature),
            MAE = MAE(predicitons_model_shade, shade_temps_new$Temperature))
# after correction with cooler4 data
data.frame( R2 = R2(predictions_shade, shade_temps_new$Temperature),
            RMSE = RMSE(predictions_shade, shade_temps_new$Temperature),
            MAE = MAE(predictions_shade, shade_temps_new$Temperature))
# after correction with all data
data.frame( R2 = R2(predictions_all_shade, shade_temps_new$Temperature),
            RMSE = RMSE(predictions_all_shade, shade_temps_new$Temperature),
            MAE = MAE(predictions_all_shade, shade_temps_new$Temperature))

# show the ablines for shade model specific and the cooler2023 test
plot(shade_temps_new$temp_c, shade_temps_new$Temperature)
abline(25.94974, -0.04698, col = 'blue')
abline(2.9168, 0.9284, col = 'red', lty = 2)

# fan stuff
# range 27.6 - 28.4
predictions_fan <- model %>% predict(fan_temps_new)
data.frame( R2 = R2(predictions_fan, fan_temps_new$Temperature),
            RMSE = RMSE(predictions_fan, fan_temps_new$Temperature),
            MAE = MAE(predictions_fan, fan_temps_new$Temperature))

model_fan <- train(Temperature ~temp_c, data = fan_temps_new, method = "lm",
                     trControl = train.control)

print(model_fan)
model_fan$finalModel

predicitons_model_fan <- model_fan %>% predict(fan_temps_new)

# predict with data from four tests
predictions_all_fan<- cooler_tests_all_lm %>% predict(fan_temps_new)

# before correction
data.frame( R2 = R2(fan_temps_new$temp_c, fan_temps_new$Temperature),
            RMSE = RMSE(fan_temps_new$temp_c, fan_temps_new$Temperature),
            MAE = MAE(fan_temps_new$temp_c, fan_temps_new$Temperature))
# after correctoin fan data only
data.frame( R2 = R2(predicitons_model_fan, fan_temps_new$Temperature),
            RMSE = RMSE(predicitons_model_fan, fan_temps_new$Temperature),
            MAE = MAE(predicitons_model_fan, fan_temps_new$Temperature))
# after correctoin cooler4 data
data.frame( R2 = R2(predictions_fan, fan_temps_new$Temperature),
            RMSE = RMSE(predictions_fan, fan_temps_new$Temperature),
            MAE = MAE(predictions_fan, fan_temps_new$Temperature))
# after correctoin all data
data.frame( R2 = R2(predictions_all_fan, fan_temps_new$Temperature),
            RMSE = RMSE(predictions_all_fan, fan_temps_new$Temperature),
            MAE = MAE(predictions_all_fan, fan_temps_new$Temperature))

# show the ablines for fan model specific and the cooler2023 test
plot(fan_temps_new$temp_c, fan_temps_new$Temperature)
abline(26.58981, 0.05065, col = 'blue')
abline(2.9168, 0.9284, col = 'red', lty = 2)

all_tests_plot <- ggplot(data = cooler_tests_all, mapping= aes(x = temp_c, y = Temperature)) +
  geom_point(mapping = aes(color = treatment))
all_tests_plot

library(plotly)
fig <- plot_ly(data = cooler_tests_all, x = ~temp_c, y = ~Temperature, trendline = 'ols')
fig

min(abs(cooler_tests_all$temp_c - cooler_tests_all$Temperature))
hist(cooler_tests_all$temp_c - cooler_tests_all$Temperature)
hist(shade_temps_new$temp_c - shade_temps_new$Temperature)
hist(sun_temps_new$temp_c - sun_temps_new$Temperature)
hist(fan_temps_new$temp_c - fan_temps_new$Temperature)
hist(cooler20231010$temp_c - cooler20231010$Temperature)
stuff <- as.data.frame(cooler_tests_all$temp_c - cooler_tests_all$Temperature)

# look at rmses
RMSE(fan_temps_new$temp_c, fan_temps_new$Temperature) # .85 before correcting
RMSE(model_all %>% predict(fan_temps_new),fan_temps_new$Temperature) # .57 with all data
RMSE(predictions_fan, fan_temps_new$Temperature) # .86 with oct 2023 cooler data

RMSE(sun_temps_new$temp_c, sun_temps_new$Temperature) # 1.17 before correcting
RMSE(model_all %>% predict(sun_temps_new),sun_temps_new$Temperature) # .68 with all data
RMSE(predictions, sun_temps_new$Temperature) # .84

RMSE(shade_temps_new$temp_c, shade_temps_new$Temperature) # 1.08 before correcting
RMSE(model_all %>% predict(shade_temps_new),shade_temps_new$Temperature) # .80 with all data
RMSE(predictions_shade, shade_temps_new$Temperature) # .94

# three cooler tests using model from three cooler tests
RMSE(master_new$temp_c, master_new$Temperature) # 1.04 before correcting
RMSE(model %>% predict(master_new), master_new$Temperature) # .88
RMSE(predictions_all, master_new$Temperature) # .88 with all three coolers data

# four cooler test
RMSE(cooler_tests_all$temp_c, cooler_tests_all$Temperature) # 1.78
RMSE(cooler_tests_all_lm$fitted.values, cooler_tests_all$Temperature) #0 .48
plot(cooler_tests_all_lm)

predict(my_model_name$finalModel, my_data, interval = "confidence") 

# confidence interval for all data
predict(model_all$finalModel, master_new, interval = "confidence") 
plot(cooler_tests_all_lm)

## 50% of the sample size
smp_size <- floor(0.5 * nrow(cooler_tests_all))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(cooler_tests_all)), size = smp_size)

train <- cooler_tests_all[train_ind, ]
test <- cooler_tests_all[-train_ind, ]

half_model <- lm(Temperature~temp_c, data = train)
RMSE(test$temp_c, test$Temperature)
RMSE(half_model %>% predict(test),test$Temperature)
half_resids <- test$Temperature - half_model %>% predict(test)
mean(half_resids)
hist(half_resids, breaks = 20)
hist(cooler_tests_all_lm$residuals, breaks = 20)
hist(cooler_tests_all$Temperature - cooler_tests_all$temp_c, breaks = 20) # camera obs temps are appearing to be less than logged temps (this is in line with emissivity)
range(cooler_tests_all$Temperature - cooler_tests_all$temp_c)
max((cooler_tests_all$Temperature - cooler_tests_all$temp_c))- min(cooler_tests_all$Temperature - cooler_tests_all$temp_c)
plot(half_model %>% predict(test), half_resids)
#### autocorrelation stuff ####
# autocorrelation stuff belwo
library(nlme)

mdl.ac <- gls(Temperature ~temp_c, data=merged_df, 
               correlation = corAR1(form=~sec),
               na.action=na.omit)
summary(mdl.ac)

mdl.ac2 <- gls(Temperature ~temp_c+ temp_c:sec, data=merged_df, 
              correlation = corAR1(form=~sec),
              na.action=na.omit)
summary(mdl.ac2)

mdl.ac3 <- gls(Temperature ~temp_c+ ImagerTemperatureC, data=merged_df, 
               correlation = corAR1(form=~sec),
               na.action=na.omit)
summary(mdl.ac3)

# test interaction with logger temp
summary(lm(Temperature~temp_c:sec, data = merged_df))
noAR <- lm(Temperature~temp_c+ temp_c:sec, data = merged_df)



noAR2 <- lm(Temperature~temp_c + ImagerTemperatureC + temp_c:ImagerTemperatureC+temp_c:sec, data = merged_df)
summary(noAR2)


noAR3 <- lm(Temperature~temp_c + ImagerTemperatureC + sec, data = merged_df)
summary(noAR3)

noAR4 <- lm(Temperature~temp_c + ImagerTemperatureC, data = merged_df)
summary(noAR4)
# can coefficients be corrected by the autoregressive term

plot(fitted(mdl.ac),residuals(mdl.ac))
abline(h=0,lty=3) 
qqnorm(mdl.ac)
acf(residuals(mdl.ac,type="p"))

plot(fitted(mdl.ac2),residuals(mdl.ac2))
abline(h=0,lty=3) 
qqnorm(mdl.ac2)
acf(residuals(mdl.ac2,type="p"))
acf(residuals(noAR))

plot(fitted(noAR4), residuals(noAR4))

plot(merged_df$sec, merged_df$diff)
plot(merged_df$ImagerTemperatureC, merged_df$diff)


library(MuMIn)
model.sel(mdl.ac2, noAR)

summary(lm(Temperature~temp_c, data = merged_df))
summary(lm(Temperature~temp_c + ImagerTemperatureC + sec, data = merged_df))
summary(lm(Temperature~temp_c + SceneTemperature + sec, data = merged_df))

plot(merged_df$SceneTemperature, merged_df$diff)
plot(merged_df$ImagerTemperatureC, merged_df$diff)

plot(merged_df$ImagerTemperatureC, merged_df$Temperature)
plot(merged_df$sec, merged_df$Temperature)
plot(merged_df$sec, merged_df$temp_c)

# plot(merged_df$sec, merged_df$Temperature)
summary(lm(Temperature~temp_c + ImagerTemperatureC + SceneTemperature + sec, data = merged_df))

# correlation plot
library(corrplot)

M <- cor(sample_n(merged_df[c('Temperature', 'temp_c', 'ImagerTemperatureC', 'SceneTemperature', 'sec', 'diff', 'RMSE')], 100))
corrplot(M, method = 'number')

base_mdl <- lm(Temperature~temp_c, data = merged_df)
?predict
merged_df$predicted <- predict(base_mdl)

rmse(merged_df$Temperature, merged_df$predicted)

summary(lm(Temperature~temp_c + sec, data = merged_df))
max(merged_df$sec)

mdl.ac <- gls(Temperature ~temp_c, data=merged_df, 
              correlation = corAR1(form=~sec),
              na.action=na.omit)
summary(mdl.ac)

summary(lm(Temperature~sec, data = merged_df))
summary(lm(Temperature~ImagerTemperatureC, data = merged_df))

base_mod <- lm(Temperature~temp_c, data = merged_df)
plot(base_mod$residuals)
plot(base_mod)

# remove every third from random start
# randomly select 25 % for cv, then calc RMSE
# report bias and vaariance on the holdout, apply to sfork orthos

sun_temps_new <- sun_temps %>% 
  select(C, NIST_temp_C) %>% rename(temp_c = C, Temperature = NIST_temp_C)

fan_temps_new <- fan_temps %>% 
  select(C, NIST_temp_C) %>% rename(temp_c = C, Temperature = NIST_temp_C)

shade_temps_new <- shade_temps %>% 
  select(C, NIST_temp_C) %>% rename(temp_c = C, Temperature = NIST_temp_C)

rmse(predict(base_mod, newdata = sun_temps_new), sun_temps_new$Temperature)
rmse(predict(base_mod, newdata = fan_temps_new), fan_temps_new$Temperature)
rmse(predict(base_mod, newdata = shade_temps_new), shade_temps_new$Temperature)


rmse(sun_temps_new$temp_c, sun_temps_new$Temperature)
rmse(fan_temps_new$temp_c, fan_temps_new$Temperature)
rmse(shade_temps_new$temp_c, shade_temps_new$Temperature)
rmse(merged_df$Temperature, merged_df$temp_c)

rmse(predict(base_mod), merged_df$Temperature)
