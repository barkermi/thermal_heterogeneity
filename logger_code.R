# read in DEQ HOBO data and extract for dates of interest

library(readxl)
library(tidyverse)
library(Metrics)
DEQ_path <- "C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\temp_loggers_DEQ\\SFMR Stage-0 Stream Temp Continuous Data Form - COPY.xlsx"

# accessing all the sheets 
sheet = excel_sheets(DEQ_path)
sheet

# remove first two sheet names
sheet <- sheet[-c(1,2,3)]
sheet

# applying sheet names to dataframe names, adjust to read starting at row with header
data_frame = lapply(setNames(sheet, sheet), 
                    function(x) read_excel(DEQ_path, sheet=x, skip = 4))

# attaching all dataframes together
data_frame = bind_rows(data_frame, .id="Sheet")
data_frame$TIME = format(as.POSIXct(data_frame$TIME), format = "%H:%M")
# printing data of all sheets
print (data_frame$DATE[2833])

# rename Sheet to Sensor
data_frame <- data_frame %>% rename(Sensor_ID = Sheet)

temps20190826 <- data_frame %>% 
  filter(as.Date(DATE, "%m/%d/%y") == "2019-08-26") %>% 
  filter(TIME == "13:00" | TIME == "13:30" | TIME == "14:00" | TIME == "14:30") %>% 
  group_by(Sensor_ID) %>% summarise(mean_HOBO = mean(TEMP_r))

# this is for part 3 of 8/26 flight that happened on 8/27
temps20190827 <- data_frame %>%
  filter(as.Date(DATE, "%m/%d/%y") == "2019-08-27") %>%
  filter(TIME == "13:00" | TIME == "13:30" | TIME == "14:00" | TIME == "14:30") %>%
  group_by(Sensor_ID) %>% summarise(mean_HOBO = mean(TEMP_r))

# this is for self-contained single flight on 8/27, flight from 2:55 - 3:15
# temps20190827 <- data_frame %>%
#   filter(as.Date(DATE, "%m/%d/%y") == "2019-08-27") %>%
#   filter(TIME == "14:30"|TIME == "15:00"|TIME == "15:30") %>%
#   group_by(Sensor_ID) %>% summarise(mean_HOBO = mean(TEMP_r))

temps20190923 <- data_frame %>%
  filter(as.Date(DATE, "%m/%d/%y") == "2019-09-23") %>%
  filter(TIME == "12:00"|TIME == "12:30"|TIME == "13:00"|TIME == "13:30"|TIME == "14:00") %>%
  group_by(Sensor_ID) %>% summarise(mean_HOBO = mean(TEMP_r))

temps20210715 <- data_frame %>%
  filter(as.Date(DATE, "%m/%d/%y") == "2021-07-15") %>%
  filter(TIME == "13:00" | TIME == "13:30" | TIME == "14:00" | TIME == "14:30") %>%
  group_by(Sensor_ID) %>% summarise(mean_HOBO = mean(TEMP_r))

temps2331257_08272019 <- data_frame %>%
  filter(as.Date(DATE, "%m/%d/%y") == "2019-08-27") %>%
  filter(TIME == "14:00") %>%
  group_by(Sensor_ID) %>% summarise(mean_HOBO = mean(TEMP_r)) %>% filter(Sensor_ID == "2331257")

# new using 8/27 flight
TIR20190827_mos <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\visible_logger20190827.csv")
TIR20190827_avg <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\visible_logger_avg20190827.csv")
TIR20190827_dis <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\visible_logger_dis20190827.csv")

# 8.27 flight, but masked from NDWI
TIR20190827_mosMask <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\visible_logger_mos20190827Mask.csv")
TIR20190827_avgMask <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\visible_logger_avg20190827Mask.csv")
TIR20190827_disMask <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\visible_logger_dis20190827Mask.csv")

# 8.27 flight, but masked from NDWI and buffered -0.68m
TIR20190827_mosMask2 <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\visible_logger_mos20190827Mask2.csv")
TIR20190827_avgMask2 <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\visible_logger_avg20190827Mask2.csv")
TIR20190827_disMask2 <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\visible_logger_dis20190827Mask2.csv")

# 8/26/2019 masked from individual orthophotos
TIR20190826_orthoMask <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\sensorTIROrtho20190826.csv")

# ndwi datat
NDWI <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\ndwi_logger20190827.csv")

# from individual ortho photos
ortho <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\comb_orthos20190827Mask.csv")

# upload masked tables and rerun, esp mean
names(TIR20190827_mos)[2] <- "Sensor_ID"
names(TIR20190827_avg)[2] <- "Sensor_ID"
names(TIR20190827_dis)[2] <- "Sensor_ID"
names(NDWI)[2] <- "Sensor_ID"
names(ortho)[2] <- "Sensor_ID"
names(TIR20190826_orthoMask)[2] <- "Sensor_ID"


names(TIR20190827_mosMask)[2] <- "Sensor_ID"
names(TIR20190827_avgMask)[2] <- "Sensor_ID"
names(TIR20190827_disMask)[2] <- "Sensor_ID"

names(TIR20190827_mosMask2)[2] <- "Sensor_ID"
names(TIR20190827_avgMask2)[2] <- "Sensor_ID"
names(TIR20190827_disMask2)[2] <- "Sensor_ID"

TIR20190827_mos <- TIR20190827_mos %>% select(-1)
TIR20190827_avg <- TIR20190827_avg %>% select(-1)
TIR20190827_dis <- TIR20190827_dis %>% select(-1)
TIR20190826_orthoMask <- TIR20190826_orthoMask %>% select(-1)
NDWI <- NDWI %>% select(-1)
ortho <- ortho %>% select(-1)

# 08/26/2019 ortho photo temps v logger temps
join20190826 <- TIR20190826_orthoMask %>% 
  left_join(temps20190826, by = "Sensor_ID") 

join20190826$diff <- join20190826$MEAN - join20190826$mean_HOBO

# drop sensor 10524511 (outlier), 10862884, 10862917, 10972426, 10792417
join20190826 <- join20190826 %>% filter(!Sensor_ID %in% c("10524511", "10862884", "10862917", "10972426"))

summary(lm(mean_HOBO~PCT20, data = join20190826))
?cor
cor(join20190826$mean_HOBO, join20190826$PCT20)
plot(join20190826$MEAN, join20190826$mean_HOBO)

# try dropping other pool loggres 10862915 and 10862879
join20190826 <- join20190826 %>% filter(!Sensor_ID %in% c("10862915", "10862879"))

summary(lm(mean_HOBO~MEAN, data = join20190826))
?cor
cor(join20190826$mean_HOBO, join20190826$PCT20)
plot(join20190826$MEAN, join20190826$mean_HOBO)

# drop 10862876
join20190826 <- join20190826 %>% filter(!Sensor_ID %in% c("10862876"))

# drop 10792417, submerged
join20190826 <- join20190826 %>% filter(!Sensor_ID %in% c("10792417"))


summary(lm(mean_HOBO~MEAN, data = join20190826))
?cor
cor(join20190826$mean_HOBO, join20190826$PCT20)
plot(join20190826$MEAN, join20190826$mean_HOBO)

TIR20190827_mosMask <- TIR20190827_mosMask %>% select(-1)
TIR20190827_avgMask <- TIR20190827_avgMask %>% select(-1)
TIR20190827_disMask <- TIR20190827_disMask %>% select(-1)

TIR20190827_mosMask2 <- TIR20190827_mosMask2 %>% select(-1)
TIR20190827_avgMask2 <- TIR20190827_avgMask2 %>% select(-1)
TIR20190827_disMask2 <- TIR20190827_disMask2 %>% select(-1)


# concatenate colnames to denote blending mode
colnames(TIR20190827_mos)[2:12] <- paste(colnames(TIR20190827_mos)[2:12], "mos", sep = "_")
colnames(TIR20190827_dis)[2:12] <- paste(colnames(TIR20190827_dis)[2:12], "dis", sep = "_")
colnames(TIR20190827_avg)[2:12] <- paste(colnames(TIR20190827_avg)[2:12], "avg", sep = "_")
colnames(NDWI)[2:12] <- paste(colnames(NDWI)[2:12], "NDWI", sep = "_")
colnames(ortho)[2:20] <- paste(colnames(ortho)[2:20], "ortho", sep = "_")


colnames(TIR20190827_mosMask)[2:20] <- paste(colnames(TIR20190827_mosMask)[2:20], "mosMask", sep = "_")
colnames(TIR20190827_disMask)[2:20] <- paste(colnames(TIR20190827_disMask)[2:20], "disMask", sep = "_")
colnames(TIR20190827_avgMask)[2:20] <- paste(colnames(TIR20190827_avgMask)[2:20], "avgMask", sep = "_")

colnames(TIR20190827_mosMask2)[2:20] <- paste(colnames(TIR20190827_mosMask2)[2:20], "mosMask2", sep = "_")
colnames(TIR20190827_disMask2)[2:20] <- paste(colnames(TIR20190827_disMask2)[2:20], "disMask2", sep = "_")
colnames(TIR20190827_avgMask2)[2:20] <- paste(colnames(TIR20190827_avgMask2)[2:20], "avgMask2", sep = "_")


join20190827 <- TIR20190827_mos %>% 
  left_join(TIR20190827_avg, by = "Sensor_ID") %>% 
  left_join(TIR20190827_dis, by = "Sensor_ID") %>% 
  left_join(TIR20190827_mosMask, by = "Sensor_ID") %>% 
  left_join(TIR20190827_avgMask, by = "Sensor_ID") %>% 
  left_join(TIR20190827_disMask, by = "Sensor_ID") %>% 
  left_join(TIR20190827_mosMask2, by = "Sensor_ID") %>% 
  left_join(TIR20190827_avgMask2, by = "Sensor_ID") %>% 
  left_join(TIR20190827_disMask2, by = "Sensor_ID") %>% 
  left_join(temps20190827, by = "Sensor_ID")

# read logger exif
logger_exif <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\loggersexif20190827.csv")
names(logger_exif)
logger_exif <- logger_exif %>% select(-1) %>% filter(Log_ID_txt != "") %>% rename(Sensor_ID = Log_ID_txt)

join20190827 <- join20190827 %>% left_join(logger_exif, by = "Sensor_ID")
join20190827 <- join20190827 %>% left_join(NDWI, by = "Sensor_ID")
join20190827 <- join20190827 %>% left_join(ortho, by = "Sensor_ID")


join20190827$wet_perc <- join20190827$AREA_mosMask/join20190827$AREA_mos
join20190827$wet_perc2 <- join20190827$AREA_mosMask2/join20190827$AREA_mos

join20190827$DIFF <- join20190827$MIN_avgMask - join20190827$mean_HOBO
join20190827$DIFF2 <- join20190827$MIN_avgMask2 - join20190827$mean_HOBO


# join20190827$DIFF <- join20190827$MIN - join20190827$mean_HOBO

# apply correction from cooler tests
# join20190827$MIN_cor <- join20190827$MIN * 0.7183615 + 7.9884541

avg_mod <- lm(mean_HOBO~MIN_avg, data = join20190827)

summary(lm(mean_HOBO~MIN_mos, data = join20190827)) #p = .84
summary(avg_mod) #p = .17
summary(lm(mean_HOBO~MIN_dis, data = join20190827)) #p = .63

summary(lm(mean_HOBO~MEAN_mos, data = join20190827)) # p = 96
summary(lm(mean_HOBO~MEAN_avg, data = join20190827)) # p = 71
summary(lm(mean_HOBO~MEAN_dis, data = join20190827)) # p = 94

# means from NDWI cells
summary(lm(mean_HOBO~MEAN_mosMask+SceneTemperature, data = join20190827)) # p = 77
summary(lm(mean_HOBO~MEAN_avgMask+ImagerTemperatureC, data = join20190827)) # p = 14
summary(lm(mean_HOBO~MEAN_disMask+ImagerTemperatureC, data = join20190827)) # p = 81

summary(lm(mean_HOBO~MIN_mosMask+ImagerTemperatureC, data = join20190827)) # p = 86
summary(lm(mean_HOBO~MIN_avgMask+ImagerTemperatureC, data = join20190827)) # p = 18
summary(lm(mean_HOBO~MIN_disMask+ImagerTemperatureC, data = join20190827)) # p = 65

# means from NDWI cells buffered
summary(lm(mean_HOBO~MEAN_mosMask2+ImagerTemperatureC, data = join20190827)) # p = 95, sig with imagtem
summary(lm(mean_HOBO~MEAN_avgMask2+ImagerTemperatureC, data = join20190827)) # p = 12, sig with imagtem
summary(lm(mean_HOBO~MEAN_disMask2+ImagerTemperatureC, data = join20190827)) # p = 89, sig with imagtem

# mins from NDWI cells buffered
summary(lm(mean_HOBO~MIN_mosMask2+ImagerTemperatureC, data = join20190827)) # p = 91, sig with imagtem r2 72
summary(lm(mean_HOBO~MIN_avgMask2+ImagerTemperatureC, data = join20190827)) # p = 17, sig with imagtem r2 73
summary(lm(mean_HOBO~MIN_disMask2+ImagerTemperatureC, data = join20190827)) # p = 75, sig with imagtem r2 72

# means from individual ortho photo
summary(lm(mean_HOBO~MEAN_ortho, data = join20190827)) # p = 91
summary(lm(mean_HOBO~MIN_ortho, data = join20190827)) # p = 85
plot(join20190827$MEAN_ortho, join20190827$mean_HOBO)
plot(join20190827$MIN_ortho, join20190827$mean_HOBO)

plot(join20190827$ImagerTemperatureC, join20190827$MEAN_ortho-join20190827$mean_HOBO)
plot(join20190827$ImagerTemperatureC, join20190827$MIN_ortho-join20190827$mean_HOBO)

summary(lm((MEAN_ortho-mean_HOBO)~ImagerTemperatureC, data = join20190827))
summary(lm((MEAN_mos-mean_HOBO)~ImagerTemperatureC, data = join20190827))
summary(lm((MEAN_dis-mean_HOBO)~ImagerTemperatureC, data = join20190827))
summary(lm((MEAN_avg-mean_HOBO)~ImagerTemperatureC, data = join20190827))
summary(lm((MEAN_mosMask2-mean_HOBO)~ImagerTemperatureC, data = join20190827))
summary(lm((MEAN_disMask2-mean_HOBO)~ImagerTemperatureC, data = join20190827))
summary(lm((MEAN_avgMask2-mean_HOBO)~ImagerTemperatureC, data = join20190827))

imagetempMod <- lm(mean_HOBO~ ImagerTemperatureC, data = join20190827)
plot(join20190827$ImagerTemperatureC, join20190827$mean_HOBO)
abline(imagetempMod, col = "blue")
summarystuff <- summary(imagetempMod)
text(x = 39.5, y = 16.4, labels = paste("r2 = ", round(summarystuff$adj.r.squared, 2), "\np = ", round(summarystuff$coefficients[2,4],2)))

LWIRtempMod <- lm(mean_HOBO~ MEAN_avgMask2, data = join20190827)
plot(join20190827$MEAN_avgMask2, join20190827$mean_HOBO)
abline(LWIRtempMod, col = "blue")
LWIRsummarystuff <- summary(LWIRtempMod)
text(x = 26, y = 17, labels = paste("r2 = ", round(LWIRsummarystuff$adj.r.squared, 2), "\np = ", round(LWIRsummarystuff$coefficients[2,4],2)))

hailmary <- join20190827 %>% filter(wet_perc2 > 0.2)

# means from NDWI cells buffered
summary(lm(mean_HOBO~MEAN_mosMask2, data = hailmary)) # p = 30
summary(lm(mean_HOBO~MEAN_avgMask2, data = hailmary)) # p = 39
summary(lm(mean_HOBO~MEAN_disMask2, data = hailmary)) # p = 35

# mins from NDWI cells buffered
summary(lm(mean_HOBO~MIN_mosMask2, data = hailmary)) # p = 33
summary(lm(mean_HOBO~MIN_avgMask2, data = hailmary)) # p = 41
summary(lm(mean_HOBO~MIN_disMask2, data = hailmary)) # p = 40

plot(join20190827$MEAN_avgMask2, join20190827$mean_HOBO)
text(join20190827$MEAN_avgMask2, join20190827$mean_HOBO, labels = join20190827$Sensor_ID)


# try dropping 2331280a, too much wood in plot
# join20190827filtnew <- join20190827 %>% filter(!Sensor_ID %in% c("2331280a", "10862879"))
join20190827filtnew <- join20190827 %>% filter(!Sensor_ID %in% c("10360382", "10862879")) %>% filter(wet_perc > .6)
join20190827filtnew <- join20190827 %>% filter(Sensor_ID %in% c("10360378",  "10524511",  "10862878",   "10862886",  "10862894",  "10862901a", "10862915",  "2331257"))
# remove highest error loggers
join20190827filtnew <- join20190827 %>% filter(!Sensor_ID %in% c('10360379', '10862923', '10360378', '10872720'))


summary(lm(mean_HOBO~MEAN_avgMask, data = join20190827filtnew)) # p .10
summary(lm(mean_HOBO~MEAN_mosMask, data = join20190827filtnew)) # p .65
summary(lm(mean_HOBO~MEAN_disMask, data = join20190827filtnew)) # p .71

summary(lm(mean_HOBO~MIN_avgMask, data = join20190827filtnew)) # p .11
summary(lm(mean_HOBO~MIN_mosMask, data = join20190827filtnew)) # p .95
summary(lm(mean_HOBO~MIN_disMask, data = join20190827filtnew)) # p .88

plot(join20190827filtnew$MEAN_avgMask, join20190827filtnew$mean_HOBO)
text(join20190827filtnew$MEAN_avgMask, join20190827filtnew$mean_HOBO, labels = join20190827filtnew$Sensor_ID)

plot(join20190827$DIFF, join20190827$mean_HOBO)
text(join20190827$DIFF, join20190827$mean_HOBO, labels = join20190827$Sensor_ID)


rmse(join20190827filtnew$mean_HOBO, join20190827filtnew$MIN_avgMask)

rmse(join20190827$mean_HOBO, join20190827$MIN_mos) # uncorrected is 4.6
rmse(join20190827$mean_HOBO, join20190827$MIN_avg) # uncorrected is 5.6
rmse(join20190827$mean_HOBO, join20190827$MIN_dis) # uncorrected is 4.4

rmse(join20190827$mean_HOBO, join20190827$MIN_mos * 0.7183615 + 7.9884541) # cooler corrected is 6.5
rmse(join20190827$mean_HOBO, join20190827$MIN_avg * 0.7183615 + 7.9884541) # cooler corrected is 7.2
rmse(join20190827$mean_HOBO, join20190827$MIN_dis * 0.7183615 + 7.9884541) # cooler corrected is 6.4

rmse(join20190827filtnew$mean_HOBO, predict(lm(mean_HOBO~MIN_avgMask, data = join20190827filtnew)))

plot(lm(mean_HOBO~MIN_avg, data = join20190827))
plot(join20190827$MIN_avg, join20190827$mean_HOBO)
plot(predict(lm(mean_HOBO~MIN_avg, data = join20190827)), join20190827$mean_HOBO)

rmse(join20190827$mean_HOBO, join20190827$MIN_cor) # corrected is 6.66

join20190827filt <- join20190827 %>% filter(Sensor_ID %in% c("10360378",  "10524511",  "10862878",  "10862879",  "10862886",  "10862894",  "10862901a", "10862915",  "2331257"))
rmse(join20190827filt$mean_HOBO, join20190827filt$MIN) # uncorrected is 4.36
rmse(join20190827filt$mean_HOBO, join20190827filt$MIN_cor) # corrected is 6.43
summary(lm(mean_HOBO~MIN,data = join20190827filt))

# drop the logger in plot with lots of wood
join20190827filt2 <- join20190827filt %>% filter(Sensor_ID != "10360378")
rmse(join20190827filt2$mean_HOBO, join20190827filt2$MIN) # uncorrected is 4.06
rmse(join20190827filt2$mean_HOBO, join20190827filt2$MIN_cor) # corrected is 6.23
summary(lm(mean_HOBO~MIN,data = join20190827filt2))
plot(join20190827filt2$MIN, join20190827filt2$mean_HOBO)

# old using 08/26 flight
# path to updated 80262019 zonal statistics "C:\Users\deadf\Box\thermal_projects\sfork\tables_figures\visible_logger20190826.csv"
# TIR2019 <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\logger_cal2019.csv")
TIR2019 <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\visible_logger20190826.csv")
imageTemp20190826 <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\ImageTemp08262019.csv")

names(imageTemp20190826)[2] <- "Sensor_ID"
imageTemp20190826 <- imageTemp20190826 %>% select(-1)

names(TIR2019)[2] <- "Sensor_ID"
TIR2019 <- TIR2019 %>% select(-1)
# TIR2019_mean <- TIR2019 %>% group_by(Sensor_ID) %>% summarise(mean_TIR = mean(TIR_Temp_C))
join2019 <- left_join(TIR2019, temps20190826, by = "Sensor_ID")
join2019 <- left_join(join2019, imageTemp20190826, by = "Sensor_ID")
join2019$mean_HOBO[match(temps2331257_08272019$Sensor_ID, join2019$Sensor_ID)] <- temps2331257_08272019$mean_HOBO

# drop the sensor from day 2 flight 2331257
join2019$wet_perc <- join2019$AREA/78.38216
join2019
summary(lm(mean_HOBO~MEAN,data = join2019)) #p 19
summary(lm(mean_HOBO~ImagerTemperatureC,data = join2019)) # p 17
plot(join2019$MEAN, join2019$mean_HOBO)
plot(join2019$ImagerTemperatureC, join2019$mean_HOBO)

join2019new <- join2019 %>% filter(!Sensor_ID %in% c("10360382"))
summary(lm(mean_HOBO~ImagerTemperatureC,data = join2019new)) # p 14
rmse(join2019new$mean_HOBO, join2019new$MEAN)
range(join2019new$MEAN - join2019new$mean_HOBO)
rmse(join2019new$mean_HOBO, predict(lm(mean_HOBO~ImagerTemperatureC,data = join2019new)))
plot(lm(mean_HOBO~ImagerTemperatureC,data = join2019new))

join2019 <- join2019 %>% filter(!Sensor_ID %in% c("2331257"))
plot(join2019$MEAN, join2019$mean_HOBO)
plot(join2019$ImagerTemperatureC, join2019$mean_HOBO)
summary(lm(mean_HOBO~MEAN,data = join2019)) # p .82
summary(lm(mean_HOBO~ImagerTemperatureC,data = join2019)) # p.01



join2019 <- join2019 %>% filter(!Sensor_ID %in% c("10862879","10360382", "10360378", "2331257"))
join2019 <- join2019 %>% filter(wet_perc >0.05)

plot(join2019$MEAN, join2019$mean_HOBO)
summary(lm(mean_HOBO~MEAN,data = join2019))
summary(lm(mean_HOBO~ImagerTemperatureC,data = join2019))
summary(lm(MEAN~ImagerTemperatureC,data = join2019))

# plot(join2019$)
mean(join2019$MIN)-mean(join2019$mean_HOBO)

join2019$MEAN - join2019$mean_HOBO
rmse(join2019$mean_HOBO, join2019$MEAN)
sqrt(mean((join2019$MEAN - join2019$mean_HOBO)^2))

join2019$mean_TIR- join2019$mean_HOBO
join2019filt <- join2019 %>% filter(Sensor_ID %in% c("10792428", "10862876", "10862886",
                                                     "10862878", "10360378",
                                                     "10524511", "10862901a", "10862915",
                                                     "10862879", "10862875a", "10862894"))
join2019filt2 <- join2019 %>% filter(!Sensor_ID %in% c("10792428","10862875a", 
                                                       "10360379","10360382", 
                                                        "10862876","10862923", 
                                                       "10862924","10792417", 
                                                       "10862908", "2331280a", "10872720"))

join2019filt3 <- join2019 %>% filter(!Sensor_ID %in% c("10792428","10862875a", 
                                                       "10360379","10360382", 
                                                       "10862876","10862923", 
                                                       "10862924","10792417", 
                                                       "10862908", "2331280a", "10872720",
                                                       "2331257"))

#### t test comparing mean temps between 8/26 8/27 ####

# filter to get data for dates and times of flights
# 8/26/2019 20:14 - 21:17, 9/4/2020 20:46 - 22:18, 7/15/2021 19:40- 21:12
hist(temps20190826$mean_HOBO, breaks = 5)
hist(temps20190827$mean_HOBO, breaks = 5)
shapiro.test((temps20190827$mean_HOBO))

t.test(temps20190826$mean_HOBO, temps20190827$mean_HOBO, paired = TRUE)

#### regresison 2019 ####
join2019filt2$mean_HOBO[9] <- temps20190827$mean_HOBO[32] # replace mean hobo temp for 8/26 with 8/27 for the sensor imaged the other day

summary(lm(mean_HOBO~mean_TIR, data = join2019))


summary(lm(mean_HOBO~mean_TIR, data = join2019filt))

summary(lm(mean_HOBO~mean_TIR, data = join2019filt2))
plot(join2019filt2$mean_TIR, join2019filt2$mean_HOBO)
abline(15.14425, 0.05971)
text(join2019filt2$mean_TIR, join2019filt2$mean_HOBO, join2019filt2$Sensor_ID)

TIR2019exif <- read.csv("C:\\Users\\deadf\\Box\\thermal_projects\\sfork\\tables_figures\\regress_sensors2019.csv")
names(TIR2019exif)

# grab Logger_ID, SceneTemperature, ImagerTemperatureC
TIR2019exif <- TIR2019exif %>% select(c('Log_ID_txt', 'SceneTemperature', 'ImagerTemperatureC'))
colnames(TIR2019exif)[1] <- "Sensor_ID"
colnames(TIR2019exif)
join2019filt2 <- left_join(join2019filt2, TIR2019exif)

shapiro.test(join2019filt2$mean_HOBO)
shapiro.test(join2019filt2$mean_TIR)
shapiro.test(join2019filt2$SceneTemperature)
shapiro.test(join2019filt2$ImagerTemperatureC)

library(lmtest)
mod1 <- lm(mean_HOBO~mean_TIR, data = join2019filt2)
summary(mod1) # p .03, adj r2 .43
dwtest(mod1)
shapiro.test(rstandard(mod1))

mod2 <- lm(mean_HOBO~mean_TIR + ImagerTemperatureC, data = join2019filt2)
summary(mod2) # p = .03 adj r2 = .57, covariates insignificant?
plot(mod2)
dwtest(mod2)
shapiro.test(rstandard(mod2))
plot(join2019filt2$mean_TIR, join2019filt2$ImagerTemperatureC)

mod3 <- lm(mean_HOBO~ImagerTemperatureC, data = join2019filt2)
summary(mod3) # p = 0.03, adj r2 = .46
dwtest(mod3)
shapiro.test(rstandard(mod3))

mod4 <- lm(mean_HOBO~SceneTemperature, data = join2019filt2)
summary(mod4) # p = .07 
dwtest(mod4)
shapiro.test(rstandard(mod4))

mod5 <- lm(mean_HOBO~mean_TIR, data = join2019filt2)
summary(mod5)

mod6 <- lm(mean_HOBO~mean_TIR*ImagerTemperatureC, data = join2019filt2)
summary(mod6)
plot(mod6)

plot(join2019filt2$mean_TIR*join2019filt2$ImagerTemperatureC, join2019filt2$mean_HOBO)

mod8 <- lm(mean_HOBO~mean_TIR*SceneTemperature, data = join2019filt2)
summary(mod8)

mod7 <- lm(SceneTemperature~ImagerTemperatureC, data = join2019filt2)
summary(mod7)

join2019 <- left_join(join2019, TIR2019exif)


temps20200904 <- data_frame %>% filter(as.Date(DATE, "%m/%d/%y") == "2020-09-04")
temps20210715 <- data_frame %>% filter(as.Date(DATE, "%m/%d/%y") == "2021-07-15")

unique(temps20190826$Sensor_ID) #34 sensors
unique(temps20200904$Sensor_ID) #4 sensors "2331280b"  "10792432"  "10792435"  "10862875b"
unique(temps20210715$Sensor_ID) #1 sensor "10792432"
# three_flights <- data_frame %>% filter(DATE %in% c(as.Date("08/26/19", "%m/%d/%y"),as.Date("09/04/20", "%m/%d/%y"), as.Date("07/15/21", "%m/%d/%y")))

# then calc mean temperature during flight using groupby and summarize
temps20190826 %>% group_by(Sensor_ID, DATE) %>% summarise(Mean_c = mean(TEMP_r))

write.csv(join2019filt2, file = "datacal.csv")

join2019filt4 <- join2019filt2[-9,] # this removes the logger that throws off the regression (from the second day)
summary(lm(mean_HOBO~mean_TIR, data = join2019filt4)) # not sig
summary(lm(mean_HOBO~SceneTemperature, data = join2019filt4)) # not sig
summary(lm(mean_HOBO~mean_TIR + ImagerTemperatureC, data = join2019filt4)) #not sig
(bestMod <- summary(lm(mean_HOBO~ImagerTemperatureC, data = join2019filt4))) # sig, r2 = .43
sqrt(mean(bestMod$residuals^2)) # RMSE = 0.08
sqrt(mean((join2019filt4$mean_HOBO- join2019filt4$mean_TIR)^2)) # before correction rmse is 1.9
join2019filt4$mean_HOBO- join2019filt4$mean_TIR

summary(lm(mean_HOBO~mean_TIR*ImagerTemperatureC, data = join2019filt4)) # not sig

plot(join2019filt4$mean_TIR, join2019filt4$mean_HOBO)
plot(join2019filt4$ImagerTemperatureC, join2019filt4$mean_HOBO)
plot(join2019filt4$SceneTemperature, join2019filt4$mean_HOBO)
