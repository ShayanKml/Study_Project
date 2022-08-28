setwd("D:\\1-Master of HSE\\2-Second Semester\\Study project\\First raw data\\Sorted")
getwd()



install.packages("SPEI")
library(SPEI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

wernersbach_data_daily = read.table("Wernersbach_2_EC.Towers_daily_2017.csv", sep = ",", dec = ".", header = T)
wernersbach_data = read.table("Wernersbach_2_EC.Towers_monthly_1968-2019.csv", sep = ",", dec = ".", header = T)

wernersbach_data$ET_thorn = thornthwaite(Tave = wernersbach_data$Temperature.Mean_Avg_DegC,
                                         lat = 50.966)

wernersbach_data$ET_har = hargreaves(Tmin = wernersbach_data$Temperature.Min_Avg_DegC,
                                     Tmax = wernersbach_data$Temperature.Max_Avg_DegC,
                                     Pre = wernersbach_data$Precipitation_Corrected_mm,
                                     lat = 50.966)

AtmosphericPressure = 101.325*exp(-(9.80665*0.02896968*330)/(288.16*8.314462618))

wernersbach_data$ET_pen = penman(Tmin = wernersbach_data$Temperature.Min_Avg_DegC,
                                 Tmax = wernersbach_data$Temperature.Max_Avg_DegC,
                                 U2 = wernersbach_data$Windspeed42m_Avg_ms.1,
                                 Rs = wernersbach_data$GlobalRadiation_Avg_Wm.2*(0.0864),
                                 P0 = 101.325,
                                 P = AtmosphericPressure,
                                 ed = wernersbach_data$VapourPressure_Avg_kPa,
                                 z = 330,
                                 crop = "tall",
                                 lat = 50.966)


##Plotting
##change the format to date:
wernersbach_data$ValueDateTime = as.Date(wernersbach_data$ValueDateTime,
                                         format = "%d.%m.%Y")

wernersbach_data_daily$ValueDateTime = as.Date(wernersbach_data_daily$ValueDateTime,
                                               format = "%m/%d/%Y")
##optional: exract months:
date_months = format(wernersbach_data$ValueDateTime, "%m")
date_years = format(wernersbach_data$ValueDateTime, "%Y")
date_days2017 = format(wernersbach_data_daily$ValueDateTime, "%d")
date_months2017 = format(wernersbach_data_daily$ValueDateTime, "%m")


##plot the data:

##daily
boxplot(wernersbach_data_daily$ReferenceEvapotranspiration_Avg_mm)
# Q <- quantile(wernersbach_data_daily$ReferenceEvapotranspiration_Avg_mm,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data_daily$ReferenceEvapotranspiration_Avg_mm)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data_daily <- subset(wernersbach_data_daily,
#                         wernersbach_data_daily$ReferenceEvapotranspiration_Avg_mm > (Q[1] - 1.5*iqr) & 
#                          wernersbach_data_daily$ReferenceEvapotranspiration_Avg_mm< (Q[2]+1.5*iqr))

plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$ReferenceEvapotranspiration_Avg_mm,
     xlab = "Days of the year 2017",
     ylab = "Refrence_ET")

##daily
boxplot(wernersbach_data_daily$Evapotranspitation.DE.Tha_Sum_mm)
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Evapotranspitation.DE.Tha_Sum_mm,
     xlab = "Days of the year 2017",
     ylab = "Evapotranspitation.DE.Tha_Sum_mm")

##daily
boxplot(wernersbach_data_daily$Evapotranspitation.DE.Hzd_Sum_mm)
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Evapotranspitation.DE.Hzd_Sum_mm,
     xlab = "Days of the year 2017",
     ylab = "Evapotranspitation.DE.Hzd_Sum_mm")

##daily_preciitation needs correction
boxplot(wernersbach_data_daily$Precipitation_Corrected_mm)

outliers <- boxplot(wernersbach_data_daily$Precipitation_Corrected_mm, plot=FALSE)$out
Q <- quantile(wernersbach_data_daily$Precipitation_Corrected_mm,
              probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(wernersbach_data_daily$Precipitation_Corrected_mm)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data_daily$Precipitation_Corrected_mm[
  wernersbach_data_daily$Precipitation_Corrected_mm > up] <- up
wernersbach_data_daily$Precipitation_Corrected_mm[
  wernersbach_data_daily$Precipitation_Corrected_mm < low] <- low

plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Precipitation_Corrected_mm,
     xlab = "Days of the year 2017",
     ylab = "Precipitation_Corrected_mm")

##daily_runoff needs correction
boxplot(wernersbach_data_daily$Runoff_Avg_mm)

outliers <- boxplot(wernersbach_data_daily$Runoff_Avg_mm, plot=FALSE)$out
Q <- quantile(wernersbach_data_daily$Runoff_Avg_mm,
              probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(wernersbach_data_daily$Runoff_Avg_mm)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data_daily$Runoff_Avg_mm[
  wernersbach_data_daily$Runoff_Avg_mm > up] <- up
wernersbach_data_daily$Runoff_Avg_mm[
  wernersbach_data_daily$Runoff_Avg_mm < low] <- low

plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Runoff_Avg_mm,
     xlab = "Days of the year 2017",
     ylab = "Runoff_Avg_mm")

##daily
boxplot(wernersbach_data_daily$GlobalRadiation_Avg_Wm.2)

plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$GlobalRadiation_Avg_Wm.2,
     xlab = "Days of the year 2017",
     ylab = "GlobalRadiation_Avg_Wm.2")

##daily
boxplot(wernersbach_data_daily$VapourPressure_Avg_kPa)

plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$VapourPressure_Avg_kPa,
     xlab = "Days of the year 2017",
     ylab = "VapourPressure_Avg_kPa")

##daily
boxplot(wernersbach_data_daily$Temp.min)

outliers <- boxplot(wernersbach_data_daily$Temp.min, plot=FALSE)$out
Q <- quantile(wernersbach_data_daily$Temp.min,
              probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(wernersbach_data_daily$Temp.min)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data_daily$Temp.min[
  wernersbach_data_daily$Temp.min > up] <- up
wernersbach_data_daily$Temp.min[
  wernersbach_data_daily$Temp.min < low] <- low

plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Temp.min,
     xlab = "Days of the year 2017",
     ylab = "Temp.min")

##daily
boxplot(wernersbach_data_daily$Temperature.Max_Avg_DegC)
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Temperature.Max_Avg_DegC,
     xlab = "Days of the year 2017",
     ylab = "Temperature.Max_Avg_DegC")

##daily
boxplot(wernersbach_data_daily$Temperature.Mean_Avg_DegC)
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Temperature.Mean_Avg_DegC,
     xlab = "Days of the year 2017",
     ylab = "Temperature.Max_Avg_DegC")

##________________________________________________________##
##________________________________________________________##





#monthly
boxplot(wernersbach_data$ReferenceEvapotranspiration_Avg_mm)

##outliers <- boxplot(wernersbach_data$ReferenceEvapotranspiration_Avg_mm, plot=FALSE)$out
##Q <- quantile(wernersbach_data$ReferenceEvapotranspiration_Avg_mm,
##              probs=c(.25, .75), na.rm = FALSE)
##iqr <- IQR(wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
##up <-  Q[2]+1.5*iqr # Upper Range  
##low<- Q[1]-1.5*iqr # Lower Range
##wernersbach_data <- subset(wernersbach_data,
##        wernersbach_data$ReferenceEvapotranspiration_Avg_mm > (Q[1] - 1.5*iqr) & 
##         wernersbach_data$ReferenceEvapotranspiration_Avg_mm < (Q[2]+1.5*iqr))

plot(date_months,
     wernersbach_data$ReferenceEvapotranspiration_Avg_mm,
     xlab = "Months of the year",
     ylab = "Refrence_ET")

##Monthly
boxplot(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm)

##outliers <- boxplot(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm, plot=FALSE)$out
##Q <- quantile(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm,
##              probs=c(.25, .75), na.rm = FALSE)
##iqr <- IQR(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm)
##up <-  Q[2]+1.5*iqr # Upper Range  
##low<- Q[1]-1.5*iqr # Lower Range
##wernersbach_data <- subset(wernersbach_data,
##                           wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm > (Q[1] - 1.5*iqr) & 
##                             wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm < (Q[2]+1.5*iqr))

plot(date_months,
     wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm,
     xlab = "Months of the year",
     ylab = "Evapotranspitation.DE.Tha_Sum_mm")

plot(date_months,
     wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm,
     xlab = "Months of the year",
     ylab = "Evapotranspitation.DE.Hzd_Sum_mm")

##monthly
boxplot(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm)

# outliers <- boxplot(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm, plot=FALSE)$out
# Q <- quantile(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm < (Q[2]+1.5*iqr))

plot(date_months,
     wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm,
     xlab = "Months of the year",
     ylab = "Evapotranspitation.DE.Hzd_Sum_mm")


##Monthly- precipitation need correction
boxplot(wernersbach_data$Precipitation_Corrected_mm)

outliers <- boxplot(wernersbach_data$Precipitation_Corrected_mm, plot=FALSE)$out
outliers
summary(wernersbach_data$Precipitation_Corrected_mm)
Q <- quantile(wernersbach_data$Precipitation_Corrected_mm,
              probs=c(.25, .75), na.rm = FALSE)

iqr <- IQR(wernersbach_data$Precipitation_Corrected_mm)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data$Precipitation_Corrected_mm[
  wernersbach_data$Precipitation_Corrected_mm > up] <- up
wernersbach_data$Precipitation_Corrected_mm[
  wernersbach_data$Precipitation_Corrected_mm < low] <- low

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$Precipitation_Corrected_mm,
     col = "blue",
     xlab = "Months of the year",
     ylab = "Precipitation_Corrected_mm")

##Monthly- Runoff needs correction
boxplot(wernersbach_data$Runoff_Avg_mm)
outliers <- boxplot(wernersbach_data$Runoff_Avg_mm, plot=FALSE)$out
Q <- quantile(wernersbach_data$Runoff_Avg_mm,
              probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(wernersbach_data$Runoff_Avg_mm)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data$Runoff_Avg_mm[
  wernersbach_data$Runoff_Avg_mm > up] <- up
wernersbach_data$Runoff_Avg_mm[
  wernersbach_data$Runoff_Avg_mm < low] <- low

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$Runoff_Avg_mm,
     xlab = "Months of the year",
     ylab = "Runoff_Avg_mm")

##Monthly
boxplot(wernersbach_data$GlobalRadiation_Avg_Wm.2)

# outliers <- boxplot(wernersbach_data$GlobalRadiation_Avg_Wm.2, plot=FALSE)$out
# Q <- quantile(wernersbach_data$GlobalRadiation_Avg_Wm.2,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$GlobalRadiation_Avg_Wm.2)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$GlobalRadiation_Avg_Wm.2 > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$GlobalRadiation_Avg_Wm.2 < (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$GlobalRadiation_Avg_Wm.2,
     xlab = "Months of the year",
     ylab = "GlobalRadiation_Avg_Wm.2")

#Monthly
boxplot(wernersbach_data$VapourPressure_Avg_kPa)

# outliers <- boxplot(wernersbach_data$VapourPressure_Avg_kPa, plot=FALSE)$out
# Q <- quantile(wernersbach_data$VapourPressure_Avg_kPa,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$VapourPressure_Avg_kPa)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$VapourPressure_Avg_kPa > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$VapourPressure_Avg_kPa < (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$VapourPressure_Avg_kPa,
     xlab = "Months of the year",
     ylab = "VapourPressure_Avg_kPa")

#Monthly
# boxplot(wernersbach_data$ET_thorn)
# 
# # outliers <- boxplot(wernersbach_data$ET_thorn, plot=FALSE)$out
# # Q <- quantile(wernersbach_data$ET_thorn,
# #               probs=c(.25, .75), na.rm = FALSE)
# # iqr <- IQR(wernersbach_data$ET_thorn)
# # up <-  Q[2]+1.5*iqr # Upper Range  
# # low<- Q[1]-1.5*iqr # Lower Range
# # wernersbach_data <- subset(wernersbach_data,
# #                            wernersbach_data$ET_thorn > (Q[1] - 1.5*iqr) & 
# #                              wernersbach_data$ET_thorn < (Q[2]+1.5*iqr))
# 
# plot(wernersbach_data$ValueDateTime,
#      wernersbach_data$ET_thorn,
#      xlab = "Months of the year",
#      ylab = "ET_thorn")

##hargreaves
# boxplot(wernersbach_data$ET_har)
# 
# # outliers <- boxplot(wernersbach_data$ET_har, plot=FALSE)$out
# # Q <- quantile(wernersbach_data$ET_har,
# #               probs=c(.25, .75), na.rm = FALSE)
# # iqr <- IQR(wernersbach_data$ET_har)
# # up <-  Q[2]+1.5*iqr # Upper Range  
# # low<- Q[1]-1.5*iqr # Lower Range
# # wernersbach_data <- subset(wernersbach_data,
# #                            wernersbach_data$ET_har > (Q[1] - 1.5*iqr) & 
# #                              wernersbach_data$ET_har < (Q[2]+1.5*iqr))
# 
# plot(wernersbach_data$ValueDateTime,
#      wernersbach_data$ET_har,
#      xlab = "Months of the year",
#      ylab = "ET_hargreaves")


##Penman
# boxplot(wernersbach_data$ET_pen)
# 
# # outliers <- boxplot(wernersbach_data$ET_pen, plot=FALSE)$out
# # Q <- quantile(wernersbach_data$ET_pen,
# #               probs=c(.25, .75), na.rm = FALSE)
# # iqr <- IQR(wernersbach_data$ET_pen)
# # up <-  Q[2]+1.5*iqr # Upper Range  
# # low<- Q[1]-1.5*iqr # Lower Range
# # wernersbach_data <- subset(wernersbach_data,
# #                            wernersbach_data$ET_pen > (Q[1] - 1.5*iqr) & 
# #                              wernersbach_data$ET_pen < (Q[2]+1.5*iqr))
# 
# plot(wernersbach_data$ValueDateTime,
#      wernersbach_data$ET_pen,
#      xlab = "Months of the year",
#      ylab = "ET_penman")



##Tha flux ET:
boxplot(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm)
# 
# outliers <- boxplot(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm, plot=FALSE)$out
# Q <- quantile(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm[
#   wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm > up] <- up
# wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm[
#   wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm < low] <- low

##Hzd flux ET:
boxplot(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm)

# outliers <- boxplot(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm, plot=FALSE)$out
# Q <- quantile(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm[
#   wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm > up] <- up
# wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm[
#   wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm < low] <- low


##windspeed- needs correction
boxplot(wernersbach_data$Windspeed42m_Avg_ms.1)
outliers <- boxplot(wernersbach_data$Windspeed42m_Avg_ms.1, plot=FALSE)$out
Q <- quantile(wernersbach_data$Windspeed42m_Avg_ms.1,
              probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(wernersbach_data$Windspeed42m_Avg_ms.1)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data$Windspeed42m_Avg_ms.1[
  wernersbach_data$Windspeed42m_Avg_ms.1 > up] <- up
wernersbach_data$Windspeed42m_Avg_ms.1[
  wernersbach_data$Windspeed42m_Avg_ms.1 < low] <- low

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$Windspeed42m_Avg_ms.1,
     xlab = "Months of the year",
     ylab = "Windspeed42m_Avg_ms.1")


##daily
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Windspeed42m_Avg_ms.1,
     xlab = "Days of the year 2017",
     ylab = "Windspeed42m_Avg_ms.1")

##Monthly
boxplot(wernersbach_data$Temperature.Min_Avg_DegC)

# outliers <- boxplot(wernersbach_data$Temperature.Min_Avg_DegC, plot=FALSE)$out
# Q <- quantile(wernersbach_data$Temperature.Min_Avg_DegC,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$Temperature.Min_Avg_DegC)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$Temperature.Min_Avg_DegC > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$Temperature.Min_Avg_DegC < (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$Temperature.Min_Avg_DegC,
     xlab = "Months of the year",
     ylab = "Temperature.Min_Avg_DegC")


##daily
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Temp.min,
     xlab = "Days of the year 2017",
     ylab = "Temp.min")


##Monthly
boxplot(wernersbach_data$Temperature.Max_Avg_DegC)

# outliers <- boxplot(wernersbach_data$Temperature.Max_Avg_DegC, plot=FALSE)$out
# Q <- quantile(wernersbach_data$Temperature.Max_Avg_DegC,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$Temperature.Max_Avg_DegC)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$Temperature.Max_Avg_DegC > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$Temperature.Max_Avg_DegC< (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$Temperature.Max_Avg_DegC,
     xlab = "Months of the year",
     ylab = "Temperature.Max_Avg_DegC")


##daily
plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Temperature.Max_Avg_DegC,
     xlab = "Days of the year 2017",
     ylab = "Temperature.Max_Avg_DegC")


##Monthly
boxplot(wernersbach_data$Temperature.Mean_Avg_DegC)

# outliers <- boxplot(wernersbach_data$Temperature.Mean_Avg_DegC, plot=FALSE)$out
# Q <- quantile(wernersbach_data$Temperature.Mean_Avg_DegC,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$Temperature.Mean_Avg_DegC)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$Temperature.Mean_Avg_DegC > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$Temperature.Mean_Avg_DegC< (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$Temperature.Mean_Avg_DegC,
     xlab = "Months of the year",
     ylab = "Mean Temperture")

##daily
boxplot(wernersbach_data_daily$Temperature.Mean_Avg_DegC)
Q <- quantile(wernersbach_data_daily$Temperature.Mean_Avg_DegC,
              probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(wernersbach_data_daily$Temperature.Mean_Avg_DegC)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data_daily <- subset(wernersbach_data_daily,
                                 wernersbach_data_daily$Temperature.Mean_Avg_DegC > (Q[1] - 1.5*iqr) & 
                                   wernersbach_data_daily$Temperature.Mean_Avg_DegC< (Q[2]+1.5*iqr))


plot(wernersbach_data_daily$ValueDateTime,
     wernersbach_data_daily$Temperature.Mean_Avg_DegC,
     xlab = "Days of the year 2017",
     ylab = "Temperature.Mean_Avg_DegC")



##Calculation after removing outliers
wernersbach_data$ET_thorn = thornthwaite(Tave = wernersbach_data$Temperature.Mean_Avg_DegC,
                                         lat = 50.966)

wernersbach_data$ET_har = hargreaves(Tmin = wernersbach_data$Temperature.Min_Avg_DegC,
                                     Tmax = wernersbach_data$Temperature.Max_Avg_DegC,
                                     Pre = wernersbach_data$Precipitation_Corrected_mm,
                                     lat = 50.966)

AtmosphericPressure = 101.325*exp(-(9.80665*0.02896968*330)/(288.16*8.314462618))

wernersbach_data$ET_pen = penman(Tmin = wernersbach_data$Temperature.Min_Avg_DegC,
                                 Tmax = wernersbach_data$Temperature.Max_Avg_DegC,
                                 U2 = wernersbach_data$Windspeed42m_Avg_ms.1,
                                 Rs = wernersbach_data$GlobalRadiation_Avg_Wm.2*(0.0864),
                                 P0 = 101.325,
                                 P = AtmosphericPressure,
                                 ed = wernersbach_data$VapourPressure_Avg_kPa,
                                 z = 330,
                                 crop = "tall",
                                 lat = 50.966)



##

boxplot(wernersbach_data$ET_thorn)

# outliers <- boxplot(wernersbach_data$ET_thorn, plot=FALSE)$out
# Q <- quantile(wernersbach_data$ET_thorn,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$ET_thorn)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$ET_thorn > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$ET_thorn < (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$ET_thorn,
     xlab = "Months of the year",
     ylab = "ET_thorn")

##hargreaves
boxplot(wernersbach_data$ET_har)

#  outliers <- boxplot(wernersbach_data$ET_har, plot=FALSE)$out
#  Q <- quantile(wernersbach_data$ET_har,
#               probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$ET_har)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                            wernersbach_data$ET_har > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$ET_har < (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$ET_har,
     xlab = "Months of the year",
     ylab = "ET_hargreaves")

##Penman
boxplot(wernersbach_data$ET_pen)

# outliers <- boxplot(wernersbach_data$ET_pen, plot=FALSE)$out
# Q <- quantile(wernersbach_data$ET_pen,
#              probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(wernersbach_data$ET_pen)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# wernersbach_data <- subset(wernersbach_data,
#                           wernersbach_data$ET_pen > (Q[1] - 1.5*iqr) & 
#                              wernersbach_data$ET_pen < (Q[2]+1.5*iqr))

plot(wernersbach_data$ValueDateTime,
     wernersbach_data$ET_pen,
     xlab = "Months of the year",
     ylab = "ET_penman")

##ET_global
wernersbach_data$ET_global
boxplot(wernersbach_data$ET_global)
outliers <- boxplot(wernersbach_data$ET_global, plot=FALSE)$out
outliers
Q <- quantile(wernersbach_data$ET_global,
              probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(wernersbach_data$ET_global)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
wernersbach_data$ET_global[
  wernersbach_data$ET_global > up] <- up
wernersbach_data$ET_global[
  wernersbach_data$ET_global < low] <- low

# 
# ##exporting data to excel
# install.packages("writexl")
# library(writexl)
# write_xlsx(wernersbach_data, "D:\\1-Master of HSE\\2-Second Semester\\Study project\\First raw data\\Sorted\\newwerbach.xlsx")
# write.csv(wernersbach_data, "D:\\1-Master of HSE\\2-Second Semester\\Study project\\First raw data\\Sorted\\newwerbach.csv")


##exporting dataframe
# write.csv(wernersbach_data,"D:\\1-Master of HSE\\2-Second Semester\\Study project\\First raw data\\Sorted\\monthlywithPET.csv", row.names = F)









##Histogram
hist(wernersbach_data$ET_pen)
hist(log(wernersbach_data$ET_pen))
hist(1/(wernersbach_data$ET_pen))
hist(wernersbach_data$ET_har)
hist(wernersbach_data$ET_thorn)
hist(wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
hist(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm)
hist(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm)
hist(wernersbach_data$ET_global)

##Boxplots
par(mfrow = c(3,3))
boxplot(wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
boxplot(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm)
boxplot(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm)
boxplot(wernersbach_data$ET_thorn)
boxplot(wernersbach_data$ET_har)
boxplot(wernersbach_data$ET_pen)
boxplot(wernersbach_data$ET_global)

##Check if there is a significant difference between Ref_ET
###and other ETs => we have more than two groups => ANOVA test
### How anova test works? --> text file

ET = c(wernersbach_data$ET_thorn,
       wernersbach_data$ET_har,
       wernersbach_data$ET_pen )

Methods = c(rep("ET_thorn", 624),
            rep("ET_hargreaves", 624),
            rep("ET_penman", 624))
My_data = cbind(ET, as.factor(Methods))
par(mfrow = c(1,1))
boxplot(ET ~ as.factor(Methods), data = My_data, cex.axis = 1)
plot(ET ~ as.factor(Methods), data = My_data, cex.axis = 1)

My_data = as.data.frame(My_data)
ggplot(data = My_data, aes(x = as.factor(Methods), y = ET)) +
  geom_jitter(col = "dark blue", alpha = 0.25) + geom_boxplot(alpha = .8) +
  xlab("Methods") + theme(axis.text.x = element_text(size = 20),
                          axis.title.x = element_text(size = 20),
                          axis.text.y = element_text(size = 30),
                          axis.title.y = element_text(size = 20),
                          plot.title = element_text(hjust = .5, size = 25)) +
  ylab("Potential Evapotranspiration (mm)") + ggtitle("Potential Evapotranspiration Boxplots")


aggregate(ET ~ Methods, data = My_data, mean)
aggregate(ET ~ Methods, data = My_data, var)
aggregate(ET ~ Methods, data = My_data, length)
My_data = as.data.frame(My_data)
ANOVA_result = aov(ET ~ Methods, data = My_data)
summary(ANOVA_result)
tukey.test = TukeyHSD(ANOVA_result, conf.level = 0.95)
par(mar=c(5.1,12,4.1,2.1))
plot(tukey.test, cex.axis = 1, las = 1)
t.test(wernersbach_data$ReferenceEvapotranspiration_Avg_mm,wernersbach_data$ET_)
##Now that we have found out our Ref_ET is not significantly different
##from ET_hargreaves and they are valid we go for correlation tests:
##How Et depends on other climate parameters
##correlation tests
plot(wernersbach_data$Temperature.Mean_Avg_DegC,
     wernersbach_data$ET_pen,
     xlab = "T_mean(C)", ylab = "ET_penman(mm)")
cor.test(wernersbach_data$Temperature.Mean_Avg_DegC,
         wernersbach_data$ET_pen)

plot(wernersbach_data$Temperature.Max_Avg_DegC,
     wernersbach_data$ET_pen,
     xlab = "T_max(C)", ylab = "ET_penman(mm)")

cor.test(wernersbach_data$Temperature.Max_Avg_DegC,
         wernersbach_data$ET_pen)

plot(wernersbach_data$Temperature.Min_Avg_DegC,
     wernersbach_data$ET_pen,
     xlab = "T_min(C)", ylab = "ET_penman(mm)")

cor.test(wernersbach_data$Temperature.Min_Avg_DegC,
         wernersbach_data$ET_pen)

plot(wernersbach_data$Precipitation_Corrected_mm,
     wernersbach_data$ET_pen,
     xlab = "P(mm)", ylab = "ET_penman(mm)")

plot(log(wernersbach_data$Precipitation_Corrected_mm),
     wernersbach_data$ET_pen,
     xlab = "P(mm)", ylab = "ET_penman(mm)")

cor.test(wernersbach_data$Precipitation_Corrected_mm,
         wernersbach_data$ET_pen)

plot(wernersbach_data$Runoff_Avg_mm,
     wernersbach_data$ET_pen,
     xlab = "Runoff(mm)", ylab = "ET_penman(mm)")

plot(log(wernersbach_data$Runoff_Avg_mm),
     wernersbach_data$ET_pen,
     xlab = "Log(Runoff(mm))", ylab = "ET_penman(mm)")

cor.test(log(wernersbach_data$Runoff_Avg_mm),
         wernersbach_data$ET_pen)

plot(wernersbach_data$GlobalRadiation_Avg_Wm.2,
     wernersbach_data$ET_pen,
     xlab = "Rn", ylab = "ET_penman(mm)")

cor.test(wernersbach_data$GlobalRadiation_Avg_Wm.2,
         wernersbach_data$ET_pen)

plot(wernersbach_data$Windspeed42m_Avg_ms.1,
     wernersbach_data$ET_pen,
     xlab = "U(m/s)", ylab = "ET_penman(mm)")

plot(log(wernersbach_data$Windspeed42m_Avg_ms.1),
     wernersbach_data$ET_pen,
     xlab = "U(m/s)", ylab = "ET_penman(mm)")

cor.test(wernersbach_data$Windspeed42m_Avg_ms.1,
         wernersbach_data$ET_pen)

cor.test(log(wernersbach_data$Windspeed42m_Avg_ms.1),
         wernersbach_data$ET_pen)

plot(wernersbach_data$VapourPressure_Avg_kPa,
     wernersbach_data$ET_pen,
     xlab = "Vapour pressure(kPa)", ylab = "ET_penman(mm)")

cor.test(wernersbach_data$VapourPressure_Avg_kPa,
         wernersbach_data$ET_pen)




##
library(ggplot2)
#when we have {} we shoul select all before running
substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
date_years2 = substrRight(date_years, 2)

p = ggplot(data = wernersbach_data, aes(
  x = date_years,
  y = ReferenceEvapotranspiration_Avg_mm,
  color = factor(date_years)))
p + geom_point(size = .8) + geom_boxplot(alpha = .5) +
  theme(axis.text.x = element_text(size = 5.7))

s = ggplot(data = wernersbach_data, aes(
  x = ReferenceEvapotranspiration_Avg_mm))
s + geom_histogram( binwidth = 10,color = "Blue", fill = "White")

h = ggplot(data = wernersbach_data,
           aes(x = ReferenceEvapotranspiration_Avg_mm))
h + geom_histogram(binwidth = 10,
                   color = "White")



##Showing trends in data for fisr presentation for averages:
##Tmean_average:
library(lubridate)

Tmean_average = aggregate(wernersbach_data$Temperature.Mean_Avg_DegC ~ 
                            date_years, wernersbach_data, mean)
colnames(Tmean_average) = c("Year", "Tmean_average")
Tmean_average$Year = as.Date(Tmean_average$Year, format = "%Y")
Tmean_average$Year = year(Tmean_average$Year)
ggplot(data = Tmean_average, aes(x = Tmean_average)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(Tmean_average$Year, Tmean_average$Tmean_average)
shapiro.test(Tmean_average$Tmean_average)
TMaxA = ggplot(data = Tmean_average,
               aes(x = Year,
                   y = Tmean_average))
TMaxA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                     axis.title.x = element_text(size = 20),
                                     axis.text.y = element_text(size = 30),
                                     axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)


###Tmax_average:
library(lubridate)

Tmax_average = aggregate(wernersbach_data$Temperature.Max_Avg_DegC ~ 
                           date_years, wernersbach_data, mean)
colnames(Tmax_average) = c("Year", "Tmax_ave")
Tmax_average$Year = as.Date(Tmax_average$Year, format = "%Y")
Tmax_average$Year = year(Tmax_average$Year)
ggplot(data = Tmax_average, aes(x = Tmax_ave)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(Tmax_average$Year, Tmax_average$Tmax_ave)
shapiro.test(Tmax_average$Tmax_ave)
TMaxA = ggplot(data = Tmax_average,
               aes(x = Year,
                   y = Tmax_ave))
TMaxA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                     axis.title.x = element_text(size = 20),
                                     axis.text.y = element_text(size = 30),
                                     axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

###Tmin_average:
Tmin_average = aggregate(wernersbach_data$Temperature.Min_Avg_DegC ~ 
                           date_years, wernersbach_data, mean)

colnames(Tmin_average) = c("Year", "Tmin_ave")

Tmin_average$Year = as.Date(Tmin_average$Year, format = "%Y")
Tmin_average$Year = year(Tmin_average$Year)

ggplot(data = Tmin_average, aes(x = Tmin_ave)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")

qqplot(Tmax_average$Year, Tmax_average$Tmax_ave)

shapiro.test(Tmax_average$Tmax_ave)

TMinA = ggplot(data = Tmin_average,
               aes(x = Year,
                   y = Tmin_ave))
TMinA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                     axis.title.x = element_text(size = 20),
                                     axis.text.y = element_text(size = 30),
                                     axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

###Globar Radiation:
GlobarR_average = aggregate(wernersbach_data$GlobalRadiation_Avg_Wm.2 ~ 
                              date_years, wernersbach_data, mean)

colnames(GlobarR_average) = c("Year", "Global.Radiation.Average")


GlobarR_average$Year = as.Date(GlobarR_average$Year, format = "%Y")
GlobarR_average$Year = year(GlobarR_average$Year)

ggplot(data = GlobarR_average, aes(x = Global.Radiation.Average)) +
  geom_histogram(binwidth = 4, color = "white", fill = "black")

qqplot(GlobarR_average$Year, GlobarR_average$Global.Radiation.Average)

shapiro.test(GlobarR_average$Global.Radiation.Average)

GRA = ggplot(data = GlobarR_average,
             aes(x = Year,
                 y = Global.Radiation.Average))
GRA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                   axis.title.x = element_text(size = 20),
                                   axis.text.y = element_text(size = 30),
                                   axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

###Precipitation:
Prep_average = aggregate(wernersbach_data$Precipitation_Corrected_mm ~ 
                           date_years, wernersbach_data, mean)

colnames(Prep_average) = c("Year", "Precipitation")

Prep_average$Year = as.Date(Prep_average$Year, format = "%Y")
Prep_average$Year = year(Prep_average$Year)

ggplot(data = Prep_average, aes(x = Precipitation)) +
  geom_histogram(binwidth = 4, color = "white", fill = "black")

qqplot(Prep_average$Year, Prep_average$Precipitation)

shapiro.test(Prep_average$Precipitation)

PrepA = ggplot(data = Prep_average,
               aes(x = Year,
                   y = Precipitation))
PrepA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                     axis.title.x = element_text(size = 20),
                                     axis.text.y = element_text(size = 30),
                                     axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

###Vapour pressure:

VP_average = aggregate(wernersbach_data$VapourPressure_Avg_kPa ~ 
                         date_years, wernersbach_data, mean)

colnames(VP_average) = c("Year", "Vapour_Pressure")

VP_average$Year = as.Date(VP_average$Year, format = "%Y")
VP_average$Year = year(VP_average$Year)

ggplot(data = VP_average, aes(x = Vapour_Pressure)) +
  geom_histogram(binwidth = 0.03, color = "white", fill = "black")

qqplot(VP_average$Year, VP_average$Vapour_Pressure)

shapiro.test(VP_average$Vapour_Pressure)

VPA = ggplot(data = VP_average,
             aes(x = Year,
                 y = Vapour_Pressure))
VPA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                   axis.title.x = element_text(size = 20),
                                   axis.text.y = element_text(size = 30),
                                   axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

###Runoff:

Runoff_average = aggregate(wernersbach_data$Runoff_Avg_mm ~ 
                             date_years, wernersbach_data, mean)

colnames(Runoff_average) = c("Year", "Runoff")


Runoff_average$Year = as.Date(Runoff_average$Year, format = "%Y")
Runoff_average$Year = year(Runoff_average$Year)

ggplot(data = Runoff_average, aes(x = Runoff)) +
  geom_histogram(binwidth = 2, color = "white", fill = "black")

qqplot(Runoff_average$Year, Runoff_average$Runoff)

shapiro.test(Runoff_average$Runoff)


RunoffA = ggplot(data = Runoff_average,
                 aes(x = Year,
                     y = Runoff))
RunoffA + 
  geom_smooth(formula = y ~ x, method = "loess", size = 2) + geom_point(size = 5) +
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20))

###ET_Pen:
ET_Penmanth = aggregate(wernersbach_data$ET_pen ~ 
                          date_years, wernersbach_data, mean)

colnames(ET_Penmanth) = c("Year", "PET_Penman")


ET_Penmanth$Year = as.Date(ET_Penmanth$Year, format = "%Y")
ET_Penmanth$Year = year(ET_Penmanth$Year)

ggplot(data = ET_Penmanth, aes(x = PET_Penman)) +
  geom_histogram(binwidth = 2, color = "white", fill = "black")

qqplot(ET_Penmanth$Year, ET_Penmanth$PET_Penman)

shapiro.test(ET_Penmanth$PET_Penman)


ET_PenmanthA = ggplot(data = ET_Penmanth,
                      aes(x = Year,
                          y = PET_Penman))
ET_PenmanthA + 
  geom_smooth(formula = y ~ x, method = "loess", size = 2) + geom_point(size = 5) +
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20))
ET_PenmanthA + 
  geom_smooth(formula = y ~ x, method = "lm", size = 2) + geom_point(size = 5) +
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20))
ET_PenmanthA + 
  geom_line(col = "red", size = 1.5)  +
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20)) +
  geom_point() + geom_smooth()

### hargreaves
ET_hargreaves = aggregate(wernersbach_data$ET_har ~ 
                            date_years, wernersbach_data, mean)
colnames(ET_hargreaves) = c("Year", "ET_Hargreaves")
ET_hargreaves$Year = as.Date(ET_hargreaves$Year, format = "%Y")
ET_hargreaves$Year = year(ET_hargreaves$Year)
ggplot(data = ET_hargreaves, aes(x = ET_Hargreaves)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(ET_hargreaves$Year, ET_hargreaves$ET_Hargreaves)
shapiro.test(ET_hargreaves$ET_Hargreaves)
ET_hargreavesA = ggplot(data = ET_hargreaves,
                        aes(x = Year,
                            y = ET_Hargreaves))
ET_hargreavesA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                              axis.title.x = element_text(size = 20),
                                              axis.text.y = element_text(size = 30),
                                              axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

ET_hargreavesA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                              axis.title.x = element_text(size = 20),
                                              axis.text.y = element_text(size = 30),
                                              axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "lm", size = 2)
### thornthwaite:
ET_thornthwaite = aggregate(wernersbach_data$ET_thorn ~ 
                              date_years, wernersbach_data, mean)
colnames(ET_thornthwaite) = c("Year", "ET_Thornthwaite")
ET_thornthwaite$Year = as.Date(ET_thornthwaite$Year, format = "%Y")
ET_thornthwaite$Year = year(ET_thornthwaite$Year)
ggplot(data = ET_thornthwaite, aes(x = ET_Thornthwaite)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(ET_thornthwaite$Year, ET_thornthwaite$ET_Thornthwaite)
shapiro.test(ET_thornthwaite$ET_Thornthwaite)
ET_thornthwaiteA = ggplot(data = ET_thornthwaite,
                          aes(x = Year,
                              y = ET_Thornthwaite))
ET_thornthwaiteA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                                axis.title.x = element_text(size = 20),
                                                axis.text.y = element_text(size = 30),
                                                axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

ET_thornthwaiteA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                                axis.title.x = element_text(size = 20),
                                                axis.text.y = element_text(size = 30),
                                                axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "lm", size = 2)
###ET_Refrence:
ET_refrence = aggregate(wernersbach_data$ReferenceEvapotranspiration_Avg_mm ~ 
                          date_years, wernersbach_data, mean)
colnames(ET_refrence) = c("Year", "ET_Refrence")
ET_refrence$Year = as.Date(ET_refrence$Year, format = "%Y")
ET_refrence$Year = year(ET_refrence$Year)
ggplot(data = ET_refrence, aes(x = ET_Refrence)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(ET_refrence$Year, ET_refrence$ET_Refrence)
shapiro.test(ET_refrence$ET_Refrence)
ET_refrenceA = ggplot(data = ET_refrence,
                      aes(x = Year,
                          y = ET_Refrence))
ET_refrenceA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                            axis.title.x = element_text(size = 20),
                                            axis.text.y = element_text(size = 30),
                                            axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

ET_refrenceA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                            axis.title.x = element_text(size = 20),
                                            axis.text.y = element_text(size = 30),
                                            axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "lm", size = 2)

###ET_Catchment(Waterbudget)
ET_waterbudget = aggregate(wernersbach_data$ET_global ~ 
                             date_years, wernersbach_data, mean)
colnames(ET_waterbudget) = c("Year", "ET_WaterBudget")
ET_waterbudget$Year = as.Date(ET_waterbudget$Year, format = "%Y")
ET_waterbudget$Year = year(ET_waterbudget$Year)
ggplot(data = ET_waterbudget, aes(x = ET_WaterBudget)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(ET_waterbudget$Year, ET_waterbudget$ET_WaterBudget)
shapiro.test(ET_waterbudget$ET_WaterBudget)
ET_waterbudgetA = ggplot(data = ET_waterbudget,
                         aes(x = Year,
                             y = ET_WaterBudget))
ET_waterbudgetA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                               axis.title.x = element_text(size = 20),
                                               axis.text.y = element_text(size = 30),
                                               axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

ET_waterbudgetA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                               axis.title.x = element_text(size = 20),
                                               axis.text.y = element_text(size = 30),
                                               axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "lm", size = 2)
### HZD flux tower
ET_HZD = aggregate(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm ~ 
                     date_years, wernersbach_data, mean)
colnames(ET_HZD) = c("Year", "ET_HZD_fluxtower")
ET_HZD$Year = as.Date(ET_HZD$Year, format = "%Y")
ET_HZD$Year = year(ET_HZD$Year)
ggplot(data = ET_HZD, aes(x = ET_HZD_fluxtower)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(ET_HZD$Year, ET_HZD$ET_HZD_fluxtower)
cor.test(ET_HZD$Year, ET_HZD$ET_HZD_fluxtower)
plot(ET_HZD$Year, ET_HZD$ET_HZD_fluxtower)
reg_pen = lm(ET_HZD_fluxtower ~ Year, data = ET_HZD)
abline(reg_pen,col = "Red")
acf(ET_HZD$ET_HZD_fluxtower, lag.max = 10)
pacf(ET_HZD$ET_HZD_fluxtower, lag.max = 10)
acf(diff(ET_HZD$ET_HZD_fluxtower), lag.max = 10)
acf(residuals(lm(ET_HZD_fluxtower ~ Year, data = ET_HZD)), lag.max = 10)

install.packages("tseries")
library(tseries)
adf.test(ET_HZD$ET_HZD_fluxtower)
kpss.test(ET_HZD$ET_HZD_fluxtower, null = "Level")
kpss.test(ET_HZD$ET_HZD_fluxtower, null = "Trend")
# acf(wernersbach_data$ET_pen, lag.max = length(wernersbach_data$ET_pen))
##==> the data is trend stationary ==> Mann Kendal for trend test
##Using Mann kendall Test for existance of the trend: 
MannKendall(ET_HZD$ET_HZD_fluxtower)
##==> AH is accepted -> The trend is not significant
##Monthly
ETHZD = ET_HZD$ET_HZD_fluxtower
TSM06 <- ts(ETHZD,
            start=c(2010), end=c(2019), frequency=12)
plot(TSM06)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSM06, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM06),TSM06),lwd=3, col=2)




library(lmtest)
Model_Mean_Har <- lm(ET_har ~ ValueDateTime, data = wernersbach_data)
lmtest::bgtest(Model_Mean_Har, order = 3)

shapiro.test(ET_HZD$ET_HZD_fluxtower)
ET_HZDA = ggplot(data = ET_HZD,
                 aes(x = Year,
                     y = ET_HZD_fluxtower))
ET_HZDA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                       axis.title.x = element_text(size = 20),
                                       axis.text.y = element_text(size = 30),
                                       axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

### ET_Tha
ET_THA = aggregate(wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm ~ 
                     date_years, wernersbach_data, mean)
colnames(ET_THA) = c("Year", "ET_Tha_fluxtower")
ET_THA$Year = as.Date(ET_THA$Year, format = "%Y")
ET_THA$Year = year(ET_THA$Year)
ggplot(data = ET_THA, aes(x = ET_Tha_fluxtower)) +
  geom_histogram(binwidth = 0.3, color = "white", fill = "black")
qqplot(ET_THA$Year, ET_THA$ET_Tha_fluxtower)
shapiro.test(ET_THA$ET_Tha_fluxtower)

cor.test(ET_THA$Year, ET_THA$ET_Tha_fluxtower)
plot(ET_THA$Year, ET_THA$ET_Tha_fluxtower)
reg_pen = lm(ET_Tha_fluxtower ~ Year, data = ET_THA)
abline(reg_pen,col = "Red")
acf(ET_THA$ET_Tha_fluxtower, lag.max = 30)
pacf(ET_THA$ET_Tha_fluxtower, lag.max = 30)
acf(diff(ET_THA$ET_Tha_fluxtower), lag.max = 30)
acf(residuals(lm(ET_Tha_fluxtower ~ Year, data = ET_THA)), lag.max = 30)

install.packages("tseries")
library(tseries)
adf.test(ET_THA$ET_Tha_fluxtower)
kpss.test(ET_THA$ET_Tha_fluxtower, null = "Level")
kpss.test(ET_THA$ET_Tha_fluxtower, null = "Trend")
# acf(wernersbach_data$ET_pen, lag.max = length(wernersbach_data$ET_pen))
##==> the data is trend stationary ==> Mann Kendal for trend test
##Using Mann kendall Test for existance of the trend: 
MannKendall(ET_THA$ET_Tha_fluxtower)
##==> AH is accepted -> The trend is not significant
##Monthly
ETTHA = ET_THA$ET_Tha_fluxtower
TSM07 <- ts(ETTHA,
            start=c(1997), end=c(2019), frequency=12)
plot(TSM07)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSM07, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM07),TSM07),lwd=3, col=2)






ET_ThaA = ggplot(data = ET_THA,
                 aes(x = Year,
                     y = ET_Tha_fluxtower))
ET_ThaA + geom_point(size = 5) + theme(axis.text.x = element_text(size = 30),
                                       axis.title.x = element_text(size = 20),
                                       axis.text.y = element_text(size = 30),
                                       axis.title.y = element_text(size = 20)) +
  geom_smooth(method = "loess", size = 2)

###Combined:


ETs = cbind(ET_Penmanth$Year, 
            ET_Penmanth$PET_Penman, ET_hargreaves$ET_Hargreaves,
            ET_thornthwaite$ET_Thornthwaite,
            ET_refrence$ET_Refrence, ET_waterbudget$ET_WaterBudget)
ETs = as.data.frame(ETs)
colnames(ETs) = c("Year", "PET_Penman", "PET_Hargreaves",
                  "PET_Thornthwaite", "ET_Refrence", "ET_WaterBudget")


library(reshape2)
ETs.long <- melt(ETs, id = "Year", measure = c("PET_Penman",
                                               "PET_Hargreaves",
                                               "PET_Thornthwaite",
                                               "ET_Refrence",
                                               "ET_WaterBudget"))
ggplot(ETs.long, aes(Year, value, colour = variable)) + geom_line(size = 1) +
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) + ylab("Evapotranspiration(mm)")
ggplot(ETs.long, aes(Year, value, colour = variable)) + geom_smooth(size = 1.2)+
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) + ylab("Evapotranspiration(mm)")
ggplot(ETs.long, aes(Year, value, colour = variable)) +
  geom_smooth(method = "loess",size = 1.2, se = F)+
  theme(axis.text.x = element_text(size = 30),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 20),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15)) + ylab("Evapotranspiration(mm)")





##Daily ET in 2017:
str(wernersbach_data_daily)
date_months2017 = factor(date_months2017)
monthabb2017 = month.abb[date_months2017]
date_days2017 = factor(date_days2017)
d = ggplot(data = wernersbach_data_daily)
d + geom_point(aes(x = monthabb2017,
                   y= ReferenceEvapotranspiration_Avg_mm)) +
  geom_boxplot(aes(x = monthabb2017,
                   y= ReferenceEvapotranspiration_Avg_mm),
               alpha = .6)














### Comparison between catchment E(ET_global) and Reference catchment:
hist(wernersbach_data$ET_global)
qqplot(wernersbach_data$ValueDateTime, wernersbach_data$ET_global)
shapiro.test(wernersbach_data$ET_global)

ReferenceET = aggregate(wernersbach_data$ReferenceEvapotranspiration_Avg_mm ~ 
                          date_years, wernersbach_data, mean)

colnames(ReferenceET) = c("Year", "REF_ET")


ReferenceET$Year = as.Date(ReferenceET$Year, format = "%Y")
ReferenceET$Year = year(ReferenceET$Year)

ggplot(data = ReferenceET, aes(x = REF_ET)) +
  geom_histogram(binwidth = 3.5, color = "white", fill = "black")

qqplot(ReferenceET$Year, ReferenceET$REF_ET)

shapiro.test(ReferenceET$REF_ET) 


hist(wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
qqplot(wernersbach_data$ValueDateTime, wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
shapiro.test(wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
CatchET = aggregate(wernersbach_data$ET_global ~ 
                      date_years, wernersbach_data, mean)

colnames(CatchET) = c("Year", "Catchment_ET")


CatchET$Year = as.Date(CatchET$Year, format = "%Y")
CatchET$Year = year(CatchET$Year)

ggplot(data = CatchET, aes(x = Catchment_ET)) +
  geom_histogram(binwidth = 3.5, color = "white", fill = "black")

qqplot(CatchET$Year, CatchET$Catchment_ET)

shapiro.test(CatchET$Catchment_ET)    ##==> follows normal distribution

##==> we can use t.test:
t.test(wernersbach_data$ET_global, wernersbach_data$ReferenceEvapotranspiration_Avg_mm)
t.test(ReferenceET$REF_ET, CatchET$Catchment_ET)
##for penman eq also:
PENMANET = aggregate(wernersbach_data$ET_pen ~ 
                       date_years, wernersbach_data, mean)

colnames(PENMANET) = c("Year", "ET_penman")


PENMANET$Year = as.Date(PENMANET$Year, format = "%Y")
PENMANET$Year = year(PENMANET$Year)

ggplot(data = PENMANET, aes(x = ET_penman)) +
  geom_histogram(binwidth = 7, color = "white", fill = "black")

qqplot(PENMANET$Year, PENMANET$ET_penman)

shapiro.test(PENMANET$ET_penman)
t.test(ReferenceET$REF_ET, PENMANET$ET_penman)

















###Time series analysis:
#######Autocorrelation, Partial Autocorrelation:
###ET_Penman_mean:


PENMANET = aggregate(wernersbach_data$ET_pen ~ 
                       date_years, wernersbach_data, mean)

colnames(PENMANET) = c("Year", "ET_penman")


PENMANET$Year = as.Date(PENMANET$Year, format = "%Y")
PENMANET$Year = year(PENMANET$Year)

ggplot(data = PENMANET, aes(x = ET_penman)) +
  geom_histogram(binwidth = 7, color = "white", fill = "black")

qqplot(PENMANET$Year, PENMANET$ET_penman)

shapiro.test(PENMANET$ET_penman)

plot(PENMANET$Year, PENMANET$ET_penman)
cor.test(PENMANET$Year, PENMANET$ET_penman)
reg_pen = lm(ET_penman ~ Year, data = PENMANET)
abline(reg_pen,col = "Red")
acf(PENMANET$ET_penman, lag.max = 52)
pacf(PENMANET$ET_penman, lag.max = 52)
acf(diff(PENMANET$ET_penman), lag.max = 52)
acf(residuals(lm(ET_penman ~ Year, data = PENMANET)), lag.max = 52)

library(lmtest)
Model_Mean_Pen <- lm(PENMANET$ET_penman ~ PENMANET$Year, data = wernersbach_data)
lmtest::bgtest(mpen, order = 3)

adf.test(PENMANET$ET_penman) ## Not stationary
kpss.test(PENMANET$ET_penman, null = "Level") ##Not level stationary
kpss.test(PENMANET$ET_penman, null = "Trend") ##Trend stationary

MannKendall(PENMANET$ET_penman) ##A trend is present in data

TSM_Penman_PET <- ts(PENMANET$ET_penman,
                     start=c(1967), end=c(2019), frequency=1)
plot(TSM_Penman_PET, col = 12)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(as.numeric(PENMANET$ET_penman), kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM_Penman_PET),TSM_Penman_PET),lwd=3, col=2)



###Penman_ET:

plot(wernersbach_data$ValueDateTime, wernersbach_data$ET_pen)
cor.test(as.numeric(wernersbach_data$ValueDateTime), wernersbach_data$ET_pen)
reg_pen = lm(ET_pen ~ as.numeric(ValueDateTime), data = wernersbach_data)
abline(reg_pen,col = "Red")
acf(wernersbach_data$ET_pen, lag.max = 700)
pacf(wernersbach_data$ET_pen, lag.max = 700)
acf(diff(wernersbach_data$ET_pen), lag.max = 700)
acf(residuals(lm(ET_pen ~ as.numeric(ValueDateTime), data = wernersbach_data)), lag.max = 700)

library(lmtest)
Model_Mean_Pen <- lm(ET_pen ~ ValueDateTime, data = wernersbach_data)
lmtest::bgtest(Model_Mean_Pen, order = 3)


### To calculate the median first grouped by the data:



# ##For Median distribution : 
# ET_median = aggregate(wernersbach_data$ET_pen ~ 
#             date_years, wernersbach_data, median)
# 
# colnames(ET_median) = c("Date", "ET_Penman")
# ET_median$Date = as.Date(ET_median$Date, format = "%Y")
# ET_median$Date = year(ET_median$Date)
# ET_median
# ####Check the distribution: (n(sample size)>30-->enough values)
# a = ggplot(data = ET_median, aes(x = ET_Penman))
# a + geom_histogram(binwidth = 5, color = "white", fill = "black")
# qqplot(ET_median$Date, ET_median$ET_Penman)
# shapiro.test(ET_median$ET_Penman)

###=> check log transformation:

# a1 = ggplot(data = ET_median, aes(x = log(ET_Penman)))
# a1 + geom_histogram(binwidth = .2, color = "white", fill = "black")
# qqplot(ET_median$Date, log(ET_median$ET_Penman))
# shapiro.test(log(ET_median$ET_Penman))
##implementation of log transformation
# ET_median$ET_Penman = log(ET_median$ET_Penman)

#####==> data is normally distrubuted
#####--> Using regular regression instead of Mann Kendall:

# b = ggplot(data = ET_median, aes(x = Date, y = ET_Penman))
# b + geom_point() +  theme(axis.text.x = element_text(size = 7.5)) +
#   geom_smooth()
# 
# 
# plot(ET_median$Date, ET_median$ET_Penman, pch = 18)
# model03 = lm(ET_median$ET_Penman ~ ET_median$Date)
# model03
# abline(model03, col = "red")
# cor(as.numeric(ET_median$Date), ET_median$ET_Penman)
# cor.test(as.numeric(ET_median$Date), ET_median$ET_Penman)

##determine whether a time series is sattionary or not":
install.packages("tseries")
library(tseries)
adf.test(wernersbach_data$ET_pen)
kpss.test(wernersbach_data$ET_pen, null = "Level")
kpss.test(wernersbach_data$ET_pen, null = "Trend")
# acf(wernersbach_data$ET_pen, lag.max = length(wernersbach_data$ET_pen))
##==> the data is trend stationary ==> Mann Kendal for trend test
##Using Mann kendall Test for existance of the trend: 
library(Kendall)
MannKendall(wernersbach_data$ET_pen)
##==> AH is accepted -> the trend is significant
##Monthly
ETpenman = wernersbach_data$ET_pen
TSM <- ts(ETpenman,
          start=c(1968), end=c(2019), frequency=12)
plot(TSM)
# kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
# lines(filter(TSM, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM),TSM),lwd=3, col=2)

## ==> forcasting:
install.packages("forecast")
library(forecast)
# forecasting model using arima model
fit_m <- auto.arima(TSM)
# Next 5 years forecasted values
forecast.Arima(fit_m, 60)
# plotting the graph with next
plot(forecast(fit_m, 60))

##Yearly
ETpenman = wernersbach_data$ET_pen
TSY <- ts(ETpenman,
          start=c(1968), end=c(2019), frequency=1)
plot(TSY)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSY, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSY),TSY),lwd=3, col=2)


## ==> forcasting:
# install.packages("forecast")
# library(forecast)
# forecasting model using arima model
fit_y <- auto.arima(TSY)
# Next 10 years forecasted values
forecast.Arima(fit_y, 10)
# plotting the graph with next
plot(forecast(fit_y, 10))




###PET_Hargreaves :
plot(wernersbach_data$ValueDateTime, wernersbach_data$ET_har)
cor.test(as.numeric(wernersbach_data$ValueDateTime), wernersbach_data$ET_har)
reg_pen = lm(ET_har ~ as.numeric(ValueDateTime), data = wernersbach_data)
abline(reg_pen,col = "Red")
acf(wernersbach_data$ET_har, lag.max = 700)
pacf(wernersbach_data$ET_har, lag.max = 700)
acf(diff(wernersbach_data$ET_har), lag.max = 700)
acf(residuals(lm(ET_har ~ as.numeric(ValueDateTime), data = wernersbach_data)), lag.max = 700)

library(lmtest)
Model_Mean_Har <- lm(ET_har ~ ValueDateTime, data = wernersbach_data)
lmtest::bgtest(Model_Mean_Har, order = 3)



##determine whether a time series is sattionary or not":
adf.test(wernersbach_data$ET_har)
kpss.test(wernersbach_data$ET_har, null = "Level")
# kpss.test(wernersbach_data$ET_pen, null = "Trend")
# acf(wernersbach_data$ET_pen, lag.max = length(wernersbach_data$ET_pen))
##==> the data is trend stationary ==> Mann Kendal for trend test
##Using Mann kendall Test for existance of the trend: 
MannKendall(wernersbach_data$ET_har)
##==> AH is accepted -> The trend is not significant
##Monthly
EThargreaves = wernersbach_data$ET_har
TSM01 <- ts(EThargreaves,
            start=c(1968), end=c(2019), frequency=12)
plot(TSM01)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSM01, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM01),TSM01),lwd=3, col=2)

###PET_Thornthaith:
##determine whether a time series is sattionary or not":
adf.test(wernersbach_data$ET_thorn)
kpss.test(wernersbach_data$ET_thorn, null = "Level")
# kpss.test(wernersbach_data$ET_pen, null = "Trend")
# acf(wernersbach_data$ET_pen, lag.max = length(wernersbach_data$ET_pen))
##==> the data is trend stationary ==> Mann Kendal for trend test
##Using Mann kendall Test for existance of the trend: 
MannKendall(wernersbach_data$ET_thorn)
##==> AH is accepted -> The trend is significant
##Monthly
ETthornth = wernersbach_data$ET_thorn
TSM03 <- ts(ETthornth,
            start=c(1968), end=c(2019), frequency=12)
plot(TSM03)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSM03, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM03),TSM03),lwd=3, col=2)


###ET_WaterBudget:
##determine whether a time series is sattionary or not":
adf.test(wernersbach_data$ET_global)
kpss.test(wernersbach_data$ET_global, null = "Level")
# kpss.test(wernersbach_data$ET_pen, null = "Trend")
# acf(wernersbach_data$ET_pen, lag.max = length(wernersbach_data$ET_pen))
##==> the data is trend stationary ==> Mann Kendal for trend test
##Using Mann kendall Test for existance of the trend: 
MannKendall(wernersbach_data$ET_global)
##==> AH is accepted -> The trend is not significant
##Monthly
ET_waterbudgetB = wernersbach_data$ET_global
TSM04 <- ts(ET_waterbudgetB,
            start=c(1968), end=c(2019), frequency=12)
plot(TSM04)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSM04, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM04),TSM04),lwd=3, col=2)


# ##Forcasting:
# fit_m03 <- auto.arima(TSM04)
# # Next 5 years forecasted values
# forecast(fit_m03, 60, level = .4)
# # plotting the graph with next
# plot(forecast(fit_m03, 60))


###HZD:
# MannKendall(wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mml)
# ##==> AH is accepted -> The trend is not significant
# ##Monthly
# ET_HZDB = wernersbach_data$Evapotranspitation.DE.Hzd_Sum_mm
# TSM05 <- ts(ET_HZDB,
#             start=c(1968), end=c(2019), frequency=12)
# plot(TSM05)
# kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
# lines(filter(TSM05, kernel/sum(kernel)), lwd=2, col="blue")
# lines(lowess(time(TSM05),TSM05),lwd=3, col=2)


###Tha:
MannKendall(wernersbach_data$ET_global)
##==> AH is accepted -> The trend is not significant
##Monthly
ET_THAB = wernersbach_data$Evapotranspitation.DE.Tha_Sum_mm
TSM06 <- ts(ET_THAB,
            start=c(1967), end=c(2019), frequency=12)
plot(TSM06)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(TSM06, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM06),TSM06),lwd=3, col=2)



###Max Temperature:

plot(wernersbach_data$ValueDateTime, wernersbach_data$Temperature.Max_Avg_DegC)
cor.test(as.numeric(wernersbach_data$ValueDateTime),
         wernersbach_data$Temperature.Max_Avg_DegC)
reg_Temp = lm(Temperature.Max_Avg_DegC ~ as.numeric(ValueDateTime), data = wernersbach_data)
abline(reg_Temp,col = "Red")
acf(wernersbach_data$Temperature.Max_Avg_DegC, lag.max = 700)
acf(diff(wernersbach_data$Temperature.Max_Avg_DegC), lag.max = 700)
acf(residuals(lm(Temperature.Max_Avg_DegC ~ as.numeric(ValueDateTime), data = wernersbach_data)), lag.max = 700)
Tmax_average
library(lmtest)
plot(Tmax_average$Year, Tmax_average$Tmax_ave)
Model_Mean_Temp <- lm(Tmax_ave ~ Year, data = Tmax_average)
abline(Model_Mean_Temp,col = "Red")

lmtest::bgtest(Model_Mean_Temp, order = 3)  ##there is a autoco

adf.test(Tmax_average$Tmax_ave) ##stationary
kpss.test(Tmax_average$Tmax_ave, null = "Level") ##stationary
kpss.test(Tmax_average$Tmax_ave, null = "Trend") ##stationary

MannKendall(Tmax_average$Tmax_ave) ##A trend is present in data

TSM_Tempmax <- ts(Tmax_average$Tmax_ave,
                  start=c(1967), end=c(2019), frequency=1)
plot(TSM_Tempmax)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(Tmax_average, kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM_Tempmax),TSM_Tempmax),lwd=3, col=2)

##Vapour pressure
adf.test(wernersbach_data$VapourPressure_Avg_kPa) ##stationary
kpss.test(wernersbach_data$VapourPressure_Avg_kPa, null = "Level") ##stationary
##___ For average:

plot(VP_average$Year, VP_average$Vapour_Pressure)
cor.test(VP_average$Year,
         VP_average$Vapour_Pressure)
reg_Temp = lm(Vapour_Pressure ~ Year, data = VP_average)
abline(reg_Temp,col = "Red")
acf(VP_average$Vapour_Pressure, lag.max = 700)
acf(diff(VP_average$Vapour_Pressure), lag.max = 700)
acf(residuals(lm(Vapour_Pressure ~ Year, data = VP_average)), lag.max = 700)

library(lmtest)
plot(VP_average$Year, VP_average$Vapour_Pressure)
Model_Mean_Vape <- lm(Vapour_Pressure ~ Year, data = VP_average)
abline(Model_Mean_Vape,col = "Red")

lmtest::bgtest(Model_Mean_Vape, order = 3)  ##there is a autoco

adf.test(VP_average$Vapour_Pressure) ## Not stationary
kpss.test(VP_average$Vapour_Pressure, null = "Level") ##Not level stationary
kpss.test(VP_average$Vapour_Pressure, null = "Trend") ##Trend stationary

MannKendall(VP_average$Vapour_Pressure) ##A trend is present in data

TSM_VapourPressure <- ts(VP_average$Vapour_Pressure,
                         start=c(1967), end=c(2019), frequency=1)
plot(TSM_VapourPressure)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(as.numeric(VP_average$Vapour_Pressure), kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM_VapourPressure),TSM_VapourPressure),lwd=3, col=2)


##Global_ Radiation:

adf.test(wernersbach_data$GlobalRadiation_Avg_Wm.2) ##stationary
kpss.test(wernersbach_data$GlobalRadiation_Avg_Wm.2, null = "Level") ##stationary
##___ For average:

plot(GlobarR_average$Year, GlobarR_average$Global.Radiation.Average)
cor.test(GlobarR_average$Year,
         GlobarR_average$Global.Radiation.Average)
reg_Temp = lm(Global.Radiation.Average ~ Year, data = GlobarR_average)
abline(reg_Temp,col = "Red")
acf(GlobarR_average$Global.Radiation.Average, lag.max = 700)
acf(diff(GlobarR_average$Global.Radiation.Average), lag.max = 700)
acf(residuals(lm(Global.Radiation.Average ~ Year, data = GlobarR_average)), lag.max = 700)

library(lmtest)
plot(GlobarR_average$Year, GlobarR_average$Global.Radiation.Average)
Model_Mean_Vape <- lm(Global.Radiation.Average ~ Year, data = GlobarR_average)
abline(Model_Mean_Vape,col = "Red")

lmtest::bgtest(Model_Mean_Vape, order = 3)  ##there is a autoco

adf.test(GlobarR_average$Global.Radiation.Average) ## Not stationary
kpss.test(GlobarR_average$Global.Radiation.Average, null = "Level") ##Not level stationary
kpss.test(GlobarR_average$Global.Radiation.Average, null = "Trend") ##Trend stationary

MannKendall(GlobarR_average$Global.Radiation.Average) ##A trend is present in data

TSM_GlobalRadiation <- ts(GlobarR_average$Global.Radiation.Average,
                          start=c(1967), end=c(2019), frequency=1)
plot(TSM_GlobalRadiation)
kernel <- rep(1, 10) # rectangular kernel, pls. vary bandwith
lines(filter(as.numeric(GlobarR_average$Global.Radiation.Average), kernel/sum(kernel)), lwd=2, col="blue")
lines(lowess(time(TSM_GlobalRadiation),TSM_GlobalRadiation),lwd=3, col=2)





