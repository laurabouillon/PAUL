rm(list = ls()) #to delete all variables

#les differentes librairies 
library("ggplot2")
library("grid")
library("gridExtra")
library("dplyr")
library("patchwork") # To display 2 charts together
library("hrbrthemes")
library("plotly") #interactive graphics
library("scales")
library("cowplot")
library("data.table")
library("lubridate")
library("ggpmisc")
library("stats")
library ("openair")
library("readxl")
library('Rcpp')
library("raster")

load(file = "E:/SACLAY/DATA_FINAL/ICOS/CO2.RData")
load(file = "E:/SACLAY/DATA_FINAL/ICOS/CH4.RData")
load(file = "E:/SACLAY/DATA_FINAL/ICOS/CO.RData")

CO_All <- CO_All[order(as.POSIXct(CO_All$DateTime, format="%Y-%m-%d %H:%M:%S")),]
write.table(CO_All,"E:/SACLAY/DATA_FINAL/ICOS/CO_HA.csv", sep=",", row.names = FALSE)

colnames(CO2_All)[2] <- "CO2"
colnames(CO_All)[2] <- "CO"
colnames(CH4_All)[2] <- "CH4"

All <- merge (
  x = CO2_All,
  y = CO_All,
  by =c("DateTime"), 
  all = TRUE)

All <- merge (
  x = All,
  y = CH4_All,
  by =c("DateTime"), 
  all = TRUE)

write.table(All,"E:/SACLAY/DATA_FINAL/ICOS/GES_HA.csv", sep=",", row.names = FALSE)
GES_SAC <- read.csv("E:/SACLAY/DATA_FINAL/ICOS/GES_HA.csv")
GES_SAC$DateTime <- as.POSIXct(GES_SAC$DateTime, format = '%d/%m/%Y %H:%M', tz="UTC")


NO2_SAC <- read.csv("C:/Users/lbouillo/Documents/R/R_NOx/NO2_2013_2021_HA.csv")
NO2_SAC$DateTime <- as.POSIXct(NO2_SAC$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
NO2_SAC <- NO2_SAC[order(as.POSIXct(NO2_SAC$DateTime, format="%Y-%m-%d %H:%M:%S")),]
colnames(NO2_SAC) [2] <- "NO2"

NO_SAC <- read.csv("C:/Users/lbouillo/Documents/R/R_NOx/NO_2013_2021_HA.csv")
NO_SAC$DateTime <- as.POSIXct(NO_SAC$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
NO_SAC <- NO_SAC[order(as.POSIXct(NO_SAC$DateTime, format="%Y-%m-%d %H:%M:%S")),]
colnames(NO_SAC) [2] <- "NO"

NOx <- merge (
  x = NO2_SAC,
  y = NO_SAC,
  by =c("DateTime"), 
  all = TRUE)

NOx <- NOx[,-c(3,5)]
write.table(NOx,"E:/SACLAY/DATA_FINAL/CAE/NOx_2013_2021_HA.csv", sep=",", row.names = FALSE)
NOx_SAC <- read.csv("E:/SACLAY/DATA_FINAL/CAE/NOx_2013_2021_HA.csv")
NOx_SAC$DateTime <- as.POSIXct(NOx_SAC$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")

load("C:/Users/lbouillo/Documents/R/R_BC/BC/BC_2012_2021_HA.RData")
colnames(BC_All) [4] <- "BCtot"
colnames(BC_All) [1] <- "DateTime"
write.table(BC_All,"E:/SACLAY/DATA_FINAL/CAE/BC_2012_2022_HA.csv", sep=",", row.names = FALSE)
BC_All <- as.data.frame(BC_All)

All <- merge (
  x = GES_SAC,
  y = NOx_SAC,
  by =c("DateTime"), 
  all = TRUE)

All <- merge (
  x = All,
  y = BC_All,
  by =c("DateTime"), 
  all = TRUE)

colnames(All) [2] <- "CO2 (ppm)"
colnames(All) [3] <- "CO (ppb)"
colnames(All) [4] <- "CH4 (ppb)"
colnames(All) [5] <- "NO2 (ppb)"
colnames(All) [6] <- "NO (ppb)"
colnames(All) [7] <- "BCwb (ng/m3)"
colnames(All) [8] <- "BCff (ng/m3)"
colnames(All) [9] <- "BCtot (ng/m3)"
write.table(All,"E:/SACLAY/DATA_FINAL/All_2012_2022_HA.csv", sep=",", row.names = FALSE)
