# ---- Initialisation---- 
rm(list = ls()) #to delete all variables

#les differentes librairies 
library("ggplot2")
library("data.table")
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
library("tidyverse")

#'------------------------------------------------------------------------------
# ---- MEU ---- 
#'------------------------------------------------------------------------------
# ---- NOx ---- 
MEU_NOX <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/N500/NOx_flag_01-06.csv", sep=",")
MEU_NOX$DateTime <- as.POSIXct(MEU_NOX$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
colnames(MEU_NOX) [1] <- "date"
MEU_NOX$NO <- as.numeric(MEU_NOX$NO)
MEU_NOX$NO2 <- as.numeric(MEU_NOX$NO2)
MEU_NOX <- as.data.table(MEU_NOX)
MEU_NOX <- as.data.table(MEU_NOX)
MEU_NOX[, month := format(as.Date(date), "%m")]
MEU_NOX_H <- MEU_NOX[month == "01" |month == "02" |month == "03"]
MEU_NOX_P <- MEU_NOX[month == "04" |month == "05" |month == "06"]


wb_markerNOX <- c("NO", "NO2")

myOutput_WB <- timeVariation(MEU_NOX
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppb"
                             ,xlab ="Hour"
                             ,ylim = c(-1, 20)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(MEU_NOX_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppb"
                             ,xlab ="Hour"
                             ,ylim = c(-1, 15)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(MEU_NOX_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppb"
                             ,xlab ="Hour"
                             ,ylim = c(-1, 10)
)
plot(myOutput_WB, subset = "hour")

MEU_NOX <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/N500/NOx_flag_01-06.csv", sep=",")
colnames(MEU_NOX) [1] <- "date"
MEU_NOX$date <- as.POSIXct(MEU_NOX$date, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
MEU_NOX$NO <- as.numeric(MEU_NOX$NO)
MEU_NOX$NO2 <- as.numeric(MEU_NOX$NO2)

MEU_NOX_hour <- timeAverage(MEU_NOX, "1 hour")

write.table(MEU_NOX_hour,"C:/Users/lbouillo/Documents/PAUL/MEU/N500/NOx_HA_01-06.csv", sep=",", row.names = FALSE)
# ---- BC ---- 
MEU_BC <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/AE33-S10-01359/DataBC_QC_01-05.csv", sep=",")

MEU_BC [, "Secondes"] <- "00"
colnames(MEU_BC) [1] <- "DateTime"
MEU_BC$DateTime <- paste(MEU_BC$DateTime ,MEU_BC$Secondes, sep=":")
MEU_BC$DateTime <- as.POSIXct(MEU_BC$DateTime, format = '%d/%m/%Y %H:%M:%S', tz="UTC")

colnames(MEU_BC) [1] <- "date"
MEU_BC$BCff <- as.numeric(MEU_BC$BCff)
MEU_BC$BCwb <- as.numeric(MEU_BC$BCwb)
MEU_BC <- as.data.table(MEU_BC)

MEU_BC <- as.data.table(MEU_BC)
MEU_BC[, month := format(as.Date(date), "%m")]
MEU_BC_H <- MEU_BC[month == "01" |month == "02" |month == "03"]
MEU_BC_P <- MEU_BC[month == "04" |month == "05" |month == "06"]


wb_markerBC <- c("BCff", "BCwb")

myOutput_WB <- timeVariation(MEU_BC
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             #,ylim = c(0, 10)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(MEU_BC
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             ,ylim = c(0, 1000)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(MEU_BC_H
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             ,ylim = c(50, 700)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(MEU_BC_P
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             ,ylim = c(0, 850)
)
plot(myOutput_WB, subset = "hour")

write.table(MEU_BC_hour,"C:/Users/lbouillo/Documents/PAUL/MEU/N500/BC_HA_01-06.csv", sep=",", row.names = FALSE)
#'------------------------------------------------------------------------------
# ---- ROV ---- 
#'------------------------------------------------------------------------------
# ---- NOx ---- 
ROV_NOX <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/N500/NOx_flag_01-06.csv", sep=",")
ROV_NOX$DateTime <- as.POSIXct(ROV_NOX$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
colnames(ROV_NOX) [1] <- "date"
ROV_NOX$NO <- as.numeric(ROV_NOX$NO)
ROV_NOX$NO2 <- as.numeric(ROV_NOX$NO2)
ROV_NOX <- as.data.table(ROV_NOX)

ROV_NOX <- as.data.table(ROV_NOX)
ROV_NOX[, month := format(as.Date(date), "%m")]
ROV_NOX_H <- ROV_NOX[month == "01" |month == "02" |month == "03"]
ROV_NOX_P <- ROV_NOX[month == "04" |month == "05" |month == "06"]

wb_markerNOX <- c("NO", "NO2")

myOutput_WB <- timeVariation(ROV_NOX
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(0, 10)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(ROV_NOX
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppb"
                             ,xlab ="Hour"
                             ,ylim = c(-1, 20)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(ROV_NOX_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppb"
                             ,xlab ="Hour"
                             ,ylim = c(-1, 15)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(ROV_NOX_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue1", "red1")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppb"
                             ,xlab ="Hour"
                             ,ylim = c(-1, 10)
)
plot(myOutput_WB, subset = "hour")

ROV_NOX <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/N500/NOx_flag_01-06.csv", sep=",")
colnames(ROV_NOX) [1] <- "date"
ROV_NOX$date <- as.POSIXct(ROV_NOX$date, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
ROV_NOX$NO <- as.numeric(ROV_NOX$NO)
ROV_NOX$NO2 <- as.numeric(ROV_NOX$NO2)

ROV_NOX_hour <- timeAverage(ROV_NOX, "1 hour")

write.table(ROV_NOX_hour,"C:/Users/lbouillo/Documents/PAUL/ROV/N500/NOx_HA_01-06.csv", sep=",", row.names = FALSE)

# ---- BC ---- 
ROV_BC <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/AE33-S10-01368/DataBC_QC_01-05.csv", sep=",")

ROV_BC [, "Secondes"] <- "00"
colnames(ROV_BC) [1] <- "DateTime"
ROV_BC$DateTime <- paste(ROV_BC$DateTime ,ROV_BC$Secondes, sep=":")
ROV_BC$DateTime <- as.POSIXct(ROV_BC$DateTime, format = '%d/%m/%Y %H:%M:%S', tz="UTC")

colnames(ROV_BC) [1] <- "date"
ROV_BC$BCff <- as.numeric(ROV_BC$BCff)
ROV_BC$BCwb <- as.numeric(ROV_BC$BCwb)
ROV_BC <- as.data.table(ROV_BC)

ROV_BC <- as.data.table(ROV_BC)
ROV_BC[, month := format(as.Date(date), "%m")]
ROV_BC_H <- ROV_BC[month == "01" |month == "02" |month == "03"]
ROV_BC_P <- ROV_BC[month == "04" |month == "05" |month == "06"]

wb_markerBC <- c("BCff", "BCwb")

myOutput_WB <- timeVariation(ROV_BC
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             #,ylim = c(0, 10)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(ROV_BC
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             ,ylim = c(0, 1000)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(ROV_BC_H
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             ,ylim = c(50, 700)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(ROV_BC_P
                             ,pollutant = wb_markerBC
                             ,name.pol = wb_markerBC
                             ,difference = F
                             #,normalise = T
                             , cols = c("grey54", "tan4")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ng.m-3"
                             ,xlab ="Hour"
                             ,ylim = c(0, 850)
)
plot(myOutput_WB, subset = "hour")

write.table(ROV_BC_hour,"C:/Users/lbouillo/Documents/PAUL/ROV/N500/BC_HA_01-06.csv", sep=",", row.names = FALSE)
