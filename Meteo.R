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

#'------------------------------------------------------------------------------
# ---- Meteo_MEU ---- 
#'------------------------------------------------------------------------------

MEU <- read.csv("C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/METEO_MEU_01_01-22_03_2023.csv")
MEU <- MEU[,-c(1,2,8:29, 31:32,34:36, 38,39,41:44)]
MEU$Month <- sub(pattern = "1", replacement = "01", MEU$Month)
MEU$Month <- sub(pattern = "2", replacement = "02", MEU$Month)
MEU$Month <- sub(pattern = "3", replacement = "03", MEU$Month)
MEU$Date <- paste(MEU$Year, MEU$Month, MEU$Day, sep="/")
MEU$Hour <- sub(pattern = "0", replacement = "00", MEU$Hour)
MEU$Hour <- sub(pattern = "1", replacement = "01", MEU$Hour)
MEU$Hour <- sub(pattern = "2", replacement = "02", MEU$Hour)
MEU$Hour <- sub(pattern = "3", replacement = "03", MEU$Hour)
MEU$Hour <- sub(pattern = "4", replacement = "04", MEU$Hour)
MEU$Hour <- sub(pattern = "5", replacement = "05", MEU$Hour)
MEU$Hour <- sub(pattern = "6", replacement = "06", MEU$Hour)
MEU$Hour <- sub(pattern = "7", replacement = "07", MEU$Hour)
MEU$Hour <- sub(pattern = "8", replacement = "08", MEU$Hour)
MEU$Hour <- sub(pattern = "9", replacement = "09", MEU$Hour)
MEU$Hour <- sub(pattern = "010", replacement = "1", MEU$Hour)
MEU$Hour <- sub(pattern = "011", replacement = "11", MEU$Hour)
MEU$Hour <- sub(pattern = "020", replacement = "2", MEU$Hour)
MEU$Hour <- sub(pattern = "022", replacement = "22", MEU$Hour)
MEU$Date <- paste(MEU$Date, MEU$Hour, sep=" ")
MEU [, "Minutes"] <- "00"
MEU [, "Secondes"] <- "00"
MEU$Date <- paste(MEU$Date, MEU$Minutes,MEU$Secondes, sep=":")
colnames(MEU) [10] <- "DateTime"

MEU$DateTime <- as.POSIXct(MEU$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
MEU <- MEU[,-c(1:5, 11:12)]

MEU$flag <- as.character(MEU$flag)
MEU <- as.data.table(MEU)
MEU_flag <- MEU[WS.Flag == "U" |WD.Flag == "U"]
names_col <- c("DateTime", "WS", "WS.Flag", "WD","WD.Flag")
MEU_flag <- as.data.frame(MEU_flag)
MEU_flag <- MEU_flag[, names_col]

write.csv(MEU_flag, "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/METEO_MEU_01_01-22_03_2023_flag.csv", row.names = TRUE)
