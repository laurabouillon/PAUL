# ---- Initialisation ---- 
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
#---- NO2_MEU ---- 
MEU_NO2 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/N500/MEU_65m_air.hdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_NO2 <- MEU_NO2[,-c(1,2,7,11,13:17)]
colnames(M_NO2) <- c("Year","Month","Day","Hour",
                     "DecimalDate","NO2","Stdev", "flag")
M_NO2$Month <- sub(pattern = "1", replacement = "01", M_NO2$Month)
M_NO2$Month <- sub(pattern = "2", replacement = "02", M_NO2$Month)
M_NO2$Month <- sub(pattern = "3", replacement = "03", M_NO2$Month)
M_NO2$Date <- paste(M_NO2$Year, M_NO2$Month, M_NO2$Day, sep="/")
M_NO2$Hour <- sub(pattern = "0", replacement = "00", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "1", replacement = "01", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "2", replacement = "02", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "3", replacement = "03", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "4", replacement = "04", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "5", replacement = "05", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "6", replacement = "06", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "7", replacement = "07", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "8", replacement = "08", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "9", replacement = "09", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "010", replacement = "1", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "011", replacement = "11", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "020", replacement = "2", M_NO2$Hour)
M_NO2$Hour <- sub(pattern = "022", replacement = "22", M_NO2$Hour)
M_NO2$Date <- paste(M_NO2$Date, M_NO2$Hour, sep=" ")
M_NO2 [, "Secondes"] <- "00"
M_NO2 [, "Minutes"] <- "00"
M_NO2$Date <- paste(M_NO2$Date, M_NO2$Minutes,M_NO2$Secondes, sep=":")
colnames(M_NO2) [9] <- "DateTime"
M_NO2$DateTime <- as.POSIXct(M_NO2$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_NO2 <- M_NO2[,-c(1:5, 10,11)]


M_NO2$flag <- as.character(M_NO2$flag)
M_NO2 <- as.data.table(M_NO2)
M_NO2_flag <- M_NO2[flag == "U"]
M_NO2_flag <- M_NO2_flag[,-c(2,3)]

NO2_AvP <- M_NO2_flag[-c(1:146, 582:2209)]
colnames(NO2_AvP) [2] <- "date"
NO2_ApP <- M_NO2_flag[-c(1:1070, 1826:2209)]
colnames(NO2_ApP) [2] <- "date"

NO2_F <- rbind(NO2_AvP, NO2_ApP)
NO2_F <- as.data.frame(NO2_F)

test <- splitByDate(NO2_F, dates= "01/07/2023",
                      labels = c("Before", "After"))

myOutput_WB <-timeVariation(test, pollutant = "NO2", 
              group = "split.by", 
              difference = F
              ,ylab ="[NO2] ppb"
              ,xlab ="Hour")
plot(myOutput_WB, subset = "hour")

#'------------------------------------------------------------------------------
#---- NO_MEU ---- 
MEU_NO <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/N500/MEU_65m_air.hdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_NO <- MEU_NO[,-c(1,2,7,11,13:17)]
colnames(M_NO) <- c("Year","Month","Day","Hour",
                     "DecimalDate","NO","Stdev", "flag")
M_NO$Month <- sub(pattern = "1", replacement = "01", M_NO$Month)
M_NO$Month <- sub(pattern = "2", replacement = "02", M_NO$Month)
M_NO$Month <- sub(pattern = "3", replacement = "03", M_NO$Month)
M_NO$Date <- paste(M_NO$Year, M_NO$Month, M_NO$Day, sep="/")
M_NO$Hour <- sub(pattern = "0", replacement = "00", M_NO$Hour)
M_NO$Hour <- sub(pattern = "1", replacement = "01", M_NO$Hour)
M_NO$Hour <- sub(pattern = "2", replacement = "02", M_NO$Hour)
M_NO$Hour <- sub(pattern = "3", replacement = "03", M_NO$Hour)
M_NO$Hour <- sub(pattern = "4", replacement = "04", M_NO$Hour)
M_NO$Hour <- sub(pattern = "5", replacement = "05", M_NO$Hour)
M_NO$Hour <- sub(pattern = "6", replacement = "06", M_NO$Hour)
M_NO$Hour <- sub(pattern = "7", replacement = "07", M_NO$Hour)
M_NO$Hour <- sub(pattern = "8", replacement = "08", M_NO$Hour)
M_NO$Hour <- sub(pattern = "9", replacement = "09", M_NO$Hour)
M_NO$Hour <- sub(pattern = "010", replacement = "1", M_NO$Hour)
M_NO$Hour <- sub(pattern = "011", replacement = "11", M_NO$Hour)
M_NO$Hour <- sub(pattern = "020", replacement = "2", M_NO$Hour)
M_NO$Hour <- sub(pattern = "022", replacement = "22", M_NO$Hour)
M_NO$Date <- paste(M_NO$Date, M_NO$Hour, sep=" ")
M_NO [, "Secondes"] <- "00"
M_NO [, "Minutes"] <- "00"
M_NO$Date <- paste(M_NO$Date, M_NO$Minutes,M_NO$Secondes, sep=":")
colnames(M_NO) [9] <- "DateTime"
M_NO$DateTime <- as.POSIXct(M_NO$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_NO <- M_NO[,-c(1:5, 10,11)]


M_NO$flag <- as.character(M_NO$flag)
M_NO <- as.data.table(M_NO)
M_NO_flag <- M_NO[flag == "U"]
M_NO_flag <- M_NO_flag[,-c(2,3)]

NO_AvP <- M_NO_flag[-c(1:146, 582:2209)]
colnames(NO_AvP) [2] <- "date"
NO_ApP <- M_NO_flag[-c(1:1070, 1826:2209)]
colnames(NO_ApP) [2] <- "date"

NO_F <- rbind(NO_AvP, NO_ApP)
NO_F <- as.data.frame(NO_F)

test <- splitByDate(NO_F, dates= "01/07/2023",
                    labels = c("Before", "After"))

myOutput_WB <-timeVariation(test, pollutant = "NO", 
                            group = "split.by", 
                            difference = F
                            ,ylab ="[NO] ppb"
                            ,xlab ="Hour")
plot(myOutput_WB, subset = "hour")

