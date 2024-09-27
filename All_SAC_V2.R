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
# ---- GES_MEU ---- 
#'------------------------------------------------------------------------------
#---- CO2_MEU ---- 
MEU_CO2 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_CO2 <- MEU_CO2[,-c(1,2,7,11,13:17)]
colnames(M_CO2) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CO2","Stdev", "flag")
M_CO2$Month <- sub(pattern = "1", replacement = "01", M_CO2$Month)
M_CO2$Month <- sub(pattern = "2", replacement = "02", M_CO2$Month)
M_CO2$Month <- sub(pattern = "3", replacement = "03", M_CO2$Month)
M_CO2$Date <- paste(M_CO2$Year, M_CO2$Month, M_CO2$Day, sep="/")
M_CO2$Hour <- sub(pattern = "0", replacement = "00", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "1", replacement = "01", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "2", replacement = "02", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "3", replacement = "03", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "4", replacement = "04", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "5", replacement = "05", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "6", replacement = "06", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "7", replacement = "07", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "8", replacement = "08", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "9", replacement = "09", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "010", replacement = "1", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "011", replacement = "11", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "020", replacement = "2", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "022", replacement = "22", M_CO2$Hour)
M_CO2$Date <- paste(M_CO2$Date, M_CO2$Hour, sep=" ")
M_CO2 [, "Secondes"] <- "00"
M_CO2 [, "Minutes"] <- "00"
M_CO2$Date <- paste(M_CO2$Date, M_CO2$Minutes,M_CO2$Secondes, sep=":")
colnames(M_CO2) [9] <- "DateTime"
M_CO2$DateTime <- as.POSIXct(M_CO2$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CO2 <- M_CO2[,-c(1:5, 10,11)]


M_CO2$flag <- as.character(M_CO2$flag)
M_CO2 <- as.data.table(M_CO2)
M_CO2_flag <- M_CO2[flag == "U"]
M_CO2_flag <- M_CO2_flag[,-c(2,3)]

#---- CH4_MEU ---- 
MEU_CH4 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_CH4 <- MEU_CH4[,-c(1,2,7,11,13:17)]
colnames(M_CH4) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CH4","Stdev", "flag")
M_CH4$Month <- sub(pattern = "1", replacement = "01", M_CH4$Month)
M_CH4$Month <- sub(pattern = "2", replacement = "02", M_CH4$Month)
M_CH4$Month <- sub(pattern = "3", replacement = "03", M_CH4$Month)
M_CH4$Date <- paste(M_CH4$Year, M_CH4$Month, M_CH4$Day, sep="/")
M_CH4$Hour <- sub(pattern = "0", replacement = "00", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "1", replacement = "01", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "2", replacement = "02", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "3", replacement = "03", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "4", replacement = "04", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "5", replacement = "05", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "6", replacement = "06", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "7", replacement = "07", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "8", replacement = "08", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "9", replacement = "09", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "010", replacement = "1", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "011", replacement = "11", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "020", replacement = "2", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "022", replacement = "22", M_CH4$Hour)
M_CH4$Date <- paste(M_CH4$Date, M_CH4$Hour, sep=" ")
M_CH4 [, "Secondes"] <- "00"
M_CH4 [, "Minutes"] <- "00"
M_CH4$Date <- paste(M_CH4$Date, M_CH4$Minutes,M_CH4$Secondes, sep=":")
colnames(M_CH4) [9] <- "DateTime"
M_CH4$DateTime <- as.POSIXct(M_CH4$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CH4 <- M_CH4[,-c(1:5, 10,11)]


M_CH4$flag <- as.character(M_CH4$flag)
M_CH4 <- as.data.table(M_CH4)
M_CH4_flag <- M_CH4[flag == "U"|flag =="O"]
M_CH4_flag <- M_CH4_flag[,-c(2,3)]

#---- CO_MEU ---- 
MEU_CO <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_CO <- MEU_CO[,-c(1,2,7,11,13:17)]
colnames(M_CO) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CO","Stdev", "flag")
M_CO$Month <- sub(pattern = "1", replacement = "01", M_CO$Month)
M_CO$Month <- sub(pattern = "2", replacement = "02", M_CO$Month)
M_CO$Month <- sub(pattern = "3", replacement = "03", M_CO$Month)
M_CO$Date <- paste(M_CO$Year, M_CO$Month, M_CO$Day, sep="/")
M_CO$Hour <- sub(pattern = "0", replacement = "00", M_CO$Hour)
M_CO$Hour <- sub(pattern = "1", replacement = "01", M_CO$Hour)
M_CO$Hour <- sub(pattern = "2", replacement = "02", M_CO$Hour)
M_CO$Hour <- sub(pattern = "3", replacement = "03", M_CO$Hour)
M_CO$Hour <- sub(pattern = "4", replacement = "04", M_CO$Hour)
M_CO$Hour <- sub(pattern = "5", replacement = "05", M_CO$Hour)
M_CO$Hour <- sub(pattern = "6", replacement = "06", M_CO$Hour)
M_CO$Hour <- sub(pattern = "7", replacement = "07", M_CO$Hour)
M_CO$Hour <- sub(pattern = "8", replacement = "08", M_CO$Hour)
M_CO$Hour <- sub(pattern = "9", replacement = "09", M_CO$Hour)
M_CO$Hour <- sub(pattern = "010", replacement = "1", M_CO$Hour)
M_CO$Hour <- sub(pattern = "011", replacement = "11", M_CO$Hour)
M_CO$Hour <- sub(pattern = "020", replacement = "2", M_CO$Hour)
M_CO$Hour <- sub(pattern = "022", replacement = "22", M_CO$Hour)
M_CO$Date <- paste(M_CO$Date, M_CO$Hour, sep=" ")
M_CO [, "Secondes"] <- "00"
M_CO [, "Minutes"] <- "00"
M_CO$Date <- paste(M_CO$Date, M_CO$Minutes,M_CO$Secondes, sep=":")
colnames(M_CO) [9] <- "DateTime"
M_CO$DateTime <- as.POSIXct(M_CO$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CO <- M_CO[,-c(1:5, 10,11)]


M_CO$flag <- as.character(M_CO$flag)
M_CO <- as.data.table(M_CO)
M_CO_flag <- M_CO[flag == "U"|flag =="O"]
M_CO_flag <- M_CO_flag[,-c(2,3)]

#---- All_MEU ---- 
GES_MEU <- merge(
  x = M_CO2_flag,
  y = M_CH4_flag, 
  all = TRUE,
  by ="DateTime"
)

GES_MEU <- merge(
  x = GES_MEU,
  y = M_CO_flag, 
  all = TRUE,
  by ="DateTime"
)


MEU_NOX <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/N500/NOx_HA_01-06.csv", sep=",")
GES_MEU <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/GES_flag_01-06.csv", sep=",")

colnames(MEU_NOX) [1] <- "DateTime"
MEU_NOX$DateTime <- as.POSIXct(MEU_NOX$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
GES_MEU$DateTime <- as.POSIXct(GES_MEU$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")

MEU_NOX$NO <- as.numeric(MEU_NOX$NO)
MEU_NOX$NO2 <- as.numeric(MEU_NOX$NO2)
GES_MEU$CO2 <- as.numeric(GES_MEU$CO2)
GES_MEU$CH4 <- as.numeric(GES_MEU$CH4)
GES_MEU$CO <- as.numeric(GES_MEU$CO)

All_MEU <- merge(
  x = GES_MEU,
  y = MEU_NOX, 
  all = TRUE,
  by ="DateTime"
)

MEU_BC <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/AE33-S10-01359/DataBC_QC_01-05.csv", sep=",")
MEU_BC [, "Secondes"] <- "00"
colnames(MEU_BC) [1] <- "DateTime"
MEU_BC$DateTime <- paste(MEU_BC$DateTime ,MEU_BC$Secondes, sep=":")
MEU_BC$DateTime <- as.POSIXct(MEU_BC$DateTime, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
MEU_BC <- MEU_BC[,-c(4)]
MEU_BC$BCff <- as.numeric(MEU_BC$BCff)
MEU_BC$BCwb <- as.numeric(MEU_BC$BCwb)
colnames(MEU_BC) [1] <- "date"
MEU_BC_HA <- timeAverage(MEU_BC, "1 hour")
colnames(MEU_BC_HA) [1] <- "DateTime"

All_MEU <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/All_01-06.csv", sep=",")
All_MEU [, "Secondes"] <- "00"
All_MEU$DateTime <- paste(All_MEU$DateTime ,All_MEU$Secondes, sep=":")
All_MEU$DateTime <- as.POSIXct(All_MEU$DateTime, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
All_MEU <- All_MEU[,-c(7)]

All_MEU$NO <- as.numeric(All_MEU$NO)
All_MEU$NO2 <- as.numeric(All_MEU$NO2)
All_MEU$CO2 <- as.numeric(All_MEU$CO2)
All_MEU$CH4 <- as.numeric(All_MEU$CH4)
All_MEU$CO <- as.numeric(All_MEU$CO)


All_MEU <- merge(
  x = All_MEU,
  y = MEU_BC_HA, 
  all = TRUE,
  by ="DateTime"
)

write.table(All_MEU,"C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/All_01-06.csv", sep=",", row.names = FALSE)

write.table(All_MEU,"C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/All_01-06.csv", sep=",", row.names = FALSE)

write.table(GES_MEU,"C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/GES_flag_01-06.csv", sep=",", row.names = FALSE)




#'------------------------------------------------------------------------------
# ---- GES_ROV ---- 
#'------------------------------------------------------------------------------
#---- CO2_ROV ---- 
ROV_CO2 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_CO2 <- ROV_CO2[,-c(1,2,7,11,13:17)]
colnames(M_CO2) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CO2","Stdev", "flag")
M_CO2$Month <- sub(pattern = "1", replacement = "01", M_CO2$Month)
M_CO2$Month <- sub(pattern = "2", replacement = "02", M_CO2$Month)
M_CO2$Month <- sub(pattern = "3", replacement = "03", M_CO2$Month)
M_CO2$Date <- paste(M_CO2$Year, M_CO2$Month, M_CO2$Day, sep="/")
M_CO2$Hour <- sub(pattern = "0", replacement = "00", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "1", replacement = "01", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "2", replacement = "02", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "3", replacement = "03", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "4", replacement = "04", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "5", replacement = "05", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "6", replacement = "06", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "7", replacement = "07", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "8", replacement = "08", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "9", replacement = "09", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "010", replacement = "1", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "011", replacement = "11", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "020", replacement = "2", M_CO2$Hour)
M_CO2$Hour <- sub(pattern = "022", replacement = "22", M_CO2$Hour)
M_CO2$Date <- paste(M_CO2$Date, M_CO2$Hour, sep=" ")
M_CO2 [, "Secondes"] <- "00"
M_CO2 [, "Minutes"] <- "00"
M_CO2$Date <- paste(M_CO2$Date, M_CO2$Minutes,M_CO2$Secondes, sep=":")
colnames(M_CO2) [9] <- "DateTime"
M_CO2$DateTime <- as.POSIXct(M_CO2$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CO2 <- M_CO2[,-c(1:5, 10,11)]


M_CO2$flag <- as.character(M_CO2$flag)
M_CO2 <- as.data.table(M_CO2)
M_CO2_flag <- M_CO2[flag == "O"|flag == "U"]
M_CO2_flag <- M_CO2_flag[,-c(2,3)]

#---- CH4_ROV ---- 
ROV_CH4 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_CH4 <- ROV_CH4[,-c(1,2,7,11,13:17)]
colnames(M_CH4) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CH4","Stdev", "flag")
M_CH4$Month <- sub(pattern = "1", replacement = "01", M_CH4$Month)
M_CH4$Month <- sub(pattern = "2", replacement = "02", M_CH4$Month)
M_CH4$Month <- sub(pattern = "3", replacement = "03", M_CH4$Month)
M_CH4$Date <- paste(M_CH4$Year, M_CH4$Month, M_CH4$Day, sep="/")
M_CH4$Hour <- sub(pattern = "0", replacement = "00", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "1", replacement = "01", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "2", replacement = "02", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "3", replacement = "03", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "4", replacement = "04", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "5", replacement = "05", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "6", replacement = "06", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "7", replacement = "07", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "8", replacement = "08", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "9", replacement = "09", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "010", replacement = "1", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "011", replacement = "11", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "020", replacement = "2", M_CH4$Hour)
M_CH4$Hour <- sub(pattern = "022", replacement = "22", M_CH4$Hour)
M_CH4$Date <- paste(M_CH4$Date, M_CH4$Hour, sep=" ")
M_CH4 [, "Secondes"] <- "00"
M_CH4 [, "Minutes"] <- "00"
M_CH4$Date <- paste(M_CH4$Date, M_CH4$Minutes,M_CH4$Secondes, sep=":")
colnames(M_CH4) [9] <- "DateTime"
M_CH4$DateTime <- as.POSIXct(M_CH4$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CH4 <- M_CH4[,-c(1:5, 10,11)]


M_CH4$flag <- as.character(M_CH4$flag)
M_CH4 <- as.data.table(M_CH4)
M_CH4_flag <- M_CH4[flag == "U"|flag =="O"]
M_CH4_flag <- M_CH4_flag[,-c(2,3)]

#---- CO_ROV ---- 
ROV_CO <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
M_CO <- ROV_CO[,-c(1,2,7,11,13:17)]
colnames(M_CO) <- c("Year","Month","Day","Hour",
                    "DecimalDate","CO","Stdev", "flag")
M_CO$Month <- sub(pattern = "1", replacement = "01", M_CO$Month)
M_CO$Month <- sub(pattern = "2", replacement = "02", M_CO$Month)
M_CO$Month <- sub(pattern = "3", replacement = "03", M_CO$Month)
M_CO$Date <- paste(M_CO$Year, M_CO$Month, M_CO$Day, sep="/")
M_CO$Hour <- sub(pattern = "0", replacement = "00", M_CO$Hour)
M_CO$Hour <- sub(pattern = "1", replacement = "01", M_CO$Hour)
M_CO$Hour <- sub(pattern = "2", replacement = "02", M_CO$Hour)
M_CO$Hour <- sub(pattern = "3", replacement = "03", M_CO$Hour)
M_CO$Hour <- sub(pattern = "4", replacement = "04", M_CO$Hour)
M_CO$Hour <- sub(pattern = "5", replacement = "05", M_CO$Hour)
M_CO$Hour <- sub(pattern = "6", replacement = "06", M_CO$Hour)
M_CO$Hour <- sub(pattern = "7", replacement = "07", M_CO$Hour)
M_CO$Hour <- sub(pattern = "8", replacement = "08", M_CO$Hour)
M_CO$Hour <- sub(pattern = "9", replacement = "09", M_CO$Hour)
M_CO$Hour <- sub(pattern = "010", replacement = "1", M_CO$Hour)
M_CO$Hour <- sub(pattern = "011", replacement = "11", M_CO$Hour)
M_CO$Hour <- sub(pattern = "020", replacement = "2", M_CO$Hour)
M_CO$Hour <- sub(pattern = "022", replacement = "22", M_CO$Hour)
M_CO$Date <- paste(M_CO$Date, M_CO$Hour, sep=" ")
M_CO [, "Secondes"] <- "00"
M_CO [, "Minutes"] <- "00"
M_CO$Date <- paste(M_CO$Date, M_CO$Minutes,M_CO$Secondes, sep=":")
colnames(M_CO) [9] <- "DateTime"
M_CO$DateTime <- as.POSIXct(M_CO$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CO <- M_CO[,-c(1:5, 10,11)]


M_CO$flag <- as.character(M_CO$flag)
M_CO <- as.data.table(M_CO)
M_CO_flag <- M_CO[flag == "U"|flag =="O"]
M_CO_flag <- M_CO_flag[,-c(2,3)]

#---- All_ROV ---- 
GES_ROV <- merge(
  x = M_CO2_flag,
  y = M_CH4_flag, 
  all = TRUE,
  by ="DateTime"
)

GES_ROV <- merge(
  x = GES_ROV,
  y = M_CO_flag, 
  all = TRUE,
  by ="DateTime"
)

ROV_NOX <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/N500/NOx_HA_01-06.csv", sep=",")
GES_ROV <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/GES_flag_01-06.csv", sep=",")

colnames(ROV_NOX) [1] <- "DateTime"
ROV_NOX$DateTime <- as.POSIXct(ROV_NOX$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
GES_ROV$DateTime <- as.POSIXct(GES_ROV$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")

ROV_NOX$NO <- as.numeric(ROV_NOX$NO)
ROV_NOX$NO2 <- as.numeric(ROV_NOX$NO2)
GES_ROV$CO2 <- as.numeric(GES_ROV$CO2)
GES_ROV$CH4 <- as.numeric(GES_ROV$CH4)
GES_ROV$CO <- as.numeric(GES_ROV$CO)

All_ROV <- merge(
  x = GES_ROV,
  y = ROV_NOX, 
  all = TRUE,
  by ="DateTime"
)

ROV_BC <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/AE33-S10-01368/DataBC_QC_01-05.csv", sep=",")
ROV_BC [, "Secondes"] <- "00"
ROV_BC$DateTime <- paste(ROV_BC$DateTime ,ROV_BC$Secondes, sep=":")
ROV_BC$DateTime <- as.POSIXct(ROV_BC$DateTime, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
ROV_BC <- ROV_BC[,-c(4)]
ROV_BC$BCff <- as.numeric(ROV_BC$BCff)
ROV_BC$BCwb <- as.numeric(ROV_BC$BCwb)
colnames(ROV_BC) [1] <- "date"
ROV_BC_HA <- timeAverage(ROV_BC, "1 hour")
colnames(ROV_BC_HA) [1] <- "DateTime"

All_ROV <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/All_01-06.csv", sep=",")
All_ROV [, "Secondes"] <- "00"
All_ROV$DateTime <- paste(All_ROV$DateTime ,All_ROV$Secondes, sep=":")
All_ROV$DateTime <- as.POSIXct(All_ROV$DateTime, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
All_ROV <- All_ROV[,-c(7)]

All_ROV$NO <- as.numeric(All_ROV$NO)
All_ROV$NO2 <- as.numeric(All_ROV$NO2)
All_ROV$CO2 <- as.numeric(All_ROV$CO2)
All_ROV$CH4 <- as.numeric(All_ROV$CH4)
All_ROV$CO <- as.numeric(All_ROV$CO)


All_ROV <- merge(
  x = All_ROV,
  y = ROV_BC_HA, 
  all = TRUE,
  by ="DateTime"
)

write.table(All_ROV,"C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/All_01-06.csv", sep=",", row.names = FALSE)


write.table(GES_ROV,"C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/GES_flag_01-06.csv", sep=",", row.names = FALSE)
