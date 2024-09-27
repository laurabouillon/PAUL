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
MEU_CO2 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/MEU_90.0m_air.hdf.all.co2",
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
M_CO2 [, "Minutes"] <- "00"
M_CO2 [, "Secondes"] <- "00"
M_CO2$Date <- paste(M_CO2$Date, M_CO2$Minutes,M_CO2$Secondes, sep=":")
colnames(M_CO2) [9] <- "DateTime"

M_CO2$DateTime <- as.POSIXct(M_CO2$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CO2 <- M_CO2[,-c(5, 10, 11)]
M_CO2[, "Species"] <- "M_CO2" #Création nouvelle colonnes
colnames(M_CO2)[5] <- "CO2"
M_CO2$flag <- as.character(M_CO2$flag)
M_CO2 <- as.data.table(M_CO2)
M_CO2_flag <- M_CO2[flag == "U"]
names_col <- c("DateTime", "Year", "Month", "Day","Hour","Species","Concentration", "Stdev", "flag")
M_CO2_flag <- as.data.frame(M_CO2_flag)
M_CO2_flag <- M_CO2_flag[, names_col]
colnames(M_CO2_flag)[7] <- "CO2"

colnames(M_CO2_flag) [1] <- "date"


wb_markerNOX <- c("CO2")

myOutput_WB <- timeVariation(M_CO2_flag
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(428, 445)
)
plot(myOutput_WB, subset = "hour")



MEU_CH4 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/MEU_90.0m_air.hdf.all.ch4",
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
M_CH4 [, "Minutes"] <- "00"
M_CH4 [, "Secondes"] <- "00"
M_CH4$Date <- paste(M_CH4$Date, M_CH4$Minutes,M_CH4$Secondes, sep=":")
colnames(M_CH4) [9] <- "DateTime"

M_CH4$DateTime <- as.POSIXct(M_CH4$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CH4 <- M_CH4[,-c(5, 10, 11)]
M_CH4[, "Species"] <- "M_CH4" #Création nouvelle colonnes
colnames(M_CH4)[5] <- "Concentration"
M_CH4$flag <- as.character(M_CH4$flag)
M_CH4 <- as.data.table(M_CH4)
M_CH4_flag <- M_CH4[flag == "U"]
names_col <- c("DateTime", "Year", "Month", "Day","Hour","Species","Concentration", "Stdev", "flag")
M_CH4_flag <- as.data.frame(M_CH4_flag)
M_CH4_flag <- M_CH4_flag[, names_col]
colnames(M_CH4_flag)[7] <- "CH4"


colnames(M_CH4_flag) [1] <- "date"


wb_markerNOX <- c("CH4")

myOutput_WB <- timeVariation(M_CH4_flag
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2020, 2120)
)
plot(myOutput_WB, subset = "hour")

MEU_CO <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/MEU_90.0m_air.hdf.all.co",
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
M_CO [, "Minutes"] <- "00"
M_CO [, "Secondes"] <- "00"
M_CO$Date <- paste(M_CO$Date, M_CO$Minutes,M_CO$Secondes, sep=":")
colnames(M_CO) [9] <- "DateTime"

M_CO$DateTime <- as.POSIXct(M_CO$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
M_CO <- M_CO[,-c(5, 10, 11)]
M_CO[, "Species"] <- "M_CO" #Création nouvelle colonnes
colnames(M_CO)[5] <- "Concentration"
M_CO$flag <- as.character(M_CO$flag)
M_CO <- as.data.table(M_CO)
M_CO_flag <- M_CO[flag == "U"]
names_col <- c("DateTime", "Year", "Month", "Day","Hour","Species","Concentration", "Stdev", "flag")
M_CO_flag <- as.data.frame(M_CO_flag)
M_CO_flag <- M_CO_flag[, names_col]
colnames(M_CO_flag)[7] <- "CO"


colnames(M_CO_flag) [1] <- "date"


wb_markerNOX <- c("CO")

myOutput_WB <- timeVariation(M_CO_flag
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(130, 220)
)
plot(myOutput_WB, subset = "hour")

#'------------------------------------------------------------------------------
# ---- GES_ROV ---- 
#'------------------------------------------------------------------------------
ROV_CO2 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/ROV_103.0m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
R_CO2 <- ROV_CO2[,-c(1,2,7,11,13:17)]
colnames(R_CO2) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CO2","Stdev", "flag")
R_CO2$Month <- sub(pattern = "1", replacement = "01", R_CO2$Month)
R_CO2$Month <- sub(pattern = "2", replacement = "02", R_CO2$Month)
R_CO2$Month <- sub(pattern = "3", replacement = "03", R_CO2$Month)
R_CO2$Date <- paste(R_CO2$Year, R_CO2$Month, R_CO2$Day, sep="/")
R_CO2$Hour <- sub(pattern = "0", replacement = "00", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "1", replacement = "01", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "2", replacement = "02", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "3", replacement = "03", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "4", replacement = "04", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "5", replacement = "05", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "6", replacement = "06", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "7", replacement = "07", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "8", replacement = "08", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "9", replacement = "09", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "010", replacement = "1", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "011", replacement = "11", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "020", replacement = "2", R_CO2$Hour)
R_CO2$Hour <- sub(pattern = "022", replacement = "22", R_CO2$Hour)
R_CO2$Date <- paste(R_CO2$Date, R_CO2$Hour, sep=" ")
R_CO2 [, "Minutes"] <- "00"
R_CO2 [, "Secondes"] <- "00"
R_CO2$Date <- paste(R_CO2$Date, R_CO2$Minutes,R_CO2$Secondes, sep=":")
colnames(R_CO2) [9] <- "DateTime"

R_CO2$DateTime <- as.POSIXct(R_CO2$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
R_CO2 <- R_CO2[,-c(5, 10, 11)]
R_CO2[, "Species"] <- "R_CO2" #Création nouvelle colonnes
colnames(R_CO2)[5] <- "Concentration"
R_CO2$flag <- as.character(R_CO2$flag)
R_CO2 <- as.data.table(R_CO2)
R_CO2_flag <- R_CO2[flag == "U"]
names_col <- c("DateTime", "Year", "Month", "Day","Hour","Species","Concentration", "Stdev", "flag")
R_CO2_flag <- as.data.frame(R_CO2_flag)
R_CO2_flag <- R_CO2_flag[, names_col]
colnames(R_CO2_flag)[7] <- "CO2"


colnames(R_CO2_flag) [1] <- "date"


wb_markerNOX <- c("CO2")

myOutput_WB <- timeVariation(R_CO2_flag
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(428, 445)
)
plot(myOutput_WB, subset = "hour")



ROV_CH4 <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/ROV_103.0m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
R_CH4 <- ROV_CH4[,-c(1,2,7,11,13:17)]
colnames(R_CH4) <- c("Year","Month","Day","Hour",
                     "DecimalDate","CH4","Stdev", "flag")
R_CH4$Month <- sub(pattern = "1", replacement = "01", R_CH4$Month)
R_CH4$Month <- sub(pattern = "2", replacement = "02", R_CH4$Month)
R_CH4$Month <- sub(pattern = "3", replacement = "03", R_CH4$Month)
R_CH4$Date <- paste(R_CH4$Year, R_CH4$Month, R_CH4$Day, sep="/")
R_CH4$Hour <- sub(pattern = "0", replacement = "00", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "1", replacement = "01", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "2", replacement = "02", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "3", replacement = "03", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "4", replacement = "04", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "5", replacement = "05", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "6", replacement = "06", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "7", replacement = "07", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "8", replacement = "08", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "9", replacement = "09", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "010", replacement = "1", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "011", replacement = "11", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "020", replacement = "2", R_CH4$Hour)
R_CH4$Hour <- sub(pattern = "022", replacement = "22", R_CH4$Hour)
R_CH4$Date <- paste(R_CH4$Date, R_CH4$Hour, sep=" ")
R_CH4 [, "Minutes"] <- "00"
R_CH4 [, "Secondes"] <- "00"
R_CH4$Date <- paste(R_CH4$Date, R_CH4$Minutes,R_CH4$Secondes, sep=":")
colnames(R_CH4) [9] <- "DateTime"

R_CH4$DateTime <- as.POSIXct(R_CH4$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
R_CH4 <- R_CH4[,-c(5, 10, 11)]
R_CH4[, "Species"] <- "R_CH4" #Création nouvelle colonnes
colnames(R_CH4)[5] <- "Concentration"
R_CH4$flag <- as.character(R_CH4$flag)
R_CH4 <- as.data.table(R_CH4)
R_CH4_flag <- R_CH4[flag == "U"]
names_col <- c("DateTime", "Year", "Month", "Day","Hour","Species","Concentration", "Stdev", "flag")
R_CH4_flag <- as.data.frame(R_CH4_flag)
R_CH4_flag <- R_CH4_flag[, names_col]
colnames(R_CH4_flag)[7] <- "CH4"


colnames(R_CH4_flag) [1] <- "date"


wb_markerNOX <- c("CH4")

myOutput_WB <- timeVariation(R_CH4_flag
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2020, 2120)
)
plot(myOutput_WB, subset = "hour")

ROV_CO <- read.table(
  "C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/ROV_103.0m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
R_CO <- ROV_CO[,-c(1,2,7,11,13:17)]
colnames(R_CO) <- c("Year","Month","Day","Hour",
                    "DecimalDate","CO","Stdev", "flag")
R_CO$Month <- sub(pattern = "1", replacement = "01", R_CO$Month)
R_CO$Month <- sub(pattern = "2", replacement = "02", R_CO$Month)
R_CO$Month <- sub(pattern = "3", replacement = "03", R_CO$Month)
R_CO$Date <- paste(R_CO$Year, R_CO$Month, R_CO$Day, sep="/")
R_CO$Hour <- sub(pattern = "0", replacement = "00", R_CO$Hour)
R_CO$Hour <- sub(pattern = "1", replacement = "01", R_CO$Hour)
R_CO$Hour <- sub(pattern = "2", replacement = "02", R_CO$Hour)
R_CO$Hour <- sub(pattern = "3", replacement = "03", R_CO$Hour)
R_CO$Hour <- sub(pattern = "4", replacement = "04", R_CO$Hour)
R_CO$Hour <- sub(pattern = "5", replacement = "05", R_CO$Hour)
R_CO$Hour <- sub(pattern = "6", replacement = "06", R_CO$Hour)
R_CO$Hour <- sub(pattern = "7", replacement = "07", R_CO$Hour)
R_CO$Hour <- sub(pattern = "8", replacement = "08", R_CO$Hour)
R_CO$Hour <- sub(pattern = "9", replacement = "09", R_CO$Hour)
R_CO$Hour <- sub(pattern = "010", replacement = "1", R_CO$Hour)
R_CO$Hour <- sub(pattern = "011", replacement = "11", R_CO$Hour)
R_CO$Hour <- sub(pattern = "020", replacement = "2", R_CO$Hour)
R_CO$Hour <- sub(pattern = "022", replacement = "22", R_CO$Hour)
R_CO$Date <- paste(R_CO$Date, R_CO$Hour, sep=" ")
R_CO [, "Minutes"] <- "00"
R_CO [, "Secondes"] <- "00"
R_CO$Date <- paste(R_CO$Date, R_CO$Minutes,R_CO$Secondes, sep=":")
colnames(R_CO) [9] <- "DateTime"

R_CO$DateTime <- as.POSIXct(R_CO$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")
R_CO <- R_CO[,-c(5, 10, 11)]
R_CO[, "Species"] <- "R_CO" #Création nouvelle colonnes
colnames(R_CO)[5] <- "Concentration"
R_CO$flag <- as.character(R_CO$flag)
R_CO <- as.data.table(R_CO)
R_CO_flag <- R_CO[flag == "U"]
names_col <- c("DateTime", "Year", "Month", "Day","Hour","Species","Concentration", "Stdev", "flag")
R_CO_flag <- as.data.frame(R_CO_flag)
R_CO_flag <- R_CO_flag[, names_col]
colnames(R_CO_flag)[7] <- "CO"


colnames(R_CO_flag) [1] <- "date"


wb_markerNOX <- c("CO")

myOutput_WB <- timeVariation(R_CO_flag
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=10))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(130, 220)
)
plot(myOutput_WB, subset = "hour")

