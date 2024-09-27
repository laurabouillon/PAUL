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

GES_MEU <- read.csv2("C:/Users/lbouillo/Documents/PAUL/MEU/ICOS/All_M_01-06.csv", sep=",")
colnames(GES_MEU)[1] <- "date"
GES_MEU$date <- as.POSIXct(GES_MEU$date, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
GES_MEU$CO2 <- as.numeric(GES_MEU$CO2)
GES_MEU$CO <- as.numeric(GES_MEU$CO)
GES_MEU$CH4 <- as.numeric(GES_MEU$CH4)
GES_MEU$NO <- as.numeric(GES_MEU$NO)
GES_MEU$NO2 <- as.numeric(GES_MEU$NO2)

GES_MEU$NOx <- GES_MEU$NO+GES_MEU$NO2

GES_MEU <- as.data.table(GES_MEU)
GES_MEU$hour <- hour(GES_MEU$date)
GES_MEU[, month := format(as.Date(date), "%m")]
GES_MEU_H <- GES_MEU[month == "01" |month == "02" |month == "03"]
GES_MEU_P <- GES_MEU[month == "04" |month == "05" |month == "06"]

GES_MEU_H$NOx <- GES_MEU_H$NO+GES_MEU_H$NO2

NOx_H <- aggregate(NOx~hour ,GES_MEU_H, mean)
max(NOx_H$NOx)

CO2_H <- aggregate(CO2~hour ,GES_MEU_H, mean)
max(CO2_H$CO2)
CH4_H <- aggregate(CH4~hour ,GES_MEU_H, mean)
max(CH4_H$CH4)
CO_H <- aggregate(CO~hour ,GES_MEU_H, mean)
max(CO_H$CO)

CO2_P <- aggregate(CO2~hour ,GES_MEU_P, mean)
max(CO2_P$CO2)
CH4_P <- aggregate(CH4~hour ,GES_MEU_P, mean)
max(CH4_P$CH4)
CO_P <- aggregate(CO~hour ,GES_MEU_P, mean)
max(CO_P$CO)


GES_ROV <- read.csv2("C:/Users/lbouillo/Documents/PAUL/ROV/ICOS/All_M_01-06.csv", sep=",")
colnames(GES_ROV)[1] <- "date"
GES_ROV [, "Secondes"] <- "00"
GES_ROV$date <- paste(GES_ROV$date, GES_ROV$Secondes, sep=":")
GES_ROV$date <- as.POSIXct(GES_ROV$date, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
GES_ROV$CO2 <- as.numeric(GES_ROV$CO2)
GES_ROV$CO <- as.numeric(GES_ROV$CO)
GES_ROV$CH4 <- as.numeric(GES_ROV$CH4)

GES_ROV <- as.data.table(GES_ROV)
GES_ROV[, month := format(as.Date(date), "%m")]
GES_ROV$hour <- hour(GES_ROV$date)
GES_ROV_H <- GES_ROV[month == "01" |month == "02" |month == "03"]
GES_ROV_P <- GES_ROV[month == "04" |month == "05" |month == "06"]

CO2_H <- aggregate(CO2~hour ,GES_ROV_H, mean)
max(CO2_H$CO2)
CH4_H <- aggregate(CH4~hour ,GES_ROV_H, mean)
max(CH4_H$CH4)
CO_H <- aggregate(CO~hour ,GES_ROV_H, mean)
max(CO_H$CO)

CO2_P <- aggregate(CO2~hour ,GES_ROV_P, mean)
max(CO2_P$CO2)
CH4_P <- aggregate(CH4~hour ,GES_ROV_P, mean)
max(CH4_P$CH4)
CO_P <- aggregate(CO~hour ,GES_ROV_P, mean)
max(CO_P$CO)

# ---- CO2 ---- 

wb_markerNOX <- c("CO2")

myOutput_WB <- timeVariation(GES_MEU_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(425, 440)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_ROV_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(425, 440)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_MEU_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="HHOUR"
                             ,ylim = c(415, 442)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_ROV_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="HHOUR"
                             ,ylim = c(415, 442)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_MEU
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(418, 445)
)
plot(myOutput_WB, subset = "day.hour")


myOutput_WB <- timeVariation(GES_ROV
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("orangered")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(418, 445)
)
plot(myOutput_WB, subset = "day.hour")


# ---- CH4 ---- 

wb_markerNOX <- c("CH4")

myOutput_WB <- timeVariation(GES_MEU_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2030, 2075)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_ROV_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2030, 2075)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_MEU_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2030, 2090)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_ROV_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2030, 2090)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_MEU
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2020, 2100)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(GES_ROV
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("turquoise")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(2020, 2100)
)
plot(myOutput_WB, subset = "day.hour")



# ---- CO ---- 

wb_markerNOX <- c("CO")

myOutput_WB <- timeVariation(GES_MEU_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(140, 180)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_ROV_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(140, 180)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_MEU_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(125, 160)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_ROV_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(125, 160)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(GES_MEU
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(120, 210)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(GES_ROV
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("springgreen")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="Concentration ppm"
                             ,xlab ="Hour"
                             ,ylim = c(120, 210)
)
plot(myOutput_WB, subset = "day.hour")
