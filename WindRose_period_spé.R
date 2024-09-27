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

# ---- MEUDON ----
MEU_meteo <- read.table(
  "E:/PAUL/MEU/Meteo/MEU_75m_air.hdf.all.mto",
  comment.char = "#", stringsAsFactors = T, sep =";") 

colnames(MEU_meteo) <- c("Site","SamplingHeight","Year","Month","Day","Hour","Minute","DecimalDate","AP","AP-Stdev","AP-NbPoints","AP-Flag","AP-QualityId","AP-AutoDescriptiveFlag","AP-ManualDescriptiveFlag","AT","AT-Stdev","AT-NbPoints","AT-Flag","AT-QualityId","AT-AutoDescriptiveFlag","AT-ManualDescriptiveFlag","RH","RH-Stdev","RH-NbPoints","RH-Flag","RH-QualityId","RH-AutoDescriptiveFlag","RH-ManualDescriptiveFlag","WS","WS-Stdev","WS-NbPoints","WS-Flag","WS-QualityId","WS-AutoDescriptiveFlag","WS-ManualDescriptiveFlag","WD","WD-Stdev","WD-NbPoints","WD-Flag","WD-QualityId","WD-AutoDescriptiveFlag","WD-ManualDescriptiveFlag","InstrumentId")

MEU_meteo<-within(MEU_meteo,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})


windRose(MEU_meteo, ws ="WS", wd="WD")


MEU <- ggplot(MEU_meteo, aes(x = WD, y = WS)) +
  geom_col(width = 1, fill = "skyblue3") +
  coord_polar(start = 0) +
  scale_y_sqrt() +
  # theme_void() +
  ggtitle("Diagramme des causes de mortalité dans l'armée à l'est")+
  scale_x_continuous(breaks = seq(0, 360, by = 45), labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"))

MEU


# ---- ROMAINVILLE ----
ROV_meteo <- read.table(
  "E:/PAUL/ROV/Meteo/ROV_103m_air.hdf.all.mto",
  comment.char = "#", stringsAsFactors = T, sep =";") 

colnames(ROV_meteo) <- c("Site","SamplingHeight","Year","Month","Day","Hour","Minute","DecimalDate","AP","AP-Stdev","AP-NbPoints","AP-Flag","AP-QualityId","AP-AutoDescriptiveFlag","AP-ManualDescriptiveFlag","AT","AT-Stdev","AT-NbPoints","AT-Flag","AT-QualityId","AT-AutoDescriptiveFlag","AT-ManualDescriptiveFlag","RH","RH-Stdev","RH-NbPoints","RH-Flag","RH-QualityId","RH-AutoDescriptiveFlag","RH-ManualDescriptiveFlag","WS","WS-Stdev","WS-NbPoints","WS-Flag","WS-QualityId","WS-AutoDescriptiveFlag","WS-ManualDescriptiveFlag","WD","WD-Stdev","WD-NbPoints","WD-Flag","WD-QualityId","WD-AutoDescriptiveFlag","WD-ManualDescriptiveFlag","InstrumentId")

ROV_meteo<-within(ROV_meteo,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})


windRose(ROV_meteo, ws ="WS", wd="WD")


ROV <- ggplot(ROV_meteo, aes(x = WD, y = WS)) +
  geom_col(width = 1, fill = "skyblue3") +
  coord_polar(start = 0) +
  scale_y_sqrt() +
  # theme_void() +
  ggtitle("Diagramme des causes de mortalité dans l'armée à l'est")+
  scale_x_continuous(breaks = seq(0, 360, by = 45), labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N"))

ROV


