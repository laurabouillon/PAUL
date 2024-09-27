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

# ---- Meteo_MEU ---- 
Meteo <- read.table(
  "E:/PAUL/MEU/Meteo/MEU_75m_air.hdf.all.mto",
  comment.char = "#", stringsAsFactors = T, sep =";") 
colnames(Meteo) <- c("Site","SamplingHeight","Year","Month","Day","Hour","Minute","DecimalDate","AP","AP_Stdev",
                         "AP_NbPoints","AP_Flag","AP_QualityId","AP_AutoDescriptiveFlag","AP_ManualDescriptiveFlag",
                         "AT","AT_Stdev","AT_NbPoints","AT_Flag","AT_QualityId","AT_AutoDescriptiveFlag","AT_ManualDescriptiveFlag",
                         "RH","RH_Stdev","RH_NbPoints","RH_Flag","RH_QualityId","RH_AutoDescriptiveFlag","RH_ManualDescriptiveFlag",
                         "WS","WS_Stdev","WS_NbPoints","WS_Flag","WS_QualityId","WS_AutoDescriptiveFlag","WS_ManualDescriptiveFlag",
                         "WD","WD_Stdev","WD_NbPoints","WD_Flag","WD_QualityId","WD_AutoDescriptiveFlag","WD_ManualDescriptiveFlag",
                         "InstrumentId")
Meteo_MEU <- Meteo[,-c(1,2,8,10,11,13:15,17,18,20:22,24,25,27:29,31,32,34:36,38,39,41:44)]

Meteo_MEU <- within(Meteo_MEU,{
  Datetime <- as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})


Meteo_MEU <- Meteo_MEU %>% mutate_if(is.factor, as.character)
                                                  
Meteo_MEU <- as.data.table(Meteo_MEU)                                                
Meteo_MEU_flag <- Meteo_MEU[AP_Flag == "U"|AP_Flag =="O"|AT_Flag == "U"|AT_Flag =="O"|
                              RH_Flag == "U"|RH_Flag =="O"|WS_Flag == "U"|WS_Flag =="O"|
                              WD_Flag == "U"|WD_Flag =="O"]


Meteo_MEU_flag <- Meteo_MEU_flag[,-c(1:5,7,9,11,13,15)]

# ---- NO2_MEU ---- 
MEU_NO2 <- read.table(
  "E:/PAUL/MEU/N500/MEU_65m_air.hdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO2 <- MEU_NO2[,-c(1,2,11,13:17)]
colnames(MEU_NO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_NO2<-within(MEU_NO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO2$flag <- as.character(MEU_NO2$flag)
MEU_NO2 <- as.data.table(MEU_NO2)
MEU_NO2_flag <- MEU_NO2[flag == "U"|flag =="O"|flag =="R"]
MEU_NO2_flag <- MEU_NO2_flag[,-c(1:6)]
MEU_NO2_flag <- MEU_NO2_flag[,-c(2:3)]

# MEU_NO2_flag <- MEU_NO2_flag[-c(841),]

# ---- MEGATABLE_MEU ---- 
MEU <- merge(
  x = Meteo_MEU_flag,
  y = MEU_NO2_flag,
  by ="Datetime",
  all.x = TRUE)





# ---- Meteo_ROV ---- 
Meteo <- read.table(
  "E:/PAUL/ROV/Meteo/ROV_103m_air.hdf.all.mto",
  comment.char = "#", stringsAsFactors = T, sep =";") 
colnames(Meteo) <- c("Site","SamplingHeight","Year","Month","Day","Hour","Minute","DecimalDate","AP","AP_Stdev",
                     "AP_NbPoints","AP_Flag","AP_QualityId","AP_AutoDescriptiveFlag","AP_ManualDescriptiveFlag",
                     "AT","AT_Stdev","AT_NbPoints","AT_Flag","AT_QualityId","AT_AutoDescriptiveFlag","AT_ManualDescriptiveFlag",
                     "RH","RH_Stdev","RH_NbPoints","RH_Flag","RH_QualityId","RH_AutoDescriptiveFlag","RH_ManualDescriptiveFlag",
                     "WS","WS_Stdev","WS_NbPoints","WS_Flag","WS_QualityId","WS_AutoDescriptiveFlag","WS_ManualDescriptiveFlag",
                     "WD","WD_Stdev","WD_NbPoints","WD_Flag","WD_QualityId","WD_AutoDescriptiveFlag","WD_ManualDescriptiveFlag",
                     "InstrumentId")
Meteo_ROV <- Meteo[,-c(1,2,8,10,11,13:15,17,18,20:22,24,25,27:29,31,32,34:36,38,39,41:44)]

Meteo_ROV <- within(Meteo_ROV,{
  Datetime <- as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                         format="%Y %m %d %H %M",tz="UTC")})


Meteo_ROV <- Meteo_ROV %>% mutate_if(is.factor, as.character)

Meteo_ROV <- as.data.table(Meteo_ROV)                                                
Meteo_ROV_flag <- Meteo_ROV[AP_Flag == "U"|AP_Flag =="O"|AT_Flag == "U"|AT_Flag =="O"|
                              RH_Flag == "U"|RH_Flag =="O"|WS_Flag == "U"|WS_Flag =="O"|
                              WD_Flag == "U"|WD_Flag =="O"]


Meteo_ROV_flag <- Meteo_ROV_flag[,-c(1:5,7,9,11,13,15)]

# ---- NO2_ROV ---- 
ROV_NO2 <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_air.hdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO2 <- ROV_NO2[,-c(1,2,11,13:17)]
colnames(ROV_NO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_NO2<-within(ROV_NO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_NO2$flag <- as.character(ROV_NO2$flag)
ROV_NO2 <- as.data.table(ROV_NO2)
ROV_NO2_flag <- ROV_NO2[flag == "U"|flag =="O"|flag =="R"]
ROV_NO2_flag <- ROV_NO2_flag[,-c(1:6)]
ROV_NO2_flag <- ROV_NO2_flag[,-c(2:3)]

# ROV_NO2_flag <- ROV_NO2_flag[-c(841),]

# ---- MEGATABLE_ROV ---- 
ROV <- merge(
  x = Meteo_ROV_flag,
  y = ROV_NO2_flag,
  by ="Datetime",
  all.x = TRUE)


rm(list=ls()[! ls() %in% c("MEU", "ROV")])


windRose(MEU, ws ="WS", wd="WD")
windRose(ROV, ws ="WS", wd="WD")
