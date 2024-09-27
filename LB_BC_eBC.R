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
library('ggpubr')
library("ncdf4")

# ---- All météo ---- 
BC<- read.table(
  "E:/SACLAY/DATA_SIRTA/BC/BCff_BCwb_2011_2022.txt", sep ="\t", dec = ".", header = TRUE)
colnames(BC) <- c("date","BCff", "BCwb")
BC$date <- as.POSIXct(BC$date, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
BC_HA <- timeAverage(BC, "1 hour")
BC_HA$BCtot <- BC_HA$BCff + BC_HA$BCwb

BC_V2 <- read.table(
  "E:/SACLAY/DATA_SIRTA/BC/eBC_SIRTA.txt", sep ="\t", dec = ".", header = TRUE)
colnames(BC_V2) <- c("date","eBC")
BC_V2$date <- as.POSIXct(BC_V2$date, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
BC_V2_HA <- timeAverage(BC_V2, "1 hour")


All <- merge(
  x = BC_HA,
  y = BC_V2_HA, 
  all = TRUE,
  by ="date")


All$sf <- All$BCwb/All$BCtot
All$eBCsf <- All$eBC * All$sf
All$eBClf <- All$eBC - All$eBCsf

All_F <- filter(All,eBCsf>=-100 & eBClf>= -100)

AQ_GES <- read.csv("E:/SACLAY/DATA_FINAL/GES_AQ_meteo_2012_2022_HA_VF.csv")

# Aq_V2 <- AQ_GES %>% distinct(.keep_all = TRUE)


AQ_GES$date <- as.POSIXct(AQ_GES$date, format = '%d/%m/%Y %H:%M', tz="UTC")
AQ_GES_HA <- timeAverage(AQ_GES, "1 hour")

All <- merge(
  x = BC,
  y = AQ_GES, 
  all = TRUE,
  by ="date")

write.table(All_F,"E:/SACLAY/DATA_FINAL/CAE/BC_2012_2022_HA.csv", sep=",", row.names = FALSE)

# ----MEU ---- 

MEU <- read.table(
  "E:/PAUL/MEU/AE33/Data_wb_ff_130123_210631_MEU.csv", sep =",", header = TRUE)
colnames(MEU) <- c("date","BClf", "BCsf", "eBC")
MEU$date <- as.POSIXct(MEU$date, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
MEU_HA <- timeAverage(MEU, "1 hour")
MEU <- MEU [-c(1:7),]
MEU$BC1 <- MEU$BCsf+MEU$BClf
MEU$BC2 <- MEU$BCsf/MEU$BC1
MEU$eBCsf <- MEU$eBC*MEU$BC2
