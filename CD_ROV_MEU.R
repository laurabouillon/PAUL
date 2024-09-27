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
MEU_CO2 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.mdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CO2 <- MEU_CO2[,-c(1,2,11,13:17)]
colnames(MEU_CO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","CO2","Stdev", "flag")

MEU_CO <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.mdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CO <- MEU_CO[,-c(1,2,11,13:17)]
colnames(MEU_CO) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","CO","Stdev", "flag")

MEU_CH4 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.mdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CH4 <- MEU_CH4[,-c(1,2,11,13:17)]
colnames(MEU_CH4) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","CH4","Stdev", "flag")

MEU_NO2 <- read.table(
  "E:/PAUL/MEU/N500/MEU_65m_air.mdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO2 <- MEU_NO2[,-c(1,2,11,13:17)]
colnames(MEU_NO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","NO2","Stdev", "flag")

MEU_NO <- read.table(
  "E:/PAUL/MEU/N500/MEU_65m_air.mdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO <- MEU_NO[,-c(1,2,11,13:17)]
colnames(MEU_NO) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","NO","Stdev", "flag")

MEU_BC <- read.csv2("E:/PAUL/MEU/AE33-S10-01359/Data_Corr_MEU.csv", sep=",")
# Modification date
MEU_CO2<-within(MEU_CO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CO<-within(MEU_CO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CH4<-within(MEU_CH4,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO<-within(MEU_NO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO2<-within(MEU_NO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_BC$DateTime <- as.POSIXct(MEU_BC$DateTime, format = '%d/%m/%Y %H:%M', tz="UTC")
colnames(MEU_BC) [1] <- "date"

# Selection flag
MEU_CO2$flag <- as.character(MEU_CO2$flag)
MEU_CO2 <- as.data.table(MEU_CO2)
MEU_CO2_flag <- MEU_CO2[flag == "U"|flag =="O"|flag =="R"]
MEU_CO2_flag <- MEU_CO2_flag[,-c(1:6,8,9)]
# MEU_CO2_flag$Station <- "MEU"
colnames(MEU_CO2_flag) <- c("CO2_MEU","date")
# colnames(MEU_CO2_flag) [3] <- "flag_CO2"

MEU_CO$flag <- as.character(MEU_CO$flag)
MEU_CO <- as.data.table(MEU_CO)
MEU_CO_flag <- MEU_CO[flag == "U"|flag =="O"|flag =="R"]
MEU_CO_flag <- MEU_CO_flag[,-c(1:6,8,9)]
# MEU_CO_flag$Station <- "MEU"
colnames(MEU_CO_flag) <- c("CO_MEU","date")
# colnames(MEU_CO_flag) [3] <- "flag_CO"

MEU_CH4$flag <- as.character(MEU_CH4$flag)
MEU_CH4 <- as.data.table(MEU_CH4)
MEU_CH4_flag <- MEU_CH4[flag == "U"|flag =="O"|flag =="R"]
MEU_CH4_flag <- MEU_CH4_flag[,-c(1:6,8,9)]
# MEU_CH4_flag$Station <- "MEU"
colnames(MEU_CH4_flag) <- c("CH4_MEU","date")
# colnames(MEU_CH4_flag) [3] <- "flag_CH4"

MEU_NO$flag <- as.character(MEU_NO$flag)
MEU_NO <- as.data.table(MEU_NO)
MEU_NO_flag <- MEU_NO[flag == "U"|flag =="O"|flag =="R"]
MEU_NO_flag <- MEU_NO_flag[,-c(1:6,8,9)]
# MEU_NO_flag$Station <- "MEU"
colnames(MEU_NO_flag) <- c("NO_MEU","date")
# colnames(MEU_NO_flag) [3] <- "flag_NO"

MEU_NO2$flag <- as.character(MEU_NO2$flag)
MEU_NO2 <- as.data.table(MEU_NO2)
MEU_NO2_flag <- MEU_NO2[flag == "U"|flag =="O"|flag =="R"]
MEU_NO2_flag <- MEU_NO2_flag[,-c(1:6,8,9)]
# MEU_NO2_flag$Station <- "MEU"
colnames(MEU_NO2_flag) <- c("NO2_MEU","date")
# colnames(MEU_NO2_flag) [3] <- "flag_NO2"

MEU_BC$BCff <- as.numeric(MEU_BC$BCff)
MEU_BC$BCwb <- as.numeric(MEU_BC$BCwb)
colnames(MEU_BC) <- c("date","BCff_MEU", "BCwb_MEU")
# MEU_BC$Station <- "MEU"

# MEU_NOx <- merge(
#   x = MEU_NO2_flag,
#   y = MEU_NO_flag,
#   by ="date")
# colnames(MEU_NOx) [2] <- "Month"





#'------------------------------------------------------------------------------
# ---- ROV ---- 
#'------------------------------------------------------------------------------
ROV_CO2 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.mdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CO2 <- ROV_CO2[,-c(1,2,11,13:17)]
colnames(ROV_CO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","CO2","Stdev", "flag")

ROV_CO <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.mdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CO <- ROV_CO[,-c(1,2,11,13:17)]
colnames(ROV_CO) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","CO","Stdev", "flag")

ROV_CH4 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.mdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CH4 <- ROV_CH4[,-c(1,2,11,13:17)]
colnames(ROV_CH4) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","CH4","Stdev", "flag")

ROV_NO2 <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_air.mdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO2 <- ROV_NO2[,-c(1,2,11,13:17)]
colnames(ROV_NO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","NO2","Stdev", "flag")

ROV_NO <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_air.mdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO <- ROV_NO[,-c(1,2,11,13:17)]
colnames(ROV_NO) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","NO","Stdev", "flag")

ROV_BC <- read.csv2("E:/PAUL/ROV/AE33-S10-01368/Data_Corr_ROV.csv", sep=",")
# Modification date
ROV_CO2<-within(ROV_CO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_CO<-within(ROV_CO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_CH4<-within(ROV_CH4,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_NO<-within(ROV_NO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_NO2<-within(ROV_NO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_BC$DateTime <- as.POSIXct(ROV_BC$DateTime, format = '%d/%m/%Y %H:%M', tz="UTC")
colnames(ROV_BC) [1] <- "date"

# Selection flag
ROV_CO2$flag <- as.character(ROV_CO2$flag)
ROV_CO2 <- as.data.table(ROV_CO2)
ROV_CO2_flag <- ROV_CO2[flag == "U"|flag =="O"|flag =="R"]
ROV_CO2_flag <- ROV_CO2_flag[,-c(1:6,8,9)]
# ROV_CO2_flag$Station <- "ROV"
colnames(ROV_CO2_flag) <- c("CO2_ROV","date")
# colnames(ROV_CO2_flag) [3] <- "flag_CO2"

ROV_CO$flag <- as.character(ROV_CO$flag)
ROV_CO <- as.data.table(ROV_CO)
ROV_CO_flag <- ROV_CO[flag == "U"|flag =="O"|flag =="R"]
ROV_CO_flag <- ROV_CO_flag[,-c(1:6,8,9)]
# ROV_CO_flag$Station <- "ROV"
colnames(ROV_CO_flag) <- c("CO_ROV","date")
# colnames(ROV_CO_flag) [3] <- "flag_CO"

ROV_CH4$flag <- as.character(ROV_CH4$flag)
ROV_CH4 <- as.data.table(ROV_CH4)
ROV_CH4_flag <- ROV_CH4[flag == "U"|flag =="O"|flag =="R"]
ROV_CH4_flag <- ROV_CH4_flag[,-c(1:6,8,9)]
# ROV_CH4_flag$Station <- "ROV"
colnames(ROV_CH4_flag) <- c("CH4_ROV","date")
# colnames(ROV_CH4_flag) [3] <- "flag_CH4"

ROV_NO$flag <- as.character(ROV_NO$flag)
ROV_NO <- as.data.table(ROV_NO)
ROV_NO_flag <- ROV_NO[flag == "U"|flag =="O"|flag =="R"]
ROV_NO_flag <- ROV_NO_flag[,-c(1:6,8,9)]
# ROV_NO_flag$Station <- "ROV"
colnames(ROV_NO_flag) <- c("NO_ROV","date")
# colnames(ROV_NO_flag) [3] <- "flag_NO"

ROV_NO2$flag <- as.character(ROV_NO2$flag)
ROV_NO2 <- as.data.table(ROV_NO2)
ROV_NO2_flag <- ROV_NO2[flag == "U"|flag =="O"|flag =="R"]
ROV_NO2_flag <- ROV_NO2_flag[,-c(1:6,8,9)]
# ROV_NO2_flag$Station <- "ROV"
colnames(ROV_NO2_flag) <- c("NO2_ROV","date")
# colnames(ROV_NO2_flag) [3] <- "flag_NO2"

ROV_BC$BCff <- as.numeric(ROV_BC$BCff)
ROV_BC$BCwb <- as.numeric(ROV_BC$BCwb)
colnames(ROV_BC) <- c("date","BCff_ROV", "BCwb_ROV")
# ROV_BC$Station <- "ROV"

# ROV_NOx <- merge(
#   x = ROV_NO2_flag,
#   y = ROV_NO_flag,
#   by ="date")
# colnames(ROV_NOx) [2] <- "Month"


#'------------------------------------------------------------------------------
# ---- Tableaux saisons ---- 
#'------------------------------------------------------------------------------
#'
All <- merge(
x = MEU_NO2_flag,
y = MEU_NO_flag,
by ="date", 
all = TRUE)
# colnames(CO2) [2] <- "Month"

All <- merge(
  x = All,
  y = MEU_CH4_flag,
  by ="date", 
  all = TRUE)
# colnames(CH4) [2] <- "Month"

All <- merge(
  x = MEU_NO_flag,
  y = All,
  by ="date", 
  all = TRUE)
# colnames(CO) [2] <- "Month"

All <- merge(
  x = MEU_NO2_flag,
  y = All,
  by ="date", 
  all = TRUE)
# colnames(NO) [2] <- "Month"

All <- merge(
  x = MEU_BC,
  y = All,
  by ="date", 
  all = TRUE)
# colnames(NO2) [2] <- "Month"
# MEU$Station <- "MEU"



All <- merge(
  x = All,
  y = ROV_CO2_flag,
  by ="date", 
  all = TRUE)
# colnames(CO2) [2] <- "Month"

All <- merge(
  x = All,
  y = ROV_CO_flag,
  by ="date", 
  all = TRUE)

All <- merge(
  x = All,
  y = ROV_CH4_flag,
  by ="date", 
  all = TRUE)
# colnames(CH4) [2] <- "Month"

All <- merge(
  x = ROV_NO_flag,
  y = All,
  by ="date", 
  all = TRUE)
# colnames(CO) [2] <- "Month"

All <- merge(
  x = ROV_NO2_flag,
  y = All,
  by ="date", 
  all = TRUE)
# colnames(NO) [2] <- "Month"

All <- merge(
  x = ROV_BC,
  y = All,
  by ="date", 
  all = TRUE)
# colnames(NO2) [2] <- "Month"
# ROV$Station <- "ROV"
All_H <- timeAverage(All, "hour")
All_H <- as.data.table(All_H)
All_H <- as.data.frame(All_H)



write.table(All_H,"E:/PAUL/DATA_FINAL/All_MEU_ROV_HA.csv", sep=",", row.names = FALSE)

write.table(All_H, "E:/PAUL/DATA_FINAL/All_MEU_ROV_HA.txt", sep = ",", dec = ".",
            row.names = FALSE, col.names = TRUE)

All <- rbind(MEU, ROV)

speciesLabels <- c(ch4 = "CH4 \n(ppb)", co = "CO \n(ppb)", co2 = "CO2 \n(ppm)")
cccDt_DA$Species_DA = factor(cccDt_DA$Species,
                             levels=c('co2','ch4','co')) 
plot <- ggplot()+
  geom_line(data = cccDt_GNS_2017_DA_SC,
            mapping = aes(x = Datetime, y = Conc_2017, color="2017"))+
  geom_line(data = cccDt_GNS_2018_DA_SC,
            mapping = aes(x = Datetime, y = Conc_2018, color="2018"))+
  geom_line(data = cccDt_GNS_2019_DA_SC,
            mapping = aes(x = Datetime, y = Conc_2019, color="2019"))+
  geom_line(data = cccDt_GNS_2020_DA_SC,
            mapping = aes(x = Datetime, y = Conc_2020, color="2020"))+
  geom_line(data = cccDt_GNS_2021_DA_SC,
            mapping = aes(x = Datetime, y = Conc_2021, color="2021"))+
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank()) +
  facet_grid(Species ~ ., scales = "free_y",
             labeller=labeller(Species=speciesLabels)) +
  xlab("Time")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "Comparison GNS - SAC_100m (daily average data & selection criteria) "))

plot_grid(
  ggplot(data = BCff,
         mapping = aes(x = date, y = ROV) + theme(axis.title.x = element_blank()), axis.text.x = element_blank()),
  ggplot(data = BCwb,
       mapping = aes(x = date, y = ROV)),
  align = "v",
  nrow = 2, ncol = 1, rel_heights = c(1, 2))

plot
ggplotly(plot)

timePlot(All, 
         pollutant = c("NO_MEU","NO_ROV"),
         group = TRUE,
         cols = c("blue", "red"),
         y.relation = "free", 
         ylab ="[NO] ppb", 
         labelFontsize=20)

timePlot(All, 
         pollutant = c("NO2_MEU","NO2_ROV"),
         group = TRUE,
         cols = c("blue", "red"),
         y.relation = "free", 
         ylab ="[NO2] ppb", 
         labelFontsize=20)

timePlot(All, 
         pollutant = c("BCff_MEU","BCff_ROV"),
         group = TRUE,
         cols = c("blue", "red"),
         y.relation = "free", 
         ylab ="[BCff] ng/m3", 
         labelFontsize=20)

timePlot(All, 
         pollutant = c("BCwb_MEU","BCwb_ROV"),
         group = TRUE,
         cols = c("blue", "red"),
         y.relation = "free", 
         ylab ="[BCwb] ng/m3", 
         labelFontsize=20)

timePlot(All, 
         pollutant = c("CO2_MEU","CO2_ROV"),
         group = TRUE,
         cols = c("blue", "red"),
         y.relation = "free", 
         ylab ="[CO2] ppm", 
         labelFontsize=20)

timePlot(All, 
         pollutant = c("CO_MEU","CO_ROV"),
         group = TRUE,
         cols = c("blue", "red"),
         y.relation = "free", 
         ylab ="[CO] ppb", 
         labelFontsize=20)




CH4 <- CH4[,-c(2,4:6,8,9)]
CO <- CO[,-c(2,4:6,8,9)]
CO2 <- CO2[,-c(2,4:6,8,9)]
NO <- NO[,-c(2,4:6,8,9)]
NO2 <- NO2[,-c(2,4:6,8,9)]

colnames(CH4) <- c("date","CH4_MEU","CH4_ROV")
colnames(CO) <- c("date","CO_MEU","CO_ROV")
colnames(CO2) <- c("date","CO2_MEU","CO2_ROV")
colnames(NO2) <- c("date","NO2_MEU","NO2_ROV")
colnames(NO) <- c("date","NO_MEU","NO_ROV")

All <- merge(
  x = CH4,
  y = CO,
  all = TRUE,
  by ="date")

All <- merge(
  x = CO2,
  y = All,
  all = TRUE,
  by ="date")

All <- merge(
  x = NO2,
  y = All,
  all = TRUE,
  by ="date")

All <- merge(
  x = NO,
  y = All,
  all = TRUE,
  by ="date")

All_H <- timeAverage(All, "hour")


write.table(All_H,"E:/PAUL/DATA_FINAL/All_HA.csv", sep=",", row.names = FALSE)

CO2_H <- filter(CO2,Month>=1 & Month<= 3)
CO2_P <- filter(CO2,Month>=4 & Month<= 6)
CO2_E <- filter(CO2,Month>=7 & Month<= 9)


CH4_H <- filter(CH4,Month>=1 & Month<= 3)
CH4_P <- filter(CH4,Month>=4 & Month<= 6)
CH4_E <- filter(CH4,Month>=7 & Month<= 9)


CO_H <- filter(CO,Month>=1 & Month<= 3)
CO_P <- filter(CO,Month>=4 & Month<= 6)
CO_E <- filter(CO,Month>=7 & Month<= 9)


NO_H <- filter(NO_flag,Month>=1 & Month<= 3)
NO_P <- filter(NO_flag,Month>=4 & Month<= 6)
NO_E <- filter(NO_flag,Month>=7 & Month<= 9)

NO2_H <- filter(NO2_flag,Month>=1 & Month<= 3)
NO2_P <- filter(NO2_flag,Month>=4 & Month<= 6)
NO2_E <- filter(NO2_flag,Month>=7 & Month<= 9)


BC <- merge(
  x = ROV_BC,
  y = MEU_BC,
  all = TRUE,
  by ="date")

BCwb <- BC[,-c(2, 4)]
BCff <- BC[,-c(3, 5)]
colnames(BCwb) <- c("date","ROV","MEU")
colnames(BCff) <- c("date","ROV","MEU")

#'------------------------------------------------------------------------------
# ---- Plot ---- 
#'------------------------------------------------------------------------------

wb_markerNOX <- c("MEU", "ROV")

myOutput_WB <- timeVariation(BCff
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue", "red")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="BCff (ng.m-3)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "day.hour")

myOutput_WB <- timeVariation(BCwb
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("blue", "red")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="BCwb (ng.m-3)"
                             ,xlab ="Hour"
                            # ,ylim = c(0, 441)
)
plot(myOutput_WB, subset = "day.hour")
                             

myOutput_WB <- timeVariation(CO2
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             # , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CO2 (ppm)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CO2_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CO2 (ppm)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CO2_E
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CO2 (ppm)"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")


myOutput_WB <- timeVariation(CH4
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             # , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CH4 (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CH4_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CH4 (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CH4_E
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CH4 (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CO_H
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CO (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CO_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CO (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(CO
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             # , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="CO (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(NO
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             # , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="NO (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(NO_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="NO (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(NO_E
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,normalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="NO (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(NO2
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,NO2rmalise = T
                             # , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="NO2 (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(NO2_P
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,NO2rmalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="NO2 (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(425, 441)
)
plot(myOutput_WB, subset = "hour")

myOutput_WB <- timeVariation(NO2_E
                             ,pollutant = wb_markerNOX
                             ,name.pol = wb_markerNOX
                             ,difference = F
                             #,NO2rmalise = T
                             , cols = c("red", "blue")
                             ,par.settings=list(fontsize=list(text=20))
                             ,ylab ="NO2 (ppb)"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

