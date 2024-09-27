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

#---- MEU ---- 
#  Chargement des données
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

MEU_BC_2023 <- read.csv2("E:/PAUL/MEU/Data_VF/Data2023_MEU.csv", sep=",")
MEU_BC_2024 <- read.csv2("E:/PAUL/MEU/Data_VF/Data2024_MEU.csv", sep=",")
MEU_BC <- rbind(MEU_BC_2023, MEU_BC_2024)

# Modification date
MEU_CO2<-within(MEU_CO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CO<-within(MEU_CO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CH4<-within(MEU_CH4,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO<-within(MEU_NO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO2<-within(MEU_NO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_BC$BCdate <- as.POSIXct(MEU_BC$BCdate, format = '%d/%m/%Y %H:%M:%S', tz="UTC")

# Selection flag
MEU_CO2$flag <- as.character(MEU_CO2$flag)
MEU_CO2 <- as.data.table(MEU_CO2)
MEU_CO2_flag <- MEU_CO2[flag == "U"|flag =="O"|flag =="R"]
MEU_CO2_flag <- MEU_CO2_flag[,-c(1:6)]
colnames(MEU_CO2_flag) [2] <- "Stdev_CO2"
colnames(MEU_CO2_flag) [3] <- "flag_CO2"

MEU_CO$flag <- as.character(MEU_CO$flag)
MEU_CO <- as.data.table(MEU_CO)
MEU_CO_flag <- MEU_CO[flag == "U"|flag =="O"|flag =="R"]
MEU_CO_flag <- MEU_CO_flag[,-c(1:6)]
colnames(MEU_CO_flag) [2] <- "Stdev_CO"
colnames(MEU_CO_flag) [3] <- "flag_CO"

MEU_CH4$flag <- as.character(MEU_CH4$flag)
MEU_CH4 <- as.data.table(MEU_CH4)
MEU_CH4_flag <- MEU_CH4[flag == "U"|flag =="O"|flag =="R"]
MEU_CH4_flag <- MEU_CH4_flag[,-c(1:6)]
colnames(MEU_CH4_flag) [2] <- "Stdev_CH4"
colnames(MEU_CH4_flag) [3] <- "flag_CH4"

MEU_NO$flag <- as.character(MEU_NO$flag)
MEU_NO <- as.data.table(MEU_NO)
MEU_NO_flag <- MEU_NO[flag == "U"|flag =="O"|flag =="R"]
MEU_NO_flag <- MEU_NO_flag[,-c(1:6)]
colnames(MEU_NO_flag) [2] <- "Stdev_NO"
colnames(MEU_NO_flag) [3] <- "flag_NO"

MEU_NO2$flag <- as.character(MEU_NO2$flag)
MEU_NO2 <- as.data.table(MEU_NO2)
MEU_NO2_flag <- MEU_NO2[flag == "U"|flag =="O"|flag =="R"]
MEU_NO2_flag <- MEU_NO2_flag[,-c(1:6)]
colnames(MEU_NO2_flag) [2] <- "Stdev_NO2"
colnames(MEU_NO2_flag) [3] <- "flag_NO2"

colnames(MEU_BC) <- c("DateTime","BCff","BCwb","BCtot")
MEU_BC$BCff <- as.numeric(MEU_BC$BCff)
MEU_BC$BCwb <- as.numeric(MEU_BC$BCwb)
MEU_BC$BCtot <- as.numeric(MEU_BC$BCtot)

MEU_BCff <- MEU_BC[,-c(3,4)]
colnames(MEU_BCff) <- c("Datetime","BCff")
MEU_BCwb <- MEU_BC[,-c(2,4)]
colnames(MEU_BCwb) <- c("Datetime","BCwb")
MEU_BCtot <- MEU_BC[,-c(2,3)]
colnames(MEU_BCtot) <- c("Datetime","BCtot")

# Création NOx
MEU_NOx_flag <- merge(
  x = MEU_NO_flag,
  y = MEU_NO2_flag, 
  all = TRUE,
  by ="Datetime")

MEU_NOx_flag$NOx <- MEU_NOx_flag$NO + MEU_NOx_flag$NO2

# Merge des différents tableaux
MEU_CO_CO2 <- merge(
  x = MEU_CO2_flag,
  y = MEU_CO_flag,
  by ="Datetime")

MEU_CO2_CH4 <- merge(
  x = MEU_CO2_flag,
  y = MEU_CH4_flag,
  by ="Datetime")

MEU_NOx_CO2 <- merge(
  x = MEU_CO2_flag,
  y = MEU_NOx_flag,
  by ="Datetime")

MEU_NOx_CO <- merge(
  x = MEU_CO_flag,
  y = MEU_NOx_flag,
  by ="Datetime")

MEU_BCtot_CO <- merge(
  x = MEU_CO_flag,
  y = MEU_BCtot,
  by ="Datetime")

MEU_BCtot_CO2 <- merge(
  x = MEU_CO2_flag,
  y = MEU_BCtot,
  by ="Datetime")


# Création de le colonnes des heures
MEU_CO_CO2[, hour := as.numeric(format(Datetime, "%H"))]
MEU_CO2_CH4[, hour := as.numeric(format(Datetime, "%H"))]
MEU_NOx_CO[, hour := as.numeric(format(Datetime, "%H"))]
MEU_NOx_CO2[, hour := as.numeric(format(Datetime, "%H"))]
MEU_BCtot_CO[, hour := as.numeric(format(Datetime, "%H"))]
MEU_BCtot_CO2[, hour := as.numeric(format(Datetime, "%H"))]

# Sélection des heures
MEU_CO_CO2_H <- filter(MEU_CO_CO2,hour>=5 & hour<= 12)
MEU_CO2_CH4_H <- filter(MEU_CO2_CH4,hour>=5 & hour<= 12)
MEU_NOx_CO2_H <- filter(MEU_NOx_CO2,hour>=5 & hour<= 12)
MEU_NOx_CO_H <- filter(MEU_NOx_CO,hour>=5 & hour<= 12)
MEU_BCtot_CO_H <- filter(MEU_BCtot_CO,hour>=5 & hour<= 12)
MEU_BCtot_CO2_H <- filter(MEU_BCtot_CO2,hour>=5 & hour<= 12)

# Changement du format de la date pour supprimer les heures, minutes, secondes
MEU_CO_CO2_H$Datetime <- format(as.POSIXct(MEU_CO_CO2_H$Datetime,
                                           format = '%Y-%m-%d %H:%M:%S'),
                                format = '%Y-%m-%d')
MEU_CO_CO2_H$Datetime <- as.POSIXct(strptime(MEU_CO_CO2_H$Datetime,'%Y-%m-%d'))

MEU_CO2_CH4_H$Datetime <- format(as.POSIXct(MEU_CO2_CH4_H$Datetime,
                                            format = '%Y-%m-%d %H:%M:%S'),
                                 format = '%Y-%m-%d')
MEU_CO2_CH4_H$Datetime <- as.POSIXct(strptime(MEU_CO2_CH4_H$Datetime,'%Y-%m-%d'))

MEU_NOx_CO2_H$Datetime <- format(as.POSIXct(MEU_NOx_CO2_H$Datetime,
                                            format = '%Y-%m-%d %H:%M:%S'),
                                 format = '%Y-%m-%d')
MEU_NOx_CO2_H$Datetime <- as.POSIXct(strptime(MEU_NOx_CO2_H$Datetime,'%Y-%m-%d'))

MEU_NOx_CO_H$Datetime <- format(as.POSIXct(MEU_NOx_CO_H$Datetime,
                                           format = '%Y-%m-%d %H:%M:%S'),
                                format = '%Y-%m-%d')
MEU_NOx_CO_H$Datetime <- as.POSIXct(strptime(MEU_NOx_CO_H$Datetime,'%Y-%m-%d'))

MEU_BCtot_CO_H$Datetime <- format(as.POSIXct(MEU_BCtot_CO_H$Datetime,
                                              format = '%Y-%m-%d %H:%M:%S'),
                                   format = '%Y-%m-%d')
MEU_BCtot_CO_H$Datetime <- as.POSIXct(strptime(MEU_BCtot_CO_H$Datetime,'%Y-%m-%d'))

MEU_BCtot_CO2_H$Datetime <- format(as.POSIXct(MEU_BCtot_CO2_H$Datetime,
                                           format = '%Y-%m-%d %H:%M:%S'),
                                format = '%Y-%m-%d')

# Changement du nom pour la library lubridate
colnames(MEU_CO_CO2_H) [1] <- "date"
colnames(MEU_CO2_CH4_H) [1] <- "date"
colnames(MEU_NOx_CO2_H) [1] <- "date"
colnames(MEU_NOx_CO_H) [1] <- "date"
colnames(MEU_BCtot_CO_H) [1] <- "date"
colnames(MEU_BCtot_CO2_H) [1] <- "date"

#  Suppression des talbeaux inutiles
rm (MEU_CO2, MEU_CO, MEU_CH4, MEU_NO, MEU_NO2, MEU_BC)
rm (MEU_CO2_flag, MEU_CO_flag, MEU_CH4_flag, MEU_NO_flag, MEU_NO2_flag, MEU_NOx_flag, MEU_BC_2023, MEU_BC_2024)

# Calcul de corrélation en fonction de la plage horaire défini dans la selection des heures (il faut faire un nouveau tableau pour voir les résultats)
MEU_Cor_CO_CO2 <- MEU_CO_CO2_H %>% group_by(date) %>%
  summarise(Correlation = cor(CO, CO2, use = "complete.obs")) 
MEU_Cor_CO_CO2$r2 <-  MEU_Cor_CO_CO2$Correlation * MEU_Cor_CO_CO2$Correlation

MEU_Cor_NOx_CO2 <- MEU_NOx_CO2_H %>% group_by(date) %>%
  summarise(Correlation = cor(NOx, CO2, use = "complete.obs"))
MEU_Cor_NOx_CO2$r2 <-  MEU_Cor_NOx_CO2$Correlation * MEU_Cor_NOx_CO2$Correlation

MEU_Cor_CH4_CO2 <- MEU_CO2_CH4_H %>% group_by(date) %>%
  summarise(Correlation = cor(CH4, CO2, use = "complete.obs"))
MEU_Cor_CH4_CO2$r2 <-  MEU_Cor_CH4_CO2$Correlation * MEU_Cor_CH4_CO2$Correlation

MEU_Cor_NOx_CO <- MEU_NOx_CO_H %>% group_by(date) %>%
  summarise(Correlation = cor(NOx, CO, use = "complete.obs"))
MEU_Cor_NOx_CO$r2 <-  MEU_Cor_NOx_CO$Correlation * MEU_Cor_NOx_CO$Correlation

MEU_Cor_BCtot_CO <- MEU_BCtot_CO_H %>% group_by(date) %>%
  summarise(Correlation = cor(BCtot, CO, use = "complete.obs"))
MEU_Cor_BCtot_CO$r2 <-  MEU_Cor_BCtot_CO$Correlation * MEU_Cor_BCtot_CO$Correlation

MEU_Cor_BCtot_CO2 <- MEU_BCtot_CO2_H %>% group_by(date) %>%
  summarise(Correlation = cor(BCtot, CO2, use = "complete.obs"))
MEU_Cor_BCtot_CO2$r2 <-  MEU_Cor_BCtot_CO2$Correlation * MEU_Cor_BCtot_CO2$Correlation

# Calcul de min en fonction de la plage horaire défini dans la selection des heures
MEU_Min_CO <-MEU_CO_CO2_H %>% group_by(date) %>%
  summarise(MinCO = min(CO, na.rm = TRUE)) 

MEU_Min_CO2 <-MEU_CO_CO2_H %>% group_by(date) %>%
  summarise(MinCO2 = min(CO2, na.rm = TRUE)) 

MEU_Min_CH4 <-MEU_CO2_CH4_H %>% group_by(date) %>%
  summarise(MinCH4 = min(CH4, na.rm = TRUE)) 

MEU_Min_NOx <-MEU_NOx_CO_H %>% group_by(date) %>%
  summarise(MinNOx = min(NOx, na.rm = TRUE))

MEU_Min_BCtot <-MEU_BCtot_CO_H %>% group_by(date) %>%
  summarise(MinBCtot = min(BCtot, na.rm = TRUE))

# Calcul de max en fonction de la plage horaire défini dans la selection des heures
MEU_Max_CO <-MEU_CO_CO2_H %>% group_by(date) %>%
  summarise(MaxCO = max(CO, na.rm = TRUE)) 

MEU_Max_CO2 <-MEU_CO_CO2_H %>% group_by(date) %>%
  summarise(MaxCO2 = max(CO2, na.rm = TRUE)) 

MEU_Max_CH4 <-MEU_CO2_CH4_H %>% group_by(date) %>%
  summarise(MaxCH4 = max(CH4, na.rm = TRUE)) 

MEU_Max_NOx <-MEU_NOx_CO_H %>% group_by(date) %>%
  summarise(MaxNOx = max(NOx, na.rm = TRUE)) 

MEU_Max_BCtot <-MEU_BCtot_CO_H %>% group_by(date) %>%
  summarise(MaxBCtot = max(BCtot, na.rm = TRUE)) 

#  Suppression des talbeaux inutiles
rm (MEU_CO_CO2, MEU_CO2_CH4, MEU_NOx_CO, MEU_NOx_CO2, MEU_BCtot_CO2, MEU_BCtot_CO)

# Regroupement des tableaux
# Tableau CO / CO2
MEU_CO_CO2 <- merge(
  x = MEU_Cor_CO_CO2,
  y = MEU_Max_CO, 
  all = TRUE,
  by ="date"
)

MEU_CO_CO2 <- merge(
  x = MEU_CO_CO2,
  y = MEU_Min_CO, 
  all = TRUE,
  by ="date"
)

MEU_CO_CO2 <- merge(
  x = MEU_CO_CO2,
  y = MEU_Max_CO2, 
  all = TRUE,
  by ="date"
)

MEU_CO_CO2 <- merge(
  x = MEU_CO_CO2,
  y = MEU_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau CH4 / CO2
MEU_CO2_CH4 <- merge(
  x = MEU_Cor_CH4_CO2,
  y = MEU_Max_CH4, 
  all = TRUE,
  by ="date"
)

MEU_CO2_CH4 <- merge(
  x = MEU_CO2_CH4,
  y = MEU_Min_CH4, 
  all = TRUE,
  by ="date"
)

MEU_CO2_CH4 <- merge(
  x = MEU_CO2_CH4,
  y = MEU_Max_CO2, 
  all = TRUE,
  by ="date"
)

MEU_CO2_CH4 <- merge(
  x = MEU_CO2_CH4,
  y = MEU_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau NOx / CO2
MEU_NOx_CO2 <- merge(
  x = MEU_Cor_NOx_CO2,
  y = MEU_Max_NOx, 
  all = TRUE,
  by ="date"
)

MEU_NOx_CO2 <- merge(
  x = MEU_NOx_CO2,
  y = MEU_Min_NOx, 
  all = TRUE,
  by ="date"
)

MEU_NOx_CO2 <- merge(
  x = MEU_NOx_CO2,
  y = MEU_Max_CO2, 
  all = TRUE,
  by ="date"
)

MEU_NOx_CO2 <- merge(
  x = MEU_NOx_CO2,
  y = MEU_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau NOx / CO
MEU_NOx_CO <- merge(
  x = MEU_Cor_NOx_CO,
  y = MEU_Max_NOx, 
  all = TRUE,
  by ="date"
)

MEU_NOx_CO <- merge(
  x = MEU_NOx_CO,
  y = MEU_Min_NOx, 
  all = TRUE,
  by ="date"
)

MEU_NOx_CO <- merge(
  x = MEU_NOx_CO,
  y = MEU_Max_CO, 
  all = TRUE,
  by ="date"
)

MEU_NOx_CO <- merge(
  x = MEU_NOx_CO,
  y = MEU_Min_CO, 
  all = TRUE,
  by ="date"
)

# Tableau BCtot / CO2
MEU_BCtot_CO2 <- merge(
  x = MEU_Cor_BCtot_CO2,
  y = MEU_Max_BCtot, 
  all = TRUE,
  by ="date"
)

MEU_BCtot_CO2 <- merge(
  x = MEU_BCtot_CO2,
  y = MEU_Min_BCtot, 
  all = TRUE,
  by ="date"
)

MEU_BCtot_CO2 <- merge(
  x = MEU_BCtot_CO2,
  y = MEU_Max_CO2, 
  all = TRUE,
  by ="date"
)

MEU_BCtot_CO2 <- merge(
  x = MEU_BCtot_CO2,
  y = MEU_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau BCtot / CO
MEU_BCtot_CO <- merge(
  x = MEU_Cor_BCtot_CO,
  y = MEU_Max_BCtot, 
  all = TRUE,
  by ="date"
)

MEU_BCtot_CO <- merge(
  x = MEU_BCtot_CO,
  y = MEU_Min_BCtot, 
  all = TRUE,
  by ="date"
)

MEU_BCtot_CO <- merge(
  x = MEU_BCtot_CO,
  y = MEU_Max_CO, 
  all = TRUE,
  by ="date"
)

MEU_BCtot_CO <- merge(
  x = MEU_BCtot_CO,
  y = MEU_Min_CO, 
  all = TRUE,
  by ="date"
)

# Calcul des deltas
MEU_CO_CO2$DeltaCO <-  MEU_CO_CO2$MaxCO - MEU_CO_CO2$MinCO
MEU_CO_CO2$DeltaCO2 <-  MEU_CO_CO2$MaxCO2 - MEU_CO_CO2$MinCO2

MEU_CO2_CH4$DeltaCH4 <-  MEU_CO2_CH4$MaxCH4 - MEU_CO2_CH4$MinCH4
MEU_CO2_CH4$DeltaCO2 <-  MEU_CO2_CH4$MaxCO2 - MEU_CO2_CH4$MinCO2

MEU_NOx_CO2$DeltaNOx <-  MEU_NOx_CO2$MaxNOx - MEU_NOx_CO2$MinNOx
MEU_NOx_CO2$DeltaCO2 <-  MEU_NOx_CO2$MaxCO2 - MEU_NOx_CO2$MinCO2

MEU_NOx_CO$DeltaNOx <-  MEU_NOx_CO$MaxNOx - MEU_NOx_CO$MinNOx
MEU_NOx_CO$DeltaCO <-  MEU_NOx_CO$MaxCO - MEU_NOx_CO$MinCO

MEU_BCtot_CO2$DeltaBCtot <-  MEU_BCtot_CO2$MaxBCtot - MEU_BCtot_CO2$MinBCtot
MEU_BCtot_CO2$DeltaCO2 <-  MEU_BCtot_CO2$MaxCO2 - MEU_BCtot_CO2$MinCO2

MEU_BCtot_CO$DeltaBCtot <-  MEU_BCtot_CO$MaxBCtot - MEU_BCtot_CO$MinBCtot
MEU_BCtot_CO$DeltaCO <-  MEU_BCtot_CO$MaxCO - MEU_BCtot_CO$MinCO

MEU_CO_CO2$DeltaCOCO2 <-  MEU_CO_CO2$DeltaCO / MEU_CO_CO2$DeltaCO2
MEU_CO2_CH4$DeltaCH4CO2 <-  MEU_CO2_CH4$DeltaCH4 / MEU_CO2_CH4$DeltaCO2
MEU_NOx_CO2$DeltaNOxCO2 <-  MEU_NOx_CO2$DeltaNOx / MEU_NOx_CO2$DeltaCO2
MEU_NOx_CO$DeltaNOxCO<-  MEU_NOx_CO$DeltaNOx / MEU_NOx_CO$DeltaCO
MEU_BCtot_CO2$DeltaBCtotCO2 <-  MEU_BCtot_CO2$DeltaBCtot / MEU_BCtot_CO2$DeltaCO2
MEU_BCtot_CO$DeltaBCtotCO<-  MEU_BCtot_CO$DeltaBCtot / MEU_BCtot_CO$DeltaCO

# Filtre des r²
MEU_CO_CO2_05 <- filter(MEU_CO_CO2,r2>=0.5)
MEU_CO2_CH4_05 <- filter(MEU_CO2_CH4,r2>=0.5)
MEU_NOx_CO2_05 <- filter(MEU_NOx_CO2,r2>=0.5)
MEU_NOx_CO_05 <- filter(MEU_NOx_CO,r2>=0.5)
MEU_BCtot_CO2_05 <- filter(MEU_BCtot_CO2,r2>=0.5)
MEU_BCtot_CO_05 <- filter(MEU_BCtot_CO,r2>=0.5)

MEU_Mean_Day <- merge(
  x = MEU_CO_CO2_05,
  y = MEU_CO2_CH4_05, 
  all = TRUE,
  by ="date"
)

MEU_Mean_Day <- merge(
  x = MEU_Mean_Day,
  y = MEU_NOx_CO2_05, 
  all = TRUE,
  by ="date"
)

MEU_Mean_Day <- merge(
  x = MEU_Mean_Day,
  y = MEU_NOx_CO_05, 
  all = TRUE,
  by ="date"
)

MEU_Mean_Day <- merge(
  x = MEU_Mean_Day,
  y = MEU_BCtot_CO2_05, 
  all = TRUE,
  by ="date"
)

MEU_Mean_Day <- merge(
  x = MEU_Mean_Day,
  y = MEU_BCtot_CO_05, 
  all = TRUE,
  by ="date"
)

MEU_Mean_Day <- MEU_Mean_Day[,-c(2:9,11:18,20:27,29:36, 38:45, 47:54)]
#---- Delta par mois ---- 
# Création de la colonne des mois
MEU_CO_CO2_05 <- as.data.table(MEU_CO_CO2_05)
MEU_CO_CO2_05[, month := format(as.Date(date), "%m")]

MEU_CO2_CH4_05 <- as.data.table(MEU_CO2_CH4_05)
MEU_CO2_CH4_05[, month := format(as.Date(date), "%m")]

MEU_NOx_CO2_05 <- as.data.table(MEU_NOx_CO2_05)
MEU_NOx_CO2_05[, month := format(as.Date(date), "%m")]

MEU_NOx_CO_05 <- as.data.table(MEU_NOx_CO_05)
MEU_NOx_CO_05[, month := format(as.Date(date), "%m")]

MEU_BCtot_CO2_05 <- as.data.table(MEU_BCtot_CO2_05)
MEU_BCtot_CO2_05[, month := format(as.Date(date), "%m")]

MEU_BCtot_CO_05 <- as.data.table(MEU_BCtot_CO_05)
MEU_BCtot_CO_05[, month := format(as.Date(date), "%m")]

# Calcul des moyennes de deltas par mois 
MEU_CO_CO2_Mean <- aggregate( DeltaCOCO2 ~ month , MEU_CO_CO2_05 , mean )
MEU_CH4_CO2_Mean <- aggregate( DeltaCH4CO2 ~ month , MEU_CO2_CH4_05 , mean )
MEU_NOx_CO2_Mean <- aggregate( DeltaNOxCO2 ~ month , MEU_NOx_CO2_05 , mean )
MEU_NOx_CO_Mean <- aggregate( DeltaNOxCO ~ month , MEU_NOx_CO_05 , mean )
MEU_BCtot_CO2_Mean <- aggregate( DeltaBCtotCO2 ~ month , MEU_BCtot_CO2_05 , mean )
MEU_BCtot_CO_Mean <- aggregate( DeltaBCtotCO ~ month , MEU_BCtot_CO_05 , mean )

# Calcul des mediane de deltas par mois 
MEU_CO_CO2_Median <- aggregate( DeltaCOCO2 ~ month , MEU_CO_CO2_05 , median )
MEU_CH4_CO2_Median <- aggregate( DeltaCH4CO2 ~ month , MEU_CO2_CH4_05 , median )
MEU_NOx_CO2_Median <- aggregate( DeltaNOxCO2 ~ month , MEU_NOx_CO2_05 , median )
MEU_NOx_CO_Median <- aggregate( DeltaNOxCO ~ month , MEU_NOx_CO_05 , median )
MEU_BCtot_CO2_Median <- aggregate( DeltaBCtotCO2 ~ month , MEU_BCtot_CO2_05 , median )
MEU_BCtot_CO_Median <- aggregate( DeltaBCtotCO ~ month , MEU_BCtot_CO_05 , median )

# Calcul des ecart-types de deltas par mois 
MEU_CO_CO2_sd <- aggregate( DeltaCOCO2 ~ month , MEU_CO_CO2_05 , sd )
MEU_CH4_CO2_sd <- aggregate( DeltaCH4CO2 ~ month , MEU_CO2_CH4_05 , sd )
MEU_NOx_CO2_sd <- aggregate( DeltaNOxCO2 ~ month , MEU_NOx_CO2_05 , sd )
MEU_NOx_CO_sd <- aggregate( DeltaNOxCO ~ month , MEU_NOx_CO_05 , sd )
MEU_BCtot_CO2_sd <- aggregate( DeltaBCtotCO2 ~ month , MEU_BCtot_CO2_05 , sd )
MEU_BCtot_CO_sd <- aggregate( DeltaBCtotCO ~ month , MEU_BCtot_CO_05 , sd )

# Merge des tableaux de moyenne par mois 
MEU_Mean <- merge(
  x = MEU_CO_CO2_Mean,
  y = MEU_CH4_CO2_Mean, 
  all = TRUE,
  by ="month"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_NOx_CO2_Mean, 
  all = TRUE,
  by ="month"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_NOx_CO_Mean, 
  all = TRUE,
  by ="month"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_BCtot_CO2_Mean, 
  all = TRUE,
  by ="month"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_BCtot_CO_Mean, 
  all = TRUE,
  by ="month"
)

# Merge des tableaux de mediane par mois 
MEU_Median <- merge(
  x = MEU_CO_CO2_Median,
  y = MEU_CH4_CO2_Median, 
  all = TRUE,
  by ="month"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_NOx_CO2_Median, 
  all = TRUE,
  by ="month"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_NOx_CO_Median, 
  all = TRUE,
  by ="month"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_BCtot_CO2_Median, 
  all = TRUE,
  by ="month"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_BCtot_CO_Median, 
  all = TRUE,
  by ="month"
)
# Merge des tableaux de sd par mois 
MEU_sd <- merge(
  x = MEU_CO_CO2_sd,
  y = MEU_CH4_CO2_sd, 
  all = TRUE,
  by ="month"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_NOx_CO2_sd, 
  all = TRUE,
  by ="month"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_NOx_CO_sd, 
  all = TRUE,
  by ="month"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_BCtot_CO2_sd, 
  all = TRUE,
  by ="month"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_BCtot_CO_sd, 
  all = TRUE,
  by ="month"
)
# Suppression de tous les tableaux inutiles
# rm(list=setdiff(ls(), "MEU_Mean")) --> pour supprimer tous les tableaux sauf 1
rm(list=ls()[! ls() %in% c("MEU_Mean","MEU_Mean_Day", "MEU_Median", "MEU_sd")])

#---- Delta par mois calendaire ---- 
# Création de la colonne des mois et années pour delta calendaire
MEU_CO_CO2_05$date <- format(as.POSIXct(MEU_CO_CO2_05$date,
                                        format = '%Y-%m-%d'),
                             format = '%Y-%m')
MEU_CO2_CH4_05$date <- format(as.POSIXct(MEU_CO2_CH4_05$date,
                                         format = '%Y-%m-%d'),
                              format = '%Y-%m')
MEU_NOx_CO2_05$date <- format(as.POSIXct(MEU_NOx_CO2_05$date,
                                         format = '%Y-%m-%d'),
                              format = '%Y-%m')
MEU_NOx_CO_05$date <- format(as.POSIXct(MEU_NOx_CO_05$date,
                                        format = '%Y-%m-%d'),
                             format = '%Y-%m')
MEU_BCtot_CO2_05$date <- format(as.POSIXct(MEU_BCtot_CO2_05$date,
                                         format = '%Y-%m-%d'),
                              format = '%Y-%m')
MEU_BCtot_CO_05$date <- format(as.POSIXct(MEU_BCtot_CO_05$date,
                                        format = '%Y-%m-%d'),
                             format = '%Y-%m')

# Calcul des moyennes de deltas par mois 
MEU_CO_CO2_Mean <- aggregate( DeltaCOCO2 ~ date , MEU_CO_CO2_05 , mean )
MEU_CH4_CO2_Mean <- aggregate( DeltaCH4CO2 ~ date , MEU_CO2_CH4_05 , mean )
MEU_NOx_CO2_Mean <- aggregate( DeltaNOxCO2 ~ date , MEU_NOx_CO2_05 , mean )
MEU_NOx_CO_Mean <- aggregate( DeltaNOxCO ~ date , MEU_NOx_CO_05 , mean )
MEU_BCtot_CO2_Mean <- aggregate( DeltaBCtotCO2 ~ date , MEU_BCtot_CO2_05 , mean )
MEU_BCtot_CO_Mean <- aggregate( DeltaBCtotCO ~ date , MEU_BCtot_CO_05 , mean )

# Calcul des mediane de deltas par mois 
MEU_CO_CO2_Median <- aggregate( DeltaCOCO2 ~ date , MEU_CO_CO2_05 , median )
MEU_CH4_CO2_Median <- aggregate( DeltaCH4CO2 ~ date , MEU_CO2_CH4_05 , median )
MEU_NOx_CO2_Median <- aggregate( DeltaNOxCO2 ~ date , MEU_NOx_CO2_05 , median )
MEU_NOx_CO_Median <- aggregate( DeltaNOxCO ~ date , MEU_NOx_CO_05 , median )
MEU_BCtot_CO2_Median <- aggregate( DeltaBCtotCO2 ~ date , MEU_BCtot_CO2_05 , median )
MEU_BCtot_CO_Median <- aggregate( DeltaBCtotCO ~ date , MEU_BCtot_CO_05 , median )

# Calcul des ecart-types de deltas par mois 
MEU_CO_CO2_sd <- aggregate( DeltaCOCO2 ~ date , MEU_CO_CO2_05 , sd )
MEU_CH4_CO2_sd <- aggregate( DeltaCH4CO2 ~ date , MEU_CO2_CH4_05 , sd )
MEU_NOx_CO2_sd <- aggregate( DeltaNOxCO2 ~ date , MEU_NOx_CO2_05 , sd )
MEU_NOx_CO_sd <- aggregate( DeltaNOxCO ~ date , MEU_NOx_CO_05 , sd )
MEU_BCtot_CO2_sd <- aggregate( DeltaBCtotCO2 ~ date , MEU_BCtot_CO2_05 , sd )
MEU_BCtot_CO_sd <- aggregate( DeltaBCtotCO ~ date , MEU_BCtot_CO_05 , sd )

# Merge des tableaux de moyenne par mois 
MEU_Mean <- merge(
  x = MEU_CO_CO2_Mean,
  y = MEU_CH4_CO2_Mean, 
  all = TRUE,
  by ="date"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_NOx_CO2_Mean, 
  all = TRUE,
  by ="date"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_NOx_CO_Mean, 
  all = TRUE,
  by ="date"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_BCtot_CO2_Mean, 
  all = TRUE,
  by ="date"
)

MEU_Mean <- merge(
  x = MEU_Mean,
  y = MEU_BCtot_CO_Mean, 
  all = TRUE,
  by ="date"
)
# Merge des tableaux de mediane par mois 
MEU_Median <- merge(
  x = MEU_CO_CO2_Median,
  y = MEU_CH4_CO2_Median, 
  all = TRUE,
  by ="date"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_NOx_CO2_Median, 
  all = TRUE,
  by ="date"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_NOx_CO_Median, 
  all = TRUE,
  by ="date"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_BCtot_CO2_Median, 
  all = TRUE,
  by ="date"
)

MEU_Median <- merge(
  x = MEU_Median,
  y = MEU_BCtot_CO_Median, 
  all = TRUE,
  by ="date"
)
# Merge des tableaux de se par mois 
MEU_sd <- merge(
  x = MEU_CO_CO2_sd,
  y = MEU_CH4_CO2_sd, 
  all = TRUE,
  by ="date"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_NOx_CO2_sd, 
  all = TRUE,
  by ="date"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_NOx_CO_sd, 
  all = TRUE,
  by ="date"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_BCtot_CO2_sd, 
  all = TRUE,
  by ="date"
)

MEU_sd <- merge(
  x = MEU_sd,
  y = MEU_BCtot_CO_sd, 
  all = TRUE,
  by ="date"
)
# Suppression de tous les tableaux inutiles
# rm(list=setdiff(ls(), "MEU_Mean")) --> pour supprimer tous les tableaux sauf 1
rm(list=ls()[! ls() %in% c("MEU_Mean","MEU_Mean_Day", "MEU_Median", "MEU_sd")])



#---- ROV ---- 
#  Chargement des données
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

ROV_BC_2023 <- read.csv2("E:/PAUL/ROV/Data_VF/Data2023_ROV.csv", sep=",")
ROV_BC_2024 <- read.csv2("E:/PAUL/ROV/Data_VF/Data2024_ROV.csv", sep=",")
ROV_BC <- rbind(ROV_BC_2023, ROV_BC_2024)
# Modification date
ROV_CO2<-within(ROV_CO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_CO<-within(ROV_CO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_CH4<-within(ROV_CH4,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_NO<-within(ROV_NO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_NO2<-within(ROV_NO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_BC$BCdate <- as.POSIXct(ROV_BC$BCdate, format = '%d/%m/%Y %H:%M:%S', tz="UTC")

# Selection flag
ROV_CO2$flag <- as.character(ROV_CO2$flag)
ROV_CO2 <- as.data.table(ROV_CO2)
ROV_CO2_flag <- ROV_CO2[flag == "U"|flag =="O"|flag =="R"]
ROV_CO2_flag <- ROV_CO2_flag[,-c(1:6)]
colnames(ROV_CO2_flag) [2] <- "Stdev_CO2"
colnames(ROV_CO2_flag) [3] <- "flag_CO2"

ROV_CO$flag <- as.character(ROV_CO$flag)
ROV_CO <- as.data.table(ROV_CO)
ROV_CO_flag <- ROV_CO[flag == "U"|flag =="O"|flag =="R"]
ROV_CO_flag <- ROV_CO_flag[,-c(1:6)]
colnames(ROV_CO_flag) [2] <- "Stdev_CO"
colnames(ROV_CO_flag) [3] <- "flag_CO"

ROV_CH4$flag <- as.character(ROV_CH4$flag)
ROV_CH4 <- as.data.table(ROV_CH4)
ROV_CH4_flag <- ROV_CH4[flag == "U"|flag =="O"|flag =="R"]
ROV_CH4_flag <- ROV_CH4_flag[,-c(1:6)]
colnames(ROV_CH4_flag) [2] <- "Stdev_CH4"
colnames(ROV_CH4_flag) [3] <- "flag_CH4"

ROV_NO$flag <- as.character(ROV_NO$flag)
ROV_NO <- as.data.table(ROV_NO)
ROV_NO_flag <- ROV_NO[flag == "U"|flag =="O"|flag =="R"]
ROV_NO_flag <- ROV_NO_flag[,-c(1:6)]
colnames(ROV_NO_flag) [2] <- "Stdev_NO"
colnames(ROV_NO_flag) [3] <- "flag_NO"

ROV_NO2$flag <- as.character(ROV_NO2$flag)
ROV_NO2 <- as.data.table(ROV_NO2)
ROV_NO2_flag <- ROV_NO2[flag == "U"|flag =="O"|flag =="R"]
ROV_NO2_flag <- ROV_NO2_flag[,-c(1:6)]
colnames(ROV_NO2_flag) [2] <- "Stdev_NO2"
colnames(ROV_NO2_flag) [3] <- "flag_NO2"

colnames(ROV_BC) <- c("DateTime","BCff","BCwb","BCtot")
ROV_BC$BCff <- as.numeric(ROV_BC$BCff)
ROV_BC$BCwb <- as.numeric(ROV_BC$BCwb)
ROV_BC$BCtot <- as.numeric(ROV_BC$BCtot)

ROV_BCff <- ROV_BC[,-c(3,4)]
colnames(ROV_BCff) <- c("Datetime","BCff")
ROV_BCwb <- ROV_BC[,-c(2,4)]
colnames(ROV_BCwb) <- c("Datetime","BCwb")
ROV_BCtot <- ROV_BC[,-c(2,3)]
colnames(ROV_BCtot) <- c("Datetime","BCtot")
# Création NOx
ROV_NOx_flag <- merge(
  x = ROV_NO_flag,
  y = ROV_NO2_flag, 
  all = TRUE,
  by ="Datetime")

ROV_NOx_flag$NOx <- ROV_NOx_flag$NO + ROV_NOx_flag$NO2

# Merge des différents tableaux
ROV_CO_CO2 <- merge(
  x = ROV_CO2_flag,
  y = ROV_CO_flag,
  by ="Datetime")

ROV_CO2_CH4 <- merge(
  x = ROV_CO2_flag,
  y = ROV_CH4_flag,
  by ="Datetime")

ROV_NOx_CO2 <- merge(
  x = ROV_CO2_flag,
  y = ROV_NOx_flag,
  by ="Datetime")

ROV_NOx_CO <- merge(
  x = ROV_CO_flag,
  y = ROV_NOx_flag,
  by ="Datetime")

ROV_BCtot_CO <- merge(
  x = ROV_CO_flag,
  y = ROV_BCtot,
  by ="Datetime")

ROV_BCtot_CO2 <- merge(
  x = ROV_CO2_flag,
  y = ROV_BCtot,
  by ="Datetime")


# Création de le colonnes des heures
ROV_CO_CO2[, hour := as.numeric(format(Datetime, "%H"))]
ROV_CO2_CH4[, hour := as.numeric(format(Datetime, "%H"))]
ROV_NOx_CO[, hour := as.numeric(format(Datetime, "%H"))]
ROV_NOx_CO2[, hour := as.numeric(format(Datetime, "%H"))]
ROV_BCtot_CO[, hour := as.numeric(format(Datetime, "%H"))]
ROV_BCtot_CO2[, hour := as.numeric(format(Datetime, "%H"))]

# Sélection des heures
ROV_CO_CO2_H <- filter(ROV_CO_CO2,hour>=5 & hour<= 12)
ROV_CO2_CH4_H <- filter(ROV_CO2_CH4,hour>=5 & hour<= 12)
ROV_NOx_CO2_H <- filter(ROV_NOx_CO2,hour>=5 & hour<= 12)
ROV_NOx_CO_H <- filter(ROV_NOx_CO,hour>=5 & hour<= 12)
ROV_BCtot_CO_H <- filter(ROV_BCtot_CO,hour>=5 & hour<= 12)
ROV_BCtot_CO2_H <- filter(ROV_BCtot_CO2,hour>=5 & hour<= 12)

# Changement du format de la date pour supprimer les heures, minutes, secondes
ROV_CO_CO2_H$Datetime <- format(as.POSIXct(ROV_CO_CO2_H$Datetime,
                                           format = '%Y-%m-%d %H:%M:%S'),
                                format = '%Y-%m-%d')
ROV_CO_CO2_H$Datetime <- as.POSIXct(strptime(ROV_CO_CO2_H$Datetime,'%Y-%m-%d'))

ROV_CO2_CH4_H$Datetime <- format(as.POSIXct(ROV_CO2_CH4_H$Datetime,
                                            format = '%Y-%m-%d %H:%M:%S'),
                                 format = '%Y-%m-%d')
ROV_CO2_CH4_H$Datetime <- as.POSIXct(strptime(ROV_CO2_CH4_H$Datetime,'%Y-%m-%d'))

ROV_NOx_CO2_H$Datetime <- format(as.POSIXct(ROV_NOx_CO2_H$Datetime,
                                            format = '%Y-%m-%d %H:%M:%S'),
                                 format = '%Y-%m-%d')
ROV_NOx_CO2_H$Datetime <- as.POSIXct(strptime(ROV_NOx_CO2_H$Datetime,'%Y-%m-%d'))

ROV_NOx_CO_H$Datetime <- format(as.POSIXct(ROV_NOx_CO_H$Datetime,
                                           format = '%Y-%m-%d %H:%M:%S'),
                                format = '%Y-%m-%d')
ROV_NOx_CO_H$Datetime <- as.POSIXct(strptime(ROV_NOx_CO_H$Datetime,'%Y-%m-%d'))

ROV_BCtot_CO_H$Datetime <- format(as.POSIXct(ROV_BCtot_CO_H$Datetime,
                                             format = '%Y-%m-%d %H:%M:%S'),
                                  format = '%Y-%m-%d')
ROV_BCtot_CO_H$Datetime <- as.POSIXct(strptime(ROV_BCtot_CO_H$Datetime,'%Y-%m-%d'))

ROV_BCtot_CO2_H$Datetime <- format(as.POSIXct(ROV_BCtot_CO2_H$Datetime,
                                              format = '%Y-%m-%d %H:%M:%S'),
                                   format = '%Y-%m-%d')
ROV_BCtot_CO2_H$Datetime <- as.POSIXct(strptime(ROV_BCtot_CO2_H$Datetime,'%Y-%m-%d'))

# Changement du nom pour la library lubridate
colnames(ROV_CO_CO2_H) [1] <- "date"
colnames(ROV_CO2_CH4_H) [1] <- "date"
colnames(ROV_NOx_CO2_H) [1] <- "date"
colnames(ROV_NOx_CO_H) [1] <- "date"
colnames(ROV_BCtot_CO_H) [1] <- "date"
colnames(ROV_BCtot_CO2_H) [1] <- "date"

#  Suppression des talbeaux inutiles
rm (ROV_CO2, ROV_CO, ROV_CH4, ROV_NO, ROV_NO2, ROV_BC)
rm (ROV_CO2_flag, ROV_CO_flag, ROV_CH4_flag, ROV_NO_flag, ROV_NO2_flag, ROV_NOx_flag, ROV_BC_2023, ROV_BC_2024)

# Calcul de corrélation en fonction de la plage horaire défini dans la selection des heures (il faut faire un nouveau tableau pour voir les résultats)
ROV_Cor_CO_CO2 <- ROV_CO_CO2_H %>% group_by(date) %>%
  summarise(Correlation = cor(CO, CO2, use = "complete.obs")) 
ROV_Cor_CO_CO2$r2 <-  ROV_Cor_CO_CO2$Correlation * ROV_Cor_CO_CO2$Correlation

ROV_Cor_NOx_CO2 <- ROV_NOx_CO2_H %>% group_by(date) %>%
  summarise(Correlation = cor(NOx, CO2, use = "complete.obs"))
ROV_Cor_NOx_CO2$r2 <-  ROV_Cor_NOx_CO2$Correlation * ROV_Cor_NOx_CO2$Correlation

ROV_Cor_CH4_CO2 <- ROV_CO2_CH4_H %>% group_by(date) %>%
  summarise(Correlation = cor(CH4, CO2, use = "complete.obs"))
ROV_Cor_CH4_CO2$r2 <-  ROV_Cor_CH4_CO2$Correlation * ROV_Cor_CH4_CO2$Correlation

ROV_Cor_NOx_CO <- ROV_NOx_CO_H %>% group_by(date) %>%
  summarise(Correlation = cor(NOx, CO, use = "complete.obs"))
ROV_Cor_NOx_CO$r2 <-  ROV_Cor_NOx_CO$Correlation * ROV_Cor_NOx_CO$Correlation

ROV_Cor_BCtot_CO <- ROV_BCtot_CO_H %>% group_by(date) %>%
  summarise(Correlation = cor(BCtot, CO, use = "complete.obs"))
ROV_Cor_BCtot_CO$r2 <-  ROV_Cor_BCtot_CO$Correlation * ROV_Cor_BCtot_CO$Correlation

ROV_Cor_BCtot_CO2 <- ROV_BCtot_CO2_H %>% group_by(date) %>%
  summarise(Correlation = cor(BCtot, CO2, use = "complete.obs"))
ROV_Cor_BCtot_CO2$r2 <-  ROV_Cor_BCtot_CO2$Correlation * ROV_Cor_NOx_CO$Correlation

# Calcul de min en fonction de la plage horaire défini dans la selection des heures
ROV_Min_CO <-ROV_CO_CO2_H %>% group_by(date) %>%
  summarise(MinCO = min(CO, na.rm = TRUE)) 

ROV_Min_CO2 <-ROV_CO_CO2_H %>% group_by(date) %>%
  summarise(MinCO2 = min(CO2, na.rm = TRUE)) 

ROV_Min_CH4 <-ROV_CO2_CH4_H %>% group_by(date) %>%
  summarise(MinCH4 = min(CH4, na.rm = TRUE)) 

ROV_Min_NOx <-ROV_NOx_CO_H %>% group_by(date) %>%
  summarise(MinNOx = min(NOx, na.rm = TRUE))

ROV_Min_BCtot <-ROV_BCtot_CO_H %>% group_by(date) %>%
  summarise(MinBCtot = min(BCtot, na.rm = TRUE))

# Calcul de max en fonction de la plage horaire défini dans la selection des heures
ROV_Max_CO <-ROV_CO_CO2_H %>% group_by(date) %>%
  summarise(MaxCO = max(CO, na.rm = TRUE)) 

ROV_Max_CO2 <-ROV_CO_CO2_H %>% group_by(date) %>%
  summarise(MaxCO2 = max(CO2, na.rm = TRUE)) 

ROV_Max_CH4 <-ROV_CO2_CH4_H %>% group_by(date) %>%
  summarise(MaxCH4 = max(CH4, na.rm = TRUE)) 

ROV_Max_NOx <-ROV_NOx_CO_H %>% group_by(date) %>%
  summarise(MaxNOx = max(NOx, na.rm = TRUE)) 

ROV_Max_BCtot <-ROV_BCtot_CO_H %>% group_by(date) %>%
  summarise(MaxBCtot = max(BCtot, na.rm = TRUE)) 

#  Suppression des talbeaux inutiles
rm (ROV_CO_CO2, ROV_CO2_CH4, ROV_NOx_CO, ROV_NOx_CO2, ROV_BCtot_CO2, ROV_BCtot_CO)

# Regroupement des tableaux
# Tableau CO / CO2
ROV_CO_CO2 <- merge(
  x = ROV_Cor_CO_CO2,
  y = ROV_Max_CO, 
  all = TRUE,
  by ="date"
)

ROV_CO_CO2 <- merge(
  x = ROV_CO_CO2,
  y = ROV_Min_CO, 
  all = TRUE,
  by ="date"
)

ROV_CO_CO2 <- merge(
  x = ROV_CO_CO2,
  y = ROV_Max_CO2, 
  all = TRUE,
  by ="date"
)

ROV_CO_CO2 <- merge(
  x = ROV_CO_CO2,
  y = ROV_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau CH4 / CO2
ROV_CO2_CH4 <- merge(
  x = ROV_Cor_CH4_CO2,
  y = ROV_Max_CH4, 
  all = TRUE,
  by ="date"
)

ROV_CO2_CH4 <- merge(
  x = ROV_CO2_CH4,
  y = ROV_Min_CH4, 
  all = TRUE,
  by ="date"
)

ROV_CO2_CH4 <- merge(
  x = ROV_CO2_CH4,
  y = ROV_Max_CO2, 
  all = TRUE,
  by ="date"
)

ROV_CO2_CH4 <- merge(
  x = ROV_CO2_CH4,
  y = ROV_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau NOx / CO2
ROV_NOx_CO2 <- merge(
  x = ROV_Cor_NOx_CO2,
  y = ROV_Max_NOx, 
  all = TRUE,
  by ="date"
)

ROV_NOx_CO2 <- merge(
  x = ROV_NOx_CO2,
  y = ROV_Min_NOx, 
  all = TRUE,
  by ="date"
)

ROV_NOx_CO2 <- merge(
  x = ROV_NOx_CO2,
  y = ROV_Max_CO2, 
  all = TRUE,
  by ="date"
)

ROV_NOx_CO2 <- merge(
  x = ROV_NOx_CO2,
  y = ROV_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau NOx / CO
ROV_NOx_CO <- merge(
  x = ROV_Cor_NOx_CO,
  y = ROV_Max_NOx, 
  all = TRUE,
  by ="date"
)

ROV_NOx_CO <- merge(
  x = ROV_NOx_CO,
  y = ROV_Min_NOx, 
  all = TRUE,
  by ="date"
)

ROV_NOx_CO <- merge(
  x = ROV_NOx_CO,
  y = ROV_Max_CO, 
  all = TRUE,
  by ="date"
)

ROV_NOx_CO <- merge(
  x = ROV_NOx_CO,
  y = ROV_Min_CO, 
  all = TRUE,
  by ="date"
)

# Tableau BCtot / CO2
ROV_BCtot_CO2 <- merge(
  x = ROV_Cor_BCtot_CO2,
  y = ROV_Max_BCtot, 
  all = TRUE,
  by ="date"
)

ROV_BCtot_CO2 <- merge(
  x = ROV_BCtot_CO2,
  y = ROV_Min_BCtot, 
  all = TRUE,
  by ="date"
)

ROV_BCtot_CO2 <- merge(
  x = ROV_BCtot_CO2,
  y = ROV_Max_CO2, 
  all = TRUE,
  by ="date"
)

ROV_BCtot_CO2 <- merge(
  x = ROV_BCtot_CO2,
  y = ROV_Min_CO2, 
  all = TRUE,
  by ="date"
)

# Tableau BCtot / CO
ROV_BCtot_CO <- merge(
  x = ROV_Cor_BCtot_CO,
  y = ROV_Max_BCtot, 
  all = TRUE,
  by ="date"
)

ROV_BCtot_CO <- merge(
  x = ROV_BCtot_CO,
  y = ROV_Min_BCtot, 
  all = TRUE,
  by ="date"
)

ROV_BCtot_CO <- merge(
  x = ROV_BCtot_CO,
  y = ROV_Max_CO, 
  all = TRUE,
  by ="date"
)

ROV_BCtot_CO <- merge(
  x = ROV_BCtot_CO,
  y = ROV_Min_CO, 
  all = TRUE,
  by ="date"
)

# Calcul des deltas
ROV_CO_CO2$DeltaCO <-  ROV_CO_CO2$MaxCO - ROV_CO_CO2$MinCO
ROV_CO_CO2$DeltaCO2 <-  ROV_CO_CO2$MaxCO2 - ROV_CO_CO2$MinCO2

ROV_CO2_CH4$DeltaCH4 <-  ROV_CO2_CH4$MaxCH4 - ROV_CO2_CH4$MinCH4
ROV_CO2_CH4$DeltaCO2 <-  ROV_CO2_CH4$MaxCO2 - ROV_CO2_CH4$MinCO2

ROV_NOx_CO2$DeltaNOx <-  ROV_NOx_CO2$MaxNOx - ROV_NOx_CO2$MinNOx
ROV_NOx_CO2$DeltaCO2 <-  ROV_NOx_CO2$MaxCO2 - ROV_NOx_CO2$MinCO2

ROV_NOx_CO$DeltaNOx <-  ROV_NOx_CO$MaxNOx - ROV_NOx_CO$MinNOx
ROV_NOx_CO$DeltaCO <-  ROV_NOx_CO$MaxCO - ROV_NOx_CO$MinCO

ROV_BCtot_CO2$DeltaBCtot <-  ROV_BCtot_CO2$MaxBCtot - ROV_BCtot_CO2$MinBCtot
ROV_BCtot_CO2$DeltaCO2 <-  ROV_BCtot_CO2$MaxCO2 - ROV_BCtot_CO2$MinCO2

ROV_BCtot_CO$DeltaBCtot <-  ROV_BCtot_CO$MaxBCtot - ROV_BCtot_CO$MinBCtot
ROV_BCtot_CO$DeltaCO <-  ROV_BCtot_CO$MaxCO - ROV_BCtot_CO$MinCO

ROV_CO_CO2$DeltaCOCO2 <-  ROV_CO_CO2$DeltaCO / ROV_CO_CO2$DeltaCO2
ROV_CO2_CH4$DeltaCH4CO2 <-  ROV_CO2_CH4$DeltaCH4 / ROV_CO2_CH4$DeltaCO2
ROV_NOx_CO2$DeltaNOxCO2 <-  ROV_NOx_CO2$DeltaNOx / ROV_NOx_CO2$DeltaCO2
ROV_NOx_CO$DeltaNOxCO<-  ROV_NOx_CO$DeltaNOx / ROV_NOx_CO$DeltaCO
ROV_BCtot_CO2$DeltaBCtotCO2 <-  ROV_BCtot_CO2$DeltaBCtot / ROV_BCtot_CO2$DeltaCO2
ROV_BCtot_CO$DeltaBCtotCO<-  ROV_BCtot_CO$DeltaBCtot / ROV_BCtot_CO$DeltaCO

# Filtre des r²
ROV_CO_CO2_05 <- filter(ROV_CO_CO2,r2>=0.5)
ROV_CO2_CH4_05 <- filter(ROV_CO2_CH4,r2>=0.5)
ROV_NOx_CO2_05 <- filter(ROV_NOx_CO2,r2>=0.5)
ROV_NOx_CO_05 <- filter(ROV_NOx_CO,r2>=0.5)
ROV_BCtot_CO2_05 <- filter(ROV_BCtot_CO2,r2>=0.5)
ROV_BCtot_CO_05 <- filter(ROV_BCtot_CO,r2>=0.5)

ROV_Mean_Day <- merge(
  x = ROV_CO_CO2_05,
  y = ROV_CO2_CH4_05, 
  all = TRUE,
  by ="date"
)

ROV_Mean_Day <- merge(
  x = ROV_Mean_Day,
  y = ROV_NOx_CO2_05, 
  all = TRUE,
  by ="date"
)

ROV_Mean_Day <- merge(
  x = ROV_Mean_Day,
  y = ROV_NOx_CO_05, 
  all = TRUE,
  by ="date"
)

ROV_Mean_Day <- merge(
  x = ROV_Mean_Day,
  y = ROV_BCtot_CO2_05, 
  all = TRUE,
  by ="date"
)

ROV_Mean_Day <- merge(
  x = ROV_Mean_Day,
  y = ROV_BCtot_CO_05, 
  all = TRUE,
  by ="date"
)

ROV_Mean_Day <- ROV_Mean_Day[,-c(2:9,11:18,20:27,29:36, 38:45, 47:54)]
#---- Delta par mois ---- 
# Création de la colonne des mois
ROV_CO_CO2_05 <- as.data.table(ROV_CO_CO2_05)
ROV_CO_CO2_05[, month := format(as.Date(date), "%m")]

ROV_CO2_CH4_05 <- as.data.table(ROV_CO2_CH4_05)
ROV_CO2_CH4_05[, month := format(as.Date(date), "%m")]

ROV_NOx_CO2_05 <- as.data.table(ROV_NOx_CO2_05)
ROV_NOx_CO2_05[, month := format(as.Date(date), "%m")]

ROV_NOx_CO_05 <- as.data.table(ROV_NOx_CO_05)
ROV_NOx_CO_05[, month := format(as.Date(date), "%m")]

ROV_BCtot_CO2_05 <- as.data.table(ROV_BCtot_CO2_05)
ROV_BCtot_CO2_05[, month := format(as.Date(date), "%m")]

ROV_BCtot_CO_05 <- as.data.table(ROV_BCtot_CO_05)
ROV_BCtot_CO_05[, month := format(as.Date(date), "%m")]

# Calcul des moyennes de deltas par mois 
ROV_CO_CO2_Mean <- aggregate( DeltaCOCO2 ~ month , ROV_CO_CO2_05 , mean )
ROV_CH4_CO2_Mean <- aggregate( DeltaCH4CO2 ~ month , ROV_CO2_CH4_05 , mean )
ROV_NOx_CO2_Mean <- aggregate( DeltaNOxCO2 ~ month , ROV_NOx_CO2_05 , mean )
ROV_NOx_CO_Mean <- aggregate( DeltaNOxCO ~ month , ROV_NOx_CO_05 , mean )
ROV_BCtot_CO2_Mean <- aggregate( DeltaBCtotCO2 ~ month , ROV_BCtot_CO2_05 , mean )
ROV_BCtot_CO_Mean <- aggregate( DeltaBCtotCO ~ month , ROV_BCtot_CO_05 , mean )

# Calcul des mediane de deltas par mois 
ROV_CO_CO2_Median <- aggregate( DeltaCOCO2 ~ month , ROV_CO_CO2_05 , median )
ROV_CH4_CO2_Median <- aggregate( DeltaCH4CO2 ~ month , ROV_CO2_CH4_05 , median )
ROV_NOx_CO2_Median <- aggregate( DeltaNOxCO2 ~ month , ROV_NOx_CO2_05 , median )
ROV_NOx_CO_Median <- aggregate( DeltaNOxCO ~ month , ROV_NOx_CO_05 , median )
ROV_BCtot_CO2_Median <- aggregate( DeltaBCtotCO2 ~ month , ROV_BCtot_CO2_05 , median )
ROV_BCtot_CO_Median <- aggregate( DeltaBCtotCO ~ month , ROV_BCtot_CO_05 , median )

# Calcul des ecart-types de deltas par mois 
ROV_CO_CO2_sd <- aggregate( DeltaCOCO2 ~ month , ROV_CO_CO2_05 , sd )
ROV_CH4_CO2_sd <- aggregate( DeltaCH4CO2 ~ month , ROV_CO2_CH4_05 , sd )
ROV_NOx_CO2_sd <- aggregate( DeltaNOxCO2 ~ month , ROV_NOx_CO2_05 , sd )
ROV_NOx_CO_sd <- aggregate( DeltaNOxCO ~ month , ROV_NOx_CO_05 , sd )
ROV_BCtot_CO2_sd <- aggregate( DeltaBCtotCO2 ~ month , ROV_BCtot_CO2_05 , sd )
ROV_BCtot_CO_sd <- aggregate( DeltaBCtotCO ~ month , ROV_BCtot_CO_05 , sd )

# Merge des tableaux de moyenne par mois 
ROV_Mean <- merge(
  x = ROV_CO_CO2_Mean,
  y = ROV_CH4_CO2_Mean, 
  all = TRUE,
  by ="month"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_NOx_CO2_Mean, 
  all = TRUE,
  by ="month"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_NOx_CO_Mean, 
  all = TRUE,
  by ="month"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_BCtot_CO2_Mean, 
  all = TRUE,
  by ="month"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_BCtot_CO_Mean, 
  all = TRUE,
  by ="month"
)

# Merge des tableaux de mediane par mois 
ROV_Median <- merge(
  x = ROV_CO_CO2_Median,
  y = ROV_CH4_CO2_Median, 
  all = TRUE,
  by ="month"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_NOx_CO2_Median, 
  all = TRUE,
  by ="month"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_NOx_CO_Median, 
  all = TRUE,
  by ="month"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_BCtot_CO2_Median, 
  all = TRUE,
  by ="month"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_BCtot_CO_Median, 
  all = TRUE,
  by ="month"
)
# Merge des tableaux de se par mois 
ROV_sd <- merge(
  x = ROV_CO_CO2_sd,
  y = ROV_CH4_CO2_sd, 
  all = TRUE,
  by ="month"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_NOx_CO2_sd, 
  all = TRUE,
  by ="month"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_NOx_CO_sd, 
  all = TRUE,
  by ="month"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_BCtot_CO2_sd, 
  all = TRUE,
  by ="month"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_BCtot_CO_sd, 
  all = TRUE,
  by ="month"
)
# Suppression de tous les tableaux inutiles
# rm(list=setdiff(ls(), "ROV_Mean")) --> pour supprimer tous les tableaux sauf 1
rm(list=ls()[! ls() %in% c("ROV_Mean","ROV_Mean_Day", "ROV_Median", "ROV_sd")])

#---- Delta par mois calendaire ---- 
# Création de la colonne des mois et années pour delta calendaire
ROV_CO_CO2_05$date <- format(as.POSIXct(ROV_CO_CO2_05$date,
                                        format = '%Y-%m-%d'),
                             format = '%Y-%m')
ROV_CO2_CH4_05$date <- format(as.POSIXct(ROV_CO2_CH4_05$date,
                                         format = '%Y-%m-%d'),
                              format = '%Y-%m')
ROV_NOx_CO2_05$date <- format(as.POSIXct(ROV_NOx_CO2_05$date,
                                         format = '%Y-%m-%d'),
                              format = '%Y-%m')
ROV_NOx_CO_05$date <- format(as.POSIXct(ROV_NOx_CO_05$date,
                                        format = '%Y-%m-%d'),
                             format = '%Y-%m')
ROV_BCtot_CO2_05$date <- format(as.POSIXct(ROV_BCtot_CO2_05$date,
                                           format = '%Y-%m-%d'),
                                format = '%Y-%m')
ROV_BCtot_CO_05$date <- format(as.POSIXct(ROV_BCtot_CO_05$date,
                                          format = '%Y-%m-%d'),
                               format = '%Y-%m')

# Calcul des moyennes de deltas par mois 
ROV_CO_CO2_Mean <- aggregate( DeltaCOCO2 ~ date , ROV_CO_CO2_05 , mean )
ROV_CH4_CO2_Mean <- aggregate( DeltaCH4CO2 ~ date , ROV_CO2_CH4_05 , mean )
ROV_NOx_CO2_Mean <- aggregate( DeltaNOxCO2 ~ date , ROV_NOx_CO2_05 , mean )
ROV_NOx_CO_Mean <- aggregate( DeltaNOxCO ~ date , ROV_NOx_CO_05 , mean )
ROV_BCtot_CO2_Mean <- aggregate( DeltaBCtotCO2 ~ date , ROV_BCtot_CO2_05 , mean )
ROV_BCtot_CO_Mean <- aggregate( DeltaBCtotCO ~ date , ROV_BCtot_CO_05 , mean )

# Calcul des mediane de deltas par mois 
ROV_CO_CO2_Median <- aggregate( DeltaCOCO2 ~ date , ROV_CO_CO2_05 , median )
ROV_CH4_CO2_Median <- aggregate( DeltaCH4CO2 ~ date , ROV_CO2_CH4_05 , median )
ROV_NOx_CO2_Median <- aggregate( DeltaNOxCO2 ~ date , ROV_NOx_CO2_05 , median )
ROV_NOx_CO_Median <- aggregate( DeltaNOxCO ~ date , ROV_NOx_CO_05 , median )
ROV_BCtot_CO2_Median <- aggregate( DeltaBCtotCO2 ~ date , ROV_BCtot_CO2_05 , median )
ROV_BCtot_CO_Median <- aggregate( DeltaBCtotCO ~ date , ROV_BCtot_CO_05 , median )

# Calcul des ecart-types de deltas par mois 
ROV_CO_CO2_sd <- aggregate( DeltaCOCO2 ~ date , ROV_CO_CO2_05 , sd )
ROV_CH4_CO2_sd <- aggregate( DeltaCH4CO2 ~ date , ROV_CO2_CH4_05 , sd )
ROV_NOx_CO2_sd <- aggregate( DeltaNOxCO2 ~ date , ROV_NOx_CO2_05 , sd )
ROV_NOx_CO_sd <- aggregate( DeltaNOxCO ~ date , ROV_NOx_CO_05 , sd )
ROV_BCtot_CO2_sd <- aggregate( DeltaBCtotCO2 ~ date , ROV_BCtot_CO2_05 , sd )
ROV_BCtot_CO_sd <- aggregate( DeltaBCtotCO ~ date , ROV_BCtot_CO_05 , sd )

# Merge des tableaux de moyenne par mois 
ROV_Mean <- merge(
  x = ROV_CO_CO2_Mean,
  y = ROV_CH4_CO2_Mean, 
  all = TRUE,
  by ="date"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_NOx_CO2_Mean, 
  all = TRUE,
  by ="date"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_NOx_CO_Mean, 
  all = TRUE,
  by ="date"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_BCtot_CO2_Mean, 
  all = TRUE,
  by ="date"
)

ROV_Mean <- merge(
  x = ROV_Mean,
  y = ROV_BCtot_CO_Mean, 
  all = TRUE,
  by ="date"
)
# Merge des tableaux de mediane par mois 
ROV_Median <- merge(
  x = ROV_CO_CO2_Median,
  y = ROV_CH4_CO2_Median, 
  all = TRUE,
  by ="date"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_NOx_CO2_Median, 
  all = TRUE,
  by ="date"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_NOx_CO_Median, 
  all = TRUE,
  by ="date"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_BCtot_CO2_Median, 
  all = TRUE,
  by ="date"
)

ROV_Median <- merge(
  x = ROV_Median,
  y = ROV_BCtot_CO_Median, 
  all = TRUE,
  by ="date"
)
# Merge des tableaux de se par mois 
ROV_sd <- merge(
  x = ROV_CO_CO2_sd,
  y = ROV_CH4_CO2_sd, 
  all = TRUE,
  by ="date"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_NOx_CO2_sd, 
  all = TRUE,
  by ="date"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_NOx_CO_sd, 
  all = TRUE,
  by ="date"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_BCtot_CO2_sd, 
  all = TRUE,
  by ="date"
)

ROV_sd <- merge(
  x = ROV_sd,
  y = ROV_BCtot_CO_sd, 
  all = TRUE,
  by ="date"
)
# Suppression de tous les tableaux inutiles
# rm(list=setdiff(ls(), "ROV_Mean")) --> pour supprimer tous les tableaux sauf 1
rm(list=ls()[! ls() %in% c("MEU_Mean","MEU_Mean_Day", "MEU_Median", "MEU_sd",
                           "ROV_Mean","ROV_Mean_Day", "ROV_Median", "ROV_sd")])



#---- fusion MEU ROV par mois ----
MEU_Mean$Station <- "MEU"
ROV_Mean$Station <- "ROV"

MEU_sd$Station <- "MEU"
ROV_sd$Station <- "ROV"

MEU_Median$Station <- "MEU"
ROV_Median$Station <- "ROV"

Mean <- rbind(MEU_Mean, ROV_Mean)

sd <- rbind(MEU_sd, ROV_sd)

Median <- rbind(MEU_Median, ROV_Median)

colnames(Mean) <- c("month","MeanCOCO2","MeanCH4CO2","MeanNOxCO2","MeanNOxCO", "MeanBCtotCO2","MeanBCtotCO","Station")
# colnames(MEU_Mean) <- c("month","MeanCOCO2_MEU","MeanCH4CO2_MEU","MeanNOxCO2_MEU","MeanNOxCO_MEU")
# 
colnames(sd) <- c("month","sdCOCO2","sdCH4CO2","sdNOxCO2","sdNOxCO","sdBCtotCO2","sdBCtotCO", "Station")
# colnames(MEU_sd) <- c("month","sdCOCO2_MEU","sdCH4CO2_MEU","sdNOxCO2_MEU","sdNOxCO_MEU")
# 
colnames(Median) <- c("month","MedianCOCO2","MedianCH4CO2","MedianNOxCO2","MedianNOxCO", "MedianBCtotCO2","MedianBCtotCO", "Station")
# colnames(MEU_Median) <- c("month","MedianCOCO2_MEU","MedianCH4CO2_MEU","MedianNOxCO2_MEU","MedianNOxCO_MEU")



All <- left_join(Mean, sd, by=c("month","Station")) %>%
  rowwise()

#---- fusion MEU ROV par mois calendaire----
MEU_Mean$Station <- "MEU"
ROV_Mean$Station <- "ROV"

MEU_sd$Station <- "MEU"
ROV_sd$Station <- "ROV"

MEU_Median$Station <- "MEU"
ROV_Median$Station <- "ROV"

Mean <- rbind(MEU_Mean, ROV_Mean)

sd <- rbind(MEU_sd, ROV_sd)

Median <- rbind(MEU_Median, MEU_Median)

colnames(Mean) <- c("date","MeanCOCO2","MeanCH4CO2","MeanNOxCO2","MeanNOxCO", "MeanBCtotCO2","MeanBCtotCO","Station")
# colnames(MEU_Mean) <- c("date","MeanCOCO2_MEU","MeanCH4CO2_MEU","MeanNOxCO2_MEU","MeanNOxCO_MEU")
# 
colnames(sd) <- c("date","sdCOCO2","sdCH4CO2","sdNOxCO2","sdNOxCO","sdBCtotCO2","sdBCtotCO", "Station")
# colnames(MEU_sd) <- c("date","sdCOCO2_MEU","sdCH4CO2_MEU","sdNOxCO2_MEU","sdNOxCO_MEU")
# 
colnames(Median) <- c("date","MedianCOCO2","MedianCH4CO2","MedianNOxCO2","MedianNOxCO", "MedianBCtotCO2","MedianBCtotCO", "Station")
# colnames(MEU_Median) <- c("date","MedianCOCO2_MEU","MedianCH4CO2_MEU","MedianNOxCO2_MEU","MedianNOxCO_MEU")
# colnames(MEU_Median) <- c("date","MedianCOCO2_MEU","MedianCH4CO2_MEU","MedianNOxCO2_MEU","MedianNOxCO_MEU")



All <- left_join(Mean, sd, by=c("date","Station")) %>%
  rowwise()

All$date <- sub(pattern = "202", replacement = "2", All$date)

#---- Plot par mois ---- 
# Changement format colonne mois
# MEU_Mean$month <- month(MEU_Mean$month, label = TRUE, locale = Sys.setlocale("LC_TIME", "English"))
# MEU_Mean$month <- month(MEU_Mean$month, label = TRUE, locale = Sys.setlocale("LC_TIME", "English"))

All$month <- as.numeric(All$month)
Mean$month <- as.numeric(Mean$month)

plot_COCO2 <- ggplot(data=All, aes(x=month, y = MeanCOCO2, col = Station)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanCOCO2 - sdCOCO2, ymax = MeanCOCO2 + sdCOCO2), width = 0.2) +
  # geom_line(data=All, aes(x=month, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))+
  xlab("Month")
plot_COCO2

plot_CH4CO2 <- ggplot(data=All, aes(x=month, y = MeanCH4CO2, col = Station)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanCH4CO2 - sdCH4CO2, ymax = MeanCH4CO2 + sdCH4CO2), width = 0.2) +
  # geom_line(data=All, aes(x=month, y = MeanCH4CO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCH4CO2_MEU - sdCH4CO2_MEU, ymax = MeanCH4CO2_MEU + sdCH4CO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression('Delta '*CH[4] *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))+
  xlab("Month")
plot_CH4CO2

plot_NOxCO2 <- ggplot(data=All, aes(x=month, y = MeanNOxCO2, col = Station)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = MeanNOxCO2 - sdNOxCO2, ymax = MeanNOxCO2 + sdNOxCO2), width = 0.2) +
  # geom_line(data=All, aes(x=month, y = MeanNOxCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanNOxCO2_MEU - sdNOxCO2_MEU, ymax = MeanNOxCO2_MEU + sdNOxCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression('Delta '*NO[x]*CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))+
  xlab("Month")
plot_NOxCO2

plot_day <- ggplot() + 
  geom_line(data=MEU_Mean_Day, aes(x=month, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2")) + 
  geom_line(data=MEU_Mean_Day, aes(x=month, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), shape= 7) +
  geom_line(data=MEU_Mean_Day, aes(x=month, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2")) + 
  geom_line(data=MEU_Mean_Day, aes(x=month, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), shape= 7) +
  geom_line(data=MEU_Mean_Day, aes(x=month, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2")) + 
  geom_line(data=MEU_Mean_Day, aes(x=month, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), shape= 7) +
  theme_bw() +
  labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))
plot_day

plot_month <- ggplot() + 
  geom_point(data=MEU_Mean, aes(x=month, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Mean, aes(x=month, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Mean, aes(x=month, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Mean, aes(x=month, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Mean, aes(x=month, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Mean, aes(x=month, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), shape= 7, size = 3) +
  theme_bw() +
  # labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))
plot_month
 

plot_median <- ggplot() + 
  geom_point(data=MEU_Median, aes(x=month, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Median, aes(x=month, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Median, aes(x=month, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Median, aes(x=month, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Median, aes(x=month, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Median, aes(x=month, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Median, aes(x=month, y = MEU), size = 3, color = "black") +
  geom_point(data=MEU_Median, aes(x=month, y = MEU), shape= 7, size = 3, color = "black") +
    theme_bw() +
  # labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylim(-10,-9)+
  ylab(bquote(ppb/ppm))
plot_median


#---- Plot par mois calendaire---- 
# Changement format colonne mois
# MEU_Mean$month <- month(MEU_Mean$month, label = TRUE, locale = Sys.setlocale("LC_TIME", "English"))
# MEU_Mean$month <- month(MEU_Mean$month, label = TRUE, locale = Sys.setlocale("LC_TIME", "English"))

# All$month <- as.numeric(All$month)
# Mean$month <- as.numeric(Mean$month)

plot_COCO2 <- ggplot(data=All, aes(x=date, y = MeanCOCO2, col = Station)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = MeanCOCO2 - sdCOCO2, ymax = MeanCOCO2 + sdCOCO2), width = 0.2, size = 1) +
  # geom_line(data=All, aes(x=date, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression(Delta*CO / Delta*CO[2]))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))+
  ylab(bquote(ppb/ppm))+
  xlab("Date")
plot_COCO2

plot_CH4CO2 <- ggplot(data=All, aes(x=date, y = MeanCH4CO2, col = Station)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = MeanCH4CO2 - sdCH4CO2, ymax = MeanCH4CO2 + sdCH4CO2), width = 0.2, size = 1) +
  # geom_line(data=All, aes(x=date, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression(Delta*CH[4] / Delta*CO[2]))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))+
  ylab(bquote(ppb/ppm))+
  xlab("Date")
plot_CH4CO2

plot_NOxCO2 <- ggplot(data=All, aes(x=date, y = MeanNOxCO2, col = Station)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = MeanNOxCO2 - sdNOxCO2, ymax = MeanNOxCO2 + sdNOxCO2), width = 0.2, size = 1) +
  # geom_line(data=All, aes(x=date, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression(Delta*NO[x] / Delta*CO[2]))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))+
  ylab(bquote(ppb/ppm))+
  xlab("Date")
plot_NOxCO2

plot_NOxCO <- ggplot(data=All, aes(x=date, y = MeanNOxCO, col = Station)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = MeanNOxCO - sdNOxCO, ymax = MeanNOxCO + sdNOxCO), width = 0.2, size = 1) +
  # geom_line(data=All, aes(x=date, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression(Delta*NO[x] / Delta*CO))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))+
  xlab("Date")
plot_NOxCO

plot_BCtotCO2 <- ggplot(data=All, aes(x=date, y = MeanBCtotCO2, col = Station)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = MeanBCtotCO2 - sdBCtotCO2, ymax = MeanBCtotCO2 + sdBCtotCO2), width = 0.2, size = 1) +
  # geom_line(data=All, aes(x=date, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression(Delta*BC / Delta*CO[2]))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))+
  ylab(bquote(ng.m^-3/ppm))+
  xlab("Date")
plot_BCtotCO2

plot_BCtotCO <- ggplot(data=All, aes(x=date, y = MeanBCtotCO, col = Station)) + 
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = MeanBCtotCO - sdBCtotCO, ymax = MeanBCtotCO + sdBCtotCO), width = 0.2, size = 1) +
  # geom_line(data=All, aes(x=date, y = MeanCOCO2_MEU, col = "MEU")) + 
  # geom_errorbar(data=All, aes(ymin = MeanCOCO2_MEU - sdCOCO2_MEU, ymax = MeanCOCO2_MEU + sdCOCO2_MEU), width = 0.2) +
  theme_bw() +
  labs(title=expression(Delta*BC / Delta*CO))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 15))+
  ylab(bquote(ng.m^-3/ppm))+
  xlab("Date")
plot_BCtotCO


plot_day <- ggplot() + 
  geom_line(data=MEU_Mean_Day, aes(x=date, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2")) + 
  geom_line(data=MEU_Mean_Day, aes(x=date, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), shape= 7) +
  geom_line(data=MEU_Mean_Day, aes(x=date, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2")) + 
  geom_line(data=MEU_Mean_Day, aes(x=date, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), shape= 7) +
  geom_line(data=MEU_Mean_Day, aes(x=date, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2")) + 
  geom_line(data=MEU_Mean_Day, aes(x=date, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), shape= 7) +
  theme_bw() +
  labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))
plot_day

plot_date <- ggplot() + 
  geom_point(data=MEU_Mean, aes(x=date, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Mean, aes(x=date, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Mean, aes(x=date, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Mean, aes(x=date, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Mean, aes(x=date, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Mean, aes(x=date, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), shape= 7, size = 3) +
  theme_bw() +
  # labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylab(bquote(ppb/ppm))
plot_date


plot_median <- ggplot() + 
  geom_point(data=MEU_Median, aes(x=date, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Median, aes(x=date, y =DeltaCOCO2, col = "DeltaCO/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Median, aes(x=date, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Median, aes(x=date, y =DeltaCH4CO2, col = "DeltaCH4/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Median, aes(x=date, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), size = 3) + 
  geom_point(data=MEU_Median, aes(x=date, y =DeltaNOxCO2, col = "DeltaNOx/DeltaCO2"), shape= 7, size = 3) +
  geom_point(data=MEU_Median, aes(x=date, y = MEU), size = 3, color = "black") +
  geom_point(data=MEU_Median, aes(x=date, y = MEU), shape= 7, size = 3, color = "black") +
  theme_bw() +
  # labs(title=expression('Delta '*CO *CO[2]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.title = element_blank())+
  ylim(-10,-9)+
  ylab(bquote(ppb/ppm))
plot_median