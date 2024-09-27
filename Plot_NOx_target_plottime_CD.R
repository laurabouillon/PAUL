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

# ---- MEU_Target_NO2 ---- 
MEU_NO2_target <- read.table(
  "E:/PAUL/MEU/N500/MEU_75m_target.mdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO2_target <- MEU_NO2_target[,-c(1,2,11,13:17)]
colnames(MEU_NO2_target) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","MEU","Stdev", "flag")
MEU_NO2_target<-within(MEU_NO2_target,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO2_target$flag <- as.character(MEU_NO2_target$flag)
MEU_NO2_target <- as.data.table(MEU_NO2_target)
MEU_NO2_target_flag <- MEU_NO2_target[flag == "U"|flag =="O"|flag =="R"]

require(data.table)
MEU_NO2_min <- filter(MEU_NO2_target,Minute>=15 & Minute<=20 )
MEU_NO2_min <- filter(MEU_NO2_min,MEU>= - 15)
MEU_NO2_min <- MEU_NO2_min[,-c(1:6)]
MEU_NO2_min <- MEU_NO2_min[,-c(2:3)]

MEU_NO2_target_flag <- MEU_NO2_target_flag[,-c(1:6)]
MEU_NO2_target_flag <- MEU_NO2_target_flag[,-c(2:3)]

# ---- ROV_Target_NO2 ---- 
ROV_NO2_target <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_target.mdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO2_target <- ROV_NO2_target[,-c(1,2,11,13:17)]
colnames(ROV_NO2_target) <- c("Year","Month","Day","Hour","Minute",
                          "DecimalDate","ROV","Stdev", "flag")
ROV_NO2_target<-within(ROV_NO2_target,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_NO2_target$flag <- as.character(ROV_NO2_target$flag)
ROV_NO2_target <- as.data.table(ROV_NO2_target)
ROV_NO2_target_flag <- ROV_NO2_target[flag == "U"|flag =="O"|flag =="R"]

require(data.table)
ROV_NO2_min <- filter(ROV_NO2_target,Minute>=15 & Minute<=20 )
ROV_NO2_min <- filter(ROV_NO2_min,ROV>= - 15)
ROV_NO2_min <- ROV_NO2_min[,-c(1:6)]
ROV_NO2_min <- ROV_NO2_min[,-c(2:3)]

ROV_NO2_target_flag <- ROV_NO2_target_flag[,-c(1:6)]
ROV_NO2_target_flag <- ROV_NO2_target_flag[,-c(2:3)]

# ---- Plot Target NO2 ---- 
plot <- ggplot()+
  geom_point(data = MEU_NO2_min,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  geom_point(data = ROV_NO2_min,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_NO2_target_flag$Datetime), max(MEU_NO2_target_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  scale_y_continuous(limits = c(-1,1)) +
  # xlab("Time")+
  ylab("[NO2] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "NO2 Blanc last 5 minutes ZOOM"))
plot
# ggplotly(plot)
# ---- MEU_Target_NO ---- 
MEU_NO_target <- read.table(
  "E:/PAUL/MEU/N500/MEU_75m_target.mdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO_target <- MEU_NO_target[,-c(1,2,11,13:17)]
colnames(MEU_NO_target) <- c("Year","Month","Day","Hour","Minute",
                              "DecimalDate","MEU","Stdev", "flag")
MEU_NO_target<-within(MEU_NO_target,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO_target$flag <- as.character(MEU_NO_target$flag)
MEU_NO_target <- as.data.table(MEU_NO_target)
MEU_NO_target_flag <- MEU_NO_target[flag == "U"|flag =="O"|flag =="R"]

require(data.table)
MEU_NO_min <- filter(MEU_NO_target,Minute>=15 & Minute<=20 )
MEU_NO_min <- filter(MEU_NO_min,MEU>= - 15)
MEU_NO_min <- MEU_NO_min[,-c(1:6)]
MEU_NO_min <- MEU_NO_min[,-c(2:3)]

MEU_NO_target_flag <- MEU_NO_target_flag[,-c(1:6)]
MEU_NO_target_flag <- MEU_NO_target_flag[,-c(2:3)]

# ---- ROV_Target_NO ---- 
ROV_NO_target <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_target.mdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO_target <- ROV_NO_target[,-c(1,2,11,13:17)]
colnames(ROV_NO_target) <- c("Year","Month","Day","Hour","Minute",
                              "DecimalDate","ROV","Stdev", "flag")
ROV_NO_target<-within(ROV_NO_target,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_NO_target$flag <- as.character(ROV_NO_target$flag)
ROV_NO_target <- as.data.table(ROV_NO_target)
ROV_NO_target_flag <- ROV_NO_target[flag == "U"|flag =="O"|flag =="R"]

require(data.table)
ROV_NO_min <- filter(ROV_NO_target,Minute>=15 & Minute<=20 )
ROV_NO_min <- filter(ROV_NO_min,ROV>= -2)
ROV_NO_min <- ROV_NO_min[,-c(1:6)]
ROV_NO_min <- ROV_NO_min[,-c(2:3)]

ROV_NO_target_flag <- ROV_NO_target_flag[,-c(1:6)]
ROV_NO_target_flag <- ROV_NO_target_flag[,-c(2:3)]

# ---- Plot Target NO ----
plot <- ggplot()+
  geom_point(data = MEU_NO_min,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  geom_point(data = ROV_NO_min,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_NO_min$Datetime), max(MEU_NO_min$Datetime), length=12), 
                   date_labels="%b-%y") +
  scale_y_continuous(limits = c(-0.5,0.5)) +
  # xlab("Time")+
  ylab("[NO] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "NO Blanc last 5 minutes ZOOM"))
plot
ggplotly(plot)

ROV_NO_target_flag <- ROV_NO_target_flag[-c(1028:1040),]
# ---- MEU_NO2 ---- 
MEU_NO2 <- read.table(
  "E:/PAUL/MEU/N500/MEU_65m_air.mdf.all.no2",
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

MEU_NO2_flag <- MEU_NO2_flag[-c(841),]
MEU_NO2_flag <- MEU_NO2_flag[-c(1886),]

# ---- ROV_NO2 ---- 
ROV_NO2 <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_air.mdf.all.no2",
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

ROV_NO2_flag <- ROV_NO2_flag[-c(1740),]

# ---- Plot NO2 ----
plot <- ggplot()+
  geom_point(data = ROV_NO2_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_NO2_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_NO2_flag$Datetime), max(MEU_NO2_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  # xlab("Time")+
  ylab("[NO2] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "NO2"))
plot
ggplotly(plot)

# ---- Zoom Plot NO2 ----
plot <- ggplot()+
  geom_point(data = ROV_NO2_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_NO2_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_NO2_flag$Datetime), max(MEU_NO2_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  ylim(-2, 20) +
  # xlab("Time")+
  ylab("[NO2] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "NO2 Zoom"))
plot
# ggplotly(plot)


# ---- MEU_NO ---- 
MEU_NO <- read.table(
  "E:/PAUL/MEU/N500/MEU_65m_air.mdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO <- MEU_NO[,-c(1,2,11,13:17)]
colnames(MEU_NO) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_NO<-within(MEU_NO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO$flag <- as.character(MEU_NO$flag)
MEU_NO <- as.data.table(MEU_NO)
MEU_NO_flag <- MEU_NO[flag == "U"|flag =="O"|flag =="R"]
MEU_NO_flag <- MEU_NO_flag[,-c(1:6)]
MEU_NO_flag <- MEU_NO_flag[,-c(2:3)]

MEU_NO_flag <- MEU_NO_flag[-c(841),]

# ---- ROV_NO ---- 
ROV_NO <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_air.mdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO <- ROV_NO[,-c(1,2,11,13:17)]
colnames(ROV_NO) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_NO<-within(ROV_NO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_NO$flag <- as.character(ROV_NO$flag)
ROV_NO <- as.data.table(ROV_NO)
ROV_NO_flag <- ROV_NO[flag == "U"|flag =="O"|flag =="R"]
ROV_NO_flag <- ROV_NO_flag[,-c(1:6)]
ROV_NO_flag <- ROV_NO_flag[,-c(2:3)]

# ---- Plot NO ----
plot <- ggplot()+
  geom_point(data = ROV_NO_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_NO_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_NO_flag$Datetime), max(MEU_NO_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  # xlab("Time")+
  ylab("[NO] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "NO"))
plot
ggplotly(plot)

# ---- Zoom Plot NO ----
plot <- ggplot()+
  geom_point(data = ROV_NO_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_NO_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_NO_flag$Datetime), max(MEU_NO_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  ylim(-2, 20) +
  # xlab("Time")+
  ylab("[NO] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "Zoom NO"))
plot
ggplotly(plot)

# ---- Plot CD MEU_ROV NO ----
NO <- merge(
  x = MEU_NO_flag,
  y = ROV_NO_flag,
  by ="Datetime",
  all = TRUE
)

colnames(NO) [1] <- "date"


wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(NO
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[NO] ppb"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")


# ---- Plot CD MEU_ROV NO2 ----
NO2 <- merge(
  x = MEU_NO2_flag,
  y = ROV_NO2_flag,
  by ="Datetime",
  all = TRUE)

NO2_SAC <- list.files(path = "E:/SACLAY/DATA_SIRTA/2024/",  # Identify all CSV files
                        pattern = "gasesz5_1b_Lz5Pno2*", recursive = TRUE, full.names = TRUE) %>% 
  lapply(read.table, sep = "") %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
colnames(NO2_SAC) <- c("Datetime","DecimalDate Start","DecimalDate End","NO2","Flag Value",
                         "Flag Valid","Quality Flag","O3","Temp", "Pressure", "Relative Humidity", "UVA")
NO2_SAC$Datetime <- as.POSIXct(NO2_SAC$Datetime, format = '%Y-%m-%dT%H:%M:%SZ', tz="UTC")

NO2 <- merge(
  x = NO2,
  y = NO2_SAC,
  by ="Datetime",
  all.x = TRUE)

NO2 <- NO2[,-c(4,5, 7:14)]
colnames(NO2) [4] <- "SAC"


colnames(NO2) [1] <- "date"

wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(NO2
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[NO2] ppb"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD NOx MEU----
MEU <- merge(
  x = MEU_NO_flag,
  y = MEU_NO2_flag,
  by ="Datetime",
  all = TRUE)

colnames(MEU) [1] <- "date"
colnames(MEU) [2] <- "NO"
colnames(MEU) [3] <- "NO2"

wb_marker <- c("NO", "NO2")
title = 'MEU'
myOutput_WB <- timeVariation(MEU
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             ,normalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="ppb"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour", main = title)

# ---- Plot CD NOx ROV----
ROV <- merge(
  x = ROV_NO_flag,
  y = ROV_NO2_flag,
  by ="Datetime",
  all = TRUE)

colnames(ROV) [1] <- "date"
colnames(ROV) [2] <- "NO"
colnames(ROV) [3] <- "NO2"

wb_marker <- c("NO", "NO2")
title = 'ROV'
myOutput_WB <- timeVariation(ROV
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             ,normalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="ppb"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour", main = title)

# ---- Plot CD NO2 Saeson ----
colnames(MEU_NO2_flag) [2] <- "date"
MEU_NO2_flag <- as.data.table(MEU_NO2_flag)
MEU_NO2_flag[, month := format(as.Date(date), "%m")]
MEU_NO2_H <- MEU_NO2_flag[month == "12" |month == "01" |month == "02"]
MEU_NO2_E <- MEU_NO2_flag[month == "06" |month == "07" |month == "08"]

MEU_NO2_H$type <- "DJF"
MEU_NO2_E$type <- "JJA"

MEU_NO2 <- rbind(MEU_NO2_H,MEU_NO2_E)

wb_markerCO2 <- c("MEU")
myOutput_WB <- timeVariation(MEU_NO2
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="NO2 (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(1, 9)
)
plot(myOutput_WB, subset = "hour")

colnames(ROV_NO2_flag) [2] <- "date"
ROV_NO2_flag <- as.data.table(ROV_NO2_flag)
ROV_NO2_flag[, month := format(as.Date(date), "%m")]
ROV_NO2_H <- ROV_NO2_flag[month == "12" |month == "01" |month == "02"]
ROV_NO2_E <- ROV_NO2_flag[month == "06" |month == "07" |month == "08"]

ROV_NO2_H$type <- "DJF"
ROV_NO2_E$type <- "JJA"

ROV_NO2 <- rbind(ROV_NO2_H,ROV_NO2_E)

wb_markerCO2 <- c("ROV")
myOutput_WB <- timeVariation(ROV_NO2
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="NO2 (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(1, 9)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD NO Saeson ----
colnames(MEU_NO_flag) [2] <- "date"
MEU_NO_flag <- as.data.table(MEU_NO_flag)
MEU_NO_flag[, month := format(as.Date(date), "%m")]
MEU_NO_H <- MEU_NO_flag[month == "12" |month == "01" |month == "02"]
MEU_NO_E <- MEU_NO_flag[month == "06" |month == "07" |month == "08"]

MEU_NO_H$type <- "DJF"
MEU_NO_E$type <- "JJA"

MEU_NO <- rbind(MEU_NO_H,MEU_NO_E)

wb_markerCO2 <- c("MEU")
myOutput_WB <- timeVariation(MEU_NO
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="NO (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(0, 6)
)
plot(myOutput_WB, subset = "hour")

colnames(ROV_NO_flag) [2] <- "date"
ROV_NO_flag <- as.data.table(ROV_NO_flag)
ROV_NO_flag[, month := format(as.Date(date), "%m")]
ROV_NO_H <- ROV_NO_flag[month == "12" |month == "01" |month == "02"]
ROV_NO_E <- ROV_NO_flag[month == "06" |month == "07" |month == "08"]

ROV_NO_H$type <- "DJF"
ROV_NO_E$type <- "JJA"

ROV_NO <- rbind(ROV_NO_H,ROV_NO_E)

wb_markerCO2 <- c("ROV")
myOutput_WB <- timeVariation(ROV_NO
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="NO (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(0, 6)
)
plot(myOutput_WB, subset = "hour")
# ---- MEU_BC ---- 
MEU_BC_2023 <- read.csv2("E:/PAUL/MEU/Data_VF/Data2023_MEU.csv", sep=",")
MEU_BC_2024 <- read.csv2("E:/PAUL/MEU/Data_VF/Data2024_MEU.csv", sep=",")

MEU_BC <- rbind(MEU_BC_2023, MEU_BC_2024)
MEU_BC$BCdate <- as.POSIXct(MEU_BC$BCdate, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
colnames(MEU_BC) <- c("DateTime","BCff","BCwb","BCtot")
MEU_BC$BCff <- as.numeric(MEU_BC$BCff)
MEU_BC$BCwb <- as.numeric(MEU_BC$BCwb)
MEU_BC$BCtot <- as.numeric(MEU_BC$BCtot)

MEU_BCff <- MEU_BC[,-c(3,4)]
colnames(MEU_BCff) <- c("date","MEU")
MEU_BCwb <- MEU_BC[,-c(2,4)]
colnames(MEU_BCwb) <- c("date","MEU")

# ---- ROV_BC ---- 
ROV_BC_2023 <- read.csv2("E:/PAUL/ROV/Data_VF/Data2023_ROV.csv", sep=",")
ROV_BC_2024 <- read.csv2("E:/PAUL/ROV/Data_VF/Data2024_ROV.csv", sep=",")

ROV_BC <- rbind(ROV_BC_2023, ROV_BC_2024)
ROV_BC$BCdate <- as.POSIXct(ROV_BC$BCdate, format = '%d/%m/%Y %H:%M:%S', tz="UTC")
colnames(ROV_BC) <- c("DateTime","BCff","BCwb","BCtot")
ROV_BC$BCff <- as.numeric(ROV_BC$BCff)
ROV_BC$BCwb <- as.numeric(ROV_BC$BCwb)
ROV_BC$BCtot <- as.numeric(ROV_BC$BCtot)

ROV_BCff <- ROV_BC[,-c(3,4)]
colnames(ROV_BCff) <- c("date","ROV")
ROV_BCwb <- ROV_BC[,-c(2,4)]
colnames(ROV_BCwb) <- c("date","ROV")

# ---- Merge BCff ---- 
BCff <- merge(
  x = ROV_BCff,
  y = MEU_BCff,
  by ="date",
  all = TRUE)
BCff_HA <- timeAverage(BCff, "1 hour")
# ---- Merge BCwb ---- 
BCwb <- merge(
  x = ROV_BCwb,
  y = MEU_BCwb,
  by ="date",
  all = TRUE)
BCwb_HA <- timeAverage(BCwb, "1 hour")
# ---- Plot BCff ----
BCff_HA <- filter(BCff_HA,ROV<= 5000 | MEU<= 5000)
BCff_HA <- filter(BCff_HA,MEU<= 5000)
plot <- ggplot()+
  geom_point(data = BCff_HA,
             mapping = aes(x = date, y = ROV, col = "ROV"))+
  geom_point(data = BCff_HA,
             mapping = aes(x = date, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_BCff$date), max(MEU_BCff$date), length=12), 
                   date_labels="%b-%y") +
  ylab("[BCff] (ng.m-3)")+
  ggtitle(
    paste0(
      "BCff"))
plot
ggplotly(plot)

# ---- Plot BCwb ----
BCwb_HA <- filter(BCwb_HA,MEU>= - 10)
BCwb_HA <- filter(BCwb_HA,ROV>= - 10)
plot <- ggplot()+
  geom_point(data = BCwb_HA,
             mapping = aes(x = date, y = ROV, col = "ROV"))+
  geom_point(data = BCwb_HA,
             mapping = aes(x = date, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_BCwb$date), max(MEU_BCwb$date), length=12), 
                   date_labels="%b-%y") +
  ylab("[BCwb] (ng.m-3)")+
  ggtitle(
    paste0(
      "BCwb"))
plot
ggplotly(plot)


# ---- Plot CD BCff ----
wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(BCff_HA
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[BCff] ng.m-3"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")


# ---- Plot CD BCwb ----
wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(BCwb_HA
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[BCwb] ng.m-3"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD BCff Saeson ----
# colnames(MEU_BCff) [2] <- "date"
MEU_BCff <- as.data.table(MEU_BCff)
MEU_BCff[, month := format(as.Date(date), "%m")]
MEU_BCff_H <- MEU_BCff[month == "12" |month == "01" |month == "02"]
MEU_BCff_E <- MEU_BCff[month == "06" |month == "07" |month == "08"]

MEU_BCff_H$type <- "DJF"
MEU_BCff_E$type <- "JJA"

MEU_BCff <- rbind(MEU_BCff_H,MEU_BCff_E)

wb_markerCO2 <- c("MEU")
myOutput_WB <- timeVariation(MEU_BCff
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,BCffrmalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="BCff (ng.m-3)"
                             ,xlab ="Hour"
                             ,ylim = c(200, 900)
)
plot(myOutput_WB, subset = "hour")

ROV_BCff <- as.data.table(ROV_BCff)
ROV_BCff[, month := format(as.Date(date), "%m")]
ROV_BCff_H <- ROV_BCff[month == "12" |month == "01" |month == "02"]
ROV_BCff_E <- ROV_BCff[month == "06" |month == "07" |month == "08"]

ROV_BCff_H$type <- "DJF"
ROV_BCff_E$type <- "JJA"

ROV_BCff <- rbind(ROV_BCff_H,ROV_BCff_E)

wb_markerCO2 <- c("ROV")
myOutput_WB <- timeVariation(ROV_BCff
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,BCffrmalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="BCff (ng.m-3)"
                             ,xlab ="Hour"
                             ,ylim = c(200, 900)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD BCwb Saeson ----
# colnames(MEU_BCwb) [2] <- "date"
MEU_BCwb <- as.data.table(MEU_BCwb)
MEU_BCwb[, month := format(as.Date(date), "%m")]
MEU_BCwb_H <- MEU_BCwb[month == "12" |month == "01" |month == "02"]
MEU_BCwb_E <- MEU_BCwb[month == "06" |month == "07" |month == "08"]

MEU_BCwb_H$type <- "DJF"
MEU_BCwb_E$type <- "JJA"

MEU_BCwb <- rbind(MEU_BCwb_H,MEU_BCwb_E)

wb_markerCO2 <- c("MEU")
myOutput_WB <- timeVariation(MEU_BCwb
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,BCwbrmalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="BCwb (ng.m-3)"
                             ,xlab ="Hour"
                             ,ylim = c(0, 350)
)
plot(myOutput_WB, subset = "hour")

ROV_BCwb <- as.data.table(ROV_BCwb)
ROV_BCwb[, month := format(as.Date(date), "%m")]
ROV_BCwb_H <- ROV_BCwb[month == "12" |month == "01" |month == "02"]
ROV_BCwb_E <- ROV_BCwb[month == "06" |month == "07" |month == "08"]

ROV_BCwb_H$type <- "DJF"
ROV_BCwb_E$type <- "JJA"

ROV_BCwb <- rbind(ROV_BCwb_H,ROV_BCwb_E)

wb_markerCO2 <- c("ROV")
myOutput_WB <- timeVariation(ROV_BCwb
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,BCwbrmalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="BCwb (ng.m-3)"
                             ,xlab ="Hour"
                             ,ylim = c(0, 350)
)
plot(myOutput_WB, subset = "hour")
# ---- MEU_CO2 ---- 
MEU_CO2 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CO2 <- MEU_CO2[,-c(1,2,11,13:17)]
colnames(MEU_CO2) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","MEU","Stdev", "flag")
MEU_CO2<-within(MEU_CO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CO2$flag <- as.character(MEU_CO2$flag)
MEU_CO2 <- as.data.table(MEU_CO2)
MEU_CO2_flag <- MEU_CO2[flag == "U"|flag =="O"|flag =="R"]
MEU_CO2_flag <- MEU_CO2_flag[,-c(1:6)]
MEU_CO2_flag <- MEU_CO2_flag[,-c(2:3)]

# ---- ROV_CO2 ---- 
ROV_CO2 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CO2 <- ROV_CO2[,-c(1,2,11,13:17)]
colnames(ROV_CO2) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","ROV","Stdev", "flag")
ROV_CO2<-within(ROV_CO2,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_CO2$flag <- as.character(ROV_CO2$flag)
ROV_CO2 <- as.data.table(ROV_CO2)
ROV_CO2_flag <- ROV_CO2[flag == "U"|flag =="O"|flag =="R"]
ROV_CO2_flag <- ROV_CO2_flag[,-c(1:6)]
ROV_CO2_flag <- ROV_CO2_flag[,-c(2:3)]

# ---- Plot CO2 ----
plot <- ggplot()+
  geom_point(data = ROV_CO2_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_CO2_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_CO2_flag$Datetime), max(MEU_CO2_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  # xlab("Time")+
  ylab("[CO2] (ppm)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "CO2"))
plot
ggplotly(plot)

# ---- Plot CD MEU_ROV CO2 ----
CO2 <- merge(
  x = MEU_CO2_flag,
  y = ROV_CO2_flag,
  by ="Datetime",
  all = TRUE
)

colnames(CO2) [1] <- "date"


wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(CO2
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[CO2] ppm"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD CO2 Saeson ----
colnames(MEU_CO2_flag) [2] <- "date"
MEU_CO2_flag <- as.data.table(MEU_CO2_flag)
MEU_CO2_flag[, month := format(as.Date(date), "%m")]
MEU_CO2_H <- MEU_CO2_flag[month == "12" |month == "01" |month == "02"]
MEU_CO2_E <- MEU_CO2_flag[month == "06" |month == "07" |month == "08"]

MEU_CO2_H$type <- "DJF"
MEU_CO2_E$type <- "JJA"

MEU_CO2 <- rbind(MEU_CO2_H,MEU_CO2_E)

wb_markerCO2 <- c("MEU")
myOutput_WB <- timeVariation(MEU_CO2
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="CO2 (ppm)"
                             ,xlab ="Hour"
                             ,ylim = c(410, 450)
)
plot(myOutput_WB, subset = "hour")

colnames(ROV_CO2_flag) [2] <- "date"
ROV_CO2_flag <- as.data.table(ROV_CO2_flag)
ROV_CO2_flag[, month := format(as.Date(date), "%m")]
ROV_CO2_H <- ROV_CO2_flag[month == "12" |month == "01" |month == "02"]
ROV_CO2_E <- ROV_CO2_flag[month == "06" |month == "07" |month == "08"]

ROV_CO2_H$type <- "DJF"
ROV_CO2_E$type <- "JJA"

ROV_CO2 <- rbind(ROV_CO2_H,ROV_CO2_E)

wb_markerCO2 <- c("ROV")
myOutput_WB <- timeVariation(ROV_CO2
                             ,pollutant = wb_markerCO2
                             ,name.pol = wb_markerCO2
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="CO2 (ppm)"
                             ,xlab ="Hour"
                             ,ylim = c(410, 450)
)
plot(myOutput_WB, subset = "hour")
# ---- MEU_CH4 ---- 
MEU_CH4 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CH4 <- MEU_CH4[,-c(1,2,11,13:17)]
colnames(MEU_CH4) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_CH4<-within(MEU_CH4,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CH4$flag <- as.character(MEU_CH4$flag)
MEU_CH4 <- as.data.table(MEU_CH4)
MEU_CH4_flag <- MEU_CH4[flag == "U"|flag =="O"|flag =="R"]
MEU_CH4_flag <- MEU_CH4_flag[,-c(1:6)]
MEU_CH4_flag <- MEU_CH4_flag[,-c(2:3)]

# ---- ROV_CH4 ---- 
ROV_CH4 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CH4 <- ROV_CH4[,-c(1,2,11,13:17)]
colnames(ROV_CH4) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_CH4<-within(ROV_CH4,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_CH4$flag <- as.character(ROV_CH4$flag)
ROV_CH4 <- as.data.table(ROV_CH4)
ROV_CH4_flag <- ROV_CH4[flag == "U"|flag =="O"|flag =="R"]
ROV_CH4_flag <- ROV_CH4_flag[,-c(1:6)]
ROV_CH4_flag <- ROV_CH4_flag[,-c(2:3)]

# ---- Plot CH4 ----
plot <- ggplot()+
  geom_point(data = ROV_CH4_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_CH4_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_CH4_flag$Datetime), max(MEU_CH4_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  # xlab("Time")+
  ylab("[CH4] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "CH4"))
plot
ggplotly(plot)

# ---- Plot CD MEU_ROV CH4 ----
CH4 <- merge(
  x = MEU_CH4_flag,
  y = ROV_CH4_flag,
  by ="Datetime",
  all = TRUE
)

colnames(CH4) [1] <- "date"


wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(CH4
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[CH4] ppb"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD CH4 Saeson ----
colnames(MEU_CH4_flag) [2] <- "date"
MEU_CH4_flag <- as.data.table(MEU_CH4_flag)
MEU_CH4_flag[, month := format(as.Date(date), "%m")]
MEU_CH4_H <- MEU_CH4_flag[month == "12" |month == "01" |month == "02"]
MEU_CH4_E <- MEU_CH4_flag[month == "06" |month == "07" |month == "08"]

MEU_CH4_H$type <- "DJF"
MEU_CH4_E$type <- "JJA"

MEU_CH4 <- rbind(MEU_CH4_H,MEU_CH4_E)

wb_markerCH4 <- c("MEU")
myOutput_WB <- timeVariation(MEU_CH4
                             ,pollutant = wb_markerCH4
                             ,name.pol = wb_markerCH4
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="CH4 (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(1990, 2100)
)
plot(myOutput_WB, subset = "hour")

colnames(ROV_CH4_flag) [2] <- "date"
ROV_CH4_flag <- as.data.table(ROV_CH4_flag)
ROV_CH4_flag[, month := format(as.Date(date), "%m")]
ROV_CH4_H <- ROV_CH4_flag[month == "12" |month == "01" |month == "02"]
ROV_CH4_E <- ROV_CH4_flag[month == "06" |month == "07" |month == "08"]

ROV_CH4_H$type <- "DJF"
ROV_CH4_E$type <- "JJA"

ROV_CH4 <- rbind(ROV_CH4_H,ROV_CH4_E)

wb_markerCH4 <- c("ROV")
myOutput_WB <- timeVariation(ROV_CH4
                             ,pollutant = wb_markerCH4
                             ,name.pol = wb_markerCH4
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="CH4 (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(1990, 2100)
)
plot(myOutput_WB, subset = "hour")
# ---- MEU_CO ---- 
MEU_CO <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CO <- MEU_CO[,-c(1,2,11,13:17)]
colnames(MEU_CO) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_CO<-within(MEU_CO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_CO$flag <- as.character(MEU_CO$flag)
MEU_CO <- as.data.table(MEU_CO)
MEU_CO_flag <- MEU_CO[flag == "U"|flag =="O"|flag =="R"]
MEU_CO_flag <- MEU_CO_flag[,-c(1:6)]
MEU_CO_flag <- MEU_CO_flag[,-c(2:3)]

# ---- ROV_CO ---- 
ROV_CO <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CO <- ROV_CO[,-c(1,2,11,13:17)]
colnames(ROV_CO) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_CO<-within(ROV_CO,{
  Datetime<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

ROV_CO$flag <- as.character(ROV_CO$flag)
ROV_CO <- as.data.table(ROV_CO)
ROV_CO_flag <- ROV_CO[flag == "U"|flag =="O"|flag =="R"]
ROV_CO_flag <- ROV_CO_flag[,-c(1:6)]
ROV_CO_flag <- ROV_CO_flag[,-c(2:3)]

# ---- Plot CO ----
plot <- ggplot()+
  geom_point(data = ROV_CO_flag,
             mapping = aes(x = Datetime, y = ROV, col = "ROV"))+
  geom_point(data = MEU_CO_flag,
             mapping = aes(x = Datetime, y = MEU, col = "MEU"))+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(MEU_CO_flag$Datetime), max(MEU_CO_flag$Datetime), length=12), 
                   date_labels="%b-%y") +
  # xlab("Time")+
  ylab("[CO] (ppb)")+
  #' option 3 (doesn't work)
  #scale_x_discrete(
  #  breaks=c("janv. 2018","avr. 2018","juil. 2018","oct. 2018","janv. 2019"),
  #  labels=c("Jan", "Apr", "July", "Oct", "Jan")) +
  ggtitle(
    paste0(
      "CO"))
plot
ggplotly

# ---- Plot CD MEU_ROV CO ----
CO <- merge(
  x = MEU_CO_flag,
  y = ROV_CO_flag,
  by ="Datetime",
  all = TRUE
)

colnames(CO) [1] <- "date"


wb_marker <- c("MEU", "ROV")
myOutput_WB <- timeVariation(CO
                             ,pollutant = wb_marker
                             ,name.pol = wb_marker
                             ,difference = F
                             #,BCffrmalise = T
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="[CO] ppb"
                             ,xlab ="Hour"
                             # ,ylim = c(415, 445)
)
plot(myOutput_WB, subset = "hour")

# ---- Plot CD CO Saeson ----
colnames(MEU_CO_flag) [2] <- "date"
MEU_CO_flag <- as.data.table(MEU_CO_flag)
MEU_CO_flag[, month := format(as.Date(date), "%m")]
MEU_CO_H <- MEU_CO_flag[month == "12" |month == "01" |month == "02"]
MEU_CO_E <- MEU_CO_flag[month == "06" |month == "07" |month == "08"]

MEU_CO_H$type <- "DJF"
MEU_CO_E$type <- "JJA"

MEU_CO <- rbind(MEU_CO_H,MEU_CO_E)

wb_markerCO <- c("MEU")
myOutput_WB <- timeVariation(MEU_CO
                             ,pollutant = wb_markerCO
                             ,name.pol = wb_markerCO
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="CO (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(120, 210)
)
plot(myOutput_WB, subset = "hour")

colnames(ROV_CO_flag) [2] <- "date"
ROV_CO_flag <- as.data.table(ROV_CO_flag)
ROV_CO_flag[, month := format(as.Date(date), "%m")]
ROV_CO_H <- ROV_CO_flag[month == "12" |month == "01" |month == "02"]
ROV_CO_E <- ROV_CO_flag[month == "06" |month == "07" |month == "08"]

ROV_CO_H$type <- "DJF"
ROV_CO_E$type <- "JJA"

ROV_CO <- rbind(ROV_CO_H,ROV_CO_E)

wb_markerCO <- c("ROV")
myOutput_WB <- timeVariation(ROV_CO
                             ,pollutant = wb_markerCO
                             ,name.pol = wb_markerCO
                             , cols = c("steelblue", "orangered")
                             ,difference = F
                             #,Normalise = T
                             ,group = "type"
                             ,par.settings=list(fontsize=list(text=25))
                             ,ylab ="CO (ppb)"
                             ,xlab ="Hour"
                             ,ylim = c(120, 210)
)
plot(myOutput_WB, subset = "hour")
