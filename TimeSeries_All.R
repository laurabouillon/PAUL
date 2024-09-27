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

# ---- MEU_NO2 ---- 
MEU_NO2 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_65m_air.hdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO2 <- MEU_NO2[,-c(1,2,11,13:17)]
colnames(MEU_NO2) <- c("Year","Month","Day","Hour","Minute",
                              "DecimalDate","MEU","Stdev", "flag")
MEU_NO2<-within(MEU_NO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                       format="%Y %m %d %H %M",tz="UTC")})

MEU_NO2$flag <- as.character(MEU_NO2$flag)
MEU_NO2 <- as.data.table(MEU_NO2)
MEU_NO2_flag <- MEU_NO2[flag == "U"|flag =="O"|flag =="R"]
MEU_NO2_DA <- timeAverage(MEU_NO2_flag, "day")

# ---- MEU_CO2 ---- 
MEU_CO2 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CO2 <- MEU_CO2[,-c(1,2,11,13:17)]
colnames(MEU_CO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_CO2<-within(MEU_CO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

MEU_CO2$flag <- as.character(MEU_CO2$flag)
MEU_CO2 <- as.data.table(MEU_CO2)
MEU_CO2_flag <- MEU_CO2[flag == "U"|flag =="O"|flag =="R"]
MEU_CO2_DA <- timeAverage(MEU_CO2_flag, "day")

# ---- MEU_CO ---- 
MEU_CO <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CO <- MEU_CO[,-c(1,2,11,13:17)]
colnames(MEU_CO) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_CO<-within(MEU_CO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

MEU_CO$flag <- as.character(MEU_CO$flag)
MEU_CO <- as.data.table(MEU_CO)
MEU_CO_flag <- MEU_CO[flag == "U"|flag =="O"|flag =="R"]
MEU_CO_DA <- timeAverage(MEU_CO_flag, "day")

# ---- MEU_CH4 ---- 
MEU_CH4 <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_90m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_CH4 <- MEU_CH4[,-c(1,2,11,13:17)]
colnames(MEU_CH4) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_CH4<-within(MEU_CH4,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

MEU_CH4$flag <- as.character(MEU_CH4$flag)
MEU_CH4 <- as.data.table(MEU_CH4)
MEU_CH4_flag <- MEU_CH4[flag == "U"|flag =="O"|flag =="R"]
MEU_CH4_DA <- timeAverage(MEU_CH4_flag, "day")

# ---- MEU_NO ---- 
MEU_NO <- read.table(
  "E:/PAUL/MEU/ICOS/MEU_65m_air.hdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
MEU_NO <- MEU_NO[,-c(1,2,11,13:17)]
colnames(MEU_NO) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","MEU","Stdev", "flag")
MEU_NO<-within(MEU_NO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

MEU_NO$flag <- as.character(MEU_NO$flag)
MEU_NO <- as.data.table(MEU_NO)
MEU_NO_flag <- MEU_NO[flag == "U"|flag =="O"|flag =="R"]
MEU_NO_DA <- timeAverage(MEU_NO_flag, "day")

# ---- MEU_BC ---- 
MEU_BC <- read.csv2("E:/PAUL/MEU/AE33/Data_wb_ff_130123_210631_MEU.csv", sep=",")
colnames(MEU_BC) <- c("date","eBClf", "eBCsf", "eBC")
MEU_BC$date <- as.POSIXct(MEU_BC$date, format = '%d/%m/%Y %H:%M', tz="UTC")
MEU_BC <- MEU_BC %>% mutate_if(is.character, as.numeric)
MEU_BC_DA <- timeAverage(MEU_BC, "day")

start_beugue <- as.POSIXct("2023-06-05", tz = "UTC")
end_beugue <-as.POSIXct("2023-06-15", tz = "UTC")

start_panne <- as.POSIXct("2023-06-19", tz = "UTC")
end_panne <-as.POSIXct("2023-07-11", tz = "UTC")

debut_tombe <- as.POSIXct("2024-02-23", tz = "UTC")
fin_tombe <- as.POSIXct("2024-03-27", tz = "UTC")


# ---- Plot NO2 ----
MEU_NO2 <- ggplot(data = MEU_NO2_DA, mapping = aes(x = date, y = MEU)) +
  geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
            fill = "cadetblue3", alpha = 0.1)+
  geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
            fill = "khaki2", alpha = 0.1)+
  geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
            fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "red", size = 1) +
  geom_line(data = MEU_NO_DA, mapping = aes(x = date, y = MEU), col = "blue", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(MEU_NO2_DA$date), max(MEU_NO2_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(NO[x]~(ppb)))

MEU_NO2  # This will print the plot


# ---- Plot BC ----

MEU_BC_DA <- MEU_BC_DA[-c(406:436),]

MEU_BC <- ggplot(data = MEU_BC_DA,
                  mapping = aes(x = date, y = eBClf)) +
  geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
            fill = "palegreen1", alpha = 0.1)+
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  geom_line(data = MEU_BC_DA,
            mapping = aes(x = date, y = eBCsf), col = "brown", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(MEU_BC_DA$date), max(MEU_BC_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(eBC~(ng.m^-3)))

MEU_BC  # This will print the plot

# ---- Plot CO ----
MEU_CO <- ggplot(data = MEU_CO_DA,
                 mapping = aes(x = date, y = MEU)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(MEU_CO_DA$date), max(MEU_CO_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(CO~(ppb)))

MEU_CO  # This will print the plot


# ---- Plot CO2 ----
MEU_CO2 <- ggplot(data = MEU_CO2_DA,
                 mapping = aes(x = date, y = MEU)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(MEU_CO2_DA$date), max(MEU_CO2_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(CO[2]~(ppm)))

MEU_CO2  # This will print the plot


# ---- Plot CH4 ----
MEU_CH4 <- ggplot(data = MEU_CH4_DA,
                 mapping = aes(x = date, y = MEU)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(MEU_CH4_DA$date), max(MEU_CH4_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(CH[4]~(ppb)))

MEU_CH4  # This will print the plot

# ---- Plot MEU ----

pwrk <- plot_grid(MEU_NO2, MEU_BC, MEU_CO, MEU_CH4, MEU_CO2,
                  ncol = 1, nrow = 5, heights = c(2, 2))
print(pwrk)






# ---- ROV_NO2 ---- 
ROV_NO2 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.no2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO2 <- ROV_NO2[,-c(1,2,11,13:17)]
colnames(ROV_NO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_NO2<-within(ROV_NO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_NO2$flag <- as.character(ROV_NO2$flag)
ROV_NO2 <- as.data.table(ROV_NO2)
ROV_NO2_flag <- ROV_NO2[flag == "U"|flag =="O"|flag =="R"]
ROV_NO2_DA <- timeAverage(ROV_NO2_flag, "day")

# ---- ROV_CO2 ---- 
ROV_CO2 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.co2",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CO2 <- ROV_CO2[,-c(1,2,11,13:17)]
colnames(ROV_CO2) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_CO2<-within(ROV_CO2,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_CO2$flag <- as.character(ROV_CO2$flag)
ROV_CO2 <- as.data.table(ROV_CO2)
ROV_CO2_flag <- ROV_CO2[flag == "U"|flag =="O"|flag =="R"]
ROV_CO2_DA <- timeAverage(ROV_CO2_flag, "day")

# ---- ROV_CO ---- 
ROV_CO <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.co",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CO <- ROV_CO[,-c(1,2,11,13:17)]
colnames(ROV_CO) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","ROV","Stdev", "flag")
ROV_CO<-within(ROV_CO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_CO$flag <- as.character(ROV_CO$flag)
ROV_CO <- as.data.table(ROV_CO)
ROV_CO_flag <- ROV_CO[flag == "U"|flag =="O"|flag =="R"]
ROV_CO_DA <- timeAverage(ROV_CO_flag, "day")

# ---- ROV_CH4 ---- 
ROV_CH4 <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.ch4",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_CH4 <- ROV_CH4[,-c(1,2,11,13:17)]
colnames(ROV_CH4) <- c("Year","Month","Day","Hour","Minute",
                       "DecimalDate","ROV","Stdev", "flag")
ROV_CH4<-within(ROV_CH4,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_CH4$flag <- as.character(ROV_CH4$flag)
ROV_CH4 <- as.data.table(ROV_CH4)
ROV_CH4_flag <- ROV_CH4[flag == "U"|flag =="O"|flag =="R"]
ROV_CH4_DA <- timeAverage(ROV_CH4_flag, "day")

# ---- ROV_NO ---- 
ROV_NO <- read.table(
  "E:/PAUL/ROV/ICOS/ROV_103m_air.hdf.all.no",
  comment.char = "#", stringsAsFactors = T, sep =";") 
ROV_NO <- ROV_NO[,-c(1,2,11,13:17)]
colnames(ROV_NO) <- c("Year","Month","Day","Hour","Minute",
                      "DecimalDate","ROV","Stdev", "flag")
ROV_NO<-within(ROV_NO,{
  date<-as.POSIXct(paste(Year,Month,Day,Hour,Minute),
                   format="%Y %m %d %H %M",tz="UTC")})

ROV_NO$flag <- as.character(ROV_NO$flag)
ROV_NO <- as.data.table(ROV_NO)
ROV_NO_flag <- ROV_NO[flag == "U"|flag =="O"|flag =="R"]
ROV_NO_DA <- timeAverage(ROV_NO_flag, "day")

# ---- ROV_BC ---- 
ROV_BC <- read.csv2("E:/PAUL/ROV/AE33/Data_wb_ff_220223_140624_ROV.csv", sep=",")
colnames(ROV_BC) <- c("date","eBClf", "eBCsf", "eBC")
ROV_BC$date <- as.POSIXct(ROV_BC$date, format = '%d/%m/%Y %H:%M', tz="UTC")
ROV_BC <- ROV_BC %>% mutate_if(is.character, as.numeric)
ROV_BC_DA <- timeAverage(ROV_BC, "day")


 
debut_ROV <- as.POSIXct("2024-05-22", tz = "UTC")
fin_ROV <- as.POSIXct("2024-06-23", tz = "UTC")


# ---- Plot NO2 ----
ROV_NO2 <- ggplot(data = ROV_NO2_DA, mapping = aes(x = date, y = ROV)) +
  geom_rect(xmin = debut_ROV, xmax = fin_ROV, ymin = -Inf, ymax = +Inf,
           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "red", size = 1) +
  geom_line(data = ROV_NO_DA, mapping = aes(x = date, y = ROV), col = "blue", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "none", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(ROV_CO2_DA$date), max(ROV_CO2_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(NO[x]~(ppb)))

ROV_NO2  # This will print the plot


# ---- Plot BC ----
# ROV_BC_DA <- ROV_BC_DA[-c(416,417),]
ROV_BC <- ggplot(data = ROV_BC_DA,
                 mapping = aes(x = date, y = eBClf)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  geom_line(data = ROV_BC_DA,
            mapping = aes(x = date, y = eBCsf), col = "brown", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(ROV_BC_DA$date), max(ROV_BC_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(eBC~(ng.m^-3)))

ROV_BC  # This will print the plot

# ---- Plot CO ----
ROV_CO <- ggplot(data = ROV_CO_DA,
                 mapping = aes(x = date, y = ROV)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(ROV_CO_DA$date), max(ROV_CO_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(CO~(ppb)))

ROV_CO  # This will print the plot


# ---- Plot CO2 ----
ROV_CO2 <- ggplot(data = ROV_CO2_DA,
                  mapping = aes(x = date, y = ROV)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(ROV_CO2_DA$date), max(ROV_CO2_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(CO[2]~(ppm)))

ROV_CO2  # This will print the plot


# ---- Plot CH4 ----
ROV_CH4 <- ggplot(data = ROV_CH4_DA,
                  mapping = aes(x = date, y = ROV)) +
  # geom_rect(xmin = start_beugue, xmax = end_beugue, ymin = -Inf, ymax = +Inf,
  #           fill = "cadetblue3", alpha = 0.1)+
  # geom_rect(xmin = start_panne, xmax = end_panne, ymin = -Inf, ymax = +Inf,
  #           fill = "khaki2", alpha = 0.1)+
  # geom_rect(xmin = debut_tombe, xmax = fin_tombe, ymin = -Inf, ymax = +Inf,
  #           fill = "palegreen1", alpha = 0.1)+
  geom_line(col = "grey5", size = 1) +
  theme_bw() +
  theme(axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5),
        legend.position = "top", 
        legend.title = element_blank()) +
  scale_x_datetime(breaks= seq(min(ROV_CH4_DA$date), max(ROV_CH4_DA$date), length=10),
                   date_labels="%m-%Y", date_breaks = "1 month") +
  ylab(expression(CH[4]~(ppb)))

ROV_CH4  # This will print the plot

# ---- Plot ROV ----

pwrk <- plot_grid(ROV_NO2, ROV_BC, ROV_CO, ROV_CH4, ROV_CO2,
                  ncol = 1, nrow = 5, heights = c(2, 2))
print(pwrk)
