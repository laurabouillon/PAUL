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
# MEU_NO2_flag <- MEU_NO2_flag[-c(1886),]

MEU_NO2_Clim <- MEU_NO2_flag %>% filter(between(Datetime, as.POSIXct('2024-07-01 00:00:00'), as.POSIXct('2024-08-22 23:00:00')))
MEU_NO2_Clim[, "Station"] <- "MEU"
colnames(MEU_NO2_Clim) [1] <- "Concentration"

# ---- ROV_NO2 ---- 
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

# ROV_NO2_flag <- ROV_NO2_flag[-c(1740),]
ROV_NO2_Clim <- ROV_NO2_flag %>% filter(between(Datetime, as.POSIXct('2024-07-01 00:00:00'), as.POSIXct('2024-08-22 23:00:00')))
ROV_NO2_Clim[, "Station"] <- "ROV"
colnames(ROV_NO2_Clim) [1] <- "Concentration"

# ---- Plot NO2 ----
NO2_Clim <- rbind(MEU_NO2_Clim, ROV_NO2_Clim)

NO2_Clim$Station <- factor(NO2_Clim$Station, levels = c("MEU", 
                                                "ROV"))

station_colors <- c("MEU" = "#FF5733", "ROV" = "#3498DB")

plot <- ggplot()+
  geom_point(data = NO2_Clim,
             mapping = aes(x = Datetime, y = Concentration, col = Station), size = 1)+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_color_manual(values = station_colors) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-08-09 10:00:00")), linetype = "dashed", color = "black", size = 1) +
  scale_x_datetime(breaks= seq(min(NO2_Clim$Datetime), max(NO2_Clim$Datetime), length=12), 
                   date_labels="%d-%m") +
  facet_grid(Station ~ ., scales = "free_y") +
  ylab("[NO2] (ppb)")
plot
ggplotly(plot)

# ---- Zoom Plot NO2 ----


plot <- ggplot()+
  geom_line(data = NO2_Clim,
             mapping = aes(x = Datetime, y = Concentration, col = Station), size = 1)+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_color_manual(values = station_colors) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-08-09 10:00:00")), linetype = "dashed", color = "black", size = 1) +
  ylim(0, 5) +
  scale_x_datetime(breaks= seq(min(NO2_Clim$Datetime), max(NO2_Clim$Datetime), length=12), 
                   date_labels="%d-%m") +
  facet_wrap(~ Station, nrow = 2)+
  ylab("[NO2] (ppb)")
plot
ggplotly(plot)





# ---- MEU_NO ---- 
MEU_NO <- read.table(
  "E:/PAUL/MEU/N500/MEU_65m_air.hdf.all.no",
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

# MEU_NO_flag <- MEU_NO_flag[-c(841),]
# MEU_NO_flag <- MEU_NO_flag[-c(1886),]

MEU_NO_Clim <- MEU_NO_flag %>% filter(between(Datetime, as.POSIXct('2024-08-06 00:00:00'), as.POSIXct('2024-08-13 23:00:00')))
MEU_NO_Clim[, "Station"] <- "MEU"
colnames(MEU_NO_Clim) [1] <- "Concentration"
# ---- ROV_NO ---- 
ROV_NO <- read.table(
  "E:/PAUL/ROV/N500/ROV_103m_air.hdf.all.no",
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

# ROV_NO_flag <- ROV_NO_flag[-c(1740),]
ROV_NO_Clim <- ROV_NO_flag %>% filter(between(Datetime, as.POSIXct('2024-08-06 00:00:00'), as.POSIXct('2024-08-13 23:00:00')))
ROV_NO_Clim[, "Station"] <- "ROV"
colnames(ROV_NO_Clim) [1] <- "Concentration"

# ---- Plot NO ----
NO_Clim <- rbind(MEU_NO_Clim, ROV_NO_Clim)

NO_Clim$Station <- factor(NO_Clim$Station, levels = c("MEU", 
                                                        "ROV"))

station_colors <- c("MEU" = "#FF5733", "ROV" = "#3498DB")

plot <- ggplot()+
  geom_point(data = NO_Clim,
             mapping = aes(x = Datetime, y = Concentration, col = Station), size = 1)+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_color_manual(values = station_colors) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-08-09 10:00:00")), linetype = "dashed", color = "black", size = 1) +
  scale_x_datetime(breaks= seq(min(NO_Clim$Datetime), max(NO_Clim$Datetime), length=12), 
                   date_labels="%d-%m") +
  facet_grid(~ Station, nrow = 2)+
  ylab("[NO] (ppb)")
plot
ggplotly(plot)

# ---- Zoom Plot NO ----


plot <- ggplot()+
  geom_line(data = NO_Clim,
            mapping = aes(x = Datetime, y = Concentration, col = Station), size = 1)+
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_color_manual(values = station_colors) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2024-08-09 10:00:00")), linetype = "dashed", color = "black", size = 1) +
  ylim(-0.5, 2) +
  scale_x_datetime(breaks= seq(min(NO_Clim$Datetime), max(NO_Clim$Datetime), length=12), 
                   date_labels="%d-%m") +
  facet_wrap(~ Station, nrow = 2)+
  ylab("[NO] (ppb)")
plot
ggplotly(plot)
