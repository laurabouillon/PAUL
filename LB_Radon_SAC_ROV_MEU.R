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

#---- CO2 ---- 
CO2_MEU <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CO2_MEU90_2022_2023-02-06_2024-02-06")
CO2_ROV <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CO2_ROV103_2022_2023-02-06_2024-02-06")
CO2_SAC <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CO2_SAC100_2022_2023-02-06_2024-02-06")

colnames(CO2_MEU) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CO2_MEU <- CO2_MEU[-c(1),]

# colnames(CO2_MEU2) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
# CO2_MEU2 <- CO2_MEU2[-c(1),]

colnames(CO2_ROV) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CO2_ROV <- CO2_ROV[-c(1),]

colnames(CO2_SAC) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CO2_SAC <- CO2_SAC[-c(1),]

CO2_SAC <- CO2_SAC[-c(1:149),]


CO2_MEU$Date <- as.POSIXct(strptime(CO2_MEU$Date,'%Y-%m-%d'))
CO2_ROV$Date <- as.POSIXct(strptime(CO2_ROV$Date,'%Y-%m-%d'))
CO2_SAC$Date <- as.POSIXct(strptime(CO2_SAC$Date,'%Y-%m-%d'))


CO2_MEU$Jghg <- as.numeric(CO2_MEU$Jghg)
CO2_ROV$Jghg <- as.numeric(CO2_ROV$Jghg)
CO2_SAC$Jghg <- as.numeric(CO2_SAC$Jghg)


CO2_MEU <- as.data.table(CO2_MEU)
CO2_MEU<- CO2_MEU[Type == "StationPixel"]
CO2_ROV <- as.data.table(CO2_ROV)
CO2_ROV<- CO2_ROV[Type == "StationPixel"]
CO2_SAC <- as.data.table(CO2_SAC)
CO2_SAC<- CO2_SAC[Type == "StationPixel"]


CO2_MEU$r2 <- as.numeric(CO2_MEU$r2)
CO2_ROV$r2 <- as.numeric(CO2_ROV$r2)
CO2_SAC$r2 <- as.numeric(CO2_SAC$r2)



# CO2_MEU_05 <- filter(CO2_MEU,r2>=0.5)
# CO2_MEU_05 <- filter(CO2_MEU,r2>=0.5)
# CO2_ROV_05 <- filter(CO2_ROV,r2>=0.5)
# CO2_ROV_05 <- filter(CO2_ROV,r2>=0.5)

CO2_MEU_03 <- filter(CO2_MEU,r2>=0.3)
CO2_ROV_03 <- filter(CO2_ROV,r2>=0.3)
CO2_SAC_03 <- filter(CO2_SAC,r2>=0.3)

CO2_MEU_05 <- filter(CO2_MEU,r2>=0.5)
CO2_ROV_05 <- filter(CO2_ROV,r2>=0.5)
CO2_SAC_05 <- filter(CO2_SAC,r2>=0.5)

#---- Plot CO2 ---- 

plot_CO2 <- ggplot()+
  geom_line(data=CO2_MEU, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CO2_ROV, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CO2_SAC, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CO[2]))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CO2_MEU_03$Date), max(CO2_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CO2")) +
  ylab(bquote(mg/m^2/h))
plot_CO2
ggplotly(plot)

#---- Plot CO2 R² > 0.3---- 

plot_CO2_03 <- ggplot()+
  geom_line(data=CO2_MEU_03, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CO2_ROV_03, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CO2_SAC_03, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CO[2]*-R^2>0.3))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CO2_MEU_03$Date), max(CO2_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CO2")) +
  ylab(bquote(mg/m^2/h))
plot_CO2_03
ggplotly(plot)
 
#---- Plot CO2 R² > 0.5---- 

plot_CO2_05 <- ggplot()+
  geom_line(data=CO2_MEU_05, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CO2_ROV_05, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CO2_SAC_05, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CO[2]*-R^2>0.5))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CO2_MEU_03$Date), max(CO2_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CO2")) +
  ylab(bquote(mg/m^2/h))
plot_CO2_05
ggplotly(plot)

#---- CH4 ---- 
CH4_MEU <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CH4_MEU90_2022_2023-02-06_2024-02-06")
CH4_ROV <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CH4_ROV103_2022_2023-02-06_2024-02-06")
CH4_SAC <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CH4_SAC100_2022_2023-02-06_2024-02-06")

colnames(CH4_MEU) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CH4_MEU <- CH4_MEU[-c(1),]

# colnames(CH4_MEU2) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
# CH4_MEU2 <- CH4_MEU2[-c(1),]

colnames(CH4_ROV) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CH4_ROV <- CH4_ROV[-c(1),]

colnames(CH4_SAC) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CH4_SAC <- CH4_SAC[-c(1),]

CH4_SAC <- CH4_SAC[-c(1:149),]


CH4_MEU$Date <- as.POSIXct(strptime(CH4_MEU$Date,'%Y-%m-%d'))
CH4_ROV$Date <- as.POSIXct(strptime(CH4_ROV$Date,'%Y-%m-%d'))
CH4_SAC$Date <- as.POSIXct(strptime(CH4_SAC$Date,'%Y-%m-%d'))


CH4_MEU$Jghg <- as.numeric(CH4_MEU$Jghg)
CH4_ROV$Jghg <- as.numeric(CH4_ROV$Jghg)
CH4_SAC$Jghg <- as.numeric(CH4_SAC$Jghg)


CH4_MEU <- as.data.table(CH4_MEU)
CH4_MEU<- CH4_MEU[Type == "StationPixel"]
CH4_ROV <- as.data.table(CH4_ROV)
CH4_ROV<- CH4_ROV[Type == "StationPixel"]
CH4_SAC <- as.data.table(CH4_SAC)
CH4_SAC<- CH4_SAC[Type == "StationPixel"]


CH4_MEU$r2 <- as.numeric(CH4_MEU$r2)
CH4_ROV$r2 <- as.numeric(CH4_ROV$r2)
CH4_SAC$r2 <- as.numeric(CH4_SAC$r2)



# CH4_MEU_05 <- filter(CH4_MEU,r2>=0.5)
# CH4_MEU_05 <- filter(CH4_MEU,r2>=0.5)
# CH4_ROV_05 <- filter(CH4_ROV,r2>=0.5)
# CH4_ROV_05 <- filter(CH4_ROV,r2>=0.5)

CH4_MEU_03 <- filter(CH4_MEU,r2>=0.3)
CH4_ROV_03 <- filter(CH4_ROV,r2>=0.3)
CH4_SAC_03 <- filter(CH4_SAC,r2>=0.3)

CH4_MEU_05 <- filter(CH4_MEU,r2>=0.5)
CH4_ROV_05 <- filter(CH4_ROV,r2>=0.5)
CH4_SAC_05 <- filter(CH4_SAC,r2>=0.5)

#---- Plot CH4 ---- 

plot_CH4 <- ggplot()+
  geom_line(data=CH4_MEU, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CH4_ROV, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CH4_SAC, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CH[4]))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CH4_MEU_03$Date), max(CH4_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CH4")) +
  ylab(bquote(mg/m^2/h))
plot_CH4
ggplotly(plot)

#---- Plot CH4 R² > 0.3---- 

plot_CH4_03 <- ggplot()+
  geom_line(data=CH4_MEU_03, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CH4_ROV_03, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CH4_SAC_03, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CH[4]*-R^2>0.3))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CH4_MEU_03$Date), max(CH4_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CH4")) +
  ylab(bquote(mg/m^2/h))
plot_CH4_03
ggplotly(plot)

#---- Plot CH4 R² > 0.5---- 

plot_CH4_05 <- ggplot()+
  geom_line(data=CH4_MEU_05, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CH4_ROV_05, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CH4_SAC_05, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CH[4]*-R^2>0.5))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CH4_MEU_03$Date), max(CH4_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CH4")) +
  ylab(bquote(mg/m^2/h))
plot_CH4_05
ggplotly(plot)

#---- CO ---- 
CO_MEU <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CO_MEU90_2022_2023-02-06_2024-02-06")
CO_ROV <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CO_ROV103_2022_2023-02-05_2024-02-05")
CO_SAC <- read.table("E:/PAUL/Rn/Rn_extract/RTMFlux_CO_SAC100_2022_2023-04-30_2024-02-05")

colnames(CO_MEU) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CO_MEU <- CO_MEU[-c(1),]

# colnames(CO_MEU2) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
# CO_MEU2 <- CO_MEU2[-c(1),]

colnames(CO_ROV) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CO_ROV <- CO_ROV[-c(1),]

colnames(CO_SAC) <- c("Date","Slope","Jghg","r2","err","nb_pt","nb_hrs","risestop","rise","Jrn","Type")
CO_SAC <- CO_SAC[-c(1),]

CO_SAC <- CO_SAC[-c(1:149),]


CO_MEU$Date <- as.POSIXct(strptime(CO_MEU$Date,'%Y-%m-%d'))
CO_ROV$Date <- as.POSIXct(strptime(CO_ROV$Date,'%Y-%m-%d'))
CO_SAC$Date <- as.POSIXct(strptime(CO_SAC$Date,'%Y-%m-%d'))


CO_MEU$Jghg <- as.numeric(CO_MEU$Jghg)
CO_ROV$Jghg <- as.numeric(CO_ROV$Jghg)
CO_SAC$Jghg <- as.numeric(CO_SAC$Jghg)


CO_MEU <- as.data.table(CO_MEU)
CO_MEU<- CO_MEU[Type == "StationPixel"]
CO_ROV <- as.data.table(CO_ROV)
CO_ROV<- CO_ROV[Type == "StationPixel"]
CO_SAC <- as.data.table(CO_SAC)
CO_SAC<- CO_SAC[Type == "StationPixel"]


CO_MEU$r2 <- as.numeric(CO_MEU$r2)
CO_ROV$r2 <- as.numeric(CO_ROV$r2)
CO_SAC$r2 <- as.numeric(CO_SAC$r2)



# CO_MEU_05 <- filter(CO_MEU,r2>=0.5)
# CO_MEU_05 <- filter(CO_MEU,r2>=0.5)
# CO_ROV_05 <- filter(CO_ROV,r2>=0.5)
# CO_ROV_05 <- filter(CO_ROV,r2>=0.5)

CO_MEU_03 <- filter(CO_MEU,r2>=0.3)
CO_ROV_03 <- filter(CO_ROV,r2>=0.3)
CO_SAC_03 <- filter(CO_SAC,r2>=0.3)

CO_MEU_05 <- filter(CO_MEU,r2>=0.5)
CO_ROV_05 <- filter(CO_ROV,r2>=0.5)
CO_SAC_05 <- filter(CO_SAC,r2>=0.5)

#---- Plot CO ---- 

plot_CO <- ggplot()+
  geom_line(data=CO_MEU, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CO_ROV, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CO_SAC, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CO))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CO_MEU_03$Date), max(CO_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CO")) +
  ylab(bquote(mg/m^2/h))
plot_CO
ggplotly(plot)

#---- Plot CO R² > 0.3---- 

plot_CO_03 <- ggplot()+
  geom_line(data=CO_MEU_03, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CO_ROV_03, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CO_SAC_03, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CO*-R^2>0.3))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CO_MEU_03$Date), max(CO_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CO")) +
  ylab(bquote(mg/m^2/h))
plot_CO_03
ggplotly(plot)

#---- Plot CO R² > 0.5---- 

plot_CO_05 <- ggplot()+
  geom_line(data=CO_MEU_05, aes(x=Date, y = Jghg, col = "MEU")) + 
  geom_line(data=CO_ROV_05, aes(x=Date, y = Jghg, col = "ROV")) +
  geom_line(data=CO_SAC_05, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
  labs(title=expression('Flux '*CO*-R^2>0.5))+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title=element_text(hjust=0.5), 
        legend.title = element_blank(),
        legend.text = element_text(size = 15)) +
  scale_x_datetime(breaks= seq(min(CO_MEU_03$Date), max(CO_MEU_03$Date), length=12), 
                   date_labels="%b-%y") +
  # ylim(-2, 20) +
  # ggtitle(
  #   paste0(
  #     "Rn CO")) +
  ylab(bquote(mg/m^2/h))
plot_CO_05
ggplotly(plot)