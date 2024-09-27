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
O3 <- read_excel("C:/Users/lbouillo/Documents/SACLAY/Donnees_O3/Copie de SIRTA_validated_NOx_O3_2013-2017.xlsx")
O3 <- O3[,-c(2:4)]
colnames(O3) [1] <- "date"
data <- list.files(path = "C:/Users/lbouillo/Documents/SACLAY/Donnees_O3/Donnee_FileZilla/",  # Identify all CSV files
                       pattern = "gasesz5_0a_Lz5Po3F1min_v01*", recursive = TRUE, full.names = TRUE) %>% 
  lapply(read.table, sep = ",") %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 

   # Supprimer ligne en fonction condition spécifique
colnames(data) <- c("DateTime","O3","PHMEAS_INST","PHREF_INST")

data <- data[,-c(3:4)]
colnames(data) [1] <- "date"
colnames(O3) [1] <- "date"
data$date <- as.POSIXct(data$date, format = '%m/%d/%Y %H:%M:%S', tz="UTC")
data$O3 <- as.POSIXct(data$O3, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
data$O3 <- as.numeric(data$O3)
data_hour <- timeAverage(data, "hour")

O3 <- O3[-c(26282:43824),]

# data <- data[-c(1161211, 1161212, 1161213, 1161216, 1487795),]
All <- rbind (O3, data_hour)


# All$O3 <- as.numeric(All$O3)


plot_O3 <- ggplot(data=All, aes(x=date, y = O3)) + 
  geom_line()
plot_O3

plot_O3 <- ggplot(data=data, aes(x=date, y = O3)) + 
  geom_line()
plot_O3

All <- All[-c(78890:84505),]

write.table(All,"E:/SACLAY/DATA_FINAL/CAE/O3_2012_2022_HA.csv", sep=",", row.names = FALSE)

plot_O3 <- ggplot() + 
  geom_line(data=All_hour, aes(x=date, y = O3)) + 
  # geom_line(data=CO_ROV_Clim_03, aes(x=Date, y = Jghg, col = "ROV")) +
  # geom_line(data=CO_SAC_Clim_03, aes(x=Date, y = Jghg, col = "SAC")) +
  theme_bw() +
labs(title=expression('Saclay '*O[3]))+
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 12),
        plot.title=element_text(hjust=0.5),
        legend.position = "none")+
  ylab(bquote(ppb))
plot_O3

All_V2 <- read.csv("E:/SACLAY/DATA_FINAL/All_meteo_2012_2022_HA.csv")
All_V2$DateTime <- as.POSIXct(All_V2$DateTime, format = '%Y-%m-%d %H:%M:%S', tz="UTC")
colnames(All_hour)[1] <- "DateTime"


All_all <- merge (
  x = All_V2,
  y = All_hour,
  by =c("DateTime"), 
  all = TRUE)

write.table(All_all,"E:/SACLAY/DATA_FINAL/All_2012_2022_HA_V2.csv", sep=",", row.names = FALSE)
