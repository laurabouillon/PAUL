rm(list = ls()) #to delete all variables

#les differentes librairies 
library("ggplot2")
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
library("raster")
library('plyr')
library('tidyr')
library("colorspace")


data_BC <- list.files(path = "C:/Users/lbouillo/Documents/PAUL/Meudon/AE33-S10-01359/",  # Identify all CSV files
                       pattern = "AE33_AE33*", recursive = TRUE, full.names = TRUE) 

#Bouble pour tout réunir dans un tableau
i <- data_BC[1]
test <- read.table(i, skip=6, sep = "")

for (i in data_BC[-1]) {
  test2 <- read.table(i, skip=6, sep = "")
  test <- rbind(test, test2)
  print(i)
}

colnames(test) <- c("Date", "Time", "Timebase", "RefCh1", "Sen1Ch1", "Sen2Ch1", "RefCh2", "Sen1Ch2", "Sen2Ch2", "RefCh3", "Sen1Ch3", "Sen2Ch3", "RefCh4", "Sen1Ch4", "Sen2Ch4", "RefCh5", "Sen1Ch5", "Sen2Ch5", "RefCh6", "Sen1Ch6", "Sen2Ch6", "RefCh7", "Sen1Ch7", "Sen2Ch7", "Flow1", "Flow2", "FlowC", "Pressure(Pa)", "Temperature(°C)", "BB(%)", "ContTemp", "SupplyTemp", "Status", "ContStatus", "DetectStatus", "LedStatus", "ValveStatus", "LedTemp", "BC11", "BC12", "BC1", "BC21", "BC22", "BC2", "BC31", "BC32", "BC3", "BC41", "BC42", "BC4", "BC51", "BC52", "BC5", "BC61", "BC62", "BC6", "BC71", "BC72", "BC7", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "TapeAdvCount")

data <- test

data$DateTime <-paste(data$Date,data$Time,sep=" ")

col_order <- c("Date", "Time","DateTime", "Timebase", "RefCh1", "Sen1Ch1", "Sen2Ch1", "RefCh2", "Sen1Ch2", "Sen2Ch2", "RefCh3", "Sen1Ch3", "Sen2Ch3", "RefCh4", "Sen1Ch4", "Sen2Ch4", "RefCh5", "Sen1Ch5", "Sen2Ch5", "RefCh6", "Sen1Ch6", "Sen2Ch6", "RefCh7", "Sen1Ch7", "Sen2Ch7", "Flow1", "Flow2", "FlowC", "Pressure(Pa)", "Temperature(°C)", "BB(%)", "ContTemp", "SupplyTemp", "Status", "ContStatus", "DetectStatus", "LedStatus", "ValveStatus", "LedTemp", "BC11", "BC12", "BC1", "BC21", "BC22", "BC2", "BC31", "BC32", "BC3", "BC41", "BC42", "BC4", "BC51", "BC52", "BC5", "BC61", "BC62", "BC6", "BC71", "BC72", "BC7", "K1", "K2", "K3", "K4", "K5", "K6", "K7", "TapeAdvCount")
data <- data[, col_order]

data2 <- data

data2$DateTime <- as.POSIXct(data2$DateTime, format = '%Y/%m/%d %H:%M:%S', tz="UTC")

data2$BC1 <- as.numeric(data2$BC1)
data2$BC2 <- as.numeric(data2$BC2)
data2$BC3 <- as.numeric(data2$BC3)
data2$BC4 <- as.numeric(data2$BC4)
data2$BC5 <- as.numeric(data2$BC5)
data2$BC6 <- as.numeric(data2$BC6)
data2$BC7 <- as.numeric(data2$BC7)

write.csv(data2, "C:/Users/lbouillo/Documents/PAUL/Meudon/data_BC.csv") 

plot_BC <-  ggplot()+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC1, color = "BC1"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC2, color = "BC2"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC3, color = "BC3"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC4, color = "BC4"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC5, color = "BC5"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC6, color = "BC6"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC7, color = "BC7"))+
  ylab("BC ng/m3")+
  ggtitle("BC Meudon")

plot_BC
firstDate <- as.POSIXct("2023-01-13", tz = "UTC")   # First date for the data extraction
lastDate  <- as.POSIXct("2023-12-25", tz = "UTC")   # Last date of the data extraction

time_labels_breaks <- seq(firstDate, lastDate, "1 day") 
time_labels_breaks <- as.POSIXct(time_labels_breaks, tz="UTC")

plot_BC <-  ggplot()+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC1, color = "BC1"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC2, color = "BC2"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC3, color = "BC3"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC4, color = "BC4"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC5, color = "BC5"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC6, color = "BC6"))+
  geom_line(data = data2, mapping = aes(x = DateTime, y = BC7, color = "BC7"))+
  #ylim(-750, 3500)+
  scale_x_datetime (breaks = time_labels_breaks ,
                    labels = strftime(time_labels_breaks, "%d", tz = "UTC"))+ 
  ylab("BC ng/m3")+
  theme_bw() +
  ggtitle("BC Meudon")

plot_BC

