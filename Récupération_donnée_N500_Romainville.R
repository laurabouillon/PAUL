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


data_all <- list.files(path = "C:/Users/lbouillo/Documents/PAUL/Romainville/N500/",  # Identify all CSV files
                       pattern = "*.csv", recursive = TRUE, full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 
data_all                                            # Print data to RStudio console


data <- data_all
colnames(data) [2] <- "DateTime"
colnames(data) [3] <- "NO"
colnames(data) [4] <- "NO2"
colnames(data) [6] <- "Calibration"

data$DateTime <- as.POSIXct(data$DateTime, format = '%m/%d/%Y %H:%M:%S', tz="UTC")

data$Calibration <- sub(pattern = "False", replacement = "0", data$Calibration)
data$Calibration <- sub(pattern = "True", replacement = "1", data$Calibration)

data$Calibration <- as.numeric(data$Calibration)
write.table(data,"C:/Users/lbouillo/Documents/PAUL/Romainville/N500/NOx_01-03.csv", sep=",", row.names = FALSE)

data_nocal <- data[data$Calibration != 1,]
data_cal <- data[data$Calibration !=0,]

data_nocal <- data_nocal[,-c(1,5, 7, 8)]
colnames(data_nocal)[1] <- "date"
data_hour <- timeAverage(data_nocal, "hour")
write.table(data_nocal,"C:/Users/lbouillo/Documents/PAUL/Meudon/N500/NOx_nocal_01-03.csv", sep=",", row.names = FALSE)

data_cal <- data_cal[,-c(1,5, 7:13)]
colnames(data_cal)[1] <- "date"
write.table(data_cal,"C:/Users/lbouillo/Documents/PAUL/Meudon/N500/NOx_cal_01-03.csv", sep=",", row.names = FALSE)


plot_NO <- ggplot()+
  geom_line(data = data_cal, mapping = aes(x = date, y = NO))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.text.x =  element_text(colour = "black", size = 15),
        axis.title.y = element_text(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.ticks.y = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "lightgrey" , linetype =, size = 0.5),
        panel.grid.minor.x = element_line(colour= "lightgrey" , linetype = "dashed", size = 0.25),
        panel.grid.major.y = element_line(colour = "lightgrey" , linetype =, size = 0.5),
        panel.grid.minor.y = element_line(colour= "lightgrey" , linetype = "dashed", size = 0.25))+
  xlab("Date")+
  ylab("Concentration NO (µg.m-3)")
plot_NO


plot <- ggplot()+
  geom_line(data = data_cal,
            mapping = aes(x = Datetime, y = NO))+
  geom_line(data = data_cal,
            mapping = aes(x = Datetime, y = NO2))+
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
plot



plot_NO_hour <- ggplot()+
  geom_line(data = data_hour, mapping = aes(x = date, y = NO))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.text.x =  element_text(colour = "black", size = 15),
        axis.title.y = element_text(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.ticks.y = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "lightgrey" , linetype =, size = 0.5),
        panel.grid.minor.x = element_line(colour= "lightgrey" , linetype = "dashed", size = 0.25),
        panel.grid.major.y = element_line(colour = "lightgrey" , linetype =, size = 0.5),
        panel.grid.minor.y = element_line(colour= "lightgrey" , linetype = "dashed", size = 0.25))+
  xlab("Date")+
  ylab("Concentration NO (µg.m-3)")
plot_NO

plot_NO2_hour <- ggplot()+
  geom_line(data = data_hour, mapping = aes(x = date, y = NO2))+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x = element_line(colour = "black"),
        axis.text.x =  element_text(colour = "black", size = 15),
        axis.title.y = element_text(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.text.y = element_text(colour = "black", size = 15),
        axis.ticks.y = element_line(colour = "black"),
        panel.grid.major.x = element_line(colour = "lightgrey" , linetype =, size = 0.5),
        panel.grid.minor.x = element_line(colour= "lightgrey" , linetype = "dashed", size = 0.25),
        panel.grid.major.y = element_line(colour = "lightgrey" , linetype =, size = 0.5),
        panel.grid.minor.y = element_line(colour= "lightgrey" , linetype = "dashed", size = 0.25))+
  xlab("Date")+
  ylab("Concentration NO2 (µg.m-3)")
plot_NO2
