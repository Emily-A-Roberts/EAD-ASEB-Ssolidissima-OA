# SONDE data Ptown and Eel Pond
# Note - I had to resave the .csv files on my computer in order to get R to read them. 

# Next, it would be nice to grab the Eel Pond data so I can plot both up... 
# Alternatively I can make two plots, one of Eel Pond - sensors at site vs. ocean sensor
# I noticed there is a month of data missing. Maybe this can be found.
# I also noticed that chl is currently being recorded as rfu not ug/L. This is the recommended value to report, but 
# I'm not sure the correct value from the calibrations are being calculated. 
# I should be able to get the ug/L or corrected RFU value... 

# Packages required:

library(ggplot2)
library(data.table)
library(plotly)
library(tibbletime)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(ggpubr)
#library(stat)
library(scales)
library(vegan)
library(pastecs)
source('~/Documents/Software/R/MultivariateStats/functions/biostats.R')
library(tibble)
library(tidyverse)
library(PerformanceAnalytics)

#dplyr, plyr, and ddply are conflicting...
conflicts()

theme_set(theme_minimal())
theme_update(panel.grid.minor.y = element_blank(), 
             panel.grid.major.y = element_line(colour = "grey91"),
             panel.grid.minor.x = element_line(colour = "grey81"), 
             panel.grid.major.x = element_line(colour = "grey61"),
             axis.line = element_blank())



#====#
# To upload ####
# from file on computer
#====#
# Eel Pond
setwd("~/Documents/School and jobs/Milford/Surfclam OA/Environmental conditions/Eel Pond YSI")
dir()
df.Eel <- data.table::fread(file = "WQBMHWQ.csv",header = T, sep = ",",fill=TRUE, skip = 2, stringsAsFactors = FALSE)
df.Eel <- as.data.frame(df.Eel)
df.Eel$datetime <- as.POSIXct(strptime(df.Eel$DateTimeStamp, format = "%m/%d/%Y %H:%M"))
str(df.Eel)
h <- head(df.Eel,60)
t <- tail(df.Eel,60)
str(h)
plot(h$datetime,h$Sal)
plot(t$datetime,t$Sal)
plot(h$datetime,h$Depth)
plot(t$datetime,t$Depth)
plot(df.Eel$datetime,df.Eel$Depth)
plot(df.Eel$datetime,df.Eel$Sal, type = "b") # It looks like there are some bad datapoints in the salinity. 
                                                       # For now cutting out salinities below 25. 
                                                       # Also, what is going on with salinities in 2023 - it seems a bit low. They may need to clean the sensor???
df.Eel <- df.Eel[df.Eel$Sal>25,]
df.Eel <- df.Eel[df.Eel$Depth>.5,]

str(df.Eel)

df.Eel.hr <- df.Eel %>% 
  mutate(datetime = ceiling_date(datetime, unit = "hour")) %>% 
  group_by(datetime) %>% 
  summarise_all(list(~mean(., na.rm = TRUE)))

# plot(df.Eel$datetime,df.Eel$DO_pct)
# plot(df.Eel.hr$datetime,df.Eel.hr$DO_pct)


# Ptown ####
setwd("~/Documents/School and jobs/Milford/Surfclam OA/Environmental conditions/KORHH Data Files/YSI data")
dir()


# 10/16/18 - 11/15/18
#"Kor Measurement File Export - 012423 154927.csv",header = T, sep = ",", skip = 8, stringsAsFactors = FALSE)


# 6/15/22 - 08/30/22
df.Ptown <- data.table::fread(file = "Kor Measurement File Export - 012423 154508.csv",header = T, sep = ",", skip = 8, stringsAsFactors = FALSE)
df.Ptown<- as.data.frame(df.Ptown)
str(df.Ptown)
names(df.Ptown) <- c("Date","Time","Sec","Site","ChlRFU","Cond_uS/cm","Depth","nlF_Cond","DO_pct","DO_%CB",
                     "DO_mgl","Pressure_psi","Sal","SpCond","TAL PE RFU","TDS_mg/L","Turb",
                     "TSS_mg/L","Wiper Position_V","pH","pH_mV","Temp","Vert_Pos","Battery_V","Cable Pwr")
df.Ptown$datetime <- paste(df.Ptown[,"Date"],df.Ptown[,"Time"], sep = " ")
df.Ptown$datetime <- as.POSIXct(strptime(df.Ptown$datetime, format = "%m/%d/%y %H:%M:%S"))
h <- head(df.Ptown,60)
t <- tail(df.Ptown,60)
str(h)
plot(h$datetime,h$Sal)
plot(t$datetime,t$Sal)
plot(h$datetime,h$Depth)
plot(t$datetime,t$Depth)
plot(df.Ptown$datetime,df.Ptown$Depth)
plot(df.Ptown$datetime,df.Ptown$Sal)
df.Ptown <- df.Ptown[df.Ptown$Sal>5,]
df.Ptown <- df.Ptown[df.Ptown$Depth>.3,]
dat_1 <- df.Ptown

# 8/31/22 - 09/25/22
df.Ptown <- data.table::fread(file = "Kor Measurement File Export - 012423 154425.csv",header = T, sep = ",", skip = 8, stringsAsFactors = FALSE)
df.Ptown<- as.data.frame(df.Ptown)
names(df.Ptown) <- c("Date","Time","Sec","Site","ChlRFU","Cond_uS/cm","Depth","nlF_Cond","DO_pct","DO_%CB",
                     "DO_mgl","Pressure_psi","Sal","SpCond","TAL PE RFU","TDS_mg/L","Turb",
                     "TSS_mg/L","Wiper Position_V","pH","pH_mV","Temp","Vert_Pos","Battery_V","Cable Pwr")
df.Ptown$datetime <- paste(df.Ptown[,"Date"],df.Ptown[,"Time"], sep = " ")
df.Ptown$datetime <- as.POSIXct(strptime(df.Ptown$datetime, format = "%m/%d/%y %H:%M:%S"))
h <- head(df.Ptown,60)
t <- tail(df.Ptown,60)
plot(h$datetime,h$Sal)
plot(t$datetime,t$Sal)
plot(h$datetime,h$Depth)
plot(t$datetime,t$Depth)
plot(df.Ptown$datetime,df.Ptown$Depth)
plot(df.Ptown$datetime,df.Ptown$Sal)
# df.Ptown <- df.Ptown[df.Ptown$Sal>5,] Not needed
# df.Ptown <- df.Ptown[df.Ptown$Depth>.3,] Not needed
dat_2 <- df.Ptown

# 10/11/22 - 11/7/22
df.Ptown <- data.table::fread(file = "Kor Measurement File Export - 012423 154319.csv",header = T, sep = ",", skip = 8, stringsAsFactors = FALSE)
df.Ptown<- as.data.frame(df.Ptown)
names(df.Ptown) <- c("Date","Time","Sec","Site","ChlRFU","Cond_uS/cm","Depth","nlF_Cond","DO_pct","DO_%CB",
                     "DO_mgl","Pressure_psi","Sal","SpCond","TAL PE RFU","TDS_mg/L","Turb",
                     "TSS_mg/L","Wiper Position_V","pH","pH_mV","Temp","Vert_Pos","Battery_V","Cable Pwr")
df.Ptown$datetime <- paste(df.Ptown[,"Date"],df.Ptown[,"Time"], sep = " ")
df.Ptown$datetime <- as.POSIXct(strptime(df.Ptown$datetime, format = "%m/%d/%y %H:%M:%S"))
h <- head(df.Ptown,60)
t <- tail(df.Ptown,60)
plot(h$datetime,h$Sal)
plot(t$datetime,t$Sal)
plot(h$datetime,h$Depth)
plot(t$datetime,t$Depth)
plot(df.Ptown$datetime,df.Ptown$Depth)
plot(df.Ptown$datetime,df.Ptown$Sal)
# df.Ptown <- df.Ptown[df.Ptown$Sal>5,] Not needed
df.Ptown <- df.Ptown[df.Ptown$Depth>.3,] 
dat_3 <- df.Ptown

# 11/7/22 - 1/21/22
df.Ptown <- data.table::fread(file = "Kor Measurement File Export - 012423 154229.csv",header = T, sep = ",", skip = 8, stringsAsFactors = FALSE)
df.Ptown<- as.data.frame(df.Ptown)
names(df.Ptown) <- c("Date","Time","Sec","Site","ChlRFU","Cond_uS/cm","Depth","nlF_Cond","DO_pct","DO_%CB",
                     "DO_mgl","Pressure_psi","Sal","SpCond","TAL PE RFU","TDS_mg/L","Turb",
                     "TSS_mg/L","Wiper Position_V","pH","pH_mV","Temp","Vert_Pos","Battery_V","Cable Pwr")
df.Ptown$datetime <- paste(df.Ptown[,"Date"],df.Ptown[,"Time"], sep = " ")
df.Ptown$datetime <- as.POSIXct(strptime(df.Ptown$datetime, format = "%m/%d/%y %H:%M:%S"))
h <- head(df.Ptown,60)
t <- tail(df.Ptown,60)
plot(h$datetime,h$Sal)
plot(t$datetime,t$Sal)
plot(h$datetime,h$Depth)
plot(t$datetime,t$Depth)
plot(df.Ptown$datetime,df.Ptown$Depth)
plot(df.Ptown$datetime,df.Ptown$Sal)
# df.Ptown <- df.Ptown[df.Ptown$Sal>5,] Not needed
df.Ptown <- df.Ptown[df.Ptown$Depth>.3,] 
dat_4 <- df.Ptown

# Combine the different series of datasets
df.Ptown <- rbind(dat_1, dat_2, dat_3, dat_4)
str(df.Ptown)


#df.data_7m <- read.csv(file = "Kor Measurement File Export - 012423 154319.csv", header = T, sep = ",", skip = 0, stringsAsFactors = FALSE)
df.all <- merge(x = df.Ptown,y = df.Eel.hr, by.x = "datetime", by.y = "datetime", suffixes = c("_P","_E"), all = TRUE)
#df.all<-df.all[df.all$TIMESTAMP <= as.POSIXct("2019-03-19 01:00:00"),] 
str(df.all)

head(df.all)
tail(df.all)

#====#
# Structure ####
#the data
#====#

#====#
#preliminary plots ####
#====#
plot(df.all$datetime, df.all$Temp, ylab = "Temp C", xlab = "Date")
plot(df.all$datetime, df.all$Sal, ylim = c(28,35), ylab = "Sal", xlab = "Date")
plot(df.all$datetime, df.all$ChlRFU,ylim = c(0,4), ylab = "Chl_RFU", xlab = "Date")
plot(df.all$datetime, df.all$DO_pct, ylab = "DO", xlab = "Date")
plot(df.all$datetime, df.all$pH, ylab = "pH", xlab = "Date")
plot(df.all$datetime, df.all$Turb, ylab = "TDS", xlab = "Date")
plot(df.all$datetime, df.all$Battery_V, ylab = "Battery", xlab = "Date")
df.all$phytocalc <- df.all$ChlRFU / (df.all$Turb+1)
plot(df.all$datetime, df.all$phytocalc, ylab = "Fluor/Turb", xlab = "Date")


# Temp plots
data_long <- gather(df.all, condition, measurement, c("Temp_P","Temp_E"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
  ylab(expression(paste("Hourly SONDE Temperature (",degree,"C)")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))

# Sal plots
data_long <- gather(df.all, condition, measurement, c("Sal_P","Sal_E"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
  ylab(expression(paste("SONDE Salinity (psu)")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))


# Oxygen plots
data_long <- gather(df.all, condition, measurement, c("DO_pct_P","DO_pct_E"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
  ylab(expression(paste("SONDE Oxygen % sat")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))

str(df.all)


str(df.all)

# pH plots
data_long <- gather(df.all, condition, measurement, c("pH_P","pH_E"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
  ylab(expression(paste("SONDE pH")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))

# Chl plots
data_long <- gather(df.all, condition, measurement, c("ChlRFU","ChlFluor"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
  ylab(expression(paste("SONDE Fluorescence RFU ")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))

# Chl plots
data_long <- gather(df.all, condition, measurement, c("Turb_P","Turb_E"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
  ylab(expression(paste("SONDE Turbidity")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))

# remove repeating values
# i <- 38045
# df.all$rep_1m <- rep(x = 0, length.out=nrow(df.all))
# for(i in 1:(length(df.all$TempC_1m)-2)){
#   if(!is.na(df.all$TempC_1m[i])&!is.na(df.all$TempC_1m[i+1])&!is.na(df.all$TempC_1m[i+2])){
#     if((df.all$TempC_1m[i+1]==df.all$TempC_1m[i])&(df.all$TempC_1m[i+2]==df.all$TempC_1m[i])){
#       df.all$rep_1m[i] <- 1
#     }
#   }
# }
# sum(df.all$rep_1m)


# df.all$rep_7m <- rep(x = 0, length.out=nrow(df.all))
# for(i in 1:(length(df.all$TempC_7m)-2)){
#   if(!is.na(df.all$TempC_7m[i])&!is.na(df.all$TempC_7m[i+1])&!is.na(df.all$TempC_7m[i+2])){
#     if((df.all$TempC_7m[i+1]==df.all$TempC_7m[i])&(df.all$TempC_7m[i+2]==df.all$TempC_7m[i])){
#       df.all$rep_7m[i] <- 1
#     }
#   }
# }
# sum(df.all$rep_7m)
# 
# df.all$TempC_1m[df.all$rep_1m==1] <- NA
# df.all$TempC_7m[df.all$rep_7m==1] <- NA
# df.all$Sal_1m[df.all$rep_1m==1] <- NA
# df.all$Sal_7m[df.all$rep_7m==1] <- NA
# df.all$pH_1m[df.all$rep_1m==1] <- NA
# df.all$pH_7m[df.all$rep_7m==1] <- NA
# df.all$DOmgl_1m[df.all$rep_1m==1] <- NA
# df.all$DOmgl_7m[df.all$rep_7m==1] <- NA
# df.all$Chlug_1m[df.all$rep_1m==1] <- NA
# df.all$Chlug_7m[df.all$rep_7m==1] <- NA

df.all$TempC_Ptown <- df.all$Temp_P
df.all$TempC_Eel <- df.all$Temp_E

data_long <- gather(df.all, condition, measurement, c("TempC_Ptown","TempC_Eel"), factor_key = TRUE)

head(data_long)
# ggplot(data_long, aes(x = datetime, y = measurement)) +
#   geom_point(aes(color = condition), alpha = 0.01) +
#   #geom_line(aes(color = condition), size = .5) +
#   scale_color_manual(values = c("#00757d","#E7B800")) +
#   ylab("Temperature")+
#   theme_minimal()

max.m <- max(data_long$measurement, na.rm = TRUE)
min.m <- min(data_long$measurement, na.rm = TRUE)

# max.m_upper_top <- min.m - 0.01*(max.m-min.m)
# max.m_upper <- min.m - 0.025*(max.m-min.m)
# max.m_lower <- min.m - 0.04*(max.m-min.m)
max.m_upper_top <- max.m + 0.07*(max.m-min.m)
max.m_upper <- max.m + 0.04*(max.m-min.m)
max.m_lower <- max.m + 0.01*(max.m-min.m)
data_long$presence[data_long$measurement>0 & data_long$condition=="TempC_Eel"] <- max.m_upper
data_long$presence[data_long$measurement>0 & data_long$condition=="TempC_Ptown"] <- max.m_lower

df.2 <- data.frame(
  condition = data_long$condition,
  date = data_long$datetime,
  measurement = data_long$presence
)
df.3 <- data.frame(
  date = as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"),
  measurement = max.m_upper_top*c(1,1)
)

data_long

temp_graph <- ggplot(data_long, aes(x = datetime, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.05, size = 0.5, show.legend = FALSE) +
  #geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#00757d","#E7B800")) +
  ylab(expression(paste("Temperature (",degree,"C)")))+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))
#+
#  geom_point(data = df.2, aes(color = condition), 
#             size = 0.5,  show.legend = FALSE, shape = 15)
#+
 # geom_line(data = df.3, color = "purple", 
 #           size = 1,  show.legend = FALSE)
temp_graph
str(df.2)



# temp_graph <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   geom_point(aes(color = condition), alpha = 0.05, size = 0.5, show.legend = FALSE) +
#   #geom_line(aes(color = condition), size = .5) +
#   scale_color_manual(values = c("#00757d","#E7B800")) +
#   ylab("Temperature (deg C)")+
#   xlim(as.POSIXct(c('2014-06-01','2019-04-01')))
# temp_graph


# Temp monthly means ====

str(df.all)


all_daily <- df.all %>% 
  mutate(year = year(datetime), month = month(datetime), day = day(datetime)) %>%
  group_by(date = as.Date(paste(year, month,day,01, sep="-"))) %>%
  summarise_all(list(~mean(., na.rm = TRUE)))

data_long <- gather(df.all, condition, measurement, c("Temp_P","Temp_E"), factor_key = TRUE)
#data_long <- gather(all_daily, condition, measurement, c("Temp_P","Temp_E"), factor_key = TRUE)
ggplot(data_long, aes(x = datetime, y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  theme_minimal()+
 ylab(expression(paste("Hourly SONDE Temperature (",degree,"C)")))+
  xlab("Date")+
  xlim(as.POSIXct(c("5/15/22","1/21/23"), format = "%m/%d/%y"))

all_monthly <- df.all %>% 
  mutate(year = year(datetime), month = month(datetime)) %>%
  group_by(date = as.Date(paste(year, month,01, sep="-"))) %>%
  summarise_all(list(~mean(., na.rm = TRUE),~max(., na.rm = TRUE),~min(., na.rm = TRUE)))

str(all_monthly)
# all_monthly$TempC_Ptown_max[all_monthly$TempC_7m_max<=0] <- NA
# all_monthly$TempC_Ptown_min[all_monthly$TempC_7m_min>=20] <- NA

data_long <- gather(all_monthly, condition, measurement, 
                    c("TempC_Ptown_mean","TempC_Ptown_max","TempC_Ptown_min",
                      "TempC_Eel_mean","TempC_Eel_max","TempC_Eel_min"), factor_key = TRUE)

ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), size = 0.5, alpha = 0.2) +
  geom_line(aes(color = condition, linetype = condition),  size = .5) +
  scale_color_manual(values = c("#00757d","#00757d","#00757d", "#E7B800", "#E7B800", "#E7B800")) +
  scale_linetype_manual(values = c("solid","dotted","dotted","solid","dotted","dotted"))+
  theme_minimal()


#I can't figure out how to have a band around the average... I could maybe overlay some plots??? 

#=====#
# Salinity ####

str(all_monthly)


df.all$Sal_1m[df.all$Sal_1m>=32|df.all$Sal_1m<=2] <- NA
df.all$Sal_7m[df.all$Sal_7m>=32|df.all$Sal_7m<=2] <- NA

df.all$Sal_7m[df.all$date >= as.POSIXct("2015-02-04 01:00:00") & df.all$date <= as.POSIXct("2015-02-06 01:00:00")] <- NA
df.all$Sal_7m[df.all$date >= as.POSIXct("2015-02-23 01:00:00") & df.all$date <= as.POSIXct("2015-02-25 01:00:00")] <- NA
df.all$Sal_1m[df.all$date >= as.POSIXct("2015-06-08 01:00:00") & df.all$date <= as.POSIXct("2015-06-10 01:00:00")] <- NA # From previous list
df.all$Sal_1m[df.all$date >= as.POSIXct("2015-06-09 01:00:00") & df.all$date <= as.POSIXct("2015-07-06 01:00:00")] <- NA # From previous list

# 

df.all$Sal_1m[df.all$rep_1m==1] <- NA
df.all$Sal_7m[df.all$rep_7m==1] <- NA
df.all$Sal_7m[df.all$Sal_7m<=df.all$Sal_1m-1] <- NA


# missing saliniy data ####
df.all$Sal_7m[df.all$date >= as.POSIXct("2017-01-15 01:00:00") & df.all$date <= as.POSIXct("2017-06-01 01:00:00")] <- NA
subset.df.1 <- df.all[df.all$date >= as.POSIXct("2017-01-15 01:00:00") & df.all$date <= as.POSIXct("2017-06-01 01:00:00"),] 
subset.df.1$Sal_7m  <- NA #Redundant but OK incase other code gets changed. 
subset.df.1$Sal_7m_missing <- subset.df.1$Sal_1m + 5.2
subset.df.1$Sal_7m_missing[subset.df.1$Sal_7m_missing>30] <- 30


df.all$Sal_7m[df.all$date >= as.POSIXct("2015-04-01 01:00:00") & df.all$date <= as.POSIXct("2015-06-10 01:00:00")] <- NA # Bad salinity readings, cleaned at end of period
subset.df.2 <- df.all[df.all$date >= as.POSIXct("2015-04-01 01:00:00") & df.all$date <= as.POSIXct("2015-06-10 01:00:00"),] 
subset.df.2$Sal_7m  <- NA #Redundant but OK incase other code gets changed. 
subset.df.2$Sal_7m_missing <- subset.df.2$Sal_1m + 5.2
subset.df.2$Sal_7m_missing[subset.df.2$Sal_7m_missing>30] <- 30

subset.df <- rbind(subset.df.1, subset.df.2)

subset.df$TIMESTAMP

library(tidyr)
library(tidyverse)

subset.df <- subset.df[!is.na(subset.df$TIMESTAMP),]
subset.df.daily <- subset.df %>% 
  mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
  mutate(dategroup = paste(year, month, day, sep="-")) %>%
  group_by(dategroup) %>%
  dplyr::summarize(m = mean(Sal_7m_missing, na.rm = TRUE))

str(subset.df.daily)


data_long <- gather(df.all, condition, measurement, c("Sal_1m","Sal_7m"), factor_key = TRUE)

max.m <- max(data_long$measurement, na.rm = TRUE)
min.m <- min(data_long$measurement, na.rm = TRUE)

max.m_upper_top <- max.m + 0.07*(max.m-min.m)
max.m_upper <- max.m + 0.04*(max.m-min.m)
max.m_lower <- max.m + 0.01*(max.m-min.m)
data_long$presence[data_long$measurement>0&data_long$condition=="Sal_1m"] <- max.m_upper
data_long$presence[data_long$measurement>0&data_long$condition=="Sal_7m"] <- max.m_lower
df.2 <- data.frame(
  condition = data_long$condition,
  date = data_long$date,
  measurement = data_long$presence
)
df.3 <- data.frame(
  date = as.POSIXct(c("5/26/16","7/14/18"), format = "%m/%d/%y"), 
  measurement = max.m_upper_top*c(1,1)
)

df.4 <- data.frame(
  date = as.POSIXct(subset.df.daily$dategroup, format = "%Y-%m-%d"), 
  measurement = subset.df.daily$m)

df.5 <- data.frame(
  date = as.POSIXct(subset.df.daily$dategroup, format = "%Y-%m-%d"),
  measurement = max.m_lower*rep(1, length.out = length(subset.df.daily$dategroup))
)
df.5 <- df.5[!is.na(subset.df.daily$m),]

str(subset.df.daily)

plot(df.4$date, df.4$measurement)

sal <- ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.05, size = 0.5, show.legend = FALSE) +
  #geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylab("Salinity (psu)")+
  geom_point(data = df.2, aes(color = condition), 
             size = 0.5,  show.legend = FALSE, shape = 15)+
  geom_point(data = df.4, color = "#8fc0a9",
             size = .5,  show.legend = FALSE, alpha = 1)+
  geom_line(data = df.3, color = "purple", 
            size = 1,  show.legend = FALSE)+
  geom_point(data = df.5, color = "#8fc0a9", 
             size = .5,  show.legend = FALSE, shape = 15)+
  xlim(as.POSIXct(c('2014-06-01','2019-04-01')))
sal
dev.off()

# Salinity-temp relationships ====
plot(df.all$Sal_1m,df.all$Sal_7m, cex = .2, col = 10*(df.all$Sal_1m/df.all$Sal_7m))

plot(df.all$TempC_1m, df.all$Sal_1m, col = 10*(df.all$Sal_1m/df.all$Sal_7m))
plot(df.all$TempC_7m, df.all$Sal_7m, col = 10*(df.all$Sal_1m/df.all$Sal_7m))

plot(df.all$TempC_1m,df.all$TempC_7m, cex = .2, col = 2*(df.all$TempC_1m/df.all$TempC_7m))
plot(df.all$TempC_1m, df.all$Sal_1m, col = 3*(df.all$TempC_1m/df.all$TempC_7m))
plot(df.all$TempC_7m, df.all$Sal_7m, col = 3*(df.all$TempC_1m/df.all$TempC_7m))

all_monthly <- df.all %>% 
  mutate(year = year(TIMESTAMP), month = month(TIMESTAMP)) %>%
  group_by(date = as.Date(paste(year, month,01, sep="-"))) %>%
  summarise_all(list(~mean(., na.rm = TRUE)))

# Mean salinity plots ====

data_long <- gather(all_monthly, condition, measurement, c("Sal_7m","Sal_1m"), factor_key = TRUE)

ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#00757d","#E7B800")) +
  theme_minimal()

all_monthly <- df.all %>% 
  mutate(year = year(TIMESTAMP), month = month(TIMESTAMP)) %>%
  group_by(date = as.Date(paste(year, month,01, sep="-"))) %>%
  summarise_all(list(~mean(., na.rm = TRUE),~max(., na.rm = TRUE),~min(., na.rm = TRUE)))

all_monthly$Sal_7m_max[all_monthly$Sal_7m_max<=0] <- NA
all_monthly$Sal_7m_min[all_monthly$Sal_7m_min>=40] <- NA

data_long <- gather(all_monthly, condition, measurement, 
                    c("Sal_7m_mean","Sal_7m_max","Sal_7m_min",
                      "Sal_1m_mean","Sal_1m_max","Sal_1m_min"), factor_key = TRUE)

ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), size = 0.5, alpha = 0.2) +
  geom_line(aes(color = condition, linetype = condition),  size = .5) +
  scale_color_manual(values = c("#00757d","#00757d","#00757d", "#E7B800", "#E7B800", "#E7B800")) +
  scale_linetype_manual(values = c("solid","dotted","dotted","solid","dotted","dotted"))+
  ylab("Salinity (psu)")+
  theme_minimal()

data_long <- 0
data_long <- gather(all_monthly, condition, measurement, c("Sal_1m","Sal_7m"), factor_key = TRUE)

ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.2) +
  geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#00757d","#E7B800")) +
  theme_minimal()




#=========#
# DO, pH, Chl ####
#=========#
str(df.all)

df.all$DOmgl_7m[df.all$DOmgl_7m<=0] <- NA
df.all$DOmgl_1m[df.all$DOmgl_1m<=0.5] <- NA
# df.data_1m$DOmgl[df.data_1m$TIMESTAMP >= "2015-07-01" & df.data_1m$TIMESTAMP <= "2015-08-06"] <- NA
df.all$DOmgl_1m[df.all$date >= as.POSIXct("2015-07-06 01:00:00")& df.all$date <= as.POSIXct("2015-08-12 01:00:00")] <- NA #Looks like 1m probe died. Cleaned on 7/6/15


data_long <- gather(df.all, condition, measurement, c("DOmgl_1m","DOmgl_7m"), factor_key = TRUE)
max.m <- max(data_long$measurement, na.rm = TRUE)
min.m <- min(data_long$measurement, na.rm = TRUE)
max.m_upper_top <- max.m + 0.07*(max.m-min.m)
max.m_upper <- max.m + 0.04*(max.m-min.m)
max.m_lower <- max.m + 0.01*(max.m-min.m)
data_long$presence[data_long$measurement>0&data_long$condition=="DOmgl_1m"] <- max.m_upper
data_long$presence[data_long$measurement>0&data_long$condition=="DOmgl_7m"] <- max.m_lower
df.2 <- data.frame(
  condition = data_long$condition,
  date = data_long$date,
  measurement = data_long$presence
)
df.3 <- data.frame(
  date = as.POSIXct(c("5/26/16","7/14/18"), format = "%m/%d/%y"), 
  measurement = max.m_upper_top*c(1,1)
)
DO <- ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.05, size = 0.5, show.legend = FALSE) +
  #geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylab("DO (mg/L)")+
  geom_point(data = df.2, aes(color = condition), 
             size = 0.5,  show.legend = FALSE, shape = 15)+
  geom_line(data = df.3, color = "purple", 
            size = 1,  show.legend = FALSE)+
  xlim(as.POSIXct(c('2014-06-01','2019-04-01')))
DO

# DO_QC <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   geom_point(aes(color = condition), alpha = 0.5, size = 0.5, show.legend = FALSE) +
#   #geom_line(aes(color = condition), size = .5) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   ylab("Dissolved oxygen (mg/L)")+
#   xlim(as.POSIXct(c('2015-07-06','2015-08-11')))
# DO_QC

df.all$pH_1m[df.all$pH_1m>9|df.all$pH_1m<6.7] <- NA
df.all$pH_1m[df.all$date >= as.POSIXct("2019-01-15 01:00:00")] <- NA
df.all$pH_7m[df.all$date >= as.POSIXct("2019-01-15 01:00:00")] <- NA

data_long <- gather(df.all, condition, measurement, c("pH_1m","pH_7m"), factor_key = TRUE)
max.m <- max(data_long$measurement, na.rm = TRUE)
min.m <- min(data_long$measurement, na.rm = TRUE)

# max.m_upper_top <- min.m - 0.01*(max.m-min.m)
# max.m_upper <- min.m - 0.025*(max.m-min.m)
# max.m_lower <- min.m - 0.04*(max.m-min.m)
max.m_upper_top <- max.m + 0.07*(max.m-min.m)
max.m_upper <- max.m + 0.04*(max.m-min.m)
max.m_lower <- max.m + 0.01*(max.m-min.m)
data_long$presence[data_long$measurement>0&data_long$condition=="pH_1m"] <- max.m_upper
data_long$presence[data_long$measurement>0&data_long$condition=="pH_7m"] <- max.m_lower
df.2 <- data.frame(
  condition = data_long$condition,
  date = data_long$date,
  measurement = data_long$presence
)
df.3 <- data.frame(
  date = as.POSIXct(c("5/26/16","7/14/18"), format = "%m/%d/%y"), 
  measurement = max.m_upper_top*c(1,1)
)
pH <- ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.05, size = 0.5, show.legend = FALSE) +
  #geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylab("pH")+
  geom_point(data = df.2, aes(color = condition), 
             size = 0.5,  show.legend = FALSE, shape = 15)+
  geom_line(data = df.3, color = "purple", 
            size = 1,  show.legend = FALSE)+
  xlim(as.POSIXct(c('2014-06-01','2019-04-01')))

df.all$Chlug_7m[df.all$Chlug_7m>200] <- 200
df.all$Chlug_1m[df.all$Chlug_1m>200] <- 200
df.all$Chlug_7m[df.all$date >= as.POSIXct("2016-07-20 01:00:00") & df.all$date <= as.POSIXct("2016-09-05 01:00:00")] <- NA #Check date that sensor was cleaned. 

data_long <- gather(df.all, condition, measurement, c("Chlug_1m","Chlug_7m"), factor_key = TRUE)
max.m <- 20
min.m <- 0

# max.m_upper_top <- min.m - 0.01*(max.m-min.m)
# max.m_upper <- min.m - 0.025*(max.m-min.m)
# max.m_lower <- min.m - 0.04*(max.m-min.m)
max.m_upper_top <- max.m + 0.07*(max.m-min.m)
max.m_upper <- max.m + 0.04*(max.m-min.m)
max.m_lower <- max.m + 0.01*(max.m-min.m)
data_long$presence[data_long$measurement>0&data_long$condition=="Chlug_1m"] <- max.m_upper
data_long$presence[data_long$measurement>0&data_long$condition=="Chlug_7m"] <- max.m_lower
df.2 <- data.frame(
  condition = data_long$condition,
  date = data_long$date,
  measurement = data_long$presence
)
df.3 <- data.frame(
  date = as.POSIXct(c("5/26/16","7/14/18"), format = "%m/%d/%y"), 
  measurement = max.m_upper_top*c(1,1)
)
chl <- ggplot(data_long, aes(x = date, y = measurement)) + 
  geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
  #geom_line(aes(color = condition), size = .5) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylim(-.1,22)+
  ylab("Chl (ug/L)")+
  geom_point(data = df.2, aes(color = condition), 
             size = 0.5,  show.legend = FALSE, shape = 15)+
  geom_line(data = df.3, color = "purple", 
            size = 1,  show.legend = FALSE)+
  xlim(as.POSIXct(c('2014-06-01','2019-04-01')))
#chl

dev.off()


setwd("~/Documents/School and jobs/UW Biology/Research/Penn_Cove_monitoring/Mussel Hotels/figures")
#pdf(file = "SW_panel.pdf", width = 8, height = 7.5)
png(file = "SW_panel.png", width = 8, height = 7.5, units = "in", res = 300)

ggarrange(temp_graph + rremove("x.text")+ rremove("xlab"),
          sal + rremove("x.text")+ rremove("xlab"),
          pH + rremove("x.text") + rremove("xlab"),
          DO + rremove("x.text") + rremove("xlab"),
          chl,
          nrow = 5)
dev.off()

# ggarrange(bxp, dp, bp + rremove("x.text"), 
#           labels = c("A", "B", "C"),
#           ncol = 2, nrow = 2)


# Thresholds ####

# To do: 
# graphs that show how many days above a certain threshold (i.e. Chl 5 mg/L, DO 5 mg / L)

str(df.all)
df.all$DO_upper <- df.all$
  df.all$DO_lower <- 
  threshold_DO <- df.all %>% 
  mutate(year = year(TIMESTAMP), month = month(TIMESTAMP)) %>%
  group_by(date = as.Date(paste(year, month,01, sep="-"))) %>%
  summarise(sum1m =(sum(DOmgl_1m<=2, na.rm = TRUE)/sum(DOmgl_1m>=0, na.rm = TRUE)),
            sum7m =(sum(DOmgl_7m<=2, na.rm = TRUE)/sum(DOmgl_7m>=0, na.rm = TRUE)))



# Hours per day of low pH
# threshold_pH <- df.all %>% 
#   mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
#   group_by(date = as.Date(paste(year, month, day, sep="-"))) %>%
#   summarise(sum1m =(24*sum(pH_1m<=7.2, na.rm = TRUE)/sum(pH_1m>=0, na.rm = TRUE)),
#             sum7m =(24*sum(pH_7m<=7.2, na.rm = TRUE)/sum(pH_7m>=0, na.rm = TRUE)))
# data_long <- gather(threshold_pH, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
# pH_time_graph <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
#   geom_line(aes(color = condition), size = .5, show.legend = FALSE) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   #ylim(-.1,25)+
#   ylab("Hours per day below \n pH threshold")+
#   xlim(as.Date(c('2014-06-01','2019-04-01')))
# pH_time_graph #Right now the threshold is 2mg/L, but it could be higher. 

# Hours per day of low pH
threshold_pH <- df.all %>% 
  mutate(week = cut(as.Date(date), "week")) %>%
  group_by(week) %>%
  summarise(sum1m =(sum(pH_1m<=7.2, na.rm = TRUE)/sum(pH_1m>=0, na.rm = TRUE)),
            sum7m =(sum(pH_7m<=7.2, na.rm = TRUE)/sum(pH_7m>=0, na.rm = TRUE)))
data_long <- gather(threshold_pH, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
pH_time_graph <- ggplot(data_long, aes(x = as.Date(week), y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_line(aes(color = condition), size = 1, show.legend = FALSE) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylim(0,1)+
  ylab("Proportion below \n pH threshold")+
  xlim(as.Date(c('2014-06-01','2019-04-01')))
pH_time_graph #Right now the threshold is 2mg/L, but it could be higher. 


# Hours per day of low DO
# threshold_DO <- df.all %>% 
#   mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
#   group_by(date = as.Date(paste(year, month, day, sep="-"))) %>%
#   summarise(sum1m =(24*sum(DOmgl_1m<=2, na.rm = TRUE)/sum(DOmgl_1m>=0, na.rm = TRUE)),
#             sum7m =(24*sum(DOmgl_7m<=2, na.rm = TRUE)/sum(DOmgl_7m>=0, na.rm = TRUE)))
# 
# data_long <- gather(threshold_DO, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
# DO_time_graph <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
#   geom_line(aes(color = condition), size = .5, show.legend = FALSE) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   #ylim(-.1,25)+
#   ylab("Hours per day below \n hypoxia threshold")+
#   xlim(as.Date(c('2014-06-01','2019-04-01')))
# DO_time_graph #Right now the threshold is 2mg/L, but it could be higher. 

as.Date(df.all$date)
threshold_DO <- df.all %>% 
  mutate(week = cut(as.Date(date), "week")) %>%
  group_by(week) %>%
  dplyr::summarise(sum1m =(sum(DOmgl_1m<=2, na.rm = TRUE)/sum(DOmgl_1m<=20, na.rm = TRUE)),
                   sum7m =(sum(DOmgl_7m<=2, na.rm = TRUE)/sum(DOmgl_7m<=20, na.rm = TRUE)))

data_long <- gather(threshold_DO, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
DO_time_graph <- ggplot(data_long, aes(x = as.Date(week), y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_line(aes(color = condition), size = 1, show.legend = FALSE) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylim(0,1)+
  ylab("Proportion below \n hypoxia threshold")+
  xlim(as.Date(c('2014-06-01','2019-04-01')))
DO_time_graph #Right now the threshold is 2mg/L, but it could be higher. 

# threshold_DO <- df.all %>% 
#   mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
#   group_by(date = as.Date(paste(year, month, day, sep="-"))) %>%
#   summarise(sum1m =(sum(DOmgl_1m<=0.5, na.rm = TRUE)/sum(DOmgl_1m>=0, na.rm = TRUE)),
#             sum7m =(sum(DOmgl_7m<=0.5, na.rm = TRUE)/sum(DOmgl_7m>=0, na.rm = TRUE)))
# data_long <- gather(threshold_DO, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
# DO_time_graph_anoxic <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
#   geom_line(aes(color = condition), size = .5, show.legend = FALSE) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   #ylim(-.1,25)+
#   ylab("Hours per day below \n anoxia threshold")+
#   xlim(as.Date(c('2014-06-01','2019-04-01')))
# DO_time_graph_anoxic #Right now the threshold is 2mg/L, but it could be higher. 


# df.all$Chlug_1m
# # Hours per day of high Chl
# threshold_chl <- df.all %>% 
#   mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
#   group_by(date = as.Date(paste(year, month, day, sep="-"))) %>%
#   summarise(sum1m =(24*sum(Chlug_1m>=6, na.rm = TRUE)/sum(Chlug_1m>=0, na.rm = TRUE)),
#             sum7m =(24*sum(Chlug_7m>=6, na.rm = TRUE)/sum(Chlug_7m>=0, na.rm = TRUE)))
# data_long <- gather(threshold_chl, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
# chl_time_graph <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
#   geom_line(aes(color = condition), size = .5, show.legend = FALSE) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   ylim(0,24)+
#   ylab("Hours per day above \nsaturating food")+
#   xlim(as.Date(c('2014-06-01','2019-04-01')))
# #chl_time_graph

df.all$Chlug_1m
# Hours per day of high Chl
threshold_chl <- df.all %>% 
  mutate(week = cut(as.Date(date), "week")) %>%
  group_by(week) %>%
  dplyr::summarise(sum1m =(sum(Chlug_1m<=6, na.rm = TRUE)/sum(Chlug_1m<=100, na.rm = TRUE)),
                   sum7m =(sum(Chlug_7m<=6, na.rm = TRUE)/sum(Chlug_7m<=100, na.rm = TRUE)))
data_long <- gather(threshold_chl, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
chl_time_graph <- ggplot(data_long, aes(x = as.Date(week), y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_line(aes(color = condition), size = 1, show.legend = FALSE) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylim(0,1)+
  xlab("Date")+
  xlim(as.Date(c('2014-06-01','2019-04-01')))+
  ylab("Proportion below \nsaturating food")
#+
#  xlim(as.Date(c('2014-06-01','2019-04-01')))
chl_time_graph

# sum(df.all$TempC_1m>=10, na.rm = TRUE)/sum(df.all$TempC_1m>=0, na.rm=TRUE)
# # Hours per day at high temp
# threshold_temp <- df.all %>% 
#   mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
#   group_by(date = as.Date(paste(year, month, day, sep="-"))) %>%
#   summarise(sum1m =24*(sum(TempC_1m>=18, na.rm = TRUE)/sum(TempC_1m>=0, na.rm = TRUE)),
#             sum7m =24*(sum(TempC_7m>=18, na.rm = TRUE)/sum(TempC_7m>=0, na.rm = TRUE)))
# data_long <- gather(threshold_temp, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
# temp_time_graph <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
#   geom_line(aes(color = condition), size = .5, show.legend = FALSE) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   ylim(0,24.1)+
#   ylab("Hours per day above \n temperature threshold")+
#   xlim(as.Date(c('2014-06-01','2019-04-01')))
# #temp_time_graph #Right now the threshold is 2mg/L, but it could be higher. 

# Hours per day at high temp
threshold_temp <- df.all %>% 
  mutate(week = cut(as.Date(date), "week")) %>%
  group_by(week) %>%
  summarise(sum1m =(sum(TempC_1m>=18, na.rm = TRUE)/sum(TempC_1m>=0, na.rm = TRUE)),
            sum7m =(sum(TempC_7m>=18, na.rm = TRUE)/sum(TempC_7m>=0, na.rm = TRUE)))
data_long <- gather(threshold_temp, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
temp_time_graph <- ggplot(data_long, aes(x = as.Date(week), y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_line(aes(color = condition), size = 1, show.legend = FALSE) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylim(0,1)+
  ylab("Proportion above \n temperature threshold")+
  xlim(as.Date(c('2014-06-01','2019-04-01')))
temp_time_graph #Right now the threshold is 2mg/L, but it could be higher. 


# Hours per day at low sal
#https://www.sciencedirect.com/science/article/pii/S1470160X17301784 14 threshold
# threshold_sal <- df.all %>% 
#   mutate(year = year(TIMESTAMP), month = month(TIMESTAMP), day = day(TIMESTAMP)) %>%
#   group_by(date = as.Date(paste(year, month, day, sep="-"))) %>%
#   summarise(sum1m =24*(sum(Sal_1m<=14, na.rm = TRUE)/sum(Sal_1m>=0, na.rm = TRUE)),
#             sum7m =24*(sum(Sal_7m<=14, na.rm = TRUE)/sum(Sal_7m>=0, na.rm = TRUE)))
# data_long <- gather(threshold_sal, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
# sal_time_graph <- ggplot(data_long, aes(x = date, y = measurement)) + 
#   #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
#   geom_line(aes(color = condition), size = .5, show.legend = FALSE) +
#   scale_color_manual(values = c("#E7B800","#00757d")) +
#   ylim(0,24.1)+
#   ylab("Hours per day below \n salinity threshold")+
#   xlim(as.Date(c('2014-06-01','2019-04-01')))
# sal_time_graph #Right now the threshold is 2mg/L, but it could be higher.

# Sal threshold ####

threshold_sal <- df.all %>% 
  mutate(week = cut(as.Date(date), "week")) %>%
  group_by(week) %>%
  summarise(sum1m =(sum(Sal_1m<=14, na.rm = TRUE)/sum(Sal_1m>=0, na.rm = TRUE)),
            sum7m =(sum(Sal_7m<=14, na.rm = TRUE)/sum(Sal_7m>=0, na.rm = TRUE)))

threshold_sal.missing <- subset.df %>% 
  mutate(week = cut(as.Date(date), "week")) %>%
  group_by(week) %>%
  summarise(sum7m =(sum(Sal_7m_missing<=14, na.rm = TRUE)/sum(Sal_7m_missing>=0, na.rm = TRUE)))

data_long <- gather(threshold_sal, condition, measurement, c("sum1m","sum7m"), factor_key = TRUE)
data_long.missing <- gather(threshold_sal.missing, condition, measurement, c("sum7m"), factor_key = TRUE)

sal_time_graph <- ggplot(data_long, aes(x = as.Date(week), y = measurement)) + 
  #geom_point(aes(color = condition), alpha = 0.1, size = 0.5, show.legend = FALSE) +
  geom_line(aes(color = condition), size = 1, show.legend = FALSE) +
  geom_point(data = data_long.missing, col = "#8fc0a9", size = 1, show.legend = FALSE, shape = 15) +
  scale_color_manual(values = c("#E7B800","#00757d")) +
  ylim(0,1)+
  ylab("Proportion below \n salinity threshold")+
  xlim(as.Date(c('2014-06-01','2019-04-01')))
sal_time_graph #Right now the threshold is 2mg/L, but it could be higher.

df.thresholds <- data.frame(
  threshold_temp,
  threshold_sal,
  threshold_pH,
  threshold_DO,
  threshold_chl
)
# ggarrange(DO_time_graph + rremove("x.text")+ rremove("xlab"),
#           chl_time_graph,
#           nrow = 2)

setwd("~/Documents/School and jobs/UW Biology/Research/Penn_Cove_monitoring/Mussel Hotels/figures")
# pdf(file = "SW_thresholds_panel.pdf", width = 8, height = 7.5)
png(file = "SW_thresholds_panel.png", width = 8, height = 8, units = "in", res = 300)
ggarrange(temp_time_graph + rremove("x.text")+ rremove("xlab"),
          sal_time_graph + rremove("x.text")+ rremove("xlab"),
          pH_time_graph + rremove("x.text")+ rremove("xlab"),
          DO_time_graph + rremove("x.text")+ rremove("xlab"),
          chl_time_graph,
          nrow = 5)
dev.off()

length(df.all$DOmgl_7m)
!is.na(df.all$DOmgl_7m)
sum(df.all$DOmgl_7m<=5, na.rm = TRUE)
sum(df.all$DOmgl_7m, na.rm = TRUE)

detach("package:plotly", unload=TRUE) #otherwise ungroup() has two uses
detach("package:plyr", unload=TRUE) 
detach("package:MASS", unload=TRUE) #otherwise MASS is used for select()
library(dplyr)

# Subset data by season ####
season_dates <- c(
  '6/21/2014',
  '9/22/2014',
  '12/21/2014',
  '3/20/2015',
  '6/21/2015',
  '9/23/2015',
  '12/21/2015',
  '3/19/2016',
  '6/20/2016',
  '9/22/2016',
  '12/21/2016',
  '3/20/2017',
  '6/20/2017',
  '9/22/2017',
  '12/21/2017',
  '3/20/2018',
  '6/21/2018',
  '9/22/2018',
  '12/21/2018',
  '3/20/2019',
  '6/21/2019')



season_name <- c(
  "Summer",
  "Autumn",
  "Winter",
  "Spring",
  "Summer",
  "Autumn",
  "Winter",
  "Spring",
  "Summer",
  "Autumn",
  "Winter",
  "Spring",
  "Summer",
  "Autumn",
  "Winter",
  "Spring",
  "Summer",
  "Autumn",
  "Winter",
  "Spring",
  "Summer")

season <- data.frame(
  season_name,
  season_dates
)

# measurement <- df.all$TempC_1m
# time <- df.all$TIMESTAMP
date_split_format <- as.Date(season_dates, format = "%m/%d/%Y")
DS <- date_split_format
DS <- DS-1 #B/c splitting after so here transforming so will be correct. 
Se <- season_name
# split <- split(df.all, cut(as.Date(time), date_split_format, include.lowest=TRUE))
# new <- unsplit(split, date_split_format)
# head(new$Se)
# df.all$month <- month(df.all$TIMESTAMP)

# df.all %>%
#   mutate(
#     season = case_when(
#       month %in% 10:12 ~ "Fall",
#       month %in%  1:3  ~ "Winter",
#       month %in%  4:6  ~ "Spring",
#       TRUE ~ "Summer"))

df.all$date <- as.Date(df.all$TIMESTAMP)
length(DS)


df.all

head(df.all)
df.all <- df.all[!is.na(df.all$TIMESTAMP),]
df.all.season <- df.all %>%
  mutate(
    season = case_when(
      date %in% DS[1]:DS[2] ~ Se[1],
      date %in% DS[2]:DS[3] ~ Se[2],
      date %in% DS[3]:DS[4] ~ Se[3],
      date %in% DS[4]:DS[5] ~ Se[4],
      date %in% DS[5]:DS[6] ~ Se[5],
      date %in% DS[6]:DS[7] ~ Se[6],
      date %in% DS[7]:DS[8] ~ Se[7],
      date %in% DS[8]:DS[9] ~ Se[8],
      date %in% DS[9]:DS[10] ~ Se[9],
      date %in% DS[10]:DS[11] ~ Se[10],
      date %in% DS[11]:DS[12] ~ Se[11],
      date %in% DS[12]:DS[13] ~ Se[12],
      date %in% DS[13]:DS[14] ~ Se[13],
      date %in% DS[14]:DS[15] ~ Se[14],
      date %in% DS[15]:DS[16] ~ Se[15],
      date %in% DS[16]:DS[17] ~ Se[16],
      date %in% DS[17]:DS[18] ~ Se[17],
      date %in% DS[18]:DS[19] ~ Se[18],
      date %in% DS[19]:DS[20] ~ Se[19]))

df.all.old <- df.all
df.all <- df.all.season

# df.all %>%
#   transmute(
#     T1 = rank(TempC_1m),
#     T7 = rank(TempC_7m),
#     S1 = rank(Sal_1m),
#     S7 = rank(Sal_7m),
#     H1 = rank(pH_1m),
#     H7 = pH_7m,
#     DOmgl_1m,
#     DOmgl_7m,
#     Chlug_1m,
#     Chlug_7m
#   )


head(df.all)

plot(df.all$Sal_7m~as.Date(df.all$TIMESTAMP))

# envdata <- as.data.frame(df.all %>%
#   transmute(
#   TempC_1m,
#   TempC_7m,
#   Sal_1m,
#   Sal_7m,
#   pH_1m,
#   pH_7m,
#   DOmgl_1m,
#   DOmgl_7m,
#   Chlug_1m,
#   Chlug_7m
# ))



#mv.outliers(envdata, method = 'euclidean', sd.limit = 3)
# envdata



SW_wide <- data.frame(
  TIMESTAMP = df.all$TIMESTAMP,
  season = df.all$season,
  Temp_1m = df.all$TempC_1m,
  Temp_7m = df.all$TempC_7m,
  Sal_1m = df.all$Sal_1m,
  Sal_7m = df.all$Sal_7m,
  pH_1m = df.all$pH_1m,
  pH_7m = df.all$pH_7m,
  DO_1m = df.all$DOmgl_1m,
  DO_7m = df.all$DOmgl_7m,
  Chl_1m = df.all$Chlug_1m,
  Chl_7m = df.all$Chlug_7m
)

tidier <- SW_wide %>%
  gather(key, param, -TIMESTAMP, -season)
tidier %>% head(8)

blah <- tidier %>%
  separate(key, into = c("factor", "depth"), sep = "\\_")
blah %>% head(8)



# This function replaces spread()

SW <-  blah %>% 
  group_by_at(vars(-param,-TIMESTAMP)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=factor, value=param) %>%    # spread
  select(-row_id)  # drop the index
SW %>% head(8)

?n

library(dplyr)

mean <- SW %>%
  group_by(season, depth)%>%
  summarise_at(vars(Chl:Temp), mean, na.rm = TRUE)

sd <- SW %>%
  group_by(season, depth)%>%
  summarise_at(vars(Chl:Temp), sd, na.rm = TRUE)

max_min <- SW %>%
  group_by(season, depth)%>%
  summarise_at(vars(Chl:Temp), list(max,min), na.rm = TRUE)

setwd("~/Documents/School and jobs/UW Biology/Research/Penn_Cove_monitoring/SONDE_data/SONDE data_plotme")
write.csv(mean, file = "mean.csv")
write.csv(sd, file = "sd.csv")
write.csv(max_min, file = "max_min.csv")


# summary <- SW %>%
#   group_by(season, depth)%>%
#   summarise_at(vars(Chl:Temp), count = n(), na.rm = TRUE)
# 


# 

SW$season <- as.factor(SW$season)
SW$depth <- as.factor(SW$depth)
library(plyr)
n_vals <- ddply(SW, .(season, depth), summarise,
                nT =  sum(Temp>1, na.rm = TRUE),
                nS =  sum(Sal>1, na.rm = TRUE),
                npH =  sum(pH>1, na.rm = TRUE),
                nDO =  sum(DO>0, na.rm = TRUE),
                nChl =  sum(Chl>0, na.rm = TRUE)
)  # 
detach("package:plyr", unload=TRUE)

write.csv(n_vals, file = "n_vals.csv")


sum()
plot(SW$Sal ~ as.Date(SW$TIMESTAMP), col = as.factor(SW$depth))
# 

plot(c(1,2),c(1,2))

sum(SW$Temp>=0, na.rm = TRUE)


env_trans <-   SW %>% 
  mutate(RT = scale(log10(Temp)),
         RS = scale(log10(Sal)),
         RpH = scale(log10(pH)),
         RDO = scale(log10(DO+1)),
         RChl = scale(log10(Chl+2)))
str(env_trans)

# env_trans <-   SW[SW$depth=="D"&SW$season=="Summer",] %>% 
#      mutate(RT = scale(log(Temp-7)),
#                RS = scale(log(Sal)),
#                RpH = scale(log(pH)),
#                RDO = scale(log(DO+1)),
#                RChl = scale(log(Chl+1)))
#   str(env_trans)
#   
library(ggpubr)

ggqqplot(env_trans$RT)
ggqqplot(env_trans$Temp)

uv.outliers(SW, id='Temp:Chl', var = "Temp", sd.limit=3)

head(env_trans)
my_data <- env_trans[,9:13]
my_data <- env_trans[,4:8]

# mv.outliers(my_data,method='euclidean', sd.limit=3)

#my_data <- env_trans[,5:8]

str(my_data)

library("Hmisc")
#install.packages("corrplot")

res2 <- rcorr(as.matrix(my_data))
res2


res <- cor(my_data)
library(corrplot)
corrplot(res, method = "number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

corrplot(res2$r,method = "number", type="lower", order="hclust", tl.col = "black",
         p.mat = res2$P, sig.level = 0.01, insig = "blank")


corrplot.mixed(res2$r,lower = "number", tl.col = "black",
               p.mat = res2$P, sig.level = 0.01, insig = "blank")

library("PerformanceAnalytics")
# chart.Correlation(my_data, histogram=TRUE, pch=19) Plots raw data and correlations

theme_set(theme_minimal())
theme_update(panel.grid.minor.y = element_blank(),
             panel.grid.major.y = element_blank(),
             panel.grid.minor.x = element_blank(),
             panel.grid.major.x = element_blank(),
             axis.line = element_line(colour = "black"))
# theme_update(panel.grid.minor.y = element_blank(), 
#              panel.grid.major.y = element_line(colour = "grey91"),
#              panel.grid.minor.x = element_line(colour = "grey81"), 
#              panel.grid.major.x = element_line(colour = "grey61"),
#              axis.line = element_blank())
# 
col2 <- c("#67001F", "#B2182B", "#F4A582", "#777777","#777777",
                   "#92C5DE", "#2166AC", "#053061")
                   palette(col2)
                   
                   nas <- df.all[is.na(df.all$season),]
                   df.all <- df.all[!is.na(df.all$season),]
                   df.all$season <- as.factor(as.character(df.all$season))
                   gg1<-ggplot(data=df.all, aes(x=pH_1m, y=DOmgl_1m)) +
                     #geom_line(col = col.spp , aes(linetype=depth.x))+
                     #scale_shape_manual(values=c(19))+
                     #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                     ylim(0,25)+
                     xlim(6.8,9)+
                     geom_point(aes(color = season), size = .05)+
                     scale_color_brewer(palette = "Dark2")+
                     stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02)
                   
                   #+labs(x="Sal_1m", y="diff_S")
                   
                   
                   
                   gg2<-ggplot(data=df.all, aes(x=pH_7m, y=DOmgl_7m)) +
                     #geom_line(col = col.spp , aes(linetype=depth.x))+
                     #scale_shape_manual(values=c(19))+
                     #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                     ylim(0,25)+
                     xlim(6.8,9)+
                     geom_point(aes(color = season), size =.05)+
                     scale_color_brewer(palette = "Dark2")+
                     stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02) +
                     labs(x="pH_7m", y="")
                   
                   ggarrange(gg1,gg2, legend = FALSE)
                   
                   gg2<-ggplot(data=df.all, aes(x=pH_7m, y=DOmgl_7m)) +
                     #geom_line(col = col.spp , aes(linetype=depth.x))+
                     #scale_shape_manual(values=c(19))+
                     #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                     ylim(0,25)+
                     xlim(6.8,9)+
                     geom_point(aes(color = season), size =2)+
                     scale_color_brewer(palette = "Dark2")+
                     #stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02) +
                     labs(x="pH_7m", y="")
                   (gg2)# for legend
                   
                   
                   gg1<-ggplot(data=df.all, aes(x=Sal_1m, y=TempC_1m)) +
                     #geom_line(col = col.spp , aes(linetype=depth.x))+
                     #scale_shape_manual(values=c(19))+
                     #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                     ylim(0,25)+
                     xlim(5,32)+
                     geom_point(aes(color = season), size = .05)+
                     scale_color_brewer(palette = "Dark2")+
                     stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02)
                   
                   #+labs(x="Sal_1m", y="diff_S")
                   
                   #df.all$TIMESERIES[is.na(df.all$season)]
                   
                   gg2<-ggplot(data=df.all, aes(x=Sal_7m, y=TempC_7m)) +
                     #geom_line(col = col.spp , aes(linetype=depth.x))+
                     #scale_shape_manual(values=c(19))+
                     #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                     ylim(0,25)+
                     xlim(5,32)+
                     geom_point(aes(color = season), size =.05)+
                     scale_color_brewer(palette = "Dark2")+
                     stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02) +
                     labs(x="Sal_7m", y="")
                   
                   ggarrange(gg1,gg2, legend = "none")
                   
                   library(RColorBrewer)
                   display.brewer.pal(5,'RdYlBu')
                   col.pal <- brewer.pal(6,'BrBG')
                   palette(col.pal[c(1,2,5,6)])
                   
                   col1 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "white",
                                                       "cyan", "#007FFF", "blue","#00007F"))
                                                       col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                                                                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                                                                           "#4393C3", "#2166AC", "#053061"))
                                                                                           col3 <- colorRampPalette(c("red", "white", "blue"))
                                                                                           col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                                                                                                                               "cyan", "#007FFF", "blue", "#000000"))
                                                                                                                               col2 <- c("#67001F", "#B2182B", "#F4A582", "#777777","#777777",
                                                                                                                                                  "#92C5DE", "#2166AC", "#053061")
                                                                                                                                                  
                                                                                                                               head(SW_wide)
                                                                                                                               my_data <- SW_wide[,c(3,5,7,9,11,4,6,8,10,12)]
                                                                                                                               names(my_data) <- c("Temp","Sal","pH","DO","Chl","Temp","Sal","pH","DO","Chl")
                                                                                                                               head(my_data)
                                                                                                                               res2 <- rcorr(as.matrix(my_data))
                                                                                                                               corrplot(res2$r,type = "lower", tl.col = "black", method = "number",
                                                                                                                                        p.mat = res2$P, sig.level = 0.001, insig = "blank", 
                                                                                                                                        col = col2, tl.pos = "lt" , tl.srt = 0)
                                                                                                                               # chart.Correlation(my_data, histogram=TRUE, pch=19) #Looks nice but takes forever to load. 
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               head(df.thresholds)
                                                                                                                               (thresholds_wide <- df.thresholds[,c(1,2,3,5,6,8,9,11,12,14,15)])
                                                                                                                               names(thresholds_wide) <- c("date","Temp_1m","Temp_7m","Sal_1m","Sal_7m","pH_1m","pH_7m","DO_1m","DO_7m","Chl_1m","Chl_7m")  #1m then 7m in pairs 
                                                                                                                               head(thresholds_wide)  
                                                                                                                               
                                                                                                                               
                                                                                                                               thresholds.season<-0
                                                                                                                               thresholds_wide$date<- as.Date(thresholds_wide$date, format = "%Y-%M-%D")
                                                                                                                               thresholds_wide<- thresholds_wide[!is.na(thresholds_wide$date),]
                                                                                                                               
                                                                                                                               thresholds_wide_old <- thresholds_wide
                                                                                                                               thresholds.season <- thresholds_wide %>%
                                                                                                                                 mutate(
                                                                                                                                   season = case_when(
                                                                                                                                     date %in% DS[1]:DS[2] ~ Se[1],
                                                                                                                                     date %in% DS[2]:DS[3] ~ Se[2],
                                                                                                                                     date %in% DS[3]:DS[4] ~ Se[3],
                                                                                                                                     date %in% DS[4]:DS[5] ~ Se[4],
                                                                                                                                     date %in% DS[5]:DS[6] ~ Se[5],
                                                                                                                                     date %in% DS[6]:DS[7] ~ Se[6],
                                                                                                                                     date %in% DS[7]:DS[8] ~ Se[7],
                                                                                                                                     date %in% DS[8]:DS[9] ~ Se[8],
                                                                                                                                     date %in% DS[9]:DS[10] ~ Se[9],
                                                                                                                                     date %in% DS[10]:DS[11] ~ Se[10],
                                                                                                                                     date %in% DS[11]:DS[12] ~ Se[11],
                                                                                                                                     date %in% DS[12]:DS[13] ~ Se[12],
                                                                                                                                     date %in% DS[13]:DS[14] ~ Se[13],
                                                                                                                                     date %in% DS[14]:DS[15] ~ Se[14],
                                                                                                                                     date %in% DS[15]:DS[16] ~ Se[15],
                                                                                                                                     date %in% DS[16]:DS[17] ~ Se[16],
                                                                                                                                     date %in% DS[17]:DS[18] ~ Se[17],
                                                                                                                                     date %in% DS[18]:DS[19] ~ Se[18],
                                                                                                                                     date %in% DS[19]:DS[20] ~ Se[19]))
                                                                                                                               
                                                                                                                               thresholds_wide <- thresholds.season
                                                                                                                               str(thresholds.season)
                                                                                                                               
                                                                                                                               
                                                                                                                               thresholds_cor <- thresholds_wide[,c(2,4,6,8,10,3,5,7,9,11)]
                                                                                                                               head(thresholds_cor)
                                                                                                                               names(thresholds_cor) <- c("Temp","Sal","pH","DO","Chl","Temp","Sal","pH","DO","Chl")
                                                                                                                               thresholds_trans <- asin(sqrt(thresholds_cor))
                                                                                                                               
                                                                                                                               head(thresholds_trans)
                                                                                                                               
                                                                                                                               res3 <- rcorr(as.matrix(thresholds_trans))
                                                                                                                               corrplot(res3$r,type = "lower", tl.col = "black", method = "number",
                                                                                                                                        p.mat = res3$P, sig.level = 0.001, insig = "blank", 
                                                                                                                                        col = col2, tl.pos = "lt" , tl.srt = 0, na.label = "square")
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               gg1<-ggplot(data=thresholds_wide, aes(x=pH_1m, y=DO_1m)) +
                                                                                                                                 #geom_line(col = col.spp , aes(linetype=depth.x))+
                                                                                                                                 #scale_shape_manual(values=c(19))+
                                                                                                                                 #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                                                                                                                                 ylim(0,1)+
                                                                                                                                 xlim(0,1)+
                                                                                                                                 geom_point(aes(color = season), size = 1)+
                                                                                                                                 #geom_point(col = "black", size = 1)+
                                                                                                                                 scale_color_brewer(palette = "Dark2")
                                                                                                                               #stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02)
                                                                                                                               
                                                                                                                               #+labs(x="Sal_1m", y="diff_S")
                                                                                                                               
                                                                                                                               gg2<-ggplot(data=thresholds_wide, aes(x=pH_7m, y=DO_7m)) +
                                                                                                                                 #geom_line(col = col.spp , aes(linetype=depth.x))+
                                                                                                                                 #scale_shape_manual(values=c(19))+
                                                                                                                                 #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                                                                                                                                 ylim(0,1)+
                                                                                                                                 xlim(0,1)+
                                                                                                                                 geom_point(aes(color = season), size = 1)+
                                                                                                                                 #geom_point(col = "black", size = 1)+
                                                                                                                                 scale_color_brewer(palette = "Dark2")+
                                                                                                                                 stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02)
                                                                                                                               gg2
                                                                                                                               #+labs(x="Sal_1m", y="diff_S")
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               ggarrange(gg1,gg2, legend = FALSE)
                                                                                                                               
                                                                                                                               gg1<-ggplot(data=thresholds_wide, aes(x=Sal_1m, y=Temp_1m)) +
                                                                                                                                 #geom_line(col = col.spp , aes(linetype=depth.x))+
                                                                                                                                 #scale_shape_manual(values=c(19))+
                                                                                                                                 #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                                                                                                                                 ylim(0,1)+
                                                                                                                                 xlim(0,1)+
                                                                                                                                 geom_point(aes(color = season), size = 1)+
                                                                                                                                 #geom_point(col = "black", size = 1)+
                                                                                                                                 scale_color_brewer(palette = "Dark2")
                                                                                                                               
                                                                                                                               #+
                                                                                                                               #  stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02)
                                                                                                                               
                                                                                                                               #+labs(x="Sal_1m", y="diff_S")
                                                                                                                               
                                                                                                                               gg2<-ggplot(data=thresholds_wide, aes(x=Sal_7m, y=Temp_7m)) +
                                                                                                                                 #geom_line(col = col.spp , aes(linetype=depth.x))+
                                                                                                                                 #scale_shape_manual(values=c(19))+
                                                                                                                                 #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                                                                                                                                 ylim(0,1)+
                                                                                                                                 xlim(0,1)+
                                                                                                                                 geom_point(aes(color = season), size = 1)+
                                                                                                                                 #geom_point(col = "black", size = 1)+
                                                                                                                                 scale_color_brewer(palette = "Dark2")
                                                                                                                               
                                                                                                                               #+
                                                                                                                               #stat_smooth(method = "lm", color = "black", linetype = 1, alpha = 0.02)
                                                                                                                               
                                                                                                                               #+labs(x="Sal_1m", y="diff_S")
                                                                                                                               
                                                                                                                               ggarrange(gg1,gg2, legend = FALSE)
                                                                                                                               ggarrange(gg1,gg2)
                                                                                                                               
                                                                                                                               
                                                                                                                               gg2<-ggplot(data=thresholds_wide, aes(x=season, y=DO_7m)) +
                                                                                                                                 #geom_line(col = col.spp , aes(linetype=depth.x))+
                                                                                                                                 #scale_shape_manual(values=c(19))+
                                                                                                                                 #geom_errorbar(aes(ymin=mean.x-se.x, ymax=mean.x+se.x), col = col.spp)+
                                                                                                                                 ylim(0,1)+
                                                                                                                                 #xlim(0,1)+
                                                                                                                                 geom_boxplot(outlier.colour="black", outlier.shape=16,
                                                                                                                                              outlier.size=2, notch=FALSE)
                                                                                                                               gg2
                                                                                                                               
                                                                                                                               
                                                                                                                               
                                                                                                                               pca(thresholds_wide~season)
                                                                                                                               
                                                                                                                               