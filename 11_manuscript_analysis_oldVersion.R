rm(list=ls())
####Load Libraries
library(lubridate)
library(readxl)
library(Hmisc)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(chron)
library(MuMIn)
library(lme4)
library(zoo) 
library(heatmaply)
library(svDialogs)
library(varhandle)
library(xts)
library(lubridate)
library(DataCombine)
library(stringr)
library(GGally)
library(lattice)
library(mgcv)
library(corrplot)
library(RColorBrewer)


#### READ IN ALL SUBSAMPLE FILES
AIDir =  "D:\\SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex\\adjValues" 
AIFiles <- list.files(AIDir, pattern="Subsample", recursive=F, full.names=T)

# INFO FILES-
basename(AIFiles)
mySites = unique( sapply(strsplit(basename(AIFiles), "_"), head, 1) )
numSites = length(mySites)

#COMBINE INDEX- 
SUBALL1 <- do.call(rbind,lapply(AIFiles,read.csv)) # ALL = d

# SIMPLE changes to data matrix
indices = colnames(SUBALL1)
fileInfo = indices[1:10]
Indexnames = indices[11:length(SUBALL1)]

#### convert data type
SUBALL1$Sites = as.character(SUBALL1$Site)
SUBALL1$Date2 <- as.Date(SUBALL1$Date, format="%Y_%m_%d")
SUBALL1$Date3 <- gsub("_","",SUBALL1$Date)
metric = "Acoustic Complexity Index" #metric to plot

####Set Treatments
SUBALL1$Treatment <- ifelse(SUBALL1$Site==10, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                            ifelse(SUBALL1$Site==20, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                                   ifelse(SUBALL1$Site==30, "Old Growth (Yes) \n Rough (No) \n Prescribed (No)",
                                          ifelse(SUBALL1$Site==40, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                                 ifelse(SUBALL1$Site==50, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                                        ifelse(SUBALL1$Site==60, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                               ifelse(SUBALL1$Site==70, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                                      ifelse(SUBALL1$Site==80, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                             ifelse(SUBALL1$Site==90, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                                    NA  ))))))))) # all other values map to NA
myTreatments = unique(SUBALL1$Treatment)


####Pull out bad days due to equipment malfunction
#Flag Site 80 from 2018/01/18 through 2018/05/03
#Flag Site 30 from 2018/04/07 through 2018/05/23
#Create Flag Field
SUBALL1$Flag <- ifelse(SUBALL1$Sites==80 & SUBALL1$Date2 >= "2018/01/18" & SUBALL1$Date2 <= "2018-05-03","F",
                       ifelse(SUBALL1$Sites==30 & SUBALL1$Date2 >= "2018/04/07" & SUBALL1$Date2 <= "2018-05-23","F",""))

#Create common field to relate date time of RAWS Park Ridge Fire Weather Data
SUBALL1$Join <- paste(SUBALL1$Date3,SUBALL1$Hr, sep ="_")
#subset out Flagged data
SUBALL<-subset(SUBALL1, Flag !='F')

##Read in weather data
wx<-read.csv("D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Weather\\parkridge_raws_master.csv", stringsAsFactors=FALSE)
#Create new fields for joined field
wx$Date1 <- gsub("/","",wx$Date)
wx$Time1 <- gsub(":00","",wx$Time)
wx$Join<-paste(wx$Date1,wx$Time1, sep ="_")

#make better names in wx data
wx1 <- cbind.data.frame(wx$Date,wx$Time,wx$Precip,wx$Wind,wx$Wind.1,wx$Av.Air,wx$Fuel,wx$Rel,wx$Fuel.1,wx$Dir,wx$Mx.Gust,wx$Solar,wx$Join)
names(wx1)<-c("date","time","precip_mm","wind_ms","wind_dir","air_c","fuel_t","rh","fuel","dir","mx_gust","solar","Join")
str(wx1)
##convert factor to number
#indx <- sapply(wx1, is.factor)
#wx1[indx] <- lapply(wx1[indx], function(x) as.numeric(as.character(x)))
wx1$wind_ms <- as.numeric(as.character(wx1$wind_ms))
wx1$wind_dir <- as.numeric(as.character(wx1$wind_dir))
wx1$air_c <- as.numeric(as.character(wx1$air_c))
wx1$fuel_t <- as.numeric(as.character(wx1$fuel_t))
wx1$rh <- as.numeric(as.character(wx1$rh))
wx1$dir <- as.numeric(as.character(wx1$dir))
wx1$mx_gust <- as.numeric(as.character(wx1$mx_gust))
wx1$solar <- as.numeric(as.character(wx1$solar))
wx1$wind_ms <- as.numeric(as.character(wx1$wind_ms))
wx1$date <- as.Date(as.character(wx1$date))

str(wx1)


###combine AIC data with weather data
combine <- merge(x = SUBALL, y = wx1, by = "Join", all.x = TRUE, all.y = FALSE)
str(combine)


##Data Exploration
xyplot(ACIout~Date2 | Sites, data = combine,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))
###Data Exploration
xyplot(ACIout~Mo | Sites, data = combine,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))


#######
ggplot(combine, aes(x=wind_ms, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Wind speed",
       x="Wind speed (ms/s)", y = "ACI")+
  theme_classic()  



#plot(combine$rh,combine$ACIout)

ggplot(combine, aes(x=rh, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs RH",
       x="Relative Humidity (%)", y = "ACI")+
  theme_classic()  


ggplot(combine, aes(x=wind_wd, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Wind Direction",
       x="Wind direction (deg)", y = "ACI")+
  theme_classic()  


ggplot(combine, aes(x=fuel_t, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Fuel Temperature",
       x="Fuel Temp", y = "ACI")+
  theme_classic()

str(combine)
#dataframe for correlation plot
mydata <- combine[, c(5,6,7,8,9,10,11,13,15,17,74,75,76,77,78,79, 81,82,83)]
###take out NA
mydata<-na.omit(mydata)
str(mydata)
#dorrelation for dataframe
M<-cor(mydata)
#plot relationships to inform global model
corrplot(M)

##example global models
##need added variables on days since fire, hourly precip, time since sunrise, etc, interaction term 
## of time since sunrise|Treatment
##first two examples with temporal autocorrelation structure
m1 <- gamm(ACIout ~ s(Treatment)+s(Date2)+s(BKdBA_low)+s(wind_ms)+s(solar)+s(air_c), data = master, 
           correlation = corCAR1(value = 0.5, form = ~ Date2 | Sites))
m2 <- gamm(ACIout ~ s(Treatment), data = master, 
           correlation = corCAR1(value = 0.5, form = ~ Date2 | Sites))
##without autocorrelation structure
m3 <- gamm(ACIout ~ Treatment+Date2+Yr+Mo+Hr+Min+BKdBA_low+wind_ms+solar+air_c,random = list(Sites=~1), data = master)


summary(m3$lme)
summary(m3$gam)



