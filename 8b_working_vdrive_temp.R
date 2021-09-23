rm(list=ls())
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
###Load All ACI Data-This data doesn't adjust for time mistakes and includes all data
AIDir =  "V:\\SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex" 
AIFiles <- list.files(AIDir, pattern="AcousticIndex", recursive=F, full.names=T)

# INFO FILES-
basename(AIFiles)
mySites = unique( sapply(strsplit(basename(AIFiles), "_"), head, 1) )
numSites = length(mySites)

#COMBINE INDEX- 
ALL1 <- do.call(rbind,lapply(AIFiles,read.csv)) # ALL = d

# SIMPLE changes to data matrix
indices = colnames(ALL1)
fileInfo = indices[1:10]
Indexnames = indices[11:length(ALL1)]

ALL1$Sites = as.character(ALL1$Site)
ALL1$Date2 <- as.Date(ALL1$Date, format="%Y_%m_%d")
ALL1$Date3 <- gsub("_","",ALL1$Date)
metric = "Acoustic Complexity Index" #metric to plot

ALL1$Treatment <- ifelse(ALL1$Site==10, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                        ifelse(ALL1$Site==20, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                               ifelse(ALL1$Site==30, "Old Growth (Yes) \n Rough (No) \n Prescribed (No)",
                                      ifelse(ALL1$Site==40, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                             ifelse(ALL1$Site==50, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                                    ifelse(ALL1$Site==60, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                           ifelse(ALL1$Site==70, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                                  ifelse(ALL1$Site==80, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                         ifelse(ALL1$Site==90, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                                NA  ))))))))) # all other values map to NA
myTreatments = unique(ALL1$Treatment)


####Pull out bad days due to equipment malfunction
#Flag Site 80 from 2018/01/18 through 2018/05/03
#Flag Site 30 from 2018/04/07 through 2018/05/23
#Create Flag Field
ALL1$Flag <- ifelse(ALL1$Sites==80 & ALL1$Date2 >= "2018/01/18" & ALL1$Date2 <= "2018-05-03","F",
                   ifelse(ALL1$Sites==30 & ALL1$Date2 >= "2018/04/07" & ALL1$Date2 <= "2018-05-23","F",""))

#Create common field to relate date time of RAWS Park Ridge Fire Weather Data
ALL1$Join <- paste(ALL1$Date3,ALL1$Hr, sep ="_")
#subset out Flagged data
ALL<-subset(ALL1, Flag !='F')






###############################


#begin date of audio 05/06/2018
#First download 09/05/2018
# Step 1: Open website
url<-"https://wrcc.dri.edu/cgi-bin/wea_list.pl?caCPKR"
browseURL(url)
# Step 2: Set starting date
#         Open log and grab day after last download end date
#         This is the date to use
log<-read.csv("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Weather\\Log\\RAWS_Log.csv")
StartDate<-as.Date(log$EndDate)+1
print(StartDate)
# Step 3: Set ending date
#         Set to date of last available ACI

# Step 4: Write info to download log
# Step 5: input password Fire2010
#         Data Format: .xls
#         Data Source= Original
#         Missing data = M
#         Data flats = Yes
#         Date format = YYYY-MM-DD hh:mm
#         Time format = LST 0-23
#         Table Header = Column header short descriptions
#         Field Delimiter = comma (,)
#         Select Units = Metric
#Step 7: Open downloaded data

##Read in weather data
wx<-read.csv("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Weather\\RAWS\\parkridge_raws_master.csv", stringsAsFactors=FALSE)
#Create new fields for joined field
wx$Date1 <- gsub("/","",wx$Date)
wx$Time1 <- gsub(":00","",wx$Time)
wx$Join<-paste(wx$Date1,wx$Time1, sep ="_")

#make better names in wx data
wx1 <- cbind.data.frame(wx$Date,wx$Time,wx$Precip,wx$Wind,wx$Wind.1,wx$Av.Air,wx$Fuel,wx$Rel,wx$Fuel.1,wx$Dir,wx$Mx.Gust,wx$Solar,wx$Join)
names(wx1)<-c("date","time","precip_mm","wind_ms","wind_dir","air_c","fuel_t","rh","fuel","dir","mx_gust","solar","Join")

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
unfactor(Precip)


###combine AIC data with weather data
combine <- merge(x = ALL, y = wx1, by = "Join", all.x = TRUE, all.y = FALSE)
str(combine)





#### Now we bring in the understory temperature data from ibuttons####
data<-read.csv("J:/seki/park_programs/Data/air_meteorology/Soundscapes/RoughFire_GG/data/tabular/ibuttons/data/working/pivot/RoughFire_temperature_Master.csv")
str(data)
data$Time <- ymd_hms(data$Time)
str(data)
#create data frame with important var
data1<-cbind.data.frame(data$ID, data$Time,data$TempC,data$Place)
#filter for low ibuttons for understory temp
data2<-subset(data1, data$Place=="Low")
#filter for only the top of the hour data, reducing 30 m measurements ot just the hourly
data3<-grepl.sub(data=data2,pattern="00:00",Var="data$Time")
#now create the join field
names(data3)<-c("site","time","temp_us","place")
#creatfield
data3$join <- gsub("-","",data3$time)
#now 
data3$join <- gsub(" ","_",data3$join)
#remove right side of hour
data3$hr<-str_sub(data3$join, -8, -7)
#now remove zeros

data3$hr1<-ifelse(str_sub(data3$hr, -2, -2)==0,gsub("0","",data3$hr),
                  str_sub(data3$hr, -2, -1))
#now add one zero to blank
data3$hr1[data3$hr1=='']<-'0'
# now make the join field
data3$temp_aci<-paste(str_sub(data3$join,0,8),"_",data3$hr1,"_",data3$site,sep="")


#### ####
###Now we will make a join field from combine (wx from Raws station, and indeces) to data3 (temperature understory)
combine$temp_aci<-paste(combine$Join,"_",combine$Site,sep="")

###Now we join combine with temperature understory

master<-merge(x=combine,y=data3,by="temp_aci",all.x = TRUE, all.y = FALSE)
###Make temp residual (raws station minus understory temp)
master$tempresid<-master$air_c-master$temp_us



###Data Exploration
xyplot(ACIout~Date2 | Sites, data = master,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))
###Data Exploration
xyplot(ACIout~time.y | Sites, data = master,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))

master1<-na.omit(master)
m2 <- gamm(ACIout ~ s(Treatment)+s(time.y)+s(BKdBA_low)+s(wind_ms)+s(solar)+s(air_c)+s(temp_us), data = master, 
           correlation = corCAR1(value = 0.5, form = ~ time.y | Sites))
m2 <- gamm(ACIout ~ s(Treatment), data = master1, 
           correlation = corCAR1(value = 0.5, form = ~ time.y | Sites))
m2 <- gamm(ACIout ~ Treatment+time.y+Yr+Mo,random = list(Sites=~1), data = master)
summary(m2$lme)
summary(m2$gam)

str(master)

#######
#plot(combine$precip_mm,combine$ACIout)
ggplot(master, aes(x=precip_mm, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Precip",
       x="Precipitation (mm)", y = "ACI")+
  theme_classic()  

str(master)

#plot(combine$rh,combine$ACIout)

ggplot(master, aes(x=rh, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs RH",
       x="Relative Humidity (%)", y = "ACI")+
  theme_classic()  


ggplot(master, aes(x=wind_ms, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Wind Speed",
       x="Wind speed (m/s)", y = "ACI")+
  theme_classic()  

#plot(combine$wind_ms,combine$ACIout)

ggplot(master, aes(x=temp_us, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs understory temperature",
       x="Understory Temperature (C)", y = "ACI")+
  theme_classic()  

ggplot(master, aes(x=air_c, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs area temperature",
       x="Area Temperature (C)", y = "ACI")+
  theme_classic()  

ggplot(master, aes(x=tempresid, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs understory temperature",
       x="difference in ridge temp to understory temperature (C)", y = "ACI")+
  theme_classic()  

p <- ggplot(master, aes(temp_us, ACIout))
p + geom_bin2d(bins=30)


p <- ggplot(master, aes(air_c, ACIout))
p + geom_bin2d()

p
p <- ggplot(master, aes(tempresid, ACIout))
p + geom_bin2d()

p <- ggplot(master, aes(fuel_t, ACIout))
p + geom_bin2d(bins=30)
######################################################################################################################
########Load All ACI Adjusted Data-This data adjust time for daylight savings and time mistakes#####################
########################################################################################################################
##########################################################################################################################
AIDir =  "V:\\SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex\\adjValues" 
AIFiles <- list.files(AIDir, pattern="DataAdjAI", recursive=F, full.names=T)

# INFO FILES-
basename(AIFiles)
mySites = unique( sapply(strsplit(basename(AIFiles), "_"), head, 1) )
numSites = length(mySites)

#COMBINE INDEX- 
ALLadj <- do.call(rbind,lapply(AIFiles,read.csv)) # ALL = d

# SIMPLE changes to data matrix
indices = colnames(ALLadj)
fileInfo = indices[1:10]
Indexnames = indices[11:length(ALLadj)]

ALLadj$Sites = as.character(ALLadj$Site)
ALLadj$Date2 <- as.Date(ALLadj$Date, format="%Y_%m_%d")
ALLadj$Date3 <- gsub("_","",ALLadj$Date)
metric = "Acoustic Complexity Index" #metric to plot

ALLadj$Treatment <- ifelse(ALLadj$Site==10, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                         ifelse(ALLadj$Site==20, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                                ifelse(ALLadj$Site==30, "Old Growth (Yes) \n Rough (No) \n Prescribed (No)",
                                       ifelse(ALLadj$Site==40, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                              ifelse(ALLadj$Site==50, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                                     ifelse(ALLadj$Site==60, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                            ifelse(ALLadj$Site==70, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                                   ifelse(ALLadj$Site==80, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                          ifelse(ALLadj$Site==90, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                                 NA  ))))))))) # all other values map to NA
myTreatments = unique(ALLadj$Treatment)


####Pull out bad days due to equipment malfunction
#Flag Site 80 from 2018/01/18 through 2018/05/03
#Flag Site 30 from 2018/04/07 through 2018/05/23
#Create Flag Field
ALLadj$Flag <- ifelse(ALLadj$Sites==80 & ALLadj$Date2 >= "2018/01/18" & ALLadj$Date2 <= "2018-05-03","F",
                    ifelse(ALLadj$Sites==30 & ALLadj$Date2 >= "2018/04/07" & ALLadj$Date2 <= "2018-05-23","F",""))

#Create common field to relate date time of RAWS Park Ridge Fire Weather Data
ALLadj$Join <- paste(ALLadj$Date3,ALLadj$Hr, sep ="_")
#subset out Flagged data
ALLdata<-subset(ALLadj, Flag !='F')


#group data first

jpeg("AIC_allsitesscatter.jpg",res = 500,height=1800, width=3200)

by_siteDate <- group_by(ALLdata, Treatment, Date2)
delay <- summarise(by_siteDate,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))
pALL = ggplot(delay, aes(Date2, ACI, group = Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = "Daily Average with loess smooth")
pALL

by_siteDate <- group_by(ALLdata, Sites, Date2)
delay <- summarise(by_siteDate,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))
pALL2 = ggplot(delay, aes(Date2, ACI, group = Sites, color = Sites)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = "Daily Average with loess smooth")
pALL2 

dev.off()




###############################


#begin date of audio 05/06/2018
#First download 09/05/2018
# Step 1: Open website
url<-"https://wrcc.dri.edu/cgi-bin/wea_list.pl?caCPKR"
browseURL(url)
# Step 2: Set starting date
#         Open log and grab day after last download end date
#         This is the date to use
log<-read.csv("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Weather\\Log\\RAWS_Log.csv")
StartDate<-as.Date(log$EndDate)+1
print(StartDate)
# Step 3: Set ending date
#         Set to date of last available ACI

# Step 4: Write info to download log
# Step 5: input password Fire2010
#         Data Format: .xls
#         Data Source= Original
#         Missing data = M
#         Data flats = Yes
#         Date format = YYYY-MM-DD hh:mm
#         Time format = LST 0-23
#         Table Header = Column header short descriptions
#         Field Delimiter = comma (,)
#         Select Units = Metric
#Step 7: Open downloaded data

##Read in weather data
wx<-read.csv("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Weather\\RAWS\\parkridge_raws_master.csv", stringsAsFactors=FALSE)
#Create new fields for joined field
wx$Date1 <- gsub("/","",wx$Date)
wx$Time1 <- gsub(":00","",wx$Time)
wx$Join<-paste(wx$Date1,wx$Time1, sep ="_")

#make better names in wx data
wx1 <- cbind.data.frame(wx$Date,wx$Time,wx$Precip,wx$Wind,wx$Wind.1,wx$Av.Air,wx$Fuel,wx$Rel,wx$Fuel.1,wx$Dir,wx$Mx.Gust,wx$Solar,wx$Join)
names(wx1)<-c("date","time","precip_mm","wind_ms","wind_dir","air_c","fuel_t","rh","fuel","dir","mx_gust","solar","Join")

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
unfactor(Precip)


###combine AIC data with weather data
combine <- merge(x = ALL, y = wx1, by = "Join", all.x = TRUE, all.y = FALSE)
str(combine)





#### Now we bring in the understory temperature data from ibuttons####
data<-read.csv("J:/seki/park_programs/Data/air_meteorology/Soundscapes/RoughFire_GG/data/tabular/ibuttons/data/working/pivot/RoughFire_temperature_Master.csv")
str(data)
data$Time <- ymd_hms(data$Time)
str(data)
#create data frame with important var
data1<-cbind.data.frame(data$ID, data$Time,data$TempC,data$Place)
#filter for low ibuttons for understory temp
data2<-subset(data1, data$Place=="Low")
#filter for only the top of the hour data, reducing 30 m measurements ot just the hourly
data3<-grepl.sub(data=data2,pattern="00:00",Var="data$Time")
#now create the join field
names(data3)<-c("site","time","temp_us","place")
#creatfield
data3$join <- gsub("-","",data3$time)
#now 
data3$join <- gsub(" ","_",data3$join)
#remove right side of hour
data3$hr<-str_sub(data3$join, -8, -7)
#now remove zeros

data3$hr1<-ifelse(str_sub(data3$hr, -2, -2)==0,gsub("0","",data3$hr),
                  str_sub(data3$hr, -2, -1))
#now add one zero to blank
data3$hr1[data3$hr1=='']<-'0'
# now make the join field
data3$temp_aci<-paste(str_sub(data3$join,0,8),"_",data3$hr1,"_",data3$site,sep="")


#### ####
###Now we will make a join field from combine (wx from Raws station, and indeces) to data3 (temperature understory)
combine$temp_aci<-paste(combine$Join,"_",combine$Site,sep="")

###Now we join combine with temperature understory

master<-merge(x=combine,y=data3,by="temp_aci",all.x = TRUE, all.y = FALSE)
###Make temp residual (raws station minus understory temp)
master$tempresid<-master$air_c-master$temp_us



###Data Exploration
xyplot(ACIout~Date2 | Sites, data = master,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))
###Data Exploration
xyplot(ACIout~time.y | Sites, data = master,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))

master1<-na.omit(master)
m2 <- gamm(ACIout ~ s(Treatment)+s(time.y)+s(BKdBA_low)+s(wind_ms)+s(solar)+s(air_c)+s(temp_us), data = master, 
           correlation = corCAR1(value = 0.5, form = ~ time.y | Sites))
m2 <- gamm(ACIout ~ s(Treatment), data = master1, 
           correlation = corCAR1(value = 0.5, form = ~ time.y | Sites))
m2 <- gamm(ACIout ~ Treatment+time.y+Yr+Mo,random = list(Sites=~1), data = master)
summary(m2$lme)
summary(m2$gam)

str(master)

#######
#plot(combine$precip_mm,combine$ACIout)
ggplot(master, aes(x=precip_mm, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Precip",
       x="Precipitation (mm)", y = "ACI")+
  theme_classic()  

str(master)

#plot(combine$rh,combine$ACIout)

ggplot(master, aes(x=rh, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs RH",
       x="Relative Humidity (%)", y = "ACI")+
  theme_classic()  


ggplot(master, aes(x=wind_ms, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Wind Speed",
       x="Wind speed (m/s)", y = "ACI")+
  theme_classic()  

#plot(combine$wind_ms,combine$ACIout)

ggplot(master, aes(x=temp_us, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs understory temperature",
       x="Understory Temperature (C)", y = "ACI")+
  theme_classic()  

ggplot(master, aes(x=air_c, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs area temperature",
       x="Area Temperature (C)", y = "ACI")+
  theme_classic()  

ggplot(master, aes(x=tempresid, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs understory temperature",
       x="difference in ridge temp to understory temperature (C)", y = "ACI")+
  theme_classic()  

p <- ggplot(master, aes(temp_us, ACIout))
p + geom_bin2d(bins=30)


p <- ggplot(master, aes(air_c, ACIout))
p + geom_bin2d()

p
p <- ggplot(master, aes(tempresid, ACIout))
p + geom_bin2d()

p <- ggplot(master, aes(fuel_t, ACIout))
p + geom_bin2d(bins=30)



