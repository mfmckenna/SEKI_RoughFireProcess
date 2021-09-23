rm(list=ls())

#This code preps the data for the statistical analysis of the rough fire soundscape analysis
# combines the data with weather (RAWLS, ibutton) and calculates variables from the data. Some graphics 
# are produced to better understand the data- these plots are not saved.

#Read output R data table into:
# 11b_manuscript_explorePlots.R
# 11c_manuscript_modelBuild.R

#created by E. Meyer and M. McKenna, 2020

#--------------------------------------------------
## SET directory with data to read in
dataDir = "D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\"

#--------------------------------------------------
##Load Libraries
library(lubridate)
library(readxl)
library(Hmisc)
library(ggplot2)
library(tidyr)
library(dplyr)
library(chron)
library(MuMIn)
library(lme4)
library(zoo) 
library(heatmaply)
library(svDialogs)
library(varhandle)
library(xts)
library(DataCombine)
library(stringr)
library(GGally)
library(lattice)
library(mgcv)
library(corrplot)
library(RColorBrewer)
library(plm)

#--------------------------------------------------
## READ IN ALL ACOUSTIC INDEX FILES
AIDir =  paste0(dataDir, "acousticindices" )
AIFiles = list.files(AIDir, pattern="Subsample", recursive=F, full.names=T)

#INFO FILES
#basename(AIFiles)
mySites = unique( sapply(strsplit(basename(AIFiles), "_"), head, 1) )
numSites = length(mySites)

#COMBINE INDEX
SUBALL1 = do.call(rbind,lapply(AIFiles,read.csv)) # ALL = d

#SIMPLE changes to data matrix
indices = colnames(SUBALL1)
fileInfo = indices[1:10]
Indexnames = indices[11:length(SUBALL1)]

#CONVERT data type
SUBALL1$Sites = as.character(SUBALL1$Site)
SUBALL1$Date2 = as.Date(SUBALL1$Date, format="%Y_%m_%d")
SUBALL1$Date3 = gsub("_","",SUBALL1$Date)
metric = "Acoustic Complexity Index" #metric to plot

#SET Treatments
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

#--------------------------------------------------
## PULL out bad days due to equipment malfunction
#Flag Site 80 from 2018/01/18 through 2018/05/03
#Flag Site 30 from 2018/04/07 through 2018/05/23
#Create Flag Field
SUBALL1$Flag=ifelse(SUBALL1$Sites==80 & SUBALL1$Date2 >= "2018/01/18" & SUBALL1$Date2 <= "2018-05-03","F",
                    ifelse(SUBALL1$Sites==30 & SUBALL1$Date2 >= "2018/04/07" & SUBALL1$Date2 <= "2018-05-23","F",""))

#Create common field to relate date time of RAWS Park Ridge Fire Weather Data
SUBALL1$Join=paste(SUBALL1$Date3,SUBALL1$Hr, sep ="_")
#subset out Flagged dataSS
SUBALL=subset(SUBALL1, Flag !='F')

#--------------------------------------------------
## REMOVE Indices not used in analysis
#SUBALL = cbind(SUBALL[,1:16],SUBALL[,57:71])
SUBALL = SUBALL[ c("Site", "SiteDay", "SiteDay2" ,
               "Date", "Date3", "DY", "Date2", "hms", "Yr","Mo",'Day','Hr', 'Min', 'Sec',
               'Treatment', 
               'ACIout', 'BKdB_low', 'BKdBA_low', 'BKdB_bird', 'BKdBA_bird', "AR", 
               "Sunrise", "Sunset",
               "TOD", "Join") ]
#--------------------------------------------------
## READ in weather data- RAWS
wx = read.csv(paste0(dataDir, "weatherRAWS\\parkridge_raws_master.csv"), stringsAsFactors=FALSE) 

#CREATE new fields for joined field
wx$Date1 <- gsub("/","",wx$Date)
wx$Time1 <- gsub(":00","",wx$Time)
wx$Join<-paste(wx$Date1,wx$Time1, sep ="_")

#MAKE better names in wx data
wx1 <- cbind.data.frame(wx$Date,wx$Time,wx$Precip,wx$Wind,wx$Wind.1,wx$Av.Air,wx$Fuel,wx$Rel,wx$Fuel.1,wx$Dir,wx$Mx.Gust,wx$Solar,wx$Join)
names(wx1)<-c("date","time","precip_mm","wind_ms","wind_dir","air_c","fuel_t","rh","fuel","dir","mx_gust","solar","Join")
# str(wx1)

#CONVERT factor to number
#indx <- sapply(wx1, is.factor)
#wx1[indx] <- lapply(wx1[indx], function(x) as.numeric(as.character(x)))
#?what does 'M" mean? whatever it means it becomes NA in this analysis...
sort( unique( as.character(wx1$wind_ms) ) )
wx1$wind_ms  <- as.numeric(as.character(wx1$wind_ms)) 
sum ( is.na (wx1$wind_ms ) )

unique( as.character(wx1$wind_dir) )
wx1$wind_dir <- as.numeric(as.character(wx1$wind_dir))
sum ( is.na (wx1$wind_dir ) )

unique( as.character(wx1$air_c) )
wx1$air_c    <- as.numeric(as.character(wx1$air_c))
sum ( is.na (wx1$air_c ) )

unique( as.character(wx1$fuel_t) )
wx1$fuel_t   <- as.numeric(as.character(wx1$fuel_t))
sum ( is.na (wx1$fuel_t ) )

unique( as.character(wx1$rh) )
wx1$rh       <- as.numeric(as.character(wx1$rh))
sum ( is.na (wx1$rh ) )

unique( as.character(wx1$dir) )
wx1$dir      <- as.numeric(as.character(wx1$dir))
sum ( is.na (wx1$dir ) )

unique( as.character(wx1$mx_gust) )
wx1$mx_gust  <- as.numeric(as.character(wx1$mx_gust))
sum ( is.na (wx1$mx_gust ) )

unique( as.character(wx1$solar) )
wx1$solar    <- as.numeric(as.character(wx1$solar))
sum ( is.na (wx1$solar ) )

wx1$date     <- as.Date(as.character(wx1$date))
sum ( is.na (wx1$date ) ) 

wx1$precip_mm <- as.numeric(as.character(wx1$precip_mm))
sum ( is.na (wx1$precip_mm ) ) 

#--------------------------------------------------
## ADD WEATHER VARIABLES- RAWS

#CALCULATE precipitation values from the data
#FIND where value changes by more than... use graphic 
plot(wx1$precip_mm)
tmp = ( abs( diff(wx1$precip_mm)) )
idx  = which (tmp>100) #wx1$precip_mm[idx[1] : idx[1] ]      
#CALCULATE the differences from the previous hour within each group... assumes every hour sampled (is this true?)
precp_cal1 = diff(wx1$precip_mm[ (1)  : (idx[1]) ] ) 
plot(precp_cal1)
length(precp_cal1)
precp_cal2 = diff(wx1$precip_mm[(idx[1]+1) : (idx[2]) ] ) 
precp_cal3 = diff(wx1$precip_mm[(idx[2]+1) : (idx[3]) ] ) 
#min(precp_cal3)-- not sure why there is a negative value here (-1)- i am going to make it NA since it only happens once
precp_cal3[which(precp_cal3<0)] = NA
precp_cal4 = diff(wx1$precip_mm[(idx[3]+1) : (idx[4]) ] ) 
precp_cal5 = diff(wx1$precip_mm[(idx[4]+1) : (idx[5]) ] ) 
precp_cal6 = diff(wx1$precip_mm[(idx[5]+1) : nrow(wx1) ] ) 
#COMBINE matrix
precp_cal= c(NA,precp_cal1,NA,precp_cal2,NA, precp_cal3,NA, precp_cal4, NA, precp_cal5, NA, precp_cal6)
plot(precp_cal)
#CHECK length(precp_cal) - nrow(wx1)
wx1 = cbind(wx1,precp_cal)
#ADD rain column- yes/no
wx1$Rain = ifelse ( wx1$precp_cal > 0 ,1, 0 ) 
#boxplot(combine$precp_cal~ combine$Rain )
#ADD Rain previous hour column- yes/no
wx1$RainPrev = shift( wx1$Rain, -1 )

#LOOK at relationship between rel humidity and precipitation
plot(as.numeric( as.character(wx1$rh)) ,wx1$precp_cal)
library(corrplot)
cat_df <- wx1[, c("precp_cal", "rh", "solar","fuel_t", "Rain")]
str(cat_df)
tst = cat_df[complete.cases(cat_df), ]
removed = nrow(cat_df) - nrow( cat_df[!complete.cases(cat_df), ]) 
corrplot(cor(tst), method="number", type = "lower")

p1 = qplot(rh,     precp_cal, data = cat_df, color = Rain)
p2 = qplot(fuel_t, precp_cal, data = cat_df, color = Rain)
p3 = qplot(solar,  precp_cal, data = cat_df, color = Rain)
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)

#--------------------------------------------------
## COMBINE Acoutic Index data with weather data
combine=merge(x = SUBALL, y = wx1, by = "Join", all.x = TRUE, all.y = FALSE)
unique(combine$precp_cal)
# str(combine)

#--------------------------------------------------
#CHECKS: some outliers- combine with acoustic data and then see if still there
# tst = combine[(combine$precp_cal >= 20 & !is.na(combine$precp_cal)), 70:86]
# APRIL 4, 2019 LOTS OF RAIN, LOW SOLAR AND HIGH HUMIDITY, NOT FUEL DATA... so keep this day

#Check: which weather for too many NAs
# names(wx1)<-c("date","time","precip_mm","wind_ms","wind_dir","air_c","fuel_t","rh","fuel","dir","mx_gust","solar","Join")
sum ( is.na (combine$precip_mm ) ) /nrow(combine) #2977
sum ( is.na (combine$wind_ms ) )   /nrow(combine)#12253 
sum ( is.na (combine$wind_dir ) )  /nrow(combine) #12401 
sum ( is.na (combine$air_c ) )     /nrow(combine) #3227 
sum ( is.na (combine$fuel_t ) )    /nrow(combine)#28867 
sum ( is.na (combine$rh ) )        /nrow(combine)#3413
sum ( is.na (combine$mx_gust ) )   /nrow(combine)#13018
sum ( is.na (combine$solar ) )     /nrow(combine) #3796

#Check: which(combine$Rain >0 )
# combine[12850 :12863 , 80:86]
# combine[1 :10 , 80:86]

#Check: how are NAs distribute across years- is there one year with fuel and wind data to do a sub-analysis?
# max(combine$fuel_t,na.rm = T)
combine$fuel_t [is.na(combine$fuel_t) ] = 999
ggplot(combine, aes(x = Date2, y = fuel_t,  color = as.factor(Yr) ) ) + 
  geom_point()
# looks like 2016 is okay- all other years missing lots of data!

max(combine$wind_ms,na.rm = T)
combine$wind_ms [is.na(combine$wind_ms) ] = 99
ggplot(combine, aes(x = Date2, y = wind_ms,  color = as.factor(Yr) ) ) + 
  geom_point()
# looks like 2016 and 2018 are okay for wind speed measurements

#--------------------------------------------------
## ADD WEATHER VARIABLES- iBUTTON
iweather = read.csv(paste0(dataDir, "ibutton\\RoughFire_temperature_Master.csv"), stringsAsFactors=FALSE) 
ttmp = substr(gsub(" ", "_", gsub(":","", gsub("-","", iweather$Time) )) , 1, 13)
#Join: SITE_YYYYMMDD_HHMM
iweather$Join2 = paste(iweather$ID, ttmp,sep ="_")
# unique(combine$Min) unique(combine$Hr)
combine$Min[combine$Min == "0"] = "00"
combine$Hr[combine$Hr == "4"] = "04"
combine$Hr[combine$Hr == "5"] = "05"
combine$Hr[combine$Hr == "6"] = "06"
combine$Hr[combine$Hr == "7"] = "07"
combine$Hr[combine$Hr == "8"] = "08"
combine$Hr[combine$Hr == "9"] = "09"
ttmp = paste(combine$Hr, (combine$Min), sep="") #get time in correct format
combine$Join2  = paste(combine$Site,combine$Date3, substr(ttmp,1,4), sep ="_")

#NOTE: no data for every 10 min sample, every 30 minute sample-- solution: 
# apply same temp to all samples within that 30 min
# so, 00 sample for 00,10,20 AND 30 sample for 30,40,50 HOW DO I DO THIS?
tmp = substr( combine$Join2, 15,16)
# unique(tmp)
tmp[tmp == "10"] = "00"
tmp[tmp == "20"] = "00"
tmp[tmp == "40"] = "30"
tmp[tmp == "50"] = "30"
# unique(tmp)
combine$Join2 = paste0( substr(combine$Join2,1,14), tmp)
combine2 = merge(x = combine, y = iweather, by = "Join2", all.x = TRUE, all.y = FALSE)
combine = combine2

#--------------------------------------------------
## CALCULATE- time since sunrise, in minutes-- get in correct format of just time
#positive is after, negative is before
# NOTE: lubbridate did not work becaus gave results in mins or hours depending on the difference... annoying!
combine =as.data.frame(combine)
tst = NULL
for (ii in 1:nrow(combine)){ #this takes FOREVER-- not sure how to make it run faster the matrix is HUGE!
  tmpSunrise = as.POSIXct( unlist(combine$Sunrise[ii]),format = "%Y-%m-%d %H:%M:%S" ,tz ="GMT" )
  tmp = paste(gsub("_","-",combine$Date[ii]), combine$hms[ii])
  tmpCurrent = as.POSIXct( tmp, format = "%Y-%m-%d %H:%M:%S" ,tz ="GMT"  )
  
  cat("ii = ",ii,"\n") #make sure it is running... pushing the limits of my surface
  tst = rbind( tst, c( as.character(paste(gsub("_","-",combine$Date[ii]), combine$hms[ii] ) ),
                       as.character( combine$Sunrise[ii]), 
                       as.numeric( difftime(  as.POSIXct(tmpCurrent), as.POSIXct(tmpSunrise), units="mins") )
  ) 
  )
  rm(tmp,tmpCurrent,tmpSunrise)
}
combine$timeSinceSunrise = tst[,3]
rm(tst,ii)
# combine$minsSinceSunrise = combine$timeSinceSunrise

#--------------------------------------------------
## CALCULATE- days since fire
fireOut = as.POSIXct( "2015-11-06", format = "%Y-%m-%d",tz ="GMT")
for (ii in 1:nrow(combine)){
  tmpDay  = as.POSIXct( unlist(combine$Date[ii]),format = "%Y_%m_%d" ,tz ="GMT" )
  combine$daySinceFire[ii] = as.numeric( difftime(  as.POSIXct(tmpDay), as.POSIXct(fireOut), units="days") )
  cat("ii = ",ii,"\n")
  rm(tmpDay)
}
#plot(combine$daySinceFire)

#--------------------------------------------------
## ADD -julian day to matrix
combine$julDay = as.numeric(julian.Date(combine$Date) )
unique( combine$Site )
#check: is site 30 still included?

#plot(combine$julDay)

#SAVE OUT BIG MATRIX
dateCreate = Sys.Date()
save( combine, file = paste0(dataDir,"SEKI_CombinedDataSet_",dateCreate) )

# #--------------------------------------------------
# ## Data Exploration
# #ACI vs Rainfall
# ggplot(combine, aes(Date2, ACIout), color=as.factor(Rain)) +
#   geom_point()
# rn = combine[(combine$precp_cal > 0 & !is.na(combine$precp_cal)),]
# plot(rn$ACIout,rn$precp_cal)
# 
# xyplot(ACIout~Date2 | Sites, data = combine,
#        xlab = "Time", col = 1, type = "h",
#        strip = function (bg = 'white', ...)
#          strip.default(bg = 'white', ...))
# ###Data Exploration
# xyplot(ACIout~Mo | Sites, data = combine,
#        xlab = "Time", col = 1, type = "h",
#        strip = function (bg = 'white', ...)
#          strip.default(bg = 'white', ...))
# 
# 
# #######
# ggplot(combine, aes(x=wind_ms, y=ACIout, color=Treatment)) + 
#   geom_point() + 
#   geom_smooth(method = 'loess') +
#   labs(title="ACI vs Wind speed",
#        x="Wind speed (ms/s)", y = "ACI")+
#   theme_classic()  
# 
# 
# 
# #plot(combine$rh,combine$ACIout)
# 
# ggplot(combine, aes(x=rh, y=ACIout, color=Treatment)) + 
#   geom_point() + 
#   geom_smooth(method = 'loess') +
#   labs(title="ACI vs RH",
#        x="Relative Humidity (%)", y = "ACI")+
#   theme_classic()  
# 
# 
# ggplot(combine, aes(x=wind_wd, y=ACIout, color=Treatment)) + 
#   geom_point() + 
#   geom_smooth(method = 'loess') +
#   labs(title="ACI vs Wind Direction",
#        x="Wind direction (deg)", y = "ACI")+
#   theme_classic()  
# 
# 
# ggplot(combine, aes(x=fuel_t, y=ACIout, color=Treatment)) + 
#   geom_point() + 
#   geom_smooth(method = 'loess') +
#   labs(title="ACI vs Fuel Temperature",
#        x="Fuel Temp", y = "ACI")+
#   theme_classic()
# 
# str(combine)
# #dataframe for correlation plot
# mydata <- combine[, c(5,6,7,8,9,10,11,13,15,17,74,75,76,77,78,79, 81,82,83)]
# ###take out NA
# mydata<-na.omit(mydata)
# str(mydata)
# #dorrelation for dataframe
# M<-cor(mydata)
# #plot relationships to inform global model
# corrplot(M)
# 
# #--------------------------------------------------
# ##example global models
# ##need added variables on days since fire, hourly precip, time since sunrise, etc, interaction term 
# ## of time since sunrise|Treatment
# ##first two examples with temporal autocorrelation structure
# m1 <- gamm(ACIout ~ s(Treatment)+s(Date2)+s(BKdBA_low)+s(wind_ms)+s(solar)+s(air_c), data = master, 
#            correlation = corCAR1(value = 0.5, form = ~ Date2 | Sites))
# m2 <- gamm(ACIout ~ s(Treatment), data = master, 
#            correlation = corCAR1(value = 0.5, form = ~ Date2 | Sites))
# ##without autocorrelation structure
# m3 <- gamm(ACIout ~ Treatment+Date2+Yr+Mo+Hr+Min+BKdBA_low+wind_ms+solar+air_c,random = list(Sites=~1), data = master)
# 
# 
# summary(m3$lme)
# summary(m3$gam)
# 
# 
# 
