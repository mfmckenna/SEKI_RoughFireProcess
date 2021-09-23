# this code reads in output file of 11a and combines the data into severity and daily mean as imput into 11d

rm(list=ls())

dataDir = "D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\"
iFile = choose.files(caption= "Open: SEKI_CombinedDataSet_2020-06-09") #SEKI_CombinedDataSet_2020-06-09
load(iFile)

library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)

#--------------------------------------------------
## PREPARE INPUT DATA FOR MODELS

#REMOVE site 30
#combine = combine[combine$Site != "30",]

#--------------------------------------------------
#FIX Julian Day
combine$julDay = yday( as.Date(combine$Date2) )
combine$DateAll = ymd_hms(paste(combine$Date2,combine$Hr,combine$Min,combine$Sec))

#--------------------------------------------------
#SIMPLIFY data matrix to only variables we will use
allData = combine
# already modifiend this in 11a code to simplify input
#allData = combine[, c(3,70,59,61,62,5,6,7,8,9,10,13,15,17,74,75,76,77,78,79,81,82,83,84,85,86,87,88,89,90,91)] 

#--------------------------------------------------
#MAKE a DAILY MEAN matrix of all values
#single column: allData %>% group_by(Site,Date2) %>% summarise(MeaVar=mean(ACIout))
allData$SiteDay2 = paste(allData$Site,allData$DY,sep = "_")
colsI = c( 17:23,29:41,46,14,50:52)
udays = unique(allData$SiteDay2)
allData$timeSinceSunrise = as.numeric(allData$timeSinceSunrise)
allData$fuel = as.numeric(allData$fuel)
dailyData = NULL
for (ii in 1:length(udays)) { # ii = 1
  tmp = allData[allData$SiteDay2 == udays[ii],colsI] 
  #what are the sunrise values
  #add column with number of observations, and unique hours
  ckday = all(as.numeric(unique(tmp$Hr)) < 12 ) #TRUE = all morning
  tmp$Hr = as.numeric(tmp$Hr)
  dailyData = rbind( dailyData, c(udays[ii], nrow(tmp), length(unique(tmp$Hr)), ckday, tmp$Treatment[1], 
                                  colMeans( tmp[,2:ncol(tmp)], na.rm = TRUE)) )
  rm(tmp)
}
dailyData = as.data.frame(dailyData)
dailyData$SiteDay = dailyData$V1
dailyData = separate(dailyData,col = "V1", into = c("Site","Day"), sep = "_")
names(dailyData)[names(dailyData)== "V5" ] = "Treatment"
names(dailyData)[names(dailyData)== "V4" ] = "sunriseOnly"
names(dailyData)[names(dailyData)== "V3" ] = "uniqueHrs"
names(dailyData)[names(dailyData)== "V2" ] = "Samples"

#quick visual of the matrix
ggplot(data = dailyData, aes(x=as.numeric(julDay), y=as.numeric(as.character(ACIout)), color = Treatment))+
  geom_point(alpha = 0.09)+
  geom_smooth(method=loess)
#fix variable formats
dailyData$ACIout = as.numeric(as.character(dailyData$ACIout))
dailyData$BKdB_low= as.numeric(as.character(dailyData$BKdB_low))
dailyData$BKdBA_low= as.numeric(as.character(dailyData$BKdBA_low))
dailyData$BKdBA_bird= as.numeric(as.character(dailyData$BKdBA_bird))
dailyData$BKdB_bird= as.numeric(as.character(dailyData$BKdB_bird))
dailyData$AR =  as.numeric(as.character(dailyData$AR))
dailyData$TempC = as.numeric(as.character(dailyData$TempC))
dailyData$Rain = as.numeric(as.character(dailyData$Rain))
dailyData$timeSinceSunrise = as.numeric(as.character(dailyData$timeSinceSunrise))
dailyData$daySinceFire = as.numeric(as.character(dailyData$daySinceFire))
dailyData$julDay = as.numeric(as.character(dailyData$julDay))
dailyData$Site = as.factor(dailyData$Site)
dailyData$Day = ymd(dailyData$Day)
dailyData$uniqueHrs = as.numeric(as.character(dailyData$uniqueHrs))
dailyData$Samples = as.numeric(as.character(dailyData$Samples))
dailyData$Hr = as.numeric(as.character(dailyData$Hr))

#repeated values for site days-- check
tst = dailyData$SiteDay[duplicated(dailyData$SiteDay)]
tmp = paste(dailyData$Site, dailyData$Day, sep = "_")
tst = tmp[duplicated(tmp)]

dateCreate = Sys.Date()
setwd(dataDir)
save( dailyData, file = paste0(dataDir,"SEKI_DailyMeanData_",dateCreate) )

#--------------------------------------------------
#MAKE difference matrix for fire severity
dmn2=subset(dailyData, Treatment == 'Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
dmn3=subset(dailyData, Treatment == 'Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
sevData = merge(dmn2, dmn3, by.x="Day", by.y="Day")
#Low severity - high severity calculation- 
# negative indicates when high severity has more ACI (e.g. 5-10)- expected
# positive indicates when low severity has more ACI  (e.g. 10-5)
sevData$diff = as.numeric(as.character(sevData$ACIout.y)) - as.numeric(as.character(sevData$ACIout.x))
hist(sevData$diff )
#quick visual of the matrix
ggplot(data = sevData, aes(x=ymd(Day), y=as.numeric(diff), color = as.factor(Site.x)) ) +
geom_point()
dateCreate = Sys.Date()
save( sevData, file = paste0(dataDir,"SEKI_DiffSeverityData_",dateCreate) )
