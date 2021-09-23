rm(list=ls())

library(corrplot)
library(lubridate)
library(dplyr)
#OBJECTIVE 3: Comparison of ACI with vegetation survey data
#only use data from rough fire sites to show how treatment and time relates
#60/70 fire without burn
#80/90 fire with burn

#NO model data for 2019- no temperature data so not using the predictive model results!!
# just plot raw ACI values, alternative could be to run model without temperatre?

#VARIABLES TO COMPARE
# Herb cover =  proxy for available habitat
# Fuel load  =  recover from fire


#READ IN DATA
#-------------------
firSur = read.csv("D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\Master_PlotConditonSummary_2016and2019_reformat.csv")

#dataDir = "D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data"
#dataIn = choose.files(caption = "Open: SEKI_DailyMeanData (most recent date)" )
#load(dataIn)
load("D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\SEKI_DailyMeanData_2020-06-25")

#ORGANIZE variables from survey data
#-------------------
head(firSur)
firSur$siteYR  = paste(firSur$Site,firSur$Year,sep = '-')
firSur$treatYR = paste(firSur$Treatment,firSur$Year,sep = '-')
firSurMod = firSur[firSur$Site > 50 , ]
ttmts = as.character( unique( firSurMod$Treatment))
shrub = aggregate(shrubCover~treatYR, data=firSurMod, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x),count=length(x)))
fuel  = aggregate(total     ~treatYR, data=firSurMod, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x),count=length(x)))

#ORGANIZE variables PAM
#-------------------
dailyData2 = dailyData[as.numeric( as.character(dailyData$Site) ) > 50 , ]
dailyData2$YR = year(dailyData2$Day)
dailyData2$siteYR = paste(dailyData2$Site,year(dailyData2$Day),sep = '-')
ttmtsPAM = as.character(unique(dailyData2$Treatment))
UBRF = dailyData2[dailyData2$Treatment == ttmtsPAM[1],] # UBRF$Site  
RxRF = dailyData2[dailyData2$Treatment == ttmtsPAM[2],] # RxRF$Site  
#head(UBRF)
UBRFm = aggregate(ACIout     ~Day, data=UBRF, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x), count=length(x)))
RxRFm = aggregate(ACIout     ~Day, data=RxRF, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x), count=length(x)))
#alternative method because output of aggregate is weird!
by_dayUBRF = UBRF %>% group_by(Day)
UBRFs = by_dayUBRF %>% summarise(meanACI=mean(ACIout), sdACI=sd(ACIout))

#PLOT DATA-- 
#-------------------
fueltmp = rbind( c(as.Date("2016-06-01"),fuel[1,2][1]), c(as.Date("2019-06-01"),fuel[1,2][2]) )


obj1 = xyplot(UBRFs$Day , UBRFs$meanACI)
plot(as.Date("2016-06-01"), fuel[1,2][1])
# --> construct separate plots for each series
obj1 <- xyplot(UBRFs$meanACI ~ UBRFs$Day, data, type = "l" , lwd=2, col="steelblue")
obj2 <- xyplot(var2 ~ x, data, type = "l", lwd=2, col="#69b3a2")

# --> Make the plot with second y axis:
doubleYScale(obj1, obj2, add.ylab2 = TRUE, use.style=FALSE )






#ggplot is not great at two axes plots...
ggplot(data = UBRFs, aes(x = Day)) +
  geom_line(aes(y = meanACI)) +
  geom_point(aes(y = fuel[1,2][1]))
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10, name="Second Axis") )

  #geom_point(as.Date("2016-06-01"), fuel[1,2][1])

#NOT USED
#------------------------------------------------------------------------------------------
#SUMMERIZE ACI DATA TO MATCH fire survey data (not used because too course a resolution!)
dailyData2 = dailyData[dailyData$YR == 2016 | dailyData$YR == 2019, ]
unique(dailyData2$YR)
sumACI = aggregate(ACIout~siteYR, data=dailyData2, FUN=function(x) c(mean=mean(x), count=length(x)))
sumACI = aggregate(ACIout~siteYR, data=dailyData2, FUN=function(x) c(mean=mean(x)))

comparePlot = as.data.frame (cbind(sumPlot$shrubCover, sumACI$ACIout,sumACI$siteYR))
colnames(comparePlot) = c("ShrubCover","ACI","SiteYear")
comparePlot = as.data.frame(comparePlot)
YR = ( sapply( strsplit(as.character(comparePlot$SiteYear),"-"),'[',2))
SITE = ( sapply( strsplit(as.character(comparePlot$SiteYear),"-"),'[',1))
comparePlot = cbind(comparePlot,YR,SITE)
#plot with color = year, shape = treatment
ggplot(comparePlot, aes(as.numeric(as.character(ShrubCover)), as.numeric(as.character(ACI)),color = SITE ) )+
  geom_point() +
  geom_smooth(method='lm') +
  labs(title = "Comparison of ACI with Plot data")+
  ylab("mean ACI") +
  xlab("mean Fuel load [kg/m^2]")


#DATA EXPLORE -look at survey variable correlation
# M=cor(firSur[,5:27],use="complete.obs")
# corrplot(M, method = "circle",type = "upper")
# df = firSur[,c(5,6,7,21:27)]
# M=cor(df,use="complete.obs")
# corrplot(M, method = "circle",type = "upper")
