# Initial plotting of Acoustic Indices from SEKI Rough Fire project

rm(list=ls())

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
#source("D:\\CODE\\Rwork\\multiplot.R")
source("D:\\SEKI SoundScape\\RoughFire\\CODE-copy11Sep2017\\multiplot.R")
# colorblind-friendly palette: grey, orange, sky blue, green, yellow, navy, red, pink
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#set wd to graphics folder
setwd("D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Graphics")

#identify the date the graphs will be created
folder = user <- dlgInput("What day are you producing these graphs (year_month_day)?")$res

#create a new directory for the updated graphs
dir.create(paste("Created_",folder,sep =""))

#select the new folder for the new graphs
newdir <-choose.dir(getwd(), caption = "Select folder")

#create new directory to dump new graphs
setwd(newdir)


#___________________________________________________________________________________________________
# READ IN FILES
AIDir =  "D:\\SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex" 
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
#____________________________
#### PLOT 1: all treatments together...#### 
#____________________________
#group data first

jpeg("AIC_alltxscatter.jpg",res = 300,height=1800, width=3200)

by_siteDate <- group_by(ALL, Treatment, Date2)
delay <- summarise(by_siteDate,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))
pALL = ggplot(delay, aes(Date2, ACI, group = Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = "Daily Average with loess smooth")
pALL
dev.off()
#### ####
#____________________________
#### PLOT 2: all sites together...####
#____________________________

jpeg("AIC_allsitesscatter.jpg",res = 300,height=1800, width=3200)
by_siteDate <- group_by(ALL, Sites, Date2)
delay <- summarise(by_siteDate,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))
pALL2 = ggplot(delay, aes(Date2, ACI, group = Sites, color = Sites)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = "Daily Average with loess smooth")
pALL2 

dev.off()
axis.text.x = element_text(angle = 45, hjust = 1)
#### ####

#____________________________
#### PLOT 3:  by site all data...####
#____________________________
ss = 1
ALL2 = ALL[ALL$Site == mySites[ss],] 
p1 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ), axis.text.x = element_text(angle = 45, hjust = 1) ) + 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) ) +
  ylim (c(0, 1.5))
 
  

ss = 2
ALL2 = ALL[ALL$Site == mySites[ss],] 
p2 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) ) +
  ylim (c(0, 1.5))

ss = 3
ALL2 = ALL[ALL$Site == mySites[ss],] 
p3 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) ) +
  ylim (c(0, 1.5))

ss = 4
ALL2 = ALL[ALL$Site == mySites[ss],] 
p4 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 5
ALL2 = ALL[ALL$Site == mySites[ss],] 
p5 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 6
ALL2 = ALL[ALL$Site == mySites[ss],] 
p6 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 7
ALL2 = ALL[ALL$Site == mySites[ss],] 
p7 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 8
ALL2 = ALL[ALL$Site == mySites[ss],] 
p8 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 9
ALL2 = ALL[ALL$Site == mySites[ss],] 
p9 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

jpeg("AIC_bysitesscatter.jpg",res = 500,height=3200, width=5200)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols=3)
dev.off()
#### ####
####____________________________
#### PLOT 4:  by site just last 2 weeks for qa/qc####
####____________________________
###
StartDate<-as.Date(today())-45
ALLsub<-subset(ALL, Date2 >= StartDate)
ss = 1
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p1 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ), axis.text.x = element_text(angle = 45, hjust = 1) ) + 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) ) +
  ylim (c(0, 1.5))



ss = 2
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p2 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) ) +
  ylim (c(0, 1.5))

ss = 3
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p3 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) ) +
  ylim (c(0, 1.5))

ss = 4
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p4 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 5
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p5 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 6
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p6 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 7
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p7 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 8
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p8 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

ss = 9
ALL2 = ALLsub[ALLsub$Site == mySites[ss],] 
p9 = ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) )+ 
  geom_point() +
  labs(x = NULL, y = metric, title = paste("Site: ", mySites[ss]) )  +
  ylim (c(0, 1.5))

jpeg("14DAY ERROR CHECK LOOK FOR ZEROS NO DATA AND OUTLIERS.jpg",res = 500,height=3200, width=5200)
multiplot(p1, p2, p3, p4, p5, p6, p7, p8, p9, cols=3)
dev.off()
#### ####

#____________________________
#### PLOT 5:  by treatment....####
#____________________________
ss = 1
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
p1 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) ) + 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Treatment: ", myTreatments[ss]) ) +
  ylim (c(0, 1.5))

ss = 2
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
p2 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) ) + 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Treatment: ", myTreatments[ss]) ) +
  ylim (c(0, 1.5))

ss = 3
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
p3 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) ) + 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Treatment: ", myTreatments[ss]) ) +
  ylim (c(0, 1.5))

ss = 4
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
p4 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) ) + 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Treatment: ", myTreatments[ss]) ) +
  ylim (c(0, 1.5))

ss = 5
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
p5 =   ggplot(ALL2, aes(x=Date2, y=ACIout, group = Date2 ) ) + 
  geom_boxplot() +
  labs(x = NULL, y = metric, title = paste("Treatment: ", myTreatments[ss]) ) +
  ylim (c(0, 1.5))

jpeg("AIC_bytreatment.jpg",res = 500,height=5200, width=5200)
multiplot(p1, p2, p3, p4, p5, rows=5)
dev.off()
#### ####
#____________________________
#### PLOT 6: by site, mean.....####
#____________________________
ss = 1
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))
pm1= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 2
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm2= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 3
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm3= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 4
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm4= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 5
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm5= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 6
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm6= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 7
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm7= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 8
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm8= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))

ss = 9
ALL2 = ALL[ALL$Site == mySites[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm9= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average loess, Site:", mySites[ss])) +
  ylim (c(0, 1))
jpeg("AIC_bysite_mean.jpg",res = 500,height=3200, width=5200)
multiplot(pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8, pm9, cols=3)
dev.off()
#### ####

#____________________________
#### PLOT 7: by treatment, mean.....####
#____________________________
ss = 1
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm1= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 1))

ss = 2
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm2= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 1))

ss = 3
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm3= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 1))

ss = 4
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm4= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 1))

ss = 5
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))
pm5= ggplot(dmn, aes(Date2, ACI) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 1))

jpeg("AIC_bytreatment_mean.jpg",res = 500,height=3200, width=5200)
multiplot(pm1, pm2, pm3, pm4, pm5, cols=2)
dev.off()
#### ####

#____________________________
#### PLOT 8: ACI noise levels by site####
#____________________________
jpeg("ACI_by_tx boxplot.jpg",res = 300,height=3200, width=5200)
par(mfrow=c(2,1))
boxplot( as.numeric(ALL$ACIout) ~ ALL$Treatment, 
         ylab ='Acoustic Complexity Index') 
dev.off()
#### ####
#____________________________
#### PLOT 8: Background noise levels by site####
#____________________________
jpeg("Background_noise_by_site.jpg",res = 500,height=3200, width=5200)
boxplot( as.numeric(ALL$BKdB_low) ~ ALL$Site, 
         ylab ='Background Noise Level (dB 31.5-1250 Hz') 
dev.off()
#### ####

ggplot(ALL, aes(Date2, BKdB_low))




ggplot(ALL, aes(x=Hr, y=BKdB_low, color=Sites)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs RH",
       x="Relative Humidity (%)", y = "ACI")+
  theme_classic()  


#### PLOT 8: Background noise levels by treatment####
#____________________________
jpeg("Background_noise_by_tx.jpg",res = 500,height=3200, width=5200)
boxplot( as.numeric(ALL$BKdB_low) ~ ALL$Treatment, 
         ylab ='Background Noise Level (dB 31.5-1250 Hz') 
dev.off()
#### ####



#____________________________
#### PLOT 9: Diel patterns- boxplot by site/treatment ####
#____________________________
jpeg("AIC_Diel_by_site_treatment.jpg",res = 500,height=3200, width=5200)

par(mfrow=c(2,3))

data10 =  ALL[ ( ALL$Site == 10 | ALL$Site == 20 ), ]
unique(data10$Site)
boxplot( data10$ACIout ~ data10$Hr , 
         main ="No Rough, Yes prescribe (10,20)", xlab="Hour",
         ylab ='Acoustic Complexity Index',
         ylim=c(0, 1) ) 

data10 = ALL[ (ALL$Site == 40 | ALL$Site == 50 ), ]
unique(data10$Site)
boxplot( data10$ACIout ~ data10$Hr , 
         main ="No Rough, No prescribe (2nd Generation) (40,50)", xlab="Hour",
         ylab ='Acoustic Complexity Index', ylim=c(0, 1.5) ) 

data10 = ALL[ (ALL$Site == 30 ), ]
unique(data10$Site)
boxplot( data10$ACIout ~ data10$Hr , 
         main ="No Rough, No prescribe (Old Growth) (30)", xlab="Hour",
         ylab ='Acoustic Complexity Index', ylim=c(0, 1.5) ) 

data10 =  ALL[ (ALL$Site == 80 | ALL$Site == 90 ), ]
unique(data10$Site)
data10 =  data10[ (data10$Hr >= 4 & data10$Hr <= 20 ), ]
boxplot( data10$ACIout ~ data10$Hr , 
         main ="Yes Rough, Yes prescribe (80,90)", xlab="Hour",
         ylab ='Acoustic Complexity Index', ylim=c(0, 1.5) )

data10 =  ALL[ (ALL$Site == 60 | ALL$Site == 70 ), ]
unique(data10$Site)
boxplot( data10$ACIout ~ data10$Hr , 
         main ="Yes Rough, No prescribe (60,70)", xlab="Hour",
         ylab ='Acoustic Complexity Index',ylim=c(0, 1.5) ) 

dev.off()
#### ####
#___________________________________________________________________________________________________
#### PLOT 10: Seasonal patterns- boxplot per month for each treatment####

jpeg("AIC_seasonal_by_month_treatment.jpg",res = 500,height=3200, width=5200)
par(mfrow=c(2,3))

dataD =  ALL[ ( ALL[,1] == 10 | ALL[,1] == 20 ), ]
unique(dataD[,1])
boxplot( dataD$ACIout ~ as.numeric(dataD$Mo)  , 
         main ="No Rough, Yes prescribe (10,20)",
         ylab ='Acoustic Complexity Index',
         ylim=c(0, 1) ) 

dataD =  ALL[ (ALL[,1] == 40  | ALL[,1] == 50  ), ]
unique(dataD[,1])
boxplot( dataD$ACIout ~ as.numeric(dataD$Mo), 
         main ="No Rough, No prescribe (2nd Generation) (40,50)",
         ylab ='Acoustic Complexity Index',
         ylim=c(0, 1) ) 

dataD =  ALL[ ( ALL[,1] == 30 ), ]
unique(dataD[,1])
boxplot( dataD$ACIout ~ as.numeric(dataD$Mo), 
         main ="No Rough, No prescribe (Old Growth) (30)",
         ylab ='Acoustic Complexity Index',
         ylim=c(0, 1) ) 

dataD =  ALL[ ( ALL[,1] == 80 | ALL[,1] == 90 ), ]
unique(dataD[,1])
dataD =  rbind( dataD[1:114,],dataD[123:nrow(dataD),])
boxplot( dataD$ACIout ~ as.numeric(dataD$Mo), 
         main ="Yes Rough, Yes prescribe (80,90)",
         ylab ='Acoustic Complexity Index',
         ylim=c(0, 1) ) 

dataD =  ALL[ ( ALL[,1] == 60 | ALL[,1] == 70 ), ]
unique(dataD[,1])
boxplot( dataD$ACIout ~ as.numeric(dataD$Mo), 
         main ="Yes Rough, No prescribe (60,70)",
         ylab ='Acoustic Complexity Index',
         ylim=c(0, 1) ) 
dev.off()
#### ####
#--------------------------------------------------------------------
#### PLOT 11: HEAT MAP of treatment by hour of day####
#group data first

jpeg("AIC_heat_treatment_hour.jpg",res = 500,height=3200, width=5200)
by_siteDHour <- group_by(ALL, Treatment,Hr)
out <- summarise(by_siteDHour,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))

p <- ggplot(out, aes(x = Hr, y = Treatment)) + 
  geom_tile(aes(fill = ACI)) + 
  scale_fill_gradient(name = 'Acoustic Complexity Index', low = 'white', high = 'red') + 
  theme(axis.title.y = element_blank())


p + ggtitle("Giant Sequoia Forests \n After Rough Fire ") + xlab("Time of Day (Hr)")

dev.off()

#### ####













#######################################Roughness
metric = "Roughness" #metric to plot



jpeg("Rough_heat_treatment_hour.jpg",res = 500,height=3200, width=5200)
by_siteDHour <- group_by(ALL, Treatment,Hr)
out <- summarise(by_siteDHour,
                 count = n(),
                 Rough = mean(Rough, na.rm = TRUE))

p <- ggplot(out, aes(x = Hr, y = Treatment)) + 
  geom_tile(aes(fill = Rough)) + 
  scale_fill_gradient(name = 'Roughness', low = 'white', high = 'red') + 
  theme(axis.title.y = element_blank())


p + ggtitle("Giant Sequoia Forests \n After Rough Fire ") + xlab("Time of Day (Hr)")

dev.off()




ss = 1
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 Rough = mean(Rough, na.rm = TRUE))
pm1= ggplot(dmn, aes(Date2, Rough) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 20))

ss = 2
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 Rough = mean(Rough, na.rm = TRUE))
pm2= ggplot(dmn, aes(Date2, Rough) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 20))

ss = 3
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 Rough = mean(Rough, na.rm = TRUE))
pm3= ggplot(dmn, aes(Date2, Rough) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 20))

ss = 4
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 Rough = mean(Rough, na.rm = TRUE))
pm4= ggplot(dmn, aes(Date2, Rough) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 20))

ss = 5
ALL2 = ALL[ALL$Treatment == myTreatments[ss],] 
by_siteDate <- group_by(ALL2, Date2)
dmn <- summarise(by_siteDate,
                 count = n(),
                 Rough = mean(Rough, na.rm = TRUE))
pm5= ggplot(dmn, aes(Date2, Rough) ) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = paste("Daily Average: ", myTreatments[ss]) ) +
  ylim (c(0, 20))

jpeg("Rough_bytreatment_mean.jpg",res = 500,height=3200, width=5200)
multiplot(pm1, pm2, pm3, pm4, pm5, cols=2)
dev.off()

#source(getwd())
#source('D:/SEKI SoundScape/RoughFire/ANALYSIS/Graphics/Created_ 2017_12_21')
