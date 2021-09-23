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
setwd(newdir)#




___________________________________________________________________________________________________
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

SUBALL1$Sites = as.character(SUBALL1$Site)
SUBALL1$Date2 <- as.Date(SUBALL1$Date, format="%Y_%m_%d")
SUBALL1$Date3 <- gsub("_","",SUBALL1$Date)
metric = "Acoustic Complexity Index" #metric to plot

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



#################################
jpeg("AIC_alltxscatter.jpg",res = 300,height=1800, width=3200)

by_siteDate <- group_by(SUBALL, Treatment, Date2)
delay <- summarise(by_siteDate,
                   count = n(),
                   ACI = mean(ACIout, na.rm = TRUE))
pALL = ggplot(delay, aes(Date2, ACI, group = Treatment, color = Treatment)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  labs(x = NULL, y = metric, title = "Daily Average with loess smooth")
pALL
dev.off()


##Data Exploration
xyplot(ACIout~Date2 | Sites, data = SUBALL,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))
###Data Exploration
xyplot(ACIout~time.y | Sites, data = SUBALL,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))


str(SUBALL)
library(mgcv)
SUBALL1<-na.omit(SUBALL)
m2 <- gamm(ACIout ~ s(Treatment)+s(time.y)+s(BKdBA_low)+s(wind_ms)+s(solar)+s(air_c)+s(temp_us), data = master, 
           correlation = corCAR1(value = 0.5, form = ~ time.y | Sites))
m2 <- gamm(ACIout ~ s(Treatment), data = master1, 
           correlation = corCAR1(value = 0.5, form = ~ time.y | Sites))
m2 <- gamm(ACIout ~ Treatment+time.y+Yr+Mo,random = list(Sites=~1), data = SUBALL)
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

















#___________________________________________________________________________________________________
#### READ IN ALL Adjusted FILES
AllAIDir =  "D:\\SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex\\adjValues" 
AllAIFiles <- list.files(AIDir, pattern="ALLdata", recursive=F, full.names=T)

# INFO FILES-
basename(AllAIFiles)
mySites = unique( sapply(strsplit(basename(AllAIFiles), "_"), head, 1) )
numSites = length(mySites)

#COMBINE INDEX- 
ALL1 <- do.call(rbind,lapply(AllAIFiles,read.csv)) # ALL = d

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

#################################
jpeg("AIC_alltxscatter_all.jpg",res = 300,height=1800, width=3200)

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
