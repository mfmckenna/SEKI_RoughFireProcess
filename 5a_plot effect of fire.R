rm(list=ls())
library(grid)
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


####Working on V drive unhash

#source("D:\\CODE\\Rwork\\multiplot.R")
#source("V:\\SEKI SoundScape\\RoughFire\\CODE\\OtherScripts\\multiplot.R")
# colorblind-friendly palette: grey, orange, sky blue, green, yellow, navy, red, pink
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#set wd to graphics folder
#setwd("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Graphics")

#identify the date the graphs will be created
#folder = user <- dlgInput("What day are you producing these graphs (year_month_day)?")$res

#create a new directory for the updated graphs
#dir.create(paste("Created_",folder,sep =""))

#select the new folder for the new graphs
#newdir <-choose.dir(getwd(), caption = "Select folder")

#create new directory to dump new graphs
#setwd(newdir)



####Working on Drobo

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
#AIDir =  "V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\AcousticIndex" 

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

by_siteDate <- group_by(ALL, Date2,Treatment)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = mean(ACIout, na.rm = TRUE))


#####Plot effect of Rough fire with/without Prescribed Fire
dmn2<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
dmn3<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
data<-merge(dmn2,dmn3,by.x="Date2", by.y="Date2")

#Low severity minus high severity calculation
data$diff<-(data$ACI.y-data$ACI.x)

#Less Diversity
dd1<-sum(data$diff<=0)
#More divesity
dd2<-sum(data$diff>0)
#min value
dd3<-min(data$diff)
#max value
dd4<-max(data$diff)

str(data)
s1=ggplot(data, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(-Inf,0]" = "red",
                                "(0]" = "yellow",
                                "(0, Inf]" = "black"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Mean ACI", title = "Low severity compared to high severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3-0.025, label= "High Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4+0.025, label= "Low Severity",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1) + ylim(-0.3,0.5)

s1 

######################################

#####Plot effect of Rough fire  no fire history vs Rough Fire with prescribed
dmn4<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (No) \n Prescribed (No)')
dmn5<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
data1<-merge(dmn4,dmn5,by.x="Date2", by.y="Date2")

#diff equals no fire history minus low severity fire
data1$diff<-(data1$ACI.x-data1$ACI.y)

#total days with greater diversity in low severity fire
dd1<-sum(data1$diff<=0)

#total days with greater diversity in no fire treatment
dd2<-sum(data1$diff>0)
#greatest difference in low severity over no fire
dd3<-min(data1$diff)
#greatest difference in no fire from low severity
dd4<-max(data1$diff)

str(data1)



s2=ggplot(data1, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(0, Inf]" = "black","(0]" = "yellow","(-Inf,0]" = "red"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Mean ACI", title = "No fire compared to low severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3 - 0.025, label= "Low Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4 + 0.025, label= "No Fire History",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  #geom_smooth(stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, fun.data = diff, fun.args = list(conf.int = 0.10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1) + ylim(-0.3,0.5)



#########################################

######################################

#####Plot effect of Rough fire  no fire history vs Rough Fire with prescribed
dmn6<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (No) \n Prescribed (No)')
dmn7<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
data2<-merge(dmn6,dmn7,by.x="Date2", by.y="Date2")

#diff equals no fire history minus low severity fire
data2$diff<-(data2$ACI.x-data2$ACI.y)

#total days with greater diversity in high severity fire
dd1<-sum(data2$diff<=0)

#total days with greater diversity in no fire treatment
dd2<-sum(data2$diff>0)



#greatest difference in high severity over no fire
dd3<-min(data2$diff)

#greatest difference in no fire from high severity
dd4<-max(data2$diff)

str(data2)
s3=ggplot(data2, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(0, Inf]" = "black","(0]" = "yellow","(-Inf,0]" = "red"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Mean ACI", title = "No fire compared to high severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3 - 0.025, label= "High Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4 + 0.025, label= "No Fire History",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  #geom_smooth(stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, fun.data = diff, fun.args = list(conf.int = 0.10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1) + ylim(-0.3,0.5)




#########################################



jpeg("effect_bytx_mean.jpg",res = 500,height=3200, width=5200)
multiplot(s1, s3, s2)
dev.off()




########################MAX ACI PLOTS


by_siteDate <- group_by(ALL, Date2,Treatment)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = max(ACIout, na.rm = TRUE))


#####Plot effect of Rough fire with/without Prescribed Fire
dmn2<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
dmn3<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
data<-merge(dmn2,dmn3,by.x="Date2", by.y="Date2")

#Low severity minus high severity calculation
data$diff<-(data$ACI.y-data$ACI.x)

#Less Diversity
dd1<-sum(data$diff<=0)
#More divesity
dd2<-sum(data$diff>0)
#min value
dd3<-min(data$diff)
#max value
dd4<-max(data$diff)

str(data)
s1=ggplot(data, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(-Inf,0]" = "red",
                                "(0]" = "yellow",
                                "(0, Inf]" = "black"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Maximum ACI", title = "Low severity compared to high severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3-0.025, label= "High Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4+0.025, label= "Low Severity",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1)

######################################

#####Plot effect of Rough fire  no fire history vs Rough Fire with prescribed
dmn4<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (No) \n Prescribed (No)')
dmn5<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
data1<-merge(dmn4,dmn5,by.x="Date2", by.y="Date2")

#diff equals no fire history minus low severity fire
data1$diff<-(data1$ACI.x-data1$ACI.y)

#total days with greater diversity in low severity fire
dd1<-sum(data1$diff<=0)

#total days with greater diversity in no fire treatment
dd2<-sum(data1$diff>0)
#greatest difference in low severity over no fire
dd3<-min(data1$diff)
#greatest difference in no fire from low severity
dd4<-max(data1$diff)

str(data1)



s2=ggplot(data1, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(0, Inf]" = "black","(0]" = "yellow","(-Inf,0]" = "red"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Maximum ACI", title = "No fire compared to low severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3 - 0.025, label= "Low Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4 + 0.025, label= "No Fire History",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  #geom_smooth(stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, fun.data = diff, fun.args = list(conf.int = 0.10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1)



#########################################

######################################

#####Plot effect of Rough fire  no fire history vs Rough Fire with prescribed
dmn6<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (No) \n Prescribed (No)')
dmn7<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
data2<-merge(dmn6,dmn7,by.x="Date2", by.y="Date2")

#diff equals no fire history minus low severity fire
data2$diff<-(data2$ACI.x-data2$ACI.y)

#total days with greater diversity in high severity fire
dd1<-sum(data2$diff<=0)

#total days with greater diversity in no fire treatment
dd2<-sum(data2$diff>0)



#greatest difference in high severity over no fire
dd3<-min(data2$diff)

#greatest difference in no fire from high severity
dd4<-max(data2$diff)

str(data2)
s3=ggplot(data2, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(0, Inf]" = "black","(0]" = "yellow","(-Inf,0]" = "red"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Maximum ACI", title = "No fire compared to high severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3 - 0.025, label= "High Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4 + 0.025, label= "No Fire History",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  #geom_smooth(stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, fun.data = diff, fun.args = list(conf.int = 0.10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1)




#########################################



jpeg("effect_bytx_max.jpg",res = 500,height=3200, width=5200)
multiplot(s1, s3, s2)
dev.off()



########################Min ACI PLOTS


by_siteDate <- group_by(ALL, Date2,Treatment)
dmn <- summarise(by_siteDate,
                 count = n(),
                 ACI = min(ACIout, na.rm = TRUE))


#####Plot effect of Rough fire with/without Prescribed Fire
dmn2<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
dmn3<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
data<-merge(dmn2,dmn3,by.x="Date2", by.y="Date2")

#Low severity minus high severity calculation
data$diff<-(data$ACI.y-data$ACI.x)

#Less Diversity
dd1<-sum(data$diff<=0)
#More divesity
dd2<-sum(data$diff>0)
#min value
dd3<-min(data$diff)
#max value
dd4<-max(data$diff)

str(data)
s1=ggplot(data, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(-Inf,0]" = "red",
                                "(0]" = "yellow",
                                "(0, Inf]" = "black"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Minimum ACI", title = "Low severity compared to high severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3-0.025, label= "High Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4+0.025, label= "Low Severity",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1)

######################################

#####Plot effect of Rough fire  no fire history vs Rough Fire with prescribed
dmn4<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (No) \n Prescribed (No)')
dmn5<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)')
data1<-merge(dmn4,dmn5,by.x="Date2", by.y="Date2")

#diff equals no fire history minus low severity fire
data1$diff<-(data1$ACI.x-data1$ACI.y)

#total days with greater diversity in low severity fire
dd1<-sum(data1$diff<=0)

#total days with greater diversity in no fire treatment
dd2<-sum(data1$diff>0)
#greatest difference in low severity over no fire
dd3<-min(data1$diff)
#greatest difference in no fire from low severity
dd4<-max(data1$diff)

str(data1)



s2=ggplot(data1, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(0, Inf]" = "black","(0]" = "yellow","(-Inf,0]" = "red"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Minimum ACI", title = "No fire compared to low severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3 - 0.025, label= "Low Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4 + 0.025, label= "No Fire History",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  #geom_smooth(stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, fun.data = diff, fun.args = list(conf.int = 0.10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1)



#########################################

######################################

#####Plot effect of Rough fire  no fire history vs Rough Fire with prescribed
dmn6<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (No) \n Prescribed (No)')
dmn7<-subset(dmn, Treatment=='Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)')
data2<-merge(dmn6,dmn7,by.x="Date2", by.y="Date2")

#diff equals no fire history minus low severity fire
data2$diff<-(data2$ACI.x-data2$ACI.y)

#total days with greater diversity in high severity fire
dd1<-sum(data2$diff<=0)

#total days with greater diversity in no fire treatment
dd2<-sum(data2$diff>0)



#greatest difference in high severity over no fire
dd3<-min(data2$diff)

#greatest difference in no fire from high severity
dd4<-max(data2$diff)

str(data2)
s3=ggplot(data2, aes(Date2, diff)) +
  geom_point(aes(colour = cut(diff, c(-Inf, 0, Inf))),
             size = 1) +
  scale_color_manual(name = "Total Days",
                     values = c("(0, Inf]" = "black","(0]" = "yellow","(-Inf,0]" = "red"),
                     labels = c(paste(dd1,"days"), paste(dd2,"days"), "NA")) +
  labs(x = NULL, y = "Δ Minimum ACI", title = "No fire compared to high severity fire") +
  annotate("text", x=as.Date("2016-2-01"), y=dd3 - 0.025, label= "High Severity",colour = "red",fontface =2) +
  annotate("text", x=as.Date("2016-2-01"), y=dd4 + 0.025, label= "No Fire History",colour = "black",fontface =2) +
  guides(colour = guide_legend(reverse=T)) +
  #geom_smooth(stat = 'summary', color = 'red', fill = 'red', alpha = 0.2, fun.data = diff, fun.args = list(conf.int = 0.10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "light gray",size=1)  +
  annotate("pointrange", x = as.Date("2015-11-25"), y = -0.01, ymin = dd3, ymax = 0,
           colour = "red", size = 1)+
  annotate("pointrange", x = as.Date("2015-11-25"), y = 0.01, ymin = 0, ymax = dd4,
           colour = "black", size = 1)




#########################################



jpeg("effect_bytx_min.jpg",res = 500,height=3200, width=5200)
multiplot(s1, s3, s2)
dev.off()

