rm(list=ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(Rmisc)
library(RColorBrewer)
#BatDir =  "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\AUDIO_V3" 
BatDir =  "D:\\SEKI SoundScape\\RoughFire\\AUDIO\\AUDIO_V3"
for (subdir in list.dirs(recursive=FALSE)) {
  BatFiles <- list.files(BatDir, pattern="__0__", recursive=T, full.names=T)
  bat<-cbind.data.frame(BatFiles)
  setwd("D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\BatAnalysis")
  write.csv(bat, file="bat.csv", na ="NaN")
}
data<-read.csv("D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\BatAnalysis\\bat.csv")
data$year<-as.numeric(substr(data$BatFiles,69,72))

data$month<-as.numeric(substr(data$BatFiles,73,74))
data$day<-as.numeric(substr(data$BatFiles,75,76))
data$time<-as.numeric(substr(data$BatFiles,78,83))
data$file<-ifelse(grepl("__0__",data$BatFiles),as.numeric(1),as.numeric(0))
data$site<-as.numeric(substr(data$BatFiles,62,63))
str(data)
data$Treatment <- ifelse(data$site==10, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                         ifelse(data$site==20, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                                ifelse(data$site==30, "Old Growth (Yes) \n Rough (No) \n Prescribed (No)",
                                       ifelse(data$site==40, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                              ifelse(data$site==50, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                                     ifelse(data$site==60, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                            ifelse(data$site==70, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (No)",
                                                                   ifelse(data$site==80, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                          ifelse(data$site==90, "Old Growth (Yes) \n Rough (Yes) \n Prescribed (Yes)",
                                                                                 NA  ))))))))) # all other values map to NA
           
                                                                                 
                                                                                 
data$Treatment <- ifelse(data$site==10, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                         ifelse(data$site==20, "Old Growth (Yes) \n Rough (No) \n Prescribed (Yes)",
                                ifelse(data$site==30, "Old Growth (Yes) \n Rough (No) \n Prescribed (No)",
                                       ifelse(data$site==40, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                              ifelse(data$site==50, "Old Growth (No) \n Rough (No) \n Prescribed (No)",
                                                     ifelse(data$site==60, "No Prescribed Fire",
                                                            ifelse(data$site==70, "No Prescribed Fire",
                                                                   ifelse(data$site==80, "Prescribed Fire",
                                                                          ifelse(data$site==90, "Prescribed Fire",
                                                                                 NA  ))))))))) # all other values map to NA
myTreatments = unique(data$Treatment)


data$date<-paste(data$month,data$day,data$year, sep="-")
data<-subset(data,data$year>"2015")

data$date2<-as.Date(data$date, format="%m-%d-%Y")
#create site as charactenumerir
data$site_char<-as.character(data$site)

#ddply(data,~month,summarise,BatFiles=count(BatFiles))

#aggregate(BatFiles~month+day+year+time+site+Treatment, data, count)

#data<-read.csv("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\BatAnalysis\\bat.csv")

#data<-read.csv("V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\BatAnalysis\\bat.csv")
str(data)



###prepare data for all site average


datave<-data %>%
  group_by(date2,site_char) %>% 
  summarise_each(funs(sum), file)



####average by day
dataveg<-datave%>% 
  group_by(date2) %>% 
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n())))
  


###add to data table for daily average
master<-merge(x=data,y=dataveg,by="date2",all.x = TRUE, all.y = FALSE)





data_t1<-subset(master, site==10|site==20)
data_t1$site<-as.character(data_t1$site)
p1<-ggplot(data=data_t1, aes(x=date2, y=file, color=site)) +
  stat_summary(fun.y = sum, geom="point") +
  geom_line(aes(y = file_mean), color="black",size =0.5) +  # plot average
  labs(title="No Rough Fire with Prescribed Fire in Old Growth Sequoia Forests",
                     x="Date", y = "Bat activity (sum of files)") +
  ylim(0,1000)



data_t2<-subset(master, site==30)
data_t2$site<-as.character(data_t2$site)
p2<-ggplot(data=data_t2, aes(x=date2, y=file, color=site)) +
  stat_summary(fun.y = sum, geom="point") +
  geom_line(aes(y = file_mean), colour = "black", size =0.5) +  # plot average
  labs(title="No Rough Fire or Prescribed Fire in Old Growth Sequoia Forests",
       x="Date", y = "Bat activity (sum of files)") +
  ylim(0,1000)


data_t3<-subset(master, site==40|site==50)
data_t3$site<-as.character(data_t3$site)
p3<-ggplot(data=data_t3, aes(x=date2, y=file, color=site)) +
  stat_summary(fun.y = sum, geom="point") +
  geom_line(aes(y = file_mean), colour = "black", size =0.5) +  # plot average
  labs(title="No Rough Fire or Prescribed Fire in Second Growth Sequoia Forests",
       x="Date", y = "Bat activity (sum of files)") +
  ylim(0,1000)

data_t4<-subset(master, site==60|site==70)
data_t4$site<-as.character(data_t4$site)
p4<-ggplot(data=data_t4, aes(x=date2, y=file, color=site)) +
  stat_summary(fun.y = sum, geom="point") +
  geom_line(aes(y = file_mean), colour = "black", size =0.5) +  # plot average
  labs(title="Rough Fire without Prescribed Fire in Old Growth Sequoia Forests",
       x="Date", y = "Bat activity (sum of files)") +
  ylim(0,1000)


data_t5<-subset(master, site==80|site==90)
data_t5$site<-as.character(data_t5$site)
p5<-ggplot(data=data_t5, aes(x=date2, y=file, color=site)) +
  stat_summary(fun.y = sum, geom="point") +
  geom_line(aes(y = file_mean), colour = "black", size =0.5) +  # plot average
  labs(title="Rough Fire after Prescribed Fire in Old Growth Sequoia Forests",
       x="Date", y = "Bat activity (sum of files)") +
  ylim(0,1000)

setwd("D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\BatAnalysis")
jpeg("bat_bysitesscatter.jpg",res = 450,height=4000, width=6000)
multiplot(p1, p2, p3, p4, p5, cols=2)
dev.off()




jpeg("bat_bytreatmentsscatter.jpg",res = 450,height=4000, width=6000)
data_tx<-subset(master, site==80|site==90|site==60|site==70)
data_tx$Treatment<-as.character(data_tx$Treatment)
ggplot(data=data_tx, aes(x=date2, y=file, color=Treatment)) +
  stat_summary(fun.y = sum, geom="point") +
  geom_point()
  scale_color_manual(breaks = c("No Prescribed Fire", "Prescribed Fire"),
                     values=c("grey", "black")) +
  #geom_line(aes(y = file_mean), colour = "black", size =0.5) +  # plot average
  geom_smooth(method = 'loess') +
  labs(title="Bat Activity after Rough Fire",
       x="Date", y = "Bat activity (sum of files)") +
  ylim(0,1000)

  ggplot(data_tx, aes(date2, file, group = Treatment, color = Treatment)) +
    geom_point() +
    geom_smooth(method = 'loess') +
    labs(x = NULL, y = metric, title = "Daily Average with loess smooth")
  
  
dev.off()

###ggplot(data_t1, aes(x=month, y=file, color=Treatment)) + 
  #stat_summary(fun.y = sum, geom="point", colour = "red", size = 1) + 
##  geom_smooth(method = 'loess') +
##  labs(title="ACI vs understory temperature",
     ##  x="difference in ridge temp to understory temperature (C)", y = "ACI")+
 ## theme_classic()  

##BatFiles <- list.files(BatDir, pattern="__0__", recursive=T, full.names=T)
##bat<-cbind.data.frame(BatFiles)

##write.csv(bat, file="bat.csv", na ="NaN")
##mean(data,vars="date2
   ##   ")



####for sci symposium
  data_tx<-subset(data, site==80|site==90|site==60|site==70)
  data_tx$Treatment <- ifelse(data_tx$site==60, "No Prescribed Fire",
                              ifelse(data_tx$site==70, "No Prescribed Fire",
                                     ifelse(data_tx$site==80, "Prescribed Fire",
                                            ifelse(data_tx$site==90, "Prescribed Fire",
                                                   NA  ))))# all other values map to NA
  data_tx$Treatment<-as.character(data_tx$Treatment)
  ggplot(data=data_tx, aes(x=date2, y=file, color=Treatment)) +
    stat_summary(fun.y = "sum", geom="point")
  
  
  png("Bat_rough_fire_prescribed-yes-no sscatter.png",res=150,height=800, width=1500)
  myTreatments = unique(data_tx$Treatment)
  pALL = ggplot(data_tx, aes(date2, file, group = as.character(Treatment), color = Treatment)) +
    stat_summary(fun.y = "sum", geom="point") +
    #geom_point(aes(shape=Treatment), size=3) +
    #geom_smooth(method = 'loess') +
    scale_color_manual(values=c("black", "gray64")) +
    theme(axis.text.x=element_text(angle=45, hjust=1)) +
    #geom_rangeframe() + 
    #theme_tufte() +
    labs(x = NULL, y = "Number of Bat Files", title = "Daily Bat Activity after Rough Fire") +
    theme(plot.title = element_text(color="#666666", face="bold", size=24, hjust=0)) +
    theme(axis.title = element_text(color="#666666", face="bold", size=22),
          axis.text.x =element_text(vjust=1.0, size=18),
          axis.text.y =element_text(vjust=0.5, size=18)) +
    theme(panel.grid.minor=element_blank(),
          panel.grid.major=element_blank()) +
    theme(legend.title = element_blank()) +
    theme(legend.text = element_text(size=16, 
                                     face="bold")) +
    theme(legend.position="bottom")
  pALL
  dev.off()
  