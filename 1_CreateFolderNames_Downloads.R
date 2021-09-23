library(svDialogs)
rm(list=ls())
setwd("D:\\SEKI SoundScape\\RoughFire\\AUDIO\\NotProcessed")
date()
mySites = c(10,20,30,40,50,60,70,80,90)

today<-format(Sys.Date(),"%m/%d/%Y")
folder<-as.character(as.Date(today, "%m/%d/%Y"), "%Y%m%d") 

for (ss in 1:length(mySites)  ) # 1:length(mySites) START loop through sites
{

#identify the date the graphs will be created
folder10 = user <- dlgInput("Please enter each site")$res

#create a new directory for the updated graphs
dir.create(paste(folder,"_",folder10,sep = ""))

}








