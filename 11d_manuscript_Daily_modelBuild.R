rm(list=ls())

#OBJECTIVE 1: Effects of treatment on biodiversity (mean daily Acoustic Complexity), controling for seasonal and envriomental factors
# EM: expect first spring after fire to have lots of activity, second season to have low because birds did not return to habitat, third season
# starting increase in activity again

library(ggplot2)
library(mgcv)
library(data.table)
library(visreg)
library(gridExtra)
#--------------------------------------------------
#READ IN DATA
dataDir = "D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data"
setwd(dataDir)
dataIn = choose.files(caption = "Open: SEKI_DailyMeanData (most recent date)" )
load(dataIn)
dataIn = dailyData
#rm(dailyData)

#--------------------------------------------------
##DATA CLEAN-UP: known sources of possible variation not relevant to the question
#1)SUNSET data
#dataIn = dataIn[dataIn$sunriseOnly == TRUE, ]
#RESULT--- does not really help model performance
#2)DAYS when background noise is high-- remove
#dataIn = dataIn[dataIn$BKdBA_low < 80,]
#RESULT--- does not really help model performance
#3) REFORMAT rain to present absent
dataIn$Rain2[dataIn$Rain >= 0] = "absent"
dataIn$Rain2[dataIn$Rain > 0]  = "present"
#hist(dataIn$Rain)
#dataIn$Rain

#--------------------------------------------------
#treatment summaries- which sites go with each treatment
treat = unique( dataIn$Treatment )
treat[1]
unique(dataIn$Site[dataIn$Treatment == treat[1]])
treat[2]
unique(dataIn$Site[dataIn$Treatment == treat[2]])
treat[3]
unique(dataIn$Site[dataIn$Treatment == treat[3]])
treat[4]
unique(dataIn$Site[dataIn$Treatment == treat[4]])
treat[5]
unique(dataIn$Site[dataIn$Treatment == treat[5]])
#treatment rename
#UB1+Rx (10,20)
dataIn$Treatment2 = dataIn$Treatment #make copy

levels(dataIn$Treatment)[1] = "UB2" #10,20
levels(dataIn$Treatment)[2] = "UB1"    #30
levels(dataIn$Treatment)[3] = "UB1+Rx"    #40,50
levels(dataIn$Treatment)[4] = "RF1"    #60,70
levels(dataIn$Treatment)[5] = "RF1+Rx" #80,90
levels( dataIn$Treatment )
#--------------------------------------------------
#INFORMATION ON STATISTICAL MODELS
# gamm model:   https://www.rdocumentation.org/packages/mgcv/versions/1.8-31/topics/gamm
# random effect in gamm: https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html
# autocorrelation in gamm: https://stats.stackexchange.com/questions/258229/how-to-specify-autocorrelation-in-gamm
# https://jroy042.github.io/nonlinear/week4.html

#--------------------------------------------------
#HYPOTHESIS GRAPHIC: Biodiversity higher with prescribed burns present, both with and without Rough Fire
plot1 = ggplot(dataIn, aes(x = as.factor(Treatment), y = as.numeric(as.character(ACIout))) ) +
  geom_boxplot()
plot2 = ggplot(dataIn, aes(x = as.factor(daySinceFire), y = as.numeric(as.character(ACIout)), color=Treatment) ) +
  geom_point()
grid.arrange(plot1, plot2, ncol=1, nrow = 2)
#strong seasonal componenet to the conditions
ggplot(dataIn, aes(x = as.factor(Treatment), y = as.numeric(as.character(BKdBA_low))) ) +
  geom_boxplot()
#dataIn$BKdBA_low
#--------------------------------------------------
#CORRELATION between observations one unit apart in time, for input into gamm model autocorrelation term
#need to do this by site, take the mean
sites = ( unique(dataIn$Site))
corACIs=NULL
for (ii in 1:9){
  tmp = dataIn[dataIn$Site == sites[ii],]
  tmp$ACIoutShift = data.table::shift(tmp$ACIout,fill=NA)
  #ggplot(tmp, aes(x = ACIoutShift, y=ACIout) ) +
    #geom_point()+
    #geom_smooth(method=lm)
  corACIs = rbind(corACIs, cor(tmp$ACIoutShift,tmp$ACIout, method = "pearson",use="complete.obs"))
  
}
corACI=mean(corACIs)

#--------------------------------------------------
#MODELS
#--------------------------------------------------
#We did not do a model selection process, because we wanted to see what variables were important in explaining the variation verse
#optomizing prediciton power. We did try a few iterations (see below) mainly removing certain conditions because of data 
ctrl = list(nthreads=6)
dataIn$ACIoutLog = log10(dataIn$ACIout)

#Predictor variables
# dataIn$Treatment2
# dataIn$daySinceFire
# dataIn$julDay
# dataIn$TempC
# dataIn$BKdBA_low
# dataIn$Rain2
# dataIn$Site
# dataIn$Day

# 1a) global.GammT = gamm(ACIout ~ (Treatment)+ s(daySinceFire)+ s(julDay)+ s(TempC)+ s(BKdBA_low)+ Rain2, 
#                     random=list(Site=~1), correlation=corCAR1(value=corACI,form=~Day|Site),
#                     data=dataIn, method="REML", na.action=na.omit, control=ctrl)

# 1b) Global model with understory temperature and autocorrelation
global.GammT = gamm(ACIoutLog ~ (Treatment2)+ s(daySinceFire, by = Treatment2)+ s(julDay)+ s(TempC)+ s(BKdBA_low)+ Rain2, 
                   random=list(Site=~1), correlation=corCAR1(value=corACI,form=~Day|Site),
                   data=dataIn, method="REML", na.action=na.omit, control=ctrl)
summary(global.GammT$gam)
gam.check(global.GammT$gam)
pacf(residuals(global.GammT$gam))
anova(global.GammT$gam)

op <- par(mfrow=c(3,3))
plot(global.GammT$gam, shade = TRUE, residuals = TRUE, all.terms = TRUE)
par(op)
#conditional plots-- nicer looking (NOTE: median for continurious and most common for discrete)
#use visreg https://pbreheny.github.io/visreg/gg.html
global.GammT$gam$data <- dataIn
par(mfrow=c(1,1))
visreg(global.GammT$gam, "daySinceFire", by="Treatment2", gg=TRUE, overlay=TRUE, strip.names=c("UB", "UB+Rx", "RF","RF+Rx"))
visreg(global.GammT$gam, "daySinceFire", "Treatment2", gg=TRUE, ylab="predicted ACI", strip.names=c("UB", "UB+Rx", "RF","RF+Rx"))
#this plot with secondary access for canaopy cover and shrub cover and pictographic of treatment
# hard to do because understory temperature missing from last year of data...
visreg2d(global.GammT$gam, "daySinceFire", "Treatment", main = "") #copy to spreadsheet for model results

# 2) Global model withOUT understory temperature and autocorrelation
global.GammNoT = gamm(ACIoutLog ~ (Treatment)+ s(daySinceFire, by = Treatment)+ s(julDay)+ s(BKdBA_low)+ Rain2, 
                    random=list(Site=~1), correlation=corCAR1(value=corACI,form=~Day|Site),
                    data=dataIn, method="REML", na.action=na.omit, control=ctrl)

summary(global.GammNoT$gam)
gam.check(global.GammNoT$gam)
pacf(residuals(global.GammNoT$gam))
anova(global.GammNoT$gam)
op <- par(mfrow=c(3,3))
plot(global.GammNoT$gam, shade = TRUE, residuals = TRUE, all.terms = TRUE)
par(op)
global.GammNoT$gam$data <- dataIn
par(mfrow=c(1,1))
visreg(global.GammNoT$gam, "daySinceFire", by="Treatment", gg=TRUE, overlay=TRUE)+
  xlim(c(200,1500))+
  theme_minimal()
visreg(global.GammNoT$gam, "daySinceFire", "Treatment", gg=TRUE, ylab="predicted ACI")


#ADD SPPRING PERIOD TO GRAPHIC
#June1st for all years
D2plot = c(as.Date("2016-06-01",format="%Y-%m-%d"),as.Date("2017-06-01",format="%Y-%m-%d"),as.Date("2018-06-01",format="%Y-%m-%d"),as.Date("2019-06-01",format="%Y-%m-%d"))
D2plot2 = NULL
for(ii in 1:length(D2plot)){
  tmp = dataIn$daySinceFire[match(D2plot[ii],dataIn$Day)]
  D2plot2 = rbind(D2plot2, tmp)
}
#May 1 st all years
D2plotS = c(as.Date("2016-05-01",format="%Y-%m-%d"),as.Date("2017-05-01",format="%Y-%m-%d"),as.Date("2018-05-01",format="%Y-%m-%d"),as.Date("2019-05-01",format="%Y-%m-%d"))
D2plotSS = NULL
for(ii in 1:length(D2plotS)){
  tmp = dataIn$daySinceFire[match(D2plotS[ii],dataIn$Day)]
  D2plotSS = rbind(D2plotSS, tmp)
}

D2plotC = cbind(D2plotS, D2plotSS, D2plot,D2plot2,c(0,0,0,0))
colnames(D2plotC) = c("dateS","daysSincefireS","dateE","daysSincefireE","value")
D2plotC = as.data.frame(D2plotC)


p = visreg(global.GammNoT$gam, "daySinceFire", by="Treatment", gg=TRUE, overlay=TRUE)+
  xlim(c(200,1500))+
  #add May1 to June 1
  geom_vline(xintercept = D2plotC$daysSincefireS,linetype="dashed", 
             color = "black", size=.3) +
  geom_vline(xintercept = D2plotC$daysSincefireE,linetype="dashed", 
             color = "black", size=.3) +
  #add fuel loadings
  #scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#E69F00"))+
  theme_minimal()
p
#ADD FIRE SURVEY DATA TO SECOND PLOT
firSur = read.csv("D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\Master_PlotConditonSummary_2016and2019_reformat.csv")
load("D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\SEKI_DailyMeanData_2020-06-25")

#ORGANIZE variables from survey data
#-------------------
head(firSur)
firSur$siteYR  = paste(firSur$Site,firSur$Year,sep = '-')
firSur$treatYR = paste(firSur$Treatment,firSur$Year,sep = '-')
ttmts = as.character( unique( firSur$Treatment))
shrub = aggregate(shrubCover~treatYR, data=firSur, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x),count=length(x)))
fuel  = aggregate(total     ~treatYR, data=firSur, FUN=function(x) c(mean=mean(x), sd=sd(x), min=min(x), max=max(x),count=length(x)))
tmp = paste(firSur$Year,"06","01", sep = '-')
firSur$Date1 = c(as.Date(tmp,format="%Y-%m-%d"))
#find a matching days since fire and add to the matrix
for(ii in 1:nrow(firSur)){
  tmp = dataIn$daySinceFire[match(firSur$Date1[ii],dataIn$Day)]
  firSur$daySinceFire[ii] = tmp
}
firSur = firSur[1:46,]
ggplot(data = firSur, aes(x=daySinceFire,shrubCover,color=Treatment))+
  geom_point() +
  geom_line() +
  xlim(c(200,1500))+
  scale_y_continuous(position = "right")+
  scale_color_manual(values = c("#00A5FF", "#E76BF3", "#BB9D00", "#00B81F", "#FC717F"))+
  #scale_y_continuous(position = "right")+
  #add May1 to June 1
  geom_vline(xintercept = D2plotC$daysSincefireS,linetype="dashed", 
             color = "gray", size=.3) +
  geom_vline(xintercept = D2plotC$daysSincefireE,linetype="dashed", 
             color = "gray", size=.3) +
    theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
ggplot(data = firSur, aes(x=daySinceFire,total,color=Treatment))+
  geom_point() +
  geom_line() +
  xlim(c(200,1500))+
  scale_y_continuous(position = "right")+
  scale_color_manual(values = c("#00A5FF", "#E76BF3", "#BB9D00", "#00B81F", "#FC717F"))+
#add May1 to June 1
  geom_vline(xintercept = D2plotC$daysSincefireS,linetype="dashed", 
             color = "gray", size=.3) +
  geom_vline(xintercept = D2plotC$daysSincefireE,linetype="dashed", 
             color = "gray", size=.3) +
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )

#strip.names=c("UB", "UB+Rx", "RF","RF+Rx")
#copy ABOVE PLOTS to spreadsheet for model results
#NOT USED
#visreg(global.GammT$gam,"Treatment", whitespace=.5)
#visreg(global.GammT$gam,"daySinceFire")
#visreg(global.GammT$gam, "daySinceFire", "Treatment", gg=TRUE, ylab="predicted ACI")
#par(mfrow=c(1,2))
#visreg(global.GammT$gam,"daySinceFire",type="conditional")
#visreg(global.GammT$gam,"daySinceFire",type="contrast")
#(not working) partial residual plot-- nicer looking (not sure how to do for multiple variables...)
# http://zevross.com/blog/2014/09/15/recreate-the-gam-partial-regression-smooth-plots-from-r-package-mgcv-with-a-little-style/

#PREDICT 
#remove rows with NA
idx =  apply(is.na(dataIn[,c("Treatment", "daySinceFire", "julDay", "TempC", "BKdBA_low", "Rain2", "Site" )]),1,any) 
dataInMod = dataIn[!idx,]
# plot(dataInMod$ACIout, predict.gam(global.GammT$gam),col =dataInMod$Treatment )
dataInMod$FittedVals = global.GammT$gam$fitted.values
dataInMod$PredVals   = predict.gam(global.GammT$gam) #these are the same!!
ggplot(dataInMod, aes(daySinceFire, PredVals, color = Treatment ))+
  geom_point() 
#can I just predict for a given month- and show days since fire
dataInMod$month = month( dataInMod$Day )
ggplot(dataInMod, aes(julDay, FittedVals, color = Treatment ))+
  geom_point() 
tmpMonth = dataInMod[dataInMod$month == 6, ]
ggplot(tmpMonth, aes(daySinceFire, FittedVals, color = Treatment ))+
  geom_point() +
  labs(title = "June")


# #--------------------------------------------------
## NOT USED IN FINAL MODEL, BUT KEEP FOR RECORD OF DIFFERNT MODELS TRIED
# #--------------------------------------------------
# # 2) Global model withOUT temperature- with autocorrelation
# global.Gamm = gamm(ACIout ~ (Treatment)+ s(daySinceFire)+ s(julDay)+ s(BKdBA_low)+ Rain2, 
#                    random=list(Site=~1), correlation=corCAR1(value=corACI,form=~Day|Site),
#                    data=dataIn, method="REML", na.action=na.omit, control=ctrl)
# 
# 
# #model performance
# anova(global.Gamm$gam)
# summary(global.Gamm$gam) #record results in excel table
# 
# #model plots
# op2 <- par(mfrow=c(1,2))
# plot(global.Gamm$gam, shade = TRUE, residuals = TRUE, all.terms = TRUE)
# par(op2)
# termplot(global.Gamm$gam,terms="x0",se=TRUE)
# 
# #--------------------------------------------------
# #OUTPUTS
# dateCreate = Sys.Date()
# setwd(dataDir)
# save( global.GammT, file = paste0(dataDir,"SEKI_dailyGAMMwithT_",dateCreate) )
# save( global.Gamm,  file = paste0(dataDir,"SEKI_dailyGAMMwithoutT_",dateCreate) )
# # this does not read back in as expected!!
# 
# #--------------------------------------------------
# # NOT RUN #
# #POSSIBLE model combinations, using gam without autocorrelation function
# # NOTE: used this to inform selection of global models- but not for model averaging
# # modlGAMM = list()
# # #global model
# # modlGAMM[[1]]  = gam(ACIout ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)+ s(TempC)+ s(BKdBA_low)+ Rain+  s(Site,bs="re"), data=dataIn, method = "REML" , na.action=na.omit)
# # #single predictor models, with random
# # modlGAMM[[2]]  = gam(ACIout ~ Treatment +          s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[3]]  = gam(ACIout ~ s(daySinceFire)+     s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[4]]  = gam(ACIout ~ s(julDay)+           s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[5]]  = gam(ACIout ~ s(timeSinceSunrise)+ s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[6]]  = gam(ACIout ~ s(TempC)+            s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[7]]  = gam(ACIout ~ s(BKdBA_low)+        s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[8]]  = gam(ACIout ~ (Rain)+              s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #combinations of all "fire" variables, with and without random
# # modlGAMM[[9]]  = gam(ACIout ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)+ s(TempC) + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[10]] = gam(ACIout ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)+ s(TempC) + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #just treatment and since fire- simplist model
# # modlGAMM[[11]] = gam(ACIout  ~ Treatment+ s(daySinceFire)                                          + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #without understory temp- because some missing data
# # modlGAMM[[12]] = gam(ACIout  ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)          + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #without seasonal- likely not the case
# # modlGAMM[[13]] = gam(ACIout  ~ Treatment+ s(daySinceFire)+            s(timeSinceSunrise)+ s(TempC)+ s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #without daily- likely not the case
# # modlGAMM[[14]] = gam(ACIout  ~ Treatment+ s(daySinceFire)+ s(julDay)+                      s(TempC)+ s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #without fire- likely not the case
# # modlGAMM[[15]] = gam(ACIout  ~ Treatment+                  s(julDay)+ s(timeSinceSunrise)+ s(TempC)+ s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #include acoustic index variables (modify these based on "best" of above)
# # modlGAMM[[16]] = gam(ACIout  ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)+ s(TempC)+ s(BKdBA_low)+ (Rain)  + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[17]] = gam(ACIout  ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)+ s(TempC)+ s(BKdBA_low)          + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # modlGAMM[[18]] = gam(ACIout  ~ Treatment+ s(daySinceFire)+ s(julDay)+ s(timeSinceSunrise)+ s(TempC)                        + s(Site,bs="re"), data=dataIn, method="REML", na.action=na.omit)
# # #compare models
# # AIC_GAMM = NULL
# # for (ii in 1:18){
# #   summary(modlGAMM[[ii]])
# #   cat("model ",ii,": AIC=", AIC(modlGAMM[[ii]]),"\n" )
# #   AIC_GAMM = rbind(AIC_GAMM, c(ii,AIC(modlGAMM[[ii]])))
# # }
# # modOrder = as.data.frame( AIC_GAMM[order(AIC_GAMM[,2]) ,] )
# # modOrder[2:18,3] = diff(modOrder[,2])
# # bestModGAM = ( modlGAMM[[modOrder[1,1]]] )
# # summary(bestModGAM)
# # plot(bestModGAM)
# 
# 
