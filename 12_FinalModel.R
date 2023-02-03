rm(list=ls())
library(ggplot2)
library(mgcv)
library(data.table)
library(visreg)
library(gridExtra)
library(MuMIn)
library(gtools)
library(future.apply)
library(gridExtra)
library(lubridate)
library()

runModel = FALSE
#--------------------------------------------------
#READ IN DATA
dataDir = "G:\\My Drive\\ActiveProjects\\MANUSCRIPTS\\2.InReview\\EM_SEKIFire\\data"
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
dataIn$Treatment2 = dataIn$Treatment #make copy
dataIn$Treatment2[dataIn$Site == 10]

levels(dataIn$Treatment)[1] = "UB2"    #40,50
unique( dataIn$Site[dataIn$Treatment == "UB2"] )
levels(dataIn$Treatment)[2] = "UB1"    #30
unique( dataIn$Site[dataIn$Treatment == "UB1"] )
levels(dataIn$Treatment)[3] = "UB1+Rx" #10,20
unique( dataIn$Site[dataIn$Treatment == "UB1+Rx"] )
levels(dataIn$Treatment)[4] = "RF1"    #60,70
unique( dataIn$Site[dataIn$Treatment == "RF1"] )
levels(dataIn$Treatment)[5] = "RF1+Rx" #80,90
unique( dataIn$Site[dataIn$Treatment == "RF1+Rx"] )
levels( dataIn$Treatment )


#--------------------------------------------------
# Look for shifts in phenology- find days with max ACI-- Table 4
tmp = dataIn[dataIn$Treatment =="RF1",]
tmp$Year = year( tmp$Day )
tmp$Mth = month( tmp$Day )
aggregate(tmp["ACIout"], by=tmp["Year"], max)
tmp = tmp[tmp$Mth == "6",]
tmp = tmp[tmp$Year == "2016",]
tmpS = tmp[rev(order(tmp$ACIout)),]
tmpS[1:3,]


#--------------------------------------------------
# summary of days of recording at each site-- Table 1
library(dplyr)
dataIn$Year = year(dataIn$Day)
data_count_2 <- dataIn %>%                              # Applying group_by & summarise
  group_by(Site,Year) %>%
  summarise(count = n_distinct(Day))
data_count_2
data_count_2 <- dataIn %>%                              # Applying group_by & summarise
  group_by(Site) %>%
  summarise(count = n_distinct(Day))
data_count_2
#check
tmp = dataIn[dataIn$Site == 10,]
length(unique( tmp$Day ))



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
#
dataIn[((dataIn$daySinceFire==400)),c(1:3,29)]
max(dataIn$daySinceFire) -min(dataIn$daySinceFire)
#--------------------------------------------------
#CORRELATION between observations one unit apart in time, for input into gamm model autocorrelation term
#need to do this by site, take the mean
sites = (unique(dataIn$Site))
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
# 1) Global model with understory temperature and autocorrelation
dataIn <- within(dataIn, {
  air_c <- as.numeric(as.character(air_c))
  Rain2 <- as.factor(Rain2)
})
dataIn$lgACI = log10(dataIn$ACIout)
names(dataIn)[6] = "Trt"; names(dataIn)[8] = "bkdB"; names(dataIn)[28] = "tSun"; names(dataIn)[29] = "dFire"
names(dataIn)[30] = "jDay"
mdls <- c(
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                                                 + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                             + s(bkdB)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                             + s(bkdB, by=Trt)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                             + s(bkdB, by=Site)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Site)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c)          + s(bkdB)           + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)  + s(bkdB)           + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Site) + s(bkdB)           + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c)          + s(bkdB, by=Trt)   + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)  + s(bkdB, by=Trt)   + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)  + s(bkdB, by=Site)  + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt)                     + s(bkdB)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt)                                         + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Site)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB, by=Trt)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB, by=Trt)),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB)           + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB, by=Trt)   + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB, by=Trt)   + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB)           + Rain2),
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Site) + s(bkdB, by=Trt)   + Rain2))
mdlix <- seq(along=mdls) - 1
names(mdls) <- paste(1+mdlix%/%26,chr(asc("a")+mdlix%%26),sep="")
nix <- apply(is.na(dataIn[,c("lgACI","dFire","jDay","air_c","bkdB","Rain2","Site","Trt")]),1,any)

if (runModel == TRUE) {
  ctrl = lmeControl()
  ctrl$maxIter=100; ctrl$msMaxIter=100; ctrl$niterEM=0; ctrl$nthreads=1 #; ctrl$nthreads=8 if not future.lapply
  
  #sekiGamms = lapply(mdls,function(mm) gamm(mm, correlation=corCAR1(value=corACI,form=~Day|Site),
  #                                          random=list(Site=~1), data=dataIn[!nix,], method="REML",
  #                                          niterPQL=40,na.action=na.omit, control=ctrl))
  plan(multisession) ## Run in parallel on local computer
  sekiGamms = future_lapply(mdls,gamm, correlation=corCAR1(value=corACI,form=~Day|Site),
                            random=list(Site=~1), data=dataIn[!nix,], method="REML",
                            niterPQL=60,na.action=na.omit, control=ctrl)
  names(sekiGamms) <- names(mdls)
  save(sekiGamms,file="sekiGamms20201017.Rdata")
  
} else {
  
  load("E:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\code\\RoughFireProcess\\sekiGamms20201017.Rdata")
  mrslt <- summary(model.avg(lapply(sekiGamms,"[[","lme"),
                             ct.args=list(model.names=names(mdls))))
  #USE THIS TABLE TO GET AICc for model selection
  
  # FOCUSING ON THE BEST MODELS
  # EVALUATE model
  tblix <- paste("X",names(mdls),sep="")
  tblix[27:29] <- c("X1NA","X1NA.1","X1NA.2") # patch to deal with crazy row names in msTable
  mBest <- sekiGamms[[match(row.names(mrslt$msTable),tblix)[1]]]$gam
  
  #mBest <- sekiGamms[[match("X1n",tblix)[1]]]$gam
  summary(mBest)
  #gam.check(mBest)
  #pacf(residuals(mBest)) # does residuals() account for random effect, assumed corr structure?
  #anova(mBest)
  
  # PLOTS-- copy to spreadsheet for interpretation ---
  # op <- par(mfrow=c(2,3))
  # plot(mBest, shade = TRUE, residuals = TRUE, all.terms = TRUE)
  # par(op)
  #termplot(mBest,terms="x0",se=TRUE)
  
  # PLOTS-- for manuscript
  #conditional plots-- nicer looking (NOTE: median for continuous and most common for discrete)
  #use visreg https://pbreheny.github.io/visreg/gg.html
  mBest$data <- dataIn
  #par(mfrow=c(1,1))
  #visreg(mBest, "dFire", by="Trt", gg=TRUE, overlay=TRUE, strip.names=unique(dataIn$Trt))
  #visreg(mBest, "dFire", "Trt",    gg=TRUE, ylab="predicted ACI") #BEST TO DIVIDE UP to see patterns
  #visreg2d(mBest, "dFire", "Trt", main = "") #copy to spreadsheet for model results
  
  #ADD SPPRING PERIOD TO GRAPHICs-- for reference
  #June 1st for all years
  D2plot = c(as.Date("2016-06-01",format="%Y-%m-%d"),as.Date("2017-06-01",format="%Y-%m-%d"),as.Date("2018-06-01",format="%Y-%m-%d"),as.Date("2019-06-01",format="%Y-%m-%d"))
  D2plot2 = NULL
  for(ii in 1:length(D2plot)){
    tmp = dataIn$dFire[match(D2plot[ii],dataIn$Day)]
    D2plot2 = rbind(D2plot2, tmp)
  }
  #May 1st all years
  D2plotS = c(as.Date("2016-05-01",format="%Y-%m-%d"),as.Date("2017-05-01",format="%Y-%m-%d"),as.Date("2018-05-01",format="%Y-%m-%d"),as.Date("2019-05-01",format="%Y-%m-%d"))
  D2plotSS = NULL
  for(ii in 1:length(D2plotS)){
    tmp = dataIn$dFire[match(D2plotS[ii],dataIn$Day)]
    D2plotSS = rbind(D2plotSS, tmp)
  }
  D2plotC = cbind(D2plotS, D2plotSS, D2plot,D2plot2,c(0,0,0,0))
  colnames(D2plotC) = c("dateS","daysSincefireS","dateE","daysSincefireE","value")
  D2plotC = as.data.frame(D2plotC)
  
  
  p = visreg(mBest, "dFire", by="Trt", gg=TRUE, overlay=TRUE, strip.names=unique(dataIn$Trt))+
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
  
  #FIGURE 2 in paper
  ## does not re-order: mBest$Trt <- factor(mBest$Trt , levels=c("RF1", "RF1+Rx", "UB1+Rx", "UB1","UB2"))
  #edited in illustrator
  p = visreg(mBest, "dFire", "Trt", gg=TRUE, ylab="predicted ACI") +
    xlim(c(200,1500))+
    xlab("Days since Rough Fire extinguished")+
    #add May1 to June 1
    #geom_vline(xintercept = D2plotC$daysSincefireS,linetype="dashed", 
               #color = "black", size=.3) +
    geom_vline(xintercept = D2plotC$daysSincefireE,linetype="dashed", 
               color = "grey", size=.2) + #June 1
    #annotate("text", x=200, y=.2, label= "(D)", size=5) +
    #add fuel loadings
    #scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#E69F00"))+
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p
  
  
  #FIGURE S1
  op <- par(mfrow=c(4,3))
  
  visreg(mBest, "bkdB", by="Site", gg=TRUE,ylab="predicted ACI")+
    #xlim(c(200,1500))+
    xlab("Background sound level (0.315 - 1.25 Hz)")+
    #scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#E69F00"))+
    theme_minimal()+
    theme(panel.grid.major = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))

  
  #FIGURES: June to show phenological changes
  dataIn$Mth = month(dataIn$Day)
  dataJune   = dataIn[dataIn$Mth == 6 ,]
  dataJune$year = year(dataJune$Day)
  #   as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay) + s(air_c, by=Trt)  + s(bkdB, by=Site)  + Rain2),
  pACIJune   = predict(mBest, dataJune)
  idx = apply(is.na(dataJune[,c("Trt","dFire","jDay","air_c","bkdB","Rain2")]),1,any) #remove rows with NA
  pACIJune2 = as.data.frame( cbind(pACIJune, dataJune$dFire[!idx],  as.character(dataJune$Trt[!idx]), 
                                   as.character(dataJune$Day[!idx]), dataJune$jDay[!idx], year(dataJune$Day )) )
  colnames(pACIJune2) = c("pACI", "dFire","FH","Day","jDay","year")
  
  #box plot of predicted ACI by fire history and month
  pACIJune2$FH <- factor(pACIJune2$FH , levels=c("RF1", "RF1+Rx", "UB1+Rx", "UB1","UB2"))
   ggplot(pACIJune2, aes(x = (as.character(year)), y=as.numeric(as.character(pACI)), fill = FH) )+
    geom_boxplot()+
    geom_jitter(position=position_jitter(0.2), alpha = .1)+
    xlab("")+
    ylab("predicted ACI")+
    geom_vline(xintercept = 1.5) +
    geom_vline(xintercept = 2.5) +
    geom_vline(xintercept = 3.5) +
    theme_minimal()+ 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
   #to re-order the box plots 
   pACIJune2$FH <- factor(pACIJune2$FH , levels=c("RF1", "RF1+Rx", "UB1+Rx", "UB1","UB2"))
   ggplot(pACIJune2, aes(x = (as.character(year)), y=as.numeric(as.character(pACI)), fill = FH) )+
     geom_boxplot()+
     geom_point(position=position_jitterdodge(),alpha = .1)+
     #geom_jitter(position=position_jitter(0.2), alpha = .1)+
     xlab("")+
     ylab("predicted ACI")+
     geom_vline(xintercept = 1.5) +
     geom_vline(xintercept = 2.5) +
     geom_vline(xintercept = 3.5) +
     theme_minimal()+ 
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"))

    #point/line of predicted ACI at rough fire sites
  dataIn$Day = as.Date(dataIn$Day)
  dataIn$ACIout = as.numeric(as.character(dataIn$ACIout))
  dataIn$FH = dataIn$Trt
  #only RF sites
  dataInRF = dataIn[dataIn$FH == "RF1" | dataIn$FH == "RF1+Rx",]
  #daily average across sites in treatments...
  uday = unique(dataInRF$Day)
  RFavg = NULL
  RF1 = NULL
  RFx = NULL
  for(dd in 1:length(uday)){
    tmp1 = dataInRF[dataInRF$Day == uday[dd],]
    tmpRF = tmp1[tmp1$FH == "RF1",]
    tmpRFx = tmp1[tmp1$FH == "RF1+Rx",]
    RF1 = rbind(RF1, rbind(cbind( as.character(uday[dd]), as.numeric(as.character(mean(tmpRFx$ACIout, na.rm = TRUE))),"RF")))
    RFx = rbind(RFx, rbind(cbind( as.character(uday[dd]), as.numeric(as.character(mean(tmpRFx$ACIout, na.rm = TRUE))),"RF+Rx")))
    tmp3 = rbind(cbind( as.character(uday[dd]), as.numeric(as.character(mean(tmpRFx$ACIout, na.rm = TRUE))),"RF+Rx"),
      cbind( as.character(uday[dd]), as.numeric(as.character(mean(tmpRF$ACIout, na.rm = TRUE))),"RF" ) )
    RFavg = rbind(RFavg,tmp3)
  }
  RFavg = as.data.frame(RFavg)
  colnames(RFavg) = c("Day","ACIout","FH")
  RFavg$Day = as.Date(RFavg$Day)
  RFavg$ACIout = as.numeric( as.character(RFavg$ACIout) )
  
  RF1 = as.data.frame(RF1)
  colnames(RF1) = c("Day","ACIout","FH")
  RF1$Day = as.Date(RF1$Day)
  RF1$ACIout = as.numeric( as.character(RF1$ACIout) )
  
  RFx = as.data.frame(RFx)
  colnames(RFx) = c("Day","ACIout","FH")
  RFx$Day = as.Date(RFx$Day)
  RFx$ACIout = as.numeric( as.character(RFx$ACIout) )
  
  #breakpoint analysis https://rpubs.com/MarkusLoew/12164
  library(segmented)
  
  p17 = ggplot(RFavg,aes(x=Day, y=ACIout, color=FH) )+
    geom_point(alpha = .1)+
    geom_ma(n=5)+
    geom_smooth(method = "loess")+
    ylab("daily ACI")+
    ggtitle("2017- 2nd spring post fire")+
    xlab("")+
    ylim(c(0,.7))+
    xlim(c(as.Date("2017-03-01"), as.Date("2017-08-01") )) +
    annotate(geom = "text", x=as.Date("2017-03-15"), y=.30, label = "RF1+Rx")+
    annotate(geom = "text", x=as.Date("2017-03-15"), y=.05, label = "RF1")+
    theme(legend.position = "",panel.background = element_rect(fill = "transparent"))
  
  p18 = ggplot(RFavg,aes(x=Day, y=ACIout, color=FH) )+
    geom_point(alpha = .1)+
    geom_ma(n=5)+
    geom_smooth(method = "loess")+
    ylab("daily ACI")+
    ggtitle("2017- 2nd spring post fire")+
    xlab("")+
    ylim(c(0,.7))+
    annotate(geom = "text", x=as.Date("2018-03-15"), y=.50, label = "RF1+Rx")+
    annotate(geom = "text", x=as.Date("2018-03-15"), y=.15, label = "RF1")+
    xlim(c(as.Date("2018-03-01"), as.Date("2018-08-01") )) +
    theme(legend.position = "",panel.background = element_rect(fill = "transparent"))
  grid.arrange(p17,p18, nrow=2,ncol=1)
  
  library(bp)
  library(tidyquant)
  
  p17 = ggplot(dataInRF,aes(x=Day, y=ACIout, color=FH) )+
    geom_point(alpha = .1)+
    #geom_ma(n=10)+
    geom_smooth(method = "loess")+
    ylab("daily ACI")+
    ggtitle("2017- 2nd spring post fire")+
    xlab("")+
    xlim(c(as.Date("2017-04-01"), as.Date("2017-08-01") )) +
    annotate(geom = "text", x=as.Date("2017-04-15"), y=.30, label = "RF1+Rx")+
    annotate(geom = "text", x=as.Date("2017-04-15"), y=.05, label = "RF1")+
    theme(legend.position = "",panel.background = element_rect(fill = "transparent"))
    #theme_minimal()
  p18 = ggplot(dataInRF,aes(x=Day, y=ACIout, color=FH) )+
    geom_point(alpha = .1)+
    geom_smooth(method = "loess")+
    ggtitle("2018- 3rd spring post fire")+
    ylab("daily ACI")+
    xlab("")+
    annotate(geom = "text", x=as.Date("2018-04-15"), y=.50, label = "RF1+Rx")+
    annotate(geom = "text", x=as.Date("2018-04-15"), y=.15, label = "RF1")+
    xlim(c(as.Date("2018-04-01"), as.Date("2018-08-01") )) +
    theme(legend.position = "",panel.background = element_rect(fill = "transparent"))
    #theme_minimal()
  p19 = ggplot(dataInRF,aes(x=Day, y=ACIout, color=FH) )+
    geom_point(alpha = .1)+
    geom_smooth(method = "loess")+
    ylab("daily ACI")+
    ggtitle("2019- year 4")+
    xlab("")+
    xlim(c(as.Date("2019-04-01"), as.Date("2019-08-01") )) +
    theme_minimal()
  grid.arrange(p17,p18, nrow=2,ncol=1)
  

}


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
  tmp = dataIn$dFire[match(firSur$Date1[ii],dataIn$Day)]
  firSur$daySinceFire[ii] = tmp
}

#SUMMARIZE by treatment and year
head(firSur)
survSum = firSur %>% group_by(Treatment, Year) %>% summarise(mean(canopyCover,na.rm = T))
firSur %>% group_by(Treatment, Year) %>% summarise(n = n())

#summarise(mean(total,na.rm = T)) summarise(mean(hr1000,na.rm = T)) summarise(mean(coverShrub,na.rm = T))  

# PLOTS-- NOT VERY HELPFUL, just use a table
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
