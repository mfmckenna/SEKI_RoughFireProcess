# Statistical Models for SEKI Rough Fire Acoustic Project

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
library(AICcmodavg)

runModel = FALSE
#--------------------------------------------------
#READ IN DATA
#--------------------------------------------------
dataDir = "G:\\My Drive\\ActiveProjects\\MANUSCRIPTS\\2.InReview\\EM_SEKIFire\\data"
setwd(dataDir)
dataIn = choose.files(caption = "Open: SEKI_DailyMeanData (most recent date)" )
load(dataIn)
dataIn = dailyData
#rm(dailyData)

#--------------------------------------------------
#what data in the daily averages?
#--------------------------------------------------
tmp = dataIn[ dataIn$sunriseOnly==F,]
tmp$mo = month(tmp$Day)
unique( tmp$mo )
unique( tmp$Year ) # all years!
#only winter months have dusk hours included in the mean

#--------------------------------------------------
##DATA CLEAN-UP: known sources of possible variation not relevant to the question
#--------------------------------------------------
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
#--------------------------------------------------
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
# Look for shifts in phenology- find days with max ACI-- old Table 4
#--------------------------------------------------
tmp = dataIn[dataIn$Treatment =="RF1",]
tmp$Year = year( tmp$Day )
tmp$Mth = month( tmp$Day )
aggregate(tmp["ACIout"], by=tmp["Year"], max)
tmp = tmp[tmp$Mth == "6",]
tmp = tmp[tmp$Year == "2016",]
tmpS = tmp[rev(order(tmp$ACIout)),]
tmpS[1:3,]
#updated with predicted ACI values to account for variation related to other factors

#--------------------------------------------------
# summary of days of recording at each site-- Table 1/ Figure S1
#--------------------------------------------------
library(dplyr)
as.data.frame(colnames(dataIn))
dataIn$Year = year(dataIn$Day)
data_count_2 <- dataIn %>%                              # Applying group_by & summarise
  group_by(Site,Year) %>%
  summarise(count = n_distinct(Day))
data_count_2
data_count_2 <- dataIn %>%                              # Applying group_by & summarise
  group_by(Site) %>%
  summarise(count = n_distinct(Day))
sum(data_count_2$count)
#check
tmp = dataIn[dataIn$Site == 10,]
length(unique( tmp$Day ))

#make a graphic of recording days across sites... tile graphic with #days of recordings
library(zoo)
dataIn$mo = month(dataIn$Day)
Date22 = as.yearmon (paste(dataIn$Year, dataIn$mo), "%Y %m")
dataIn$Date22 = Date22

data_count_2 <- dataIn %>%                              
  group_by(Site,Date22) %>%
  summarise(count = n_distinct(Day))
data_count_2

ggplot(data_count_2, aes(as.factor(Date22), Site, fill = as.numeric(as.character(count)) ) ) +
  geom_tile()+
  scale_fill_gradient2(low="white", high="black", name = "Days with recordings")+
  xlab("")+
  ylab("Acoustic Monitoring Site")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        text=element_text(size=12,family="Times New Roman"))

#--------------------------------------------------
# difference between RF+Rx-RF+Rx-- DO NOT USE, USE PREDICTED VALUES!!!
#https://dominicroye.github.io/en/2019/visualize-monthly-precipitation-anomalies/
#--------------------------------------------------
RF  = dataIn[dataIn$Treatment=="RF1",]
RFx = dataIn[dataIn$Treatment=="RF1+Rx",]
#(ERROR) NEED TO ALGN DAYS and take the mean across treatments!!
anom =  as.data.frame(cbind(as.character(RF$Day), RFx$ACIout -RF$ACIout) )
anom$V1 = as.Date(anom$V1)
anom$V2 = as.numeric(as.character(anom$V2))
anom$sign= ifelse(anom$V2 > 0, "pos", "neg") %>% factor(c("pos", "neg"))
anom$mo = month(anom$V1)
anom$yr = year(anom$V1)
library(zoo)
Date2 = as.yearmon (paste(anom$yr, anom$mo), "%Y %m")
anom$Date2 = Date2

#daily differences...
ggplot(anom, aes(as.Date(V1), V2, fill = sign)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  scale_x_date(date_breaks = "month", date_labels = "%b") +
  #scale_y_continuous(breaks = seq(-100, 100, 20)) +
  scale_fill_manual(values = c( "#034e7b","#99000d")) +
  labs(y = "Difference in ACI", x = "") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        text=element_text(size=12,family="Times New Roman"))
# add text, angle axes labels

#monthly difference
anom<-subset(anom, mo!=1 & mo!=2 &  mo!=3 & mo!=4 & mo!=5)
data_norm <-     group_by(anom, Date2) %>%
  summarise(mu = median(V2),  
            mx = max(V2),
            min = min(V2),
            q25 = quantile(V2, .25),
            q75 = quantile(V2, .75),
            iqr = q75-q25)
sign = ifelse(data_norm$mu > 0, "pos", "neg") %>% factor(c("pos", "neg"))
data_norm$sign = sign
data_norm$mo = as.factor(month(data_norm$Date2))
D2plot = c( as.yearmon("2016 06",format="%Y %m"), as.yearmon("2017 06 ",format="%Y %m"), as.yearmon("2018 06",format="%Y %m"), as.yearmon("2019 06",format="%Y %m"))
# max and min differences
g1.1 <- ggplot(data_norm) #+
   #geom_crossbar(aes(x = Date2, y = 0, ymin = min, ymax = mx),
                 #fatten = 0, fill = "grey95", colour = "NA")
#percentiles
g1.2 <- g1.1 + geom_crossbar(aes(x = Date2, y = 0, ymin = q25, ymax = q75),
                             fatten = 0, fill = "grey90",colour = "NA")
g1.2
#plot the monthly mean value with variation by month year
g1.3 <- g1.2 + geom_crossbar(aes(x = Date2, y = 0, ymin = 0, ymax = mu, fill = mo),
                             fatten = 0, width = 0.05, alpha = .8, colour = "NA",
                             show.legend = FALSE) 
g1.3
g1 <- g1.3 + geom_hline(yintercept = 0)+
  
  scale_fill_manual(values=c("grey20","grey30","grey40","grey50","grey60","grey70","grey80"))+
  scale_y_continuous("Difference in Acoustic Complexity Index \n (montly summaries)")+
                     #breaks = seq(-100, 500, 25),
                     #expand = c(0, 5))+
  labs(x = "",
       title = "",
       caption="")+
  #geom_vline(xintercept = D2plot,linetype="dashed", color = "black", size=.2) +
  annotate("text", x=max(Date2)-3.2, y= .5, label= "RF1+Rx > RF1",fontface = "bold",size = 4) + 
  annotate("text", x=max(Date2)-3.2, y=-.35, label= "RF1+Rx < RF1",fontface = "bold",size = 4) + 
  annotate("text", x=D2plot[1], y= -.1 , label= "June 2016",fontface = "bold",size = 3, angle =-90) + 
  annotate("text", x=D2plot[2], y= -.1 , label= "June 2017",fontface = "bold",size = 3, angle =-90) + 
  annotate("text", x=D2plot[3], y= -.1 , label= "June 2018",fontface = "bold",size = 3, angle =-90) + 
  annotate("text", x=D2plot[4], y= -.1 , label= "June 2019",fontface = "bold",size = 3, angle =-90) + 
  theme_minimal()
g1

#--------------------------------------------------
#INFORMATION ON STATISTICAL MODELS
# gamm model:   https://www.rdocumentation.org/packages/mgcv/versions/1.8-31/topics/gamm
# random effect in gamm: https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/random.effects.html
# autocorrelation in gamm: https://stats.stackexchange.com/questions/258229/how-to-specify-autocorrelation-in-gamm
# https://jroy042.github.io/nonlinear/week4.html
# temporal/seasonal data: https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
# evience ratios: https://rdrr.io/cran/AICcmodavg/man/evidence.html#:~:text=The%20evidence%20ratio%20can%20be,be%20specified%20in%20the%20model.
#--------------------------------------------------
#HYPOTHESIS GRAPHICS 
#treatment- prescribed burns present, both with and without Rough Fire
plot1 = ggplot(dataIn, aes(x = as.factor(Treatment), y = as.numeric(as.character(ACIout))) ) +
  geom_boxplot()+ggtitle ("fire history")
#days since fire
plot2 = ggplot(dataIn, aes(x = as.numeric(daySinceFire), y = as.numeric(as.character(ACIout))) ) +
  geom_point() +
  geom_smooth(method = 'loess') + ggtitle ("trend since fire")
#season
plot3 = ggplot(dataIn, aes(x = as.factor(mo), y = as.numeric(as.character(ACIout))) ) +
  geom_boxplot() + ggtitle ("season")
grid.arrange(plot2, plot3, plot1, ncol=1, nrow = 3)
#strong seasonal component to the conditions
ggplot(dataIn, aes(x = as.factor(Treatment), y = as.numeric(as.character(BKdBA_low))) ) +
  geom_boxplot()
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
#https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
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
  #1a
  as.formula(lgACI ~       s(dFire, by=Trt) + s(jDay)),
  #1b
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)),
  #1c
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                                                 + Rain2),
  #1d
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                             + s(bkdB)),
  #1e
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                             + s(bkdB, by=Trt)),
  #1f-- second best
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)                             + s(bkdB, by=Site)),
  #1g
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c)),
  #1h
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)),
  #1i
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Site)),
  #1j
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c)          + s(bkdB)           + Rain2),
  #1k
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)  + s(bkdB)           + Rain2),
  #1l
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Site) + s(bkdB)           + Rain2),
  #1m
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c)          + s(bkdB, by=Trt)   + Rain2),
  #1n
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)  + s(bkdB, by=Trt)   + Rain2),
  #1o--- best
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay)         + s(air_c, by=Trt)  + s(bkdB, by=Site)  + Rain2),
  #1p
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt)),
  #1q
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)),
  #1r
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt)                     + s(bkdB)),
  #1s
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt)                                         + Rain2),
  #1t
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)),
  #1u
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Site)),
  #1v
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB)),
  #1w
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB)),
  #1x
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB, by=Trt)),
  #1y
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB, by=Trt)),
  #1z
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB)           + Rain2),
  #2a
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c)          + s(bkdB, by=Trt)   + Rain2),
  #2b
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB, by=Trt)   + Rain2),
  #2c
  as.formula(lgACI ~ Trt + s(dFire, by=Trt) + s(jDay, by=Trt) + s(air_c, by=Trt)  + s(bkdB)           + Rain2),
  #2d
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
  
  sekiGamms = future_lapply(mdls, gamm, correlation=corCAR1(value=corACI,form=~Day|Site),
                            random=list(Site=~1), data=dataIn[!nix,], method="REML",
                            niterPQL=60, na.action=na.omit, control=ctrl)
  
  save(sekiGamms,file="sekiGamms20220202.Rdata")
  
  mrslt <- summary(model.avg(lapply(sekiGamms,"[[","lme"),
                             ct.args=list(model.names=names(mdls)) ) )
  mrslt$msTable
  
  #model comparison metrics-- table
  library(AICcmodavg)
  names(sekiGamms) <- names(mdls)
  aicctable.out = aictab(cand.set = lapply(sekiGamms,"[[","lme"), modnames =list(model.names=names(mdls)) )
  evidence(aic.table =   aicctable.out, model.high = "top", model.low = "second.ranked")
  
  
} else {
  
  load("G:\\My Drive\\ActiveProjects\\MANUSCRIPTS\\2.InReview\\EM_SEKIFire\\data\\sekiGamms20201017.Rdata")
  load("G:\\My Drive\\ActiveProjects\\MANUSCRIPTS\\2.InReview\\EM_SEKIFire\\data\\sekiGamms20220202.Rdata")
  
  #USE THIS TABLE TO GET AICc for model selection-- why only lme-- importance of the spline fit, how this works is the spline then bring splines into linear model
  mrslt = summary(model.avg( lapply(sekiGamms,"[[","lme"), ct.args = list(model.names=names(mdls)) ) )
  
  
  #sekiGamms$`1a`$gam #this tells us about the non-linear smooths
  
  #EVIDENCE RATIO
  #https://rdrr.io/cran/AICcmodavg/man/evidence.html#:~:text=The%20evidence%20ratio%20can%20be,be%20specified%20in%20the%20model.
  aic.table.1  = aictab(cand.set = lapply(sekiGamms,"[[","lme"), modnames = names(mdls), second.ord = TRUE, sort = T)
  evidence(aic.table = aic.table.1)
  
  
}

  # FOCUSING ON THE BEST MODELS
  # EVALUATE model--- 20201
  tblix <- paste("X",names(mdls),sep="")
  tblix[27:29] = c("X1NA","X1NA.1","X1NA.2") # patch to deal with crazy row names in msTable
  mBest <- sekiGamms[[match(row.names(mrslt$msTable),tblix)[1]]]$gam
  
  # EVALUATE model--- 202202
  tblix = paste("X",names(mdls),sep="")
  row.names(mrslt$msTable)[5]
  tblix[5] = c("X1NA")
  tblix[11:12] = c("X1NA.1","X1NA.2") # patch to deal with crazy row names in msTable
  row.names(mrslt$msTable)[16]
  tblix[16] = c("X1NA.3") 
  tblix
  row.names(mrslt$msTable)
  mBest = sekiGamms[[match(row.names(mrslt$msTable),tblix)[1]]]$gam
  
  #mBestLME = sekiGamms[[match(row.names(mrslt$msTable),tblix)[1]]]$lme
  
  summary(mBestLME)
  summary(mBest)
  summary(mBest)$r.sq
  
  #CHECKS--
  # gam.check(mBest)
  # pacf(residuals(mBest)) # does residuals() account for random effect, assumed corr structure?
  # anova(mBest)
  
  #PLOTS-- copy to spreadsheet for interpretation
  # op <- par(mfrow=c(2,3))
  # plot(mBest, shade = TRUE, residuals = TRUE, all.terms = TRUE)
  # par(op)
  # termplot(mBest,terms="x0",se=TRUE)
  
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
  
  #FIGURE 2 in paper, edited in illustrator
  #------------------------------------
  ## does not re-order (ugh): mBest$Trt <- ordered(mBest$Trt , levels=c("RF1", "RF1+Rx", "UB1+Rx", "UB1","UB2"))
  #edited in illustrator
  p = visreg(mBest, "dFire", "Trt", gg=TRUE, points=list(size=.1) ,nrow = 5) +
    xlim(c(200,1500))+
    ylab("predicted ACI") +
    xlab("Days since Rough Fire extinguished")+
    #add June 1
    geom_vline(xintercept = D2plotC$daysSincefireE,linetype="dashed", 
               color = "grey", size=.4) + #June 1
    #annotate("text", x=200, y=.2, label= "(D)", size=5) +
    #add fuel loadings
    #scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#E69F00"))+
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p
  
  
  #FIGURE S3- background noise
  #------------------------------------
  op <- par(mfrow=c(4,3))
  
  visreg(mBest, "bkdB", by="Site", gg=TRUE,ylab="predicted ACI")+
    #xlim(c(200,1500))+
    xlab("Background sound level (0.315 - 1.25 Hz)")+
    #scale_color_manual(values = c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#E69F00"))+
    theme_minimal()+
    theme(panel.grid.major = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text.x = element_text(hjust = 1), 
                  text=element_text(size=12,family="Times New Roman"))

  
  #FIGURES: June to show phenological changes
  #-------------------------------------------------------------
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
     geom_point(position=position_jitterdodge(0.1),alpha = .1)+
     geom_boxplot(alpha = .7,outlier.shape = NA)+
     
     #geom_jitter(position=position_jitter(0.2), alpha = .1)+
     xlab("")+
     scale_fill_discrete(name = "Fire History")+
     ylab("predicted Acoustic Complexity Index (log10ACI) \n in month of June")+
     geom_vline(xintercept = 1.5) +
     geom_vline(xintercept = 2.5) +
     geom_vline(xintercept = 3.5) +
     theme_minimal()+ 
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           text=element_text(size=14,family="Times New Roman"))

  
   
   
   #FIGURES: Monthly difference in pACI RF1 and RF1+Rx
   #-------------------------------------------------------------
   pACI  = predict(mBest, dataIn) #all data predicted ACI
   
   pACI_all = as.data.frame( cbind(pACI, dataIn )) 
   colnames(pACI_all)
   plot(pACI_all$pACI,pACI_all$ACIout)
   
   RF  = pACI_all[pACI_all$Trt=="RF1",]
   RFx = pACI_all[pACI_all$Trt=="RF1+Rx",]
   #difference for each value... assumes days match up, not quite right!
   
   RF2 = group_by(RF, Day) %>%
     summarise(mu = mean(pACI) ) 
   RFx2 = group_by(RFx, Day) %>%
     summarise(mu = mean(pACI) )
   
   anom = merge(RF2,RFx2, by="Day")
   colnames(anom) = c("V1","RF","RFx")
   anom$V2 = abs(anom$RF) - abs(anom$RFx)
   anom$V1 = as.Date(anom$V1)
   anom$V2 = as.numeric(as.character(anom$V2))
   
   anom$sign= ifelse(anom$RF < anom$RFx, "pos", "neg") %>% factor(c("pos", "neg"))
   anom$mo = month(anom$V1)
   anom$yr = year(anom$V1)
   
   library(zoo)
   Date2 = as.yearmon (paste(anom$yr, anom$mo), "%Y %m")
   anom$Date2 = Date2
   
   #daily differences...
   ggplot(anom, aes(as.Date(V1), V2, fill = sign)) + 
     geom_bar(stat = "identity", show.legend = FALSE) + 
     scale_x_date(date_breaks = "month", date_labels = "%b") +
     #scale_y_continuous(breaks = seq(-100, 100, 20)) +
     scale_fill_manual(values = c( "#034e7b","#99000d")) +
     labs(y = "Difference in ACI", x = "") +
     theme_minimal()
   # add text, angle axes labels
   
   #monthly difference
   anom<-subset(anom, mo!=1 & mo!=2 &  mo!=3 & mo!=4 & mo!=5)
   
   
   data_norm <-     group_by(anom, Date2) %>%
     summarise(mu = median(V2),  
               mx = max(V2),
               min = min(V2),
               q25 = quantile(V2, .25),
               q75 = quantile(V2, .75),
               iqr = q75-q25)
   sign = ifelse(data_norm$mu > 0, "pos", "neg") %>% factor(c("pos", "neg"))
   data_norm$sign = sign
   data_norm$mo = as.factor(month(data_norm$Date2))
   D2plot = c( as.yearmon("2016 06",format="%Y %m"), as.yearmon("2017 06 ",format="%Y %m"), as.yearmon("2018 06",format="%Y %m"), as.yearmon("2019 06",format="%Y %m"))
   
   # max and min differences
   g1.1 <- ggplot(data_norm) #+
   #geom_crossbar(aes(x = Date2, y = 0, ymin = min, ymax = mx),
   #fatten = 0, fill = "grey95", colour = "NA")
   #g1.1
   
   #percentiles
   g1.2 <- g1.1 + geom_crossbar(aes(x = Date2, y = 0, ymin = q25, ymax = q75),
                                fatten = 0, fill = "grey90",colour = "black",lwd=.02)
   g1.2
   
   #plot the monthly mean value with variation by month year
   
   
   g1.3 <- g1.2 + geom_crossbar(aes(x = Date2, y = 0, ymin = 0, ymax = mu, fill = mo),
                                fatten = 0, width = 0.05, alpha = .8,colour = "black",lwd=.2,
                                show.legend = FALSE) 
   g1.3
   
   g1 <- g1.3 + geom_hline(yintercept = 0)+
     
     scale_fill_manual(values=c("grey20","grey30","grey40","grey50","grey60","grey70","grey80"))+
     scale_y_continuous("Difference in predicted Acoustic Complexity Index \n (montly summaries)")+
     #breaks = seq(-100, 500, 25),
     #expand = c(0, 5))+
     labs(x = "",
          title = "",
          caption="")+
     #geom_vline(xintercept = D2plot,linetype="dashed", color = "black", size=.2) +
     annotate("text", x=max(Date2)-3.2, y= .5, label= "RF1+Rx > RF1",fontface = "bold",size = 4) + 
     annotate("text", x=max(Date2)-3.2, y=-.35, label= "RF1+Rx < RF1",fontface = "bold",size = 4) + 
     annotate("text", x=D2plot[1], y= -.1 , label= "June 2016",fontface = "bold",size = 3, angle =-90) + 
     annotate("text", x=D2plot[2], y= -.1 , label= "June 2017",fontface = "bold",size = 3, angle =-90) + 
     annotate("text", x=D2plot[3], y= -.1 , label= "June 2018",fontface = "bold",size = 3, angle =-90) + 
     annotate("text", x=D2plot[4], y= -.1 , label= "June 2019",fontface = "bold",size = 3, angle =-90) + 
     theme_minimal()+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           text=element_text(size=12,family="Times New Roman"))
   g1
   
   
   
   #--------------------------------------------------
   # Look for shifts in phenology- find days with max ACI-- Table 4
   #--------------------------------------------------
   # find days with max ACI-- Table 4
   #RF sites
   RF2 = group_by(RF, Day) %>%
     summarise(mu = mean(pACI), sd = sd(pACI) ) 
   RF2$Year = year( RF2$Day )
   RF2$Mth = month( RF2$Day )
   #aggregate(RF["pACI"], by=RF["Year"], max)
   tmp = RF2[RF2$Mth == "6",]
   tmp = tmp[tmp$Year == "2019",] # sub-in: 2016, 2017, 2018
   tmpS = tmp[rev(order(tmp$mu)),]
   tmpS[1:3,]
   
   RFx2 = group_by(RFx, Day) %>%
     summarise(mu = mean(pACI), sd = sd(pACI) ) 
   RFx2$Year = year( RFx2$Day )
   RFx2$Mth = month( RFx2$Day )
   #aggregate(RF["pACI"], by=RF["Year"], max)
   tmp = RFx2[RFx2$Mth == "6",]
   tmp = tmp[tmp$Year == "2016",]
   tmpS = tmp[rev(order(tmp$mu)),]
   tmpS[1:3,]
   
  #point/line of predicted ACI at rough fire sites
   #-------------------------------------------------------------
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
  