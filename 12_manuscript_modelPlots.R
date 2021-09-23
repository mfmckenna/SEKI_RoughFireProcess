rm(list=ls())

#--------------------------------------------------
#READ IN DATA
dataDir = "D:\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\"
setwd(dataDir)
modelIn = choose.files(caption = "SEKI_dailyGAMMwithoutT_2020-06-16")
load(modelIn)
modelIn = choose.files(caption = "SEKI_dailyGAMMwithT_2020-06-16")
load(modelIn)


#--------------------------------------------------
library(ggplot2)

#--------------------------------------------------
#tables-- significance with variables

#graphics-- 
op <- par(mfrow=c(2,3))
plot(global.Gamm, main = "Global GAMM- no temp")
par(op)

op <- par(mfrow=c(2,4))
plot(global.GammT$gam, main = "Global GAMM", shade = TRUE, residuals = TRUE, all.terms = TRUE)
par(op)

termplot(global.GammT$gam,terms="x0",se=TRUE)
