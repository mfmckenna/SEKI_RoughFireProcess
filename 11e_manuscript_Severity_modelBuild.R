rm(list=ls())

#OBJECTIVE 2: Time course of recovery from wildfire, response variable is difference in daily mean ACI to show decrease over time (and by season)

library(ggplot2)
library(mgcv)
library(data.table)

#--------------------------------------------------
#READ IN DATA
dataDir = "D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data"
setwd(dataDir)
dataIn = choose.files(caption = "Open: SEKI_SeverityMeanData (most recent date)" )
load(dataIn)
dataIn = sevData

#--------------------------------------------------
#DATA CLEAN UP
#1) summarize for daily mean value
dDay = NULL
uDay = unique(dataIn$Day)
for (ii in 1:length(uDay)) {
  tmp = dataIn[dataIn$Day == uDay[ii],]
  dDay = rbind(dDay, c(as.character(uDay[ii]),mean(tmp$diff,na.rm = T),  tmp$daySinceFire.x[1], tmp$julDay.x[1]))
}
colnames(dDay) = c("date","severity","daysSinceFire","julDay")
dDay = as.data.frame(dDay)
dDay$dateF = as.Date(dDay$date)
dDay$severity = as.numeric(as.character(dDay$severity))
dDay$daysSinceFire = as.numeric(as.character(dDay$daysSinceFire))
dDay$julDay = as.numeric(as.character(dDay$julDay))

#--------------------------------------------------
#HYPOTHESIS GRAPHIC:
ggplot(dDay,aes(julDay,severity,col=daysSinceFire)) +
  geom_point()+
  geom_linerange(aes(x=julDay, ymax=as.numeric(as.character(severity)), ymin=0),
                 position = position_jitter(height = 0L, seed = 1L))
ggplot(dDay,aes(daysSinceFire,severity,col=julDay)) +
  geom_point()+
  geom_linerange(aes(x=daysSinceFire, ymax=as.numeric(as.character(severity)), ymin=0),
                 position = position_jitter(height = 0L, seed = 1L))


###TREND ANALYSIS- exploration of methods
#--------------------------------------------------
#can I remove the seasonal trend to be able to see a significant downward trend?
#can we identify or forecast when the trend will reach 0, meaning no difference in the time series?
library(forecast)
library(Ecdat)
library(xts)

#MISSING VALUES: need to fill the missing values to run the ARMA model-- how do I do this?
# create a time series with NA for missing days... and then interpolate
starDate = as.Date( sort(dDay$date)[1])
endDate  = as.Date( sort(dDay$date)[nrow(dDay)])
TS = as.data.frame( seq( starDate  , endDate, "day") )
#NAs for missing values
for (ii in 1:nrow(TS) ){
  mtch = match(TS[ii,1], as.Date( dDay$date) )
  if ( is.na( mtch ) ) {  TS$sev[ii] = NA 
  }else {TS$sev[ii] = dDay$severity[mtch] }
  
}
colnames(TS) = c("Date","DifferenceACI")
TS_na = xts(TS$DifferenceACI, order.by=as.Date(TS$Date))
plot(TS_na)

#INTERPOLATE for missing values
intrpNA = na.approx(TS$Difference) 
TS_intp = xts(intrpNA, order.by=as.Date(TS$Date))
plot(TS_intp)

#SOME GRAPHICS TO SHOW THE CORRELATION/Seasonality
pacf(TS_intp,lag = 600)

#method explore #1--- not statistical, just moving average
#--------------------------------------------------
# https://anomaly.io/seasonal-trend-decomposition-in-r/index.html
# https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-moving-average-ma-models.html
#this essentially takes a moving average of the data to see the trend
#window of the exact size of the seasonality
#moving average, seasonality is annual, and data point each day so averge window of 300 (days) 
# RESULT: # still seems to have seasonality included.... 
trend_air = ma(TS_intp, order = 300, centre = T) 
#lines(trend_air)
trend_air = xts(trend_air, order.by=as.Date(TS$Date))
plot(( trend_air) ) 
#remove prevously calculated trend- to see seasonality
detrend_air = TS_intp / trend_air
plot(as.ts(detrend_air))
#average seasonality
m_air = t(matrix(data = detrend_air, nrow = 12))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,12)))
random_air = TS_intp / (trend_air * seasonal_air)
plot(as.ts(random_air))
recomposed_air = trend_air*seasonal_air*random_air
plot(as.ts(recomposed_air))
data = xts(dDay$severity, order.by=as.Date(dDay$date))
plot( data )
trend_air = ma(data, order = 300, centre = T) #moving average, collected each day so 
trend = xts(trend_air, order.by=as.Date(dDay$date))
plot(trend)
seasonality = data / trend
plot(seasonality)

#method explore #2--- autoregressive models with sesonal component
# mainly used to forcast into the future.
#--------------------------------------------------
#https://otexts.com/fpp2/seasonal-arima.html
#finds the best model... iteratively
interp_arima = auto.arima(TS_intp)
plot(forecast(interp_arima, h = 10))

#model evaluation
interp_arima$coef
acf(interp_arima$residuals)

#exploring models by building using known structure in the data
TS_intp %>%  diff(lag=12) %>% diff() %>% ggtsdisplay()
TS_intp %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

fit3 <- Arima(TS_intp, order=c(0,1,3), seasonal=c(0,1,1))
checkresiduals(fit3)

#now it is ready for forcasting... huge error bars, need better model fit
fit3 %>% forecast(h=30) %>% autoplot()

#method explore #3--- model the time series data to show trends
#--------------------------------------------------
# http://greenbrown.r-forge.r-project.org/trends.php--- NOT WORKING BUT Looks like what we want!!
#install.packages("greenbrown", repos="http://R-Forge.R-project.org")
#library(greenbrown)
#trd = Trend(TS_intp)
# ?? FIND code to adapt to this....

#https://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html

decompose(TS_intp)



#--------------------------------------------------
#MODEL-- GAMM/LME
#not really what we want to show decreasing trend and "recovery"
#--------------------------------------------------
ctrl = list(nthreads=6)
global.GammT = gamm(severity ~ s(daysSinceFire)+ s(julDay), correlation=corCAR1(value=.5,form=~dateF),
                    data=dDay, method="REML", na.action=na.omit, control=ctrl)
#evaluate model--gam
gam.check(global.GammT$gam)
#acf(residuals(global.GammT$gam))
pacf(residuals(global.GammT$gam))
#model performance
anova(global.GammT$gam)
summary(global.GammT$gam) #record results in excel table
#model plots
op <- par(mfrow=c(2,1))
plot(global.GammT$gam, shade = TRUE, residuals = TRUE, all.terms = TRUE)
par(op)
#evaluate model-- lme
pacf(residuals(global.GammT$lme))
#model performance
anova(global.GammT$lme)
summary(global.GammT$lme) #record results in excel table
#model plots
op <- par(mfrow=c(2,1))
plot(global.GammT$lme, shade = TRUE, residuals = TRUE, all.terms = TRUE)
par(op)

#without julian day included
global.GammT = gamm(severity ~ s(daysSinceFire), correlation=corCAR1(value=.5,form=~dateF),
                    data=dDay, method="REML", na.action=na.omit, control=ctrl)
summary(global.GammT$gam)
