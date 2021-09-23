rm(list=ls())

dataDir = "D:\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\"
iFile = choose.files()
load(iFile)
 
library(ggplot2)
library(lattice)
library(corrplot)


##Data Exploration
#--------------------------------------------------
#ACI vs Rainfall
ggplot(combine, aes(Date2, ACIout), color=as.factor(Rain)) +
  geom_point()
rn = combine[(combine$precp_cal > 0 & !is.na(combine$precp_cal)),]
plot(rn$ACIout,rn$precp_cal)

#--------------------------------------------------
##Data Exploration
xyplot(ACIout~Date2 | Sites, data = combine,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))
###Data Exploration
xyplot(ACIout~Mo | Sites, data = combine,
       xlab = "Time", col = 1, type = "h",
       strip = function (bg = 'white', ...)
         strip.default(bg = 'white', ...))

#--------------------------------------------------
## Distributions of response variable ACI
# (ACI- 10 min, hourly, daily)

#--------------------------------------------------
## Distributions of model variables
# (list: )

#--------------------------------------------------
ggplot(combine, aes(x=wind_ms, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Wind speed",
       x="Wind speed (ms/s)", y = "ACI")+
  theme_classic()  


#--------------------------------------------------
ggplot(combine, aes(x=rh, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs RH",
       x="Relative Humidity (%)", y = "ACI")+
  theme_classic()  

#--------------------------------------------------
ggplot(combine, aes(x=wind_wd, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Wind Direction",
       x="Wind direction (deg)", y = "ACI")+
  theme_classic()  

#--------------------------------------------------
ggplot(combine, aes(x=fuel_t, y=ACIout, color=Treatment)) + 
  geom_point() + 
  geom_smooth(method = 'loess') +
  labs(title="ACI vs Fuel Temperature",
       x="Fuel Temp", y = "ACI")+
  theme_classic()

#--------------------------------------------------
str(combine)
#dataframe for correlation plot
mydata <- combine[, c(5,6,7,8,9,10,11,13,15,17,74,75,76,77,78,79,81,82,83)]
###take out NA
mydata<-na.omit(mydata)
str(mydata)
#dorrelation for dataframe
M<-cor(mydata)
#plot relationships to inform global model
corrplot(M)
