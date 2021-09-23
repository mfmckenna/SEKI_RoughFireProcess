# Adjust timing on SEKI rough fire files

##--------------------------------------------------------------------------------
# WHAT IS THE PROBLEM(S)!?!!
#IDEALLY we wanted to record:
# sunrise: 1 hour before sunrise to 3 hours after (4 hours)
# sunset: 1.5 hours before sunset to 1 hour after sunset (2.5 hours)
# but, got a little messed up with time changes.... 
# so just going to sample time around sunrise sunset that all time periods have in common

##--------------------------------------------------------------------------------
# WHAT:   reads in Acoustic Index output files and adjusts to correct (local) time, subsamples the data to a common length
# OUTPUT: 
#(1) csv file with all indices with adjusted time and only for data that overlap in time
#(2) csv file with summary by day of the information available
rm(list=ls(all=TRUE)) 
#sessionInfo()

##--------------------------------------------------------------------------------
# LOAD LIBRARIES
library(suncalc)
library(chron)
library(tidyverse)
library(hms)
library(dplyr)
#devtools::install_github("r-lib/rlang", build_vignettes = TRUE)
#detach("package:lubridate", unload=TRUE)
#library(lubridate)
#remotes::update_packages()
##--------------------------------------------------------------------------------
# DIRECTORY SET UP
dirP = "D:\\"
outDir   =  paste(dirP, "SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex", sep="")

##--------------------------------------------------------------------------------
# CREATE MATRIX WITH CORRECTIONS FOR EACH DATE
siteName = c("10","20","30","40","50","60","70","80","90")
siteLat  = as.numeric( c("36.744947","36.746885","36.747222","36.718817","36.719991","36.749007","36.747172","36.752189","36.750113") )
siteLon  = as.numeric( c("-118.97727","-118.975762","-118.971562","-118.965995","-118.961017","-118.981671","-118.981695","-118.981108","-118.978932"))
sRsample = 2 # 2 hours after sunrise, expect 12 x 10 min sample
eSR = 12    # expect 12 x 10 min sample
SSsample = -30 # 30 minutes before sunset
eSS = 3 # expect 3 x 10 min sample

renameStr   = ( c("2018-03-12","2018-03-12","2018-03-12","2018-03-12","2018-03-12","2018-03-12","2018-03-12","2018-03-12"))
renameEnd   = ( c("2018-06-07","2018-06-07","2018-06-07","2018-06-07","2018-06-07","2018-06-07","2018-06-07","2018-05-10"))
renameSite  =   c("10","20","30","40","50","60","70","80")
rename      = as.numeric( c("-1","-1","-1","-1","-1","-1","-1","-1"))
renameInfo  = as.data.frame (cbind(renameSite,renameStr,renameEnd,rename))

##--------------------------------------------------------------------------------
# READ IN DIRECTORY WITH ACOUSTIC INDEX RESULTS
AIDir   =  paste(dirP, "SEKI Soundscape\\RoughFire\\ANALYSIS\\AcousticIndex", sep="")
AIFiles = list.files(AIDir, pattern="AcousticIndex", recursive=F, full.names=T)
# INFO FILES
mySites = unique( sapply(strsplit(basename(AIFiles), "_"), head, 1) )
numSites = length(mySites)
sites = as.data.frame( cbind( sapply(strsplit(basename(AIFiles), "_"), head, 1), (AIFiles)))


##--------------------------------------------------------------------------------
# LOOP THROUGH SITES, correct time, get data in common time period, and output a new file
for (ii in 1:length(mySites)){ # ii=1
  
  tsite = sites[sites$V1==mySites[ii],]
  cat("Processing site...", mySites[ii],"\n")
  
  # GET ALL DATA FOR ONE SITE, and FORMAT
  ALL   = do.call(rbind,lapply(as.character(tsite[,2]),read.csv)) # ALL = d
  ALL$DY = gsub("_", "", ALL$Date)
  ALL$Hrck = ALL$Hr
  ALL$Date2 = as.Date( gsub("_", "-", ALL$Date), format = "%Y-%m-%d" )
  ALL$SiteDay = paste(ALL$Site,ALL$DY, ALL$Hr, sep="_")
  ALL$SiteDay2 = paste(ALL$Site,ALL$DY, ALL$Hr,ALL$Min, sep="_")
  ALL$hms = hms(  hours = ALL$Hr, minutes = ALL$Min, seconds = ALL$Sec) 
  
  # CHANGE times in file if necessary, using the rename matrix
  #select only adjustment time periods for a given site ((ii))
  tmpRename = renameInfo[renameInfo$renameSite == mySites[ii],]
  # some sites might need to change multiple time periods, so need to loop though each and make the change
  for (rr in 1:nrow(tmpRename)) # rr = 1
  {
    #for the dates that need adjustment, make the adjustment!
    ALL$Hr[ALL$Date2   >= as.Date( as.character(renameInfo$renameStr[rr]),format = "%Y-%m-%d") 
           & ALL$Date2 <= as.Date( as.character(renameInfo$renameEnd[rr]),format = "%Y-%m-%d") ] = 
      ALL$Hr[ALL$Date2 >= as.Date( as.character(renameInfo$renameStr[rr]),format = "%Y-%m-%d") 
             & ALL$Date2 <= as.Date( as.character(renameInfo$renameEnd[rr]),format = "%Y-%m-%d") ] + as.numeric(as.character( renameInfo$rename[rr]) )
    
    #make sure a change was made to the times... should get 0 or -1
    # cat("Does this show a different hours? 0,", as.numeric(as.character( renameInfo$rename[rr]) ), " : ", unique( as.numeric(ALL$Hr) -  as.numeric(ALL$Hrck ) ) )
    
    
  }
  
  # EXTRACT time period of interest based on sunrise/sunset 
  Lat =  siteLat[ii]
  Lon =  siteLon[ii]
  # need list of all possible days at the site... even missing days
  udays = unique(ALL$Date2)
  dys = seq(as.Date(udays[1]), as.Date(udays[length(udays)]), by="days") 
  jDay = as.numeric(format(as.POSIXct(dys, format="%Y_%m_%d"),"%j"))
  estsun = getSunlightTimes(date = dys, lat = Lat, lon = Lon, keep = c("sunrise","sunset") , tz = "PST8PDT")
  
  #loop through each day to get just values of interest...
  ALLsub  = NULL
  Allno   = NULL
  outInfo = NULL
  
  for (dd in 1:length(dys)) {
    ALLtmp = ALL[ALL$Date2 == dys[dd],]
    suntmp =  estsun[estsun$date == dys[dd],]
    #remove any duplicated rows from the day... possible if AI code run twice
    ALLtmp = ALLtmp[!duplicated(ALLtmp$SiteDay2),]
    
    Allno = rbind(Allno, ALLtmp)
    
    nsample = nrow(ALLtmp)
    #rows with sunrise data... hour must be less than two hours before sunset
    #nsampleSR = nrow (ALLtmp[ALLtmp$Hr  <=  as.numeric( hour(suntmp$sunset))-2,] )
    nsampleSR = nrow (ALLtmp[ALLtmp$Hr  <=  as.numeric( substr((suntmp$sunset), 11, 13 ) ) -2,] )
    #rows with sunrise data... hour must be greater than two hours before sunset
    nasampleSS = nrow (ALLtmp[ALLtmp$Hr >= as.numeric( substr((suntmp$sunset), 11, 13 ) ) -2,] )
    
    #some days will not have data... create and empty row
    if (nrow(ALLtmp) < 1 ) #no data for this day
    {
      # cat("day with no data: ", dd, as.character(dys[dd]), "\n")
      
      outInfo = rbind(outInfo, c(mySites[ii], as.character(dys[dd]), as.character(suntmp$sunrise), 
                                 0, "No Data", "No Data",as.character(suntmp$sunset),0, "No Data","No Data",nsample,nsampleSR,nasampleSS) )
      
    } else {
      ALLtmp$Sunrise = suntmp$sunrise
      ALLtmp$Sunset  = suntmp$sunset
      
      #format sunrise time
      rTime = ( hms(hours = as.numeric( substr(suntmp$sunrise,12,13) ), minutes = as.numeric( substr(suntmp$sunrise,15,16)), seconds = as.numeric( substr(suntmp$sunrise,18,19) )) )
      #format adjusted after sunrise time
      tsdj = suntmp$sunrise +  hms(hours=sRsample, minutes = 0 , seconds = 0)
      rTimeEd = ( hms(hours = as.numeric( substr(tsdj,12,13) ), minutes = as.numeric( substr(tsdj,15,16)), seconds = as.numeric( substr(tsdj,18,19) )) )
      
      #format sunset time
      sTime = ( hms(hours = as.numeric( substr(suntmp$sunset,12,13) ), minutes = as.numeric( substr(suntmp$sunset,15,16)), seconds = as.numeric( substr(suntmp$sunset,18,19) )) )
      #format adjusted before sunset time
      ssdj = suntmp$sunset +  hms(hours=0, minutes = SSsample , seconds = 0)
      sTimeEd = ( hms(hours = as.numeric( substr(ssdj,12,13) ), minutes = as.numeric( substr(ssdj,15,16)), seconds = as.numeric( substr(ssdj,18,19) )) )
      
      #get data for defined period around sunrise and sunset- truncate to correct time
      #must be greater than sunrise but less than sunrise + 2 hours
      srise = ALLtmp[ALLtmp$hms >= rTime & ALLtmp$hms <= rTimeEd , ]
      #must be less than sunset, but greater han 
      sset  = ALLtmp[ALLtmp$hms >= sTimeEd & ALLtmp$hms <= sTime , ]
      
      
      
      # check to see if expected number of samples, and then reassemble matrix
      if ( (nrow(srise) == eSR ) & (nrow(sset) == eSS ) ){ # correct number of values for both sunrise/sunset
        sset$TOD = "evening"
        srise$TOD = "morning"
        ALLsub  = rbind(ALLsub, rbind(srise,sset))
        outInfo = rbind(outInfo, c(mySites[ii], as.character(dys[dd]), 
                                   as.character(suntmp$sunrise), nrow(srise), "good", as.character(srise$hms)[1], 
                                   as.character(suntmp$sunset),  nrow(sset) , "good", as.character(sset$hms)[1],
                                  nsample,nsampleSR,nasampleSS) )
      } else if ( (nrow(srise) == eSR ) & (nrow(sset) != eSS ) ) {# correct number of sunrise, not sunset
        srise$TOD = "morning"
        ALLsub  = rbind(ALLsub, srise)
        outInfo = rbind(outInfo, c(mySites[ii], as.character(dys[dd]), 
                                   as.character(suntmp$sunrise), nrow(srise),"good", as.character(srise$hms)[1], 
                                   as.character(suntmp$sunset), nrow(sset), "not included", as.character(sset$hms)[1],
                                   nsample,nsampleSR,nasampleSS))
      } else if ( (nrow(srise) != eSR ) & (nrow(sset) == eSS ) ) { # correct number of sunset, not sunrise
        sset$TOD = "evening"
        ALLsub  = rbind(ALLsub, sset)
        outInfo = rbind(outInfo, c(mySites[ii], as.character(dys[dd]), 
                                   as.character(suntmp$sunrise), nrow(srise), "not included",as.character(srise$hms)[1], 
                                   as.character(suntmp$sunset), nrow(sset), "good", as.character(sset$hms)[1],
                                   nsample,nsampleSR,nasampleSS) )
      } else if ( (nrow(srise) != eSR ) & (nrow(sset) != eSS ) ) { # neither are true, do not append to matrix
        outInfo = rbind(outInfo, c(mySites[ii], as.character(dys[dd]), 
                                   as.character(suntmp$sunrise), nrow(srise), "not included",as.character(srise$hms)[1], 
                                   as.character(suntmp$sunset), nrow(sset),  "not included", as.character(sset$hms)[1],
                                   nsample,nsampleSR,nasampleSS))
      }
      
      
    }
    
    
    
    
    
  } #end of day loop
  
  #write out site files...
  colnames(outInfo) =  c("Site","Day","Sunrise","SunriseSamples","used", "SRfirstSample","Sunset","SunsetSamples","used", "SSfirstSample","total samples","SR samples","SSsamples")
  outInfo = as.data.frame(outInfo)
  
  
  #some checks...
  # unique(outInfo$SunriseSamples)
  # unique(outInfo$SunsetSamples)
  # outInfo[ (outInfo$SunsetSamples) == 3, ]
  dateCreate = Sys.Date()
  lastDate = ALLsub$Date [which.max(ALLsub$Date)]
  cat("writing out results for site...", mySites[ii] ,"\n")
  
  #OUTPUT #1: AI values with adjusted time and truncated to common time periods
  write.csv(ALLsub,file= paste(AIDir, "\\", "adjValues", "\\", mySites[ii],"_DataAdjAI_Subsample_LastDateIncluded_", lastDate, ".csv",sep="") )
  write.csv(Allno, file= paste(AIDir, "\\", "adjValues", "\\", mySites[ii],"_DataAdjAI_ALLdata_LastDateIncluded_", lastDate, ".csv",sep="") )
  
  #OUTPUT #2: summary of recordings for each recording day
  write.csv(outInfo,file= paste(AIDir, "\\", "adjValues", "\\", mySites[ii],"_SummaryAdjAI_LastDateIncluded_", lastDate, ".csv",sep="") )
  
  
  rm(ALL, ALLsub, udays, Lat,Lon, dys, jDay, estsun)
} # end of site loop


