# Read in list of WAV files and adjusts the time to local from GMT

## OUTPUTS
# renamed WAV files
# text file with origional names and new names

rm(list=ls())

## LOAD PACKAGES 
library(tuneR)

#-------------------------------------------
## SET DIRECTORY WHERE ALL AUDIO FILES TO PROCESS ARE...
#-------------------------------------------
WAVDirsDIR =  "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\NotProcessed" # new files from cards
#WAVDirsDIR =  "F:\\RESEARCH\\SEKI Soundscape\\RoughFire\\TEST\\EXAMPLE_June15" # for testing

setwd(WAVDirsDIR)
filpat = ".+\\d{8}_\\d{6}.wav"
File2process = list.files(WAVDirsDIR, pattern = filpat) 

#-------------------------------------------
## CHECK ONE file before running...
#-------------------------------------------
tmp1      = unlist (strsplit( as.character(File2process[1])[1], "[.]") )
tmp2 = unlist (strsplit( tmp1[1], "__") )
dateStamp = unlist (strsplit( tmp1[1], "_") )[5]
timeStamp = unlist (strsplit( tmp1[1], "_") )[6]
DateTime  = paste(dateStamp, timeStamp, sep="_")
GMTime    = as.POSIXct(DateTime,"%Y%m%d_%H%M%S", tz = "UTC")
LocalTime = format(GMTime, tz="America/Los_Angeles", usetz = T) 
new1 = gsub("-","", LocalTime)
new2 = gsub(":","", new1)
new3 = gsub(" ","_", new2)
new4 = gsub("_PDT","", new3)
new5 = paste( tmp2[1],tmp2[2],new4, sep = "__" )
FilenameNew = paste(new5,tmp1[2],sep=".")

cat("RENAME ", File2process[1]," TO ",  FilenameNew)
rm(tmp1, dateStamp,timeStamp,DateTime, GMTime, LocalTime,new1,new2,new3,new4,new5 )


#-------------------------------------------
## RUN FOR ALL FILES IN DIRECTORY...
#-------------------------------------------
outFil = NULL
for (ff in 1: length(File2process)) {  # ff = 1
 
  Finfo = file.info(File2process[ff])
  
  #extract and convert time in file name
  tmp1      = unlist (strsplit( as.character(File2process[ff])[1], "[.]") )
  tmp2 = unlist (strsplit( tmp1[1], "__") )
  dateStamp = unlist (strsplit( tmp1[1], "_") )[5]
  timeStamp = unlist (strsplit( tmp1[1], "_") )[6]
  DateTime  = paste(dateStamp, timeStamp, sep="_")

  GMTime    = as.POSIXct(DateTime,"%Y%m%d_%H%M%S", tz = "UTC")
  LocalTime = format(GMTime, tz="America/Los_Angeles", usetz = T) 

  #rename the file
  new1 = gsub("-","", LocalTime)
  new2 = gsub(":","", new1)
  new3 = gsub(" ","_", new2)
  new4 = gsub("_PDT","", new3)
  new5 = paste( tmp2[1],tmp2[2],new4, sep = "__" )
  FilenameNew = paste(new5,tmp1[2],sep=".")
  outFil = rbind(outFil, c(File2process[ff],as.character(GMTime),LocalTime,FilenameNew))
  
  file.rename ( File2process[ff], FilenameNew )
  
  rm(tmp1, dateStamp,timeStamp,DateTime, GMTime, LocalTime,Finfo,new1,new2,new3,new4,new5 )
  
}

dstmp = Sys.Date()
write.table(outFil, paste(WAVDirsDIR, "_FileReName_", dstmp, ".txt", sep=""), sep="\t")
