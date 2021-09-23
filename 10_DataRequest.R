
rm(list=ls())
library(stringr)
library(stringi)
library(FedData)
library(svDialogs)
library(eply)

###TEST###
###Directory of files to copy for data request

WAVDirsDIR =  choose.dir(getwd(), caption = "Select folder of files to copy")#ALL directories


WAVDirsDRIVE = choose.dir(getwd(), caption = "Select folder of drive")#ALL directories





####Step 1: Enter all dates
{dates <- readline(prompt='Enter all dates YYYYMMDD (separate by comma): ')
values = read.table(text = dates, sep=",")
dframe<-data.frame(values)
data<-t(dframe)
}

###Step 2: Acoustic or Ultrasonic data?

{
dtype = readline(prompt='Enter 0 for bat files or 1 for acoustic files): ')
  filtype = ifelse(dtype=="0", "__0__", ifelse(dtype=="1", "__1__",NA))
}


###Step 3: Build query function
{{
filpat = paste(filtype,data,sep ="")

  filpat2<-cbind(filpat)
}
for (ss in 1:length(filpat2))
{
  
  dat=pattern <- paste(filpat,"|",sep="")
  dat1=toString(dat)
  dat2=noquote(dat1)
  dat3=gsub(", ",'',dat2)
  last<-substr(dat3, 1, nchar(dat3)-1)
    }

}


###Step 4: Obtain file list (This may take time)
  
{
File2Move = list.files(WAVDirsDIR, pattern = last, recursive=T,full.names = T) 


#targetdir  =  choose.dir(getwd(), caption = "Destination for the copied files")
}

###Step 5: Copy files 

file.copy(File2Move, WAVDirsDRIVE,overwrite = TRUE)
  
