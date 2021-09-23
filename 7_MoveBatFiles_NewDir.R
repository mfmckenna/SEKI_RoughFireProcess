# MOVE bat acoustic files to a new directory

# necessary so we can run Kalidoscope detectors on acoustic files only

rm(list=ls())

## LOAD PACKAGES AND FUNCTIONS
my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

#-------------------------------------------
## SET DIRECTORY WHERE ALL AUDIO FILES TO PROCESS ARE...
#-------------------------------------------
#WAVDirsDIR =  "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\AUDIO_V3"   #ALL directories
#targetdir  =  "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\BAT"

## Megan's computer
# WAVDirsDIR =  "F:\\RESEARCH\\SEKI Soundscape\\RoughFire\\AUDIO\\AUDIO_V3\\" 
# targetdir  =  "F:\\RESEARCH\\SEKI Soundscape\\RoughFire\\AUDIO\\BAT"

filpat = ".+__0__\\d{8}_\\d{6}.wav" #bat files
File2Move = list.files(WAVDirsDIR, pattern = filpat, recursive=T,full.names = T) 
File2MoveName = basename( list.files(WAVDirsDIR, pattern = filpat, recursive=T) )
File2MoveNew = paste(targetdir,File2MoveName,sep="\\")
for (mm in 1: length(File2MoveNew) ){
    my.file.rename(from = File2Move[mm],
                   to = File2MoveNew[mm])
}
rm(filpat,File2Move,File2MoveName,File2MoveNew)


filpat = ".+_0[+]1_\\d{8}_\\d{6}.wav" #bat files
File2Move = list.files(WAVDirsDIR, pattern = filpat, recursive=T, full.names = T) 
File2MoveName = basename( list.files(WAVDirsDIR, pattern = filpat, recursive=T) )
File2MoveNew = paste(targetdir,File2MoveName,sep="\\")
for (mm in 1: length(File2MoveNew) ){
  my.file.rename(from = File2Move[mm],
                 to = File2MoveNew[mm])
}
rm(filpat,File2Move,File2MoveName,File2MoveNew)