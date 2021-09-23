# CHECK and CLEAN UP directories... after runiing batch PAMGUIDE
# ChECK and Clean for just the DROBO
# runs for each file type....recommend running for each file type (between the START/STOP)

rm(list=ls(all=TRUE)) 

# DIRECTORY WITH ALL FILES THAT SHOULD BE COPIED...
#WAVDirsDIR =  "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\NotProcessed\\" 
WAVDirsDIR =  "D:\\SEKI SoundScape\\RoughFire\\AUDIO\\NotProcessed\\" #Drobo for catchup

#START---------------------------------------
# (1) CHECK _1__ audio files...
## SET DIRECTORY WHERE ALL AUDIO FILES WERE....
develop.WAV =  list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\NotProcessed\\", pattern = "_1__", recursive  = T, full.names = T) 
## SET DIRECTORY WHERE ALL AUDIO FILES WERE COPIED...
main.WAV   =   list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\AUDIO_V3\\", pattern = "_1__", recursive  = T)


## ARE ALL FILES PRESENT IN DEVELOP in MAIN?
both <- basename(develop.WAV) %in% basename(main.WAV)
if ( all(both) ){
  # IF both all TRUE then you are good!
  cat("All files copied... DELETE!") 
  test = 1
} else {
  test = 0
  cat("Audio _1_  files not copied...check again before deleting!\n")
  difs <- setdiff(develop.WAV,main.WAV) 
  #What is different in develop.WAV?
  difs 
  }


# NOW REMOVE THE FILES!
if (test == 1) {
  file.remove(develop.WAV)
}
#STOP---------------------------------------



#START---------------------------------------
# (2) CHECK _0__ audio files...
## SET DIRECTORY WHERE ALL AUDIO FILES WERE....
develop.WAV0 =  list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\NotProcessed\\", pattern = "_0__", recursive  = T, full.names = T) 
## SET DIRECTORY WHERE ALL AUDIO FILES WERE COPIED...
main.WAV0   =   list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\AUDIO_V3\\",     pattern = "_0__", recursive  = T)

## ARE ALL FILES PRESENT IN DEVELOP in MAIN?
both0 <-  basename(develop.WAV0) %in% basename(main.WAV0)
if ( all(both0) ){
  # IF both0 all TRUE then you are good!
  cat("All files copied... DELETE!") 
  test = 1
} else {
  test = 0
  cat("Audio _0_  files not copied... check again before deleting!\n")
  difs <- setdiff(develop.WAV0,main.WAV0) 
  #What is different in develop.WAV
  difs 
  }

# NOW REMOVE THE FILES!
if (test == 1) {
  file.remove(develop.WAV0)
}
#STOP---------------------------------------




#START---------------------------------------
# (3) CHECK Summary  files...
## SET DIRECTORY WHERE ALL Summary FILES WERE....
develop.SUM =  list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\NotProcessed\\", pattern = "Summary", recursive=T, full.names=T) 
## SET DIRECTORY WHERE ALL Summary FILES WERE COPIED...
main.SUM  =   list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\AUDIO_V3\\", pattern = "Summary", recursive  = T)

## ARE ALL FILES PRESENT IN DEVELOP in MAIN?
bothS <- basename(develop.SUM) %in% basename(main.SUM)
if ( all(bothS) ){
  # IF bothS all TRUE then you are good!
  cat("All files copied... DELETE!") 
  test = 1
} else {
  test = 0
  cat("Summary files not copied... check again before deleting!\n")
  difs <- setdiff(develop.SUM,main.SUM) #What is different in develop.WAV
  difs }


if (test == 1) {
  file.remove(develop.SUM)
}
#STOP---------------------------------------
#START---------------------------------------
# (3) CHECK Summary  files...
## SET DIRECTORY WHERE ALL Summary FILES WERE....
develop.SUM =  list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\NotProcessed\\", pattern = "Summary", recursive=T, full.names=T) 
## SET DIRECTORY WHERE ALL Summary FILES WERE COPIED...
main.SUM  =   list.files ("D:\\SEKI Soundscape\\RoughFire\\AUDIO\\AUDIO_V3\\", pattern = "README", recursive  = T)

## ARE ALL FILES PRESENT IN DEVELOP in MAIN?
bothS <- basename(develop.SUM) %in% basename(main.SUM)
if ( all(bothS) ){
  # IF bothS all TRUE then you are good!
  cat("All files copied... DELETE!") 
  test = 1
} else {
  test = 0
  cat("README files not copied... check again before deleting!\n")
  difs <- setdiff(develop.SUM,main.SUM) #What is different in develop.WAV
  difs }


if (test == 1) {
  file.remove(develop.SUM)
}
#STOP---------------------------------------



#START---------------------------------------
# (4) DELETE DIRECTORIES.... ONLY IF NO FILES!!
#check to make sure ALL folders are empty
if ( all(file.exists(WAVDirsDIR), na.rm = F) == F){
  cat("Directories are all empty... delete!")
  test1 = 1
} else {
  cat("Directories still have files in them... check again before deleting!\n")
  filesIN = list.files(WAVDirsDIR, all.files = TRUE, include.dirs = F, no.. = TRUE)
  cat(" LIST OF FILES:\n", filesIN)
  test1 = 0
}
setwd(WAVDirsDIR)
#remove all folders in directory
if (test1 == 1) {
  deldirs = list.dirs(WAVDirsDIR)
  unlink(deldirs,recursive=T)
  cat("Directories Deleted... on to the next week!")
}

#STOP---------------------------------------



# https://stackoverflow.com/questions/23353067/comparing-two-lists-r

