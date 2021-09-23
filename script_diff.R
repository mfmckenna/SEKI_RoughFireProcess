library(diffr)
library(shiny)

####1_CreateFolderNames_Downloads.R
file1<-("D:\\\\SEKI SoundScape\\\\RoughFire\\\\CODE-copy11Sep2017\\\\RoughFireProcess\\\\1_CreateFolderNames_Downloads.R")
file2<-("V:\\\\SEKI SoundScape\\\\RoughFire\\\\CODE\\\\RoughFireProcess\\\\1_CreateFolderNames_Downloads.R")
diffr(file1,file2)

####1a_renameFiles_localTime.R
file1<-("D:\\SEKI SoundScape\\RoughFire\\CODE-copy11Sep2017\\RoughFireProcess\\1a_renameFiles_localTime.R")
file2<-("V:\\SEKI SoundScape\\RoughFire\\CODE\\RoughFireProcess\\1a_renameFiles_localTime.R")
diffr(file1,file2,before = "f1", after = "f2")


####1b_ChangeFileNameMistake.R
file1<-("D:\\SEKI SoundScape\\RoughFire\\CODE-copy11Sep2017\\RoughFireProcess\\1b_ChangeFileNameMistake.R")
file2<-("V:\\SEKI SoundScape\\RoughFire\\CODE\\RoughFireProcess\\1b_ChangeFileNameMistake.R")
diffr(file1,file2,before = "f1", after = "f2")

####2_batch_PAMGUIDE_NVSPL_V9_RoughFire.R
file1<-("D:\\SEKI SoundScape\\RoughFire\\CODE-copy11Sep2017\\RoughFireProcess\\2_batch_PAMGUIDE_NVSPL_V9_RoughFire.R")
file2<-("V:\\SEKI SoundScape\\RoughFire\\CODE\\RoughFireProcess\\2_batch_PAMGUIDE_NVSPL_V9_RoughFire.R")
diffr(file1,file2,before = "f1", after = "f2")


####3_Chk_Cln_DIRS_drobo.R


####4_calculate_AI_NVSPL_V17_RoughFire.R
####5_plot_AIoutput_SEKI_V1_newnames.R
file1<-("D:\\SEKI SoundScape\\RoughFire\\CODE-copy11Sep2017\\RoughFireProcess\\5_plot_AIoutput_SEKI_V1_newnames.R")
file2<-("V:\\SEKI SoundScape\\RoughFire\\CODE\\RoughFireProcess\\5_plot_AIoutput_SEKI_V1_newnames.R")
diffr(file1,file2,before = "f1", after = "f2")

####6_copy all new files to Network.R
####7_MoveBatFiles_NewDir.R
####8_working.R

file1<-("D:\\SEKI SoundScape\\RoughFire\\CODE-copy11Sep2017\\RoughFireProcess\\8_working.R")
file2<-("V:\\SEKI SoundScape\\RoughFire\\CODE\\RoughFireProcess\\8_working.R")
diffr(file1,file2,before = "f1", after = "f2")


####9_BatSummary.R



####10_DataRequest.R






library(diffr)
file1 = tempfile()
writeLines("hello, world!\\n", con = file1)
file2 = tempfile()
writeLines(paste0(
  "hello world?\\nI don't get it\\n",
  paste0(sample(letters, 65, replace = TRUE), collapse = "")), con = file2)
diffr(file1, file2, before = "f1", after = "f2")
