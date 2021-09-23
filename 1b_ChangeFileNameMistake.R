
library(svDialogs)

###Navigate to the folder where files need to be changed
path<-"D:\\SEKI SoundScape\\RoughFire\\AUDIO\\NotProcessed\\"
###Copy and past the Date_site folder
folder = user <- dlgInput("Please enter the folder that you want to change file names in (Site/Date folder)")$res
setwd(paste(path,folder,"\\","Data",sep = ""))


###List Files 
Files <- list.files(all.files = TRUE, pattern = ".wav")
###Change the part that needs to be changed, To: From:
newName <- sub("50__", "80__", Files)

###Rename all files in the directory
file.rename(Files, newName)

###Verify the changes or navigate to the folder
NewFiles <- list.files(all.files = TRUE, pattern = ".wav")
