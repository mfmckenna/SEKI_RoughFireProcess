

####Copy newly downloaded WAV files to network drive
#This script copies all new files from the Drobo (D:) to the network drive (V:)
#It does not overwrite files that are different in the V: if they aren't in the D:



library(R.utils)


#Copy all new graphics from drobo to V
drobo = "D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Graphics\\"
v = "V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\Graphics\\"
temp = list.files("one")          
copyDirectory(drobo, v, private =TRUE, overwrite = FALSE, recursive = TRUE)


#Copy all new nvspls from drobo to V
drobo = "D:\\SEKI SoundScape\\RoughFire\\NVSPL\\"
v = "V:\\SEKI SoundScape\\RoughFire\\NVSPL"
temp = list.files("one")          
copyDirectory(drobo, v, private =TRUE, overwrite = FALSE, recursive = TRUE)


#Copy all new aci from drobo to V
drobo = "D:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\AcousticIndex\\"
v = "V:\\SEKI SoundScape\\RoughFire\\ANALYSIS\\AcousticIndex\\"
temp = list.files("one")          
copyDirectory(drobo, v, private =TRUE, overwrite = FALSE, recursive = TRUE)



#Copy all new short files from drobo to V
drobo = "D:\\SEKI SoundScape\\RoughFire\\AUDIO\\shortFiles\\"
v = "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\shortFiles\\"
temp = list.files("one")          
copyDirectory(drobo, v, private =TRUE, overwrite = FALSE, recursive = TRUE)


#Copy all new audio files from drobo to V
drobo = "D:\\SEKI SoundScape\\RoughFire\\AUDIO\\AUDIO_V3\\"
v = "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\AUDIO_V3\\"
temp = list.files("one")          
copyDirectory(drobo, v, private =TRUE, overwrite = FALSE, recursive = TRUE)



#Copy all new audio files from drobo to V
drobo = "D:\\SEKI SoundScape\\RoughFire\\Analysis\\"
v = "V:\\SEKI SoundScape\\RoughFire\\AUDIO\\AUDIO_V3\\"
temp = list.files("one")          
copyDirectory(drobo, v, private =TRUE, overwrite = FALSE, recursive = TRUE)


### END ###

