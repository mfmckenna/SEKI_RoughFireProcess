# Fire survey data

firSur = read.csv("D:\\RESEARCH\\NSNSD_Projects\\SEKI Rough Fire Analysis\\data\\Master_PlotConditonSummary_2016and2019_reformat.csv")

library(corrplot)
M <- cor(firSur[,5:27],use="complete.obs")
corrplot(M, method = "circle",type = "upper")

df = firSur[,c(5,6,7,21:27)]
M <- cor(df,use="complete.obs")
corrplot(M, method = "circle",type = "upper")

# add ACI for site-year (repeats for plots)
# average the site-plots together, exclude site 30
#choose a few variables- shrub cover, canopy cover, fuel loadings
#compare with ACI