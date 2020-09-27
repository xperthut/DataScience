source("header.R")

naData = read.csv("DryHeightNoAdjust.csv")
aData = read.csv("DryHeightWithAdjust.csv")

naData = as.data.frame(naData[,-c(1)])

heightDS = as.data.frame(merge(naData, aData, by = c("genotype","date")))
heightDS$hdiff=heightDS$height.na-heightDS$height
heightDS$hadiff=heightDS$height.smoothed.na-heightDS$height.smoothed