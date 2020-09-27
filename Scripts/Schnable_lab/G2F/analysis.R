
# Clear all data
rm(list=ls())

library(lubridate)
library(dplyr)     # provides data manipulating functions.
library(magrittr)  # ceci n'est pas un pipe
library(ISLR)
library(pheatmap)
library(tidyr)
library(stringr)
library(Amelia)
library(plyr)
#install.packages("Amelia")
#install.packages("Zelig")
#update.packages()

#######  Function to compute GDD #######
GDD<-function(temp, type="C"){
  ft = temp
  if(type=="C"){
    ft = 32 + ((ft*9)/5)
  }
  
  return (max(((min(max(ft),86)+max(50,min(ft)))/2)-50,0))
}

DaysOfFeb<-function(Year){
  if(Year%%4==0 | (Year%%100==0 & Year%%400==0)) return(29)
  return(28)
}

DaysOfYear<-function(Year, Month, Day){
  
  M = c(1:12)
  DR = c(31,DaysOfFeb(Year),31,30,31,30,31,31,30,31,30,31)
  
  # Check the range
  if(Month<1 | Month>12) return(-1)
  if(Day<1 | Day>DR[Month]) return(-1)
  
  #day.of.year = data.frame(m=1, d=1, y=Year, DOY=1)
  
  doy = 1
  f = 0
  for(i in M){
    k=1
    if(i==1) k=2
    for(d in k:DR[i]){
      doy = doy+1
      
      if(Day==d & Month==i){
        f=1
        break
      }
      #day.of.year.2014 = rbind(day.of.year.2014, data.frame(Month=i, Day=d, Year=2014, DOY=doy))
    }
    if(f==1) break
  }
  
  return(doy)
}

# Read files
ph14 = read.csv(file.choose())
env14 = read.csv(file.choose())
ph15 = read.csv(file.choose())
env15 = read.csv(file.choose())

# Remove date time column and convert categorical values to numerical value
FL = unique(env14$Experiment.s.)
env14$Experiment.Loc = -1
for(i in 1:nrow(env14)){
  env14$Experiment.Loc[i] = which(FL==as.character(env14$Experiment.s.[i]))
}

# Removed extra columns
env14 = data.frame(env14[,-c(1:5,7,8,17)])

# Rearrange columns
env14 = data.frame(env14[,c(1,10,2:9)])

# See the missing data
#missmap(env14)

# Compute missing value using imputation
#computed_data = amelia(env14, m=5, ts="Day.of.Year", p2s = 0)
#env14 = data.frame(computed_data$imputations[[length(computed_data$imputations)/2]])
#rm(computed_data)

# Convert index to field location
env14$Field.Location = ""
for(i in 1:nrow(env14)){
  env14$Field.Location[i] = as.character(FL[env14$Experiment.Loc[i]])
}

# Removed extra columns
env14 = data.frame(env14[,-c(2)])

# Rearrange columns
env14 = data.frame(env14[,c(1,10,2:9)])

# Drop rows with missing column value
env14 = env14%>%drop_na()

# Convert temperature to Ferhenhite
env14$Temperature..C. = 32 + ((env14$Temperature..C.*9)/5)

# Group env data based on experiment and day
env14.clean = env14%>%group_by(Day.of.Year, Field.Location)%>%summarise(GDD=GDD(Temperature..C., type = "F"), Temperature=mean(Temperature..C.), Dew.point=mean(Dew.Point..C.), Relative.humidity=mean(Relative.Humidity....),Solar.radiation=mean(Solar.Radiation..W.m2.), Rainfall=mean(Rainfall..mm.))

# Remove old data
rm(env14)

## Gen=Field.Location
## Location = State, city
## Other = Replicate, Plot
## Datetime = year

# Create days of year for 2014

M = c(1:12)
DR = c(31,28,31,30,31,30,31,31,30,31,30,31)

day.of.year.2014 = data.frame(Month=1, Day=1, Year=2014, DOY=1)

doy = 1
for(i in M){
  k=1
  if(i==1) k=2
  for(d in k:DR[i]){
    doy = doy+1
    day.of.year.2014 = rbind(day.of.year.2014, data.frame(Month=i, Day=d, Year=2014, DOY=doy))
  }
}

ph14$Date.Planted.DOY = 0
ph14$Date.Harvested.DOY = 0

for(i in 1:nrow(ph14)){
  dat.dp = as.Date(as.character(ph14$Date.Planted[i]),format = "%m/%d/%Y")
  dat.dp.day = day(dat.dp)
  dat.dp.month = month(dat.dp)
  dat.dp.year = 2000+year(dat.dp)
  
  dat.dh = as.Date(as.character(ph14$Date.Harvested[i]),format = "%m/%d/%Y")
  dat.dh.day = day(dat.dh)
  dat.dh.month = month(dat.dh)
  dat.dh.year = 2000+year(dat.dh)
  
  ph14$Date.Planted.DOY[i] = day.of.year.2014$DOY[which(day.of.year.2014$Day == dat.dp.day & day.of.year.2014$Month == dat.dp.month & day.of.year.2014$Year == dat.dp.year)]
  ph14$Date.Harvested.DOY[i] = day.of.year.2014$DOY[which(day.of.year.2014$Day == dat.dh.day & day.of.year.2014$Month == dat.dh.month & day.of.year.2014$Year == dat.dh.year)]

  rm(dat.dp, dat.dp.day, dat.dp.month, dat.dp.year, dat.dh, dat.dh.day, dat.dh.month, dat.dh.year)  
}

# Split genotype from panel
ph14$Genotype = ""
ph14$panel = ""
for(i in 1:nrow(ph14)){
  a = unlist(strsplit(as.character(ph14$Pedigree[i]), "/"))
  ph14$Genotype[i] = a[1]
  if(length(a)>1){
    ph14$panel[i] = a[2]
  }
}

# Take selective columns
ph14 = data.frame(ph14[,c(1,23,24,2,25,26,3,4,9,10,16,18,19,22)])

summary(ph14)

# Remove all row contains NA
ph14 = data.frame(ph14%>%drop_na())

# Aggregate data based on Field location, doy.planned, doy.hervested, 
ph14.clean = ph14%>%group_by(Field.Location, Genotype, panel, Date.Planted.DOY, Date.Harvested.DOY, Year, State, City, Block, Plot)%>%summarise(Pollen.DAP = mean(Pollen.DAP..days.), Plant.height = mean(Plant.Height..cm.), Ear.height = mean(Ear.Height..cm.), Yield=mean(Grain.Yield..bu.A.))

# remove old data
rm(ph14)

# Get distince datetime based on location
fdt14 = data.frame(distinct(ph14.clean[,c(1,4,5)]))

## Add status 
fdt14$Status = 0
fdt14$SStatus = 0
fdt14$FStatus = 0
for(i in 1:nrow(fdt14)){
  s = which(env14.clean$Field.Location==as.character(fdt14$Field.Location[i]) & 
              env14.clean$Day.of.Year==fdt14$Date.Planted.DOY[i])
  
  if(length(s)>0) fdt14$SStatus[i] = 1
  
  e = which(env14.clean$Field.Location==as.character(fdt14$Field.Location[i]) & 
              env14.clean$Day.of.Year==fdt14$Date.Harvested.DOY[i])
  
  if(length(e)>0) fdt14$FStatus[i] = 1
  
  if(length(s)>0 & length(e)>0) fdt14$Status[i] = 1
}

# Column of phenomics dataset
# Field.Location, State, City, Year, Block, Plot, Pollen.DAP, Silk.DAP, Plant.height, Ear.height, Yield, 
# Temperature, Dew.point, Relative.humidity, Solar.radiation, Rainfall

phenomics.2014 = NULL
for(i in 1:nrow(fdt14)){
  tEnv = env14.clean[which(env14.clean$Field.Location == as.character(fdt14$Field.Location[i]) & env14.clean$Day.of.Year>=fdt14$Date.Planted.DOY[i] & env14.clean$Day.of.Year<=fdt14$Date.Harvested.DOY[i]),]
  if(nrow(tEnv)>0){
    tEnv$DAP = tEnv$Day.of.Year-fdt14$Date.Planted.DOY[i]+1
    tEnv$CGDD = cumsum(tEnv$GDD)
    tEnv = data.frame(tEnv[,c(9,10,3:8)])
    
    tEnv$Field.Location=""
    tEnv$Genotype=""
    tEnv$Panel=""
    tEnv$Year=0
    tEnv$State=""
    tEnv$City=""
    tEnv$Block=0
    tEnv$Plot=0
    tEnv$Pollen.DAP=0.0
    tEnv$Plant.height=0.0
    tEnv$Ear.height=0.0
    tEnv$Yield=0.0
    tEnv = data.frame(tEnv[,c(9:12,1,13:20,2:8)])
    
    
    tPh = ph14.clean[which(ph14.clean$Field.Location == as.character(fdt14$Field.Location[i]) & ph14.clean$Date.Planted.DOY==fdt14$Date.Planted.DOY[i] & ph14.clean$Date.Harvested.DOY==fdt14$Date.Harvested.DOY[i] & str_length(ph14.clean$panel)>0),]
    tPh = data.frame(tPh[,c(1:3,6:14)])
    
    ## Add code here
    for(j in 1:nrow(tPh)){
      tmpPhenomics = data.frame(tEnv)
      tmpPD = data.frame(tPh[j,])
      
      tmpPhenomics$Field.Location=tmpPD$Field.Location
      tmpPhenomics$Genotype=tmpPD$Genotype
      tmpPhenomics$Panel=tmpPD$panel
      tmpPhenomics$Year=tmpPD$Year
      tmpPhenomics$State=tmpPD$State
      tmpPhenomics$City=tmpPD$City
      tmpPhenomics$Block=tmpPD$Block
      tmpPhenomics$Plot=tmpPD$Plot
      tmpPhenomics$Pollen.DAP=tmpPD$Pollen.DAP
      tmpPhenomics$Plant.height=tmpPD$Plant.height
      tmpPhenomics$Ear.height=tmpPD$Ear.height
      tmpPhenomics$Yield=tmpPD$Yield
      
      if(is.null(phenomics.2014)){
        phenomics.2014 = data.frame(tmpPhenomics)
      }else{
        phenomics.2014 = data.frame(rbind(phenomics.2014, tmpPhenomics))
      }
      
      rm(tmpPhenomics, tmpPD)
    }
  }
}

# Save phenomic data to a csv file
write.csv(phenomics.2014, "/Users/methun/self/Research/Github/BCBLAB/Project/2017_10_10_g2f/Dataset/data4Ananth/R/phenomics_2014.csv")

# Remove data
rm(day.of.year.2014, env14.clean, fdt14, ph14.clean, tEnv, tPh)


#######################################################
## Generate data from 2015
#######################################################

# Remove date time column and convert categorical values to numerical value
FL = unique(env15$Experiment.s.)
env15$Experiment.Loc = -1
for(i in 1:nrow(env15)){
  env15$Experiment.Loc[i] = which(FL==as.character(env15$Experiment.s.[i]))
}

# Removed extra columns
env15 = data.frame(env15[,-c(1:5,7,8,23)])

# Rearrange columns
env15 = data.frame(env15[,c(1,16,2:6)])

# See the missing data
#missmap(env15)

# Compute missing value using imputation
#computed_data = amelia(env15, m=5, ts="Day.of.Year", p2s = 0)
#env15 = data.frame(computed_data$imputations[[length(computed_data$imputations)/2]])
#rm(computed_data)

# Convert index to field location
env15$Field.Location = ""
for(i in 1:nrow(env15)){
  env15$Field.Location[i] = as.character(FL[env15$Experiment.Loc[i]])
}

# Removed extra columns
env15 = data.frame(env15[,-c(2)])

# Rearrange columns
env15 = data.frame(env15[,c(1,7,2:6)])

# Drop rows of missing columns
env15 = env15%>%drop_na()

# Convert temperature to Ferhenhite
env15$Temperature..C. = 32 + ((env15$Temperature..C.*9)/5)

# Group env data based on experiment and day
env15.clean = env15%>%group_by(Day.of.Year, Field.Location)%>%summarise(GDD=GDD(Temperature..C.,type = "F"), Temperature=mean(Temperature..C.), Dew.point=mean(Dew.Point..C.), Relative.humidity=mean(Relative.Humidity....),Solar.radiation=mean(Solar.Radiation..W.m2.), Rainfall=mean(Rainfall..mm.))

# Remove old data
rm(env15)

## Gen=Field.Location
## Location = State, city
## Other = Replicate, Plot
## Datetime = year

# Create days of year for 2015

M = c(1:12)
DR = c(31,28,31,30,31,30,31,31,30,31,30,31)

day.of.year.2015 = data.frame(Month=1, Day=1, Year=2015, DOY=1)

doy = 1
for(i in M){
  k=1
  if(i==1) k=2
  for(d in k:DR[i]){
    doy = doy+1
    day.of.year.2015 = rbind(day.of.year.2015, data.frame(Month=i, Day=d, Year=2015, DOY=doy))
  }
}

ph15$Date.Planted.DOY = 0
ph15$Date.Harvested.DOY = 0

for(i in 1:nrow(ph15)){
  dat.dp = as.Date(as.character(ph15$Date.Planted[i]),format = "%m/%d/%Y")
  dat.dp.day = day(dat.dp)
  dat.dp.month = month(dat.dp)
  dat.dp.year = 2000+year(dat.dp)
  
  dat.dh = as.Date(as.character(ph15$Date.Harvested[i]),format = "%m/%d/%Y")
  dat.dh.day = day(dat.dh)
  dat.dh.month = month(dat.dh)
  dat.dh.year = 2000+year(dat.dh)
  
  ph15$Date.Planted.DOY[i] = day.of.year.2015$DOY[which(day.of.year.2015$Day == dat.dp.day & day.of.year.2015$Month == dat.dp.month & day.of.year.2015$Year == dat.dp.year)]
  ph15$Date.Harvested.DOY[i] = day.of.year.2015$DOY[which(day.of.year.2015$Day == dat.dh.day & day.of.year.2015$Month == dat.dh.month & day.of.year.2015$Year == dat.dh.year)]
  
  rm(dat.dp, dat.dp.day, dat.dp.month, dat.dp.year, dat.dh, dat.dh.day, dat.dh.month, dat.dh.year)  
}

# Split genotype from panel
ph15$Genotype = ""
ph15$panel = ""
for(i in 1:nrow(ph15)){
  a = unlist(strsplit(as.character(ph15$Pedigree[i]), "/"))
  ph15$Genotype[i] = a[1]
  if(length(a)>1){
    ph15$panel[i] = a[2]
  }
}

# Take selective columns
ph15 = data.frame(ph15[,c(1,21,22,2,23,24,3,4,7,8,14,16,17,20)])

summary(ph15)

# Remove all row contains NA
ph15 = data.frame(ph15%>%drop_na())

# Aggregate data based on Field location, doy.planned, doy.hervested, 
ph15.clean = ph15%>%group_by(Field.Location, Genotype, panel, Date.Planted.DOY, Date.Harvested.DOY, Year, State, City, Block, Plot)%>%summarise(Pollen.DAP = mean(Pollen.DAP..days.), Plant.height = mean(Plant.Height..cm.), Ear.height = mean(Ear.Height..cm.), Yield=mean(Grain.Yield..bu.A.))

# remove old data
rm(ph15)

# Get distince datetime based on location
fdt15 = data.frame(distinct(ph15.clean[,c(1,4,5)]))

## Add status 
fdt15$Status = 0
fdt15$SStatus = 0
fdt15$FStatus = 0
for(i in 1:nrow(fdt15)){
  s = which(env15.clean$Field.Location==as.character(fdt15$Field.Location[i]) & 
              env15.clean$Day.of.Year==fdt15$Date.Planted.DOY[i])
  
  if(length(s)>0) fdt15$SStatus[i] = 1
  
  e = which(env15.clean$Field.Location==as.character(fdt15$Field.Location[i]) & 
              env15.clean$Day.of.Year==fdt15$Date.Harvested.DOY[i])
  
  if(length(e)>0) fdt15$FStatus[i] = 1
  
  if(length(s)>0 & length(e)>0) fdt15$Status[i] = 1
}

# Find correlation among environments
#corEnv = cor(env15.clean[,c(6:10)], use = "pairwise.complete.obs")
#corEnvSq = corEnv^2
#pheatmap(corEnv)

# Column of phenomics dataset
# Field.Location, State, City, Year, Block, Plot, Pollen.DAP, Silk.DAP, Plant.height, Ear.height, Yield, 
# Temperature, Dew.point, Relative.humidity, Solar.radiation, Rainfall

phenomics.2015 = NULL
for(i in 1:nrow(fdt15)){
  tEnv = env15.clean[which(env15.clean$Field.Location == as.character(fdt15$Field.Location[i]) & env15.clean$Day.of.Year>=fdt15$Date.Planted.DOY[i] & env15.clean$Day.of.Year<=fdt15$Date.Harvested.DOY[i]),]
  
  if(nrow(tEnv)>0){
    tEnv$DAP = tEnv$Day.of.Year-fdt15$Date.Planted.DOY[i]+1
    tEnv$CGDD = cumsum(tEnv$GDD)
    tEnv = data.frame(tEnv[,c(9,10,3:8)])
    
    tEnv$Field.Location=""
    tEnv$Genotype=""
    tEnv$Panel=""
    tEnv$Year=0
    tEnv$State=""
    tEnv$City=""
    tEnv$Block=0
    tEnv$Plot=0
    tEnv$Pollen.DAP=0.0
    tEnv$Plant.height=0.0
    tEnv$Ear.height=0.0
    tEnv$Yield=0.0
    tEnv = data.frame(tEnv[,c(9:12,1,13:20,2:8)])
    
    
    tPh = ph15.clean[which(ph15.clean$Field.Location == as.character(fdt15$Field.Location[i]) & ph15.clean$Date.Planted.DOY==fdt15$Date.Planted.DOY[i] & ph15.clean$Date.Harvested.DOY==fdt15$Date.Harvested.DOY[i] & str_length(ph15.clean$panel)>0),]
    tPh = data.frame(tPh[,c(1:3,6:14)])
    
    ## Add code here
    for(j in 1:nrow(tPh)){
      tmpPhenomics = data.frame(tEnv)
      tmpPD = data.frame(tPh[j,])
      
      tmpPhenomics$Field.Location=tmpPD$Field.Location
      tmpPhenomics$Genotype=tmpPD$Genotype
      tmpPhenomics$Panel=tmpPD$panel
      tmpPhenomics$Year=tmpPD$Year
      tmpPhenomics$State=tmpPD$State
      tmpPhenomics$City=tmpPD$City
      tmpPhenomics$Block=tmpPD$Block
      tmpPhenomics$Plot=tmpPD$Plot
      tmpPhenomics$Pollen.DAP=tmpPD$Pollen.DAP
      tmpPhenomics$Plant.height=tmpPD$Plant.height
      tmpPhenomics$Ear.height=tmpPD$Ear.height
      tmpPhenomics$Yield=tmpPD$Yield
      
      if(is.null(phenomics.2015)){
        phenomics.2015 = data.frame(tmpPhenomics)
      }else{
        phenomics.2015 = data.frame(rbind(phenomics.2015, tmpPhenomics))
      }
      
      rm(tmpPhenomics, tmpPD)
    }
  }
  
}

# Save phenomic data to a csv file
write.csv(phenomics.2015, "/Users/methun/self/Research/Github/BCBLAB/Project/2017_10_10_g2f/Dataset/data4Ananth/R/phenomics_2015.csv")

# Remove data
rm(day.of.year.2015, env15.clean, fdt15, ph15.clean, tEnv, tPh)


#############################################################
## Process both 2014 and 2015 data and append
#############################################################
#phenomics.2014 = read.csv(file.choose())
#phenomics.2015 = read.csv(file.choose())

#phenomics.2014 = data.frame(phenomics.2014[,-c(1)])
#phenomics.2015 = data.frame(phenomics.2015[,-c(1)])

phenomics.data = data.frame(rbind(phenomics.2014, phenomics.2015))
#phenomics.data$Temperature = 32 + ((phenomics.data$Temperature*9)/5)

rm(phenomics.2014, phenomics.2015)

# Remove duplicate rows
phenomics.data = phenomics.data%>%distinct

# Save phenomic data to a csv file
write.csv(phenomics.data, "/Users/methun/self/Research/Github/BCBLAB/Project/2017_10_10_g2f/Dataset/data4Ananth/R/phenomics.csv")

#lc = c()
ll=0
ii=0
for(i in 1:nrow(ph14.clean)){
  d = env14.clean[which(env14.clean$Experiment.s. == as.character(ph14.clean$Field.Location[i]) & env14.clean$Day.of.Year>=ph14.clean$Date.Planted.DOY[i] & env14.clean$Day.of.Year<=ph14.clean$Date.Harvested.DOY[i]),]
  
  #lc = append(lc, nrow(d))
  ll = ll+nrow(d)
  if(ll>=848178){ii=i; break;}
}

ll
ii

rm(phenomics.2014,ph14.clean,env14.clean)


## Correlation study
d = data.frame(ph14[,c(23,6,8:10)])
corTab = cor(d, use = "pairwise.complete.obs")
corTabSq = corTab^2
heatmap(corTabSq)

# Find correlation among environments
corEnv = cor(env14.clean[,c(6:10)], use = "pairwise.complete.obs")
corEnvSq = corEnv^2
pheatmap(corEnv)


d = phenomics.data%>%group_by(DAP)%>%summarise(Freq=n())
dT = phenomics.data%>%group_by(Temperature)%>%summarise(Freq=n())


Eps = mean(c(sd(phenomics.data$Pollen.DAP),sd(phenomics.data$Plant.height), sd(phenomics.data$Ear.height), sd(phenomics.data$Yield)))

## For single filter, rainfall
n = max(d$Rainfall)-min(d$Rainfall)
r = n/2
s = seq(min(d$Rainfall), max(d$Rainfall), r)
l = c()
for(i in 2:length(s)){
  a = length(which(d$Rainfall>=s[i-1] & d$Rainfall<=s[i]))
  l = append(l, a)
}
l
plot(l)
range(l)

## For single filter, humidity
n = max(d$Relative.humidity)-min(d$Relative.humidity)
r = n/80
s = seq(min(d$Relative.humidity), max(d$Relative.humidity), r)
l = c()
for(i in 2:length(s)){
  a = length(which(d$Relative.humidity>=s[i-1] & d$Relative.humidity<=s[i]))
  l = append(l, a)
}
l
plot(l)
range(l)

## For single filter, temperature
n = max(d$Temperature)-min(d$Temperature)
r = n/80
s = seq(min(d$Temperature), max(d$Temperature), r)
l = c()
for(i in 2:length(s)){
  a = length(which(d$Temperature>=s[i-1] & d$Temperature<=s[i]))
  l = append(l, a)
}
l
plot(l)
range(l)

## For single filter, DAP vs perf
n = max(d$DAP)-min(d$DAP)
r = n/80
s = seq(min(d$DAP), max(d$DAP), r)
l = c()
for(i in 2:length(s)){
  d = d$Yield[which(d$DAP>=s[i-1] & d$DAP<=s[i])]
  if(length(d)>0){l = append(l, sd(d))}
}
mean(l)

## For single filter, DAP
n = max(d$DAP)-min(d$DAP)
r = n/58
s = seq(min(d$DAP), max(d$DAP), r)
l = c()
for(i in 2:length(s)){
  a = length(which(d$DAP>=s[i-1] & d$DAP<=s[i]))
  l = append(l, a)
}
l
plot(l)
range(l)

## For double filter
n = max(d$Temperature)-min(d$Temperature)
m = max(d$DAP)-min(d$DAP)
q = m/10
r = n/20
s = seq(min(d$Temperature), max(d$Temperature), r)
t = seq(min(d$DAP), max(d$DAP), q)
l = c()
for(i in 2:length(t)){
  for(j in 2:length(s)){
    a = length(d$Yield[which(d$Temperature>=s[j-1] & d$Temperature<=s[j] & d$DAP>=t[i-1] & d$DAP<=t[i])])
    l = append(l, a)
  }
}
l
plot(l)
range(l)

unique(phenomics.data$Field.Location[which(phenomics.data$State=="TX")])

dFL = phenomics.data%>%group_by(Field.Location)%>%summarise(Freq = n())

d = data.frame(phenomics.data[which(phenomics.data$Field.Location=="DEH1" | 
              phenomics.data$Field.Location=="GAH1" | phenomics.data$Field.Location=="INH1" | 
              phenomics.data$Field.Location=="MOH1" | phenomics.data$Field.Location=="NEH1" | 
              phenomics.data$Field.Location=="NYH2" | phenomics.data$Field.Location=="TXH1" | 
              phenomics.data$Field.Location=="TXH2" | phenomics.data$Field.Location=="OHH1"),])

d = data.frame(phenomics.data[which(phenomics.data$Field.Location=="IAH1" | 
                                      phenomics.data$Field.Location=="ILH1" | phenomics.data$Field.Location=="MNH1" | 
                                      phenomics.data$Field.Location=="MOH2" | phenomics.data$Field.Location=="ONH1"),])

d = data.frame(phenomics.data[which(phenomics.data$Field.Location=="IAH1" | 
                                      phenomics.data$Field.Location=="MOH1" | phenomics.data$Field.Location=="NYH2" | 
                                      phenomics.data$Field.Location=="MOH2"),])

d = data.frame(phenomics.data[which(phenomics.data$Field.Location=="TXH1" |
                                      phenomics.data$Field.Location=="MOH1" |
                                      phenomics.data$Field.Location=="ONH1" |
                                      phenomics.data$Field.Location=="NEH1"),])


d = d%>%distinct

DEH1 GAH1 ILH1 INH1 MNH1 NEH1 ONH1 TXH1 TXH2 OHH1

IAH1 MOH1 MOH2 NYH2 

write.csv(d, "/Users/methun/self/Research/Github/BCBLAB/Project/2017_10_10_g2f/Dataset/data4Ananth/R/Green2Field_4Loc.csv")

# Read node analysis file: node_info.csv
ni = read.csv(file.choose())
ni=data.frame(ni%>%distinct)

ph14 = read.csv(file.choose())
ph14 = data.frame(ph14[,c(2:5,9:10,13,14,22)])
# Remove all row contains NA
ph14 = data.frame(ph14%>%drop_na())

ph14=data.frame(ph14%>%distinct)

ph15 = read.csv(file.choose())
ph15 = data.frame(ph15[,c(2:5,7,8,11,12,20)])
# Remove all row contains NA
ph15 = data.frame(ph15%>%drop_na())

ph15=data.frame(ph15%>%distinct)

ph.data = data.frame(rbind(ph14, ph15))
ph.data=data.frame(ph.data%>%distinct)
rm(ph14, ph15)

ni.data = NULL
for(i in 1:nrow(ni)){
  pad = data.frame(ph.data[which(ph.data$Field.Location == as.character(ni$Field.Location[i]) & 
                                 ph.data$State == as.character(ni$State[i]) & 
                                ph.data$City == as.character(ni$City[i]) & 
                                 ph.data$Block == ni$Block[i] & 
                                 ph.data$Plot == ni$Plot[i]), c(4,9)])
  if(nrow(pad)>0){
    pad = data.frame(pad%>%distinct)
    if(is.null(ni.data)){
      ni.data = data.frame(cbind(ni[i,],pad))
    }else{
      ni.data = data.frame(rbind(ni.data, data.frame(cbind(ni[i,],pad))))
    }
  }
}
ni.data = data.frame(ni.data%>%distinct)
warnings()

write.csv(ni.data, "/Users/methun/self/Research/Github/BCBLAB/Project/2017_10_10_g2f/Dataset/data4Ananth/R/node_gen.csv")

Lid=c(158,160,162,164,166,168,170,172,174,176,178)
Hid = Lid+1

LGen=data.frame()
for(i in 1:length(Lid)){
  g = ni.data$Grain.Yield..bu.A.[which(ni.data$Node.id==Lid[i])]
  
  for(j in 1:length(g)){
    LGen = append(LGen, g[j])
  }
}
#LGen = unique(LGen)

HGen=c()
for(i in 1:length(Hid)){
  g = ni.data$Grain.Yield..bu.A.[which(ni.data$Node.id==Hid[i])]
  
  for(j in 1:length(g)){
    HGen = append(HGen, g[j])
  }
}
#HGen = unique(HGen)
range(LGen)
range(HGen)

unique(ni.data$State[which(ni.data$City=="Lincoln")])
unique(ni.data$State[which(ni.data$City=="College Station")])
unique(ni.data$State[which(ni.data$City=="Columbia")])

mean(ni.data$Grain.Yield..bu.A.[which(ni.data$City=="Lincoln")])
mean(ni.data$Grain.Yield..bu.A.[which(ni.data$City=="College Station")])
mean(ni.data$Grain.Yield..bu.A.[which(ni.data$City=="Columbia")])

mean(ni.data$Grain.Yield..bu.A.[which(ni.data$Node.id==161)])
mean(ni.data$Grain.Yield..bu.A.[which(ni.data$Node.id==162)])

# College Station Lincoln         Columbia

plot(ni.data$Grain.Yield..bu.A.[which(ni.data$Node.id==160 & ni.data$City == "Columbia")])
plot(ni.data$Grain.Yield..bu.A.[which(ni.data$Node.id==161)])

ni.data[which(ni.data$Node.id==160 & ni.data$Grain.Yield..bu.A.==min(ni.data$Grain.Yield..bu.A.)),]


################################
## DBSCAN parameter detection ##
################################
# Eps
# Find standard deviation
GR.sd = floor(sd(phenomics$Yield))

# For single filter
s=6
a=floor(min(phenomics$Temperature))
b=ceiling(max(phenomics$Temperature))
t = seq(a,b,(b-a)/s)
q = c()

for(i in 2:s+1){
  p = sort(phenomics$Yield[which(phenomics$Temperature>=t[i-1] & phenomics$Temperature<=t[i])],
           decreasing = F)
  
  ifelse(length(q)>0, yes=(q=c(q,sd(p))), no=(q=sd(p)))
}

mean(q)


a=floor(min(data$MeanHumidity))
b=ceiling(max(data$MeanHumidity))
r = seq(a,b,(b-a)/s)
p.sd=c()
for(i in 2:11){
  for(j in 2:11){
    p = sort(data$Height[which(data$MeanTemp>=t[i-1] & data$MeanTemp<=t[i] & 
                                 data$MeanHumidity>=r[j-1] & data$MeanHumidity<=r[j])],
             decreasing = F)
    
    ifelse(length(p)>0, yes=(p.sd=c(p.sd,sd(p))), no=(NA))
    
  }
}

mean(p.sd)


#### Test code
s = sort(n18$X15.000000)
l=c(s[1])
p = s[1]
for(i in 2:length(s)){
  if(s[i]-p<=2){p=s[i]}
  else {append(l, p); append(l, s[i]); p=s[i]}
}
append(l,p)
