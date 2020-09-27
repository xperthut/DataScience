
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

# User defined functions
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
  
  if(Day==1 & Month==1) return(1)
  
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

DaysOfYearByDate<-function(someDate){
  d = as.Date(someDate)
  if(length(d)==1) return(DaysOfYear(year(d), month(d), day(d)))
  
  l=c(DaysOfYear(year(d[1]), month(d[1]), day(d[1])))
  for(i in 2:length(d)){
    l = append(l, DaysOfYear(year(d[i]), month(d[i]), day(d[i])))
  }
  
  return(l)
}

# Read yield data
perf.data = read.csv(file.choose())
# Remove NA rows for column Yield, Plant height, Ear height and Pollen DAP
l = c()
l = unique(append(l, which(is.na(perf.data$Year))))
l = unique(append(l, which(is.na(perf.data$Environment))))
l = unique(append(l, which(is.na(perf.data$Pedigree))))
l = unique(append(l, which(is.na(perf.data$Yield))))
l = unique(append(l, which(is.na(perf.data$Planted))))
l = unique(append(l, which(is.na(perf.data$Harvested))))
l = unique(append(l, which(is.na(perf.data$Anthesis))))
l = unique(append(l, which(is.na(perf.data$PlantHeight))))
l = unique(append(l, which(is.na(perf.data$EarHeight))))

if(length(l)>0){
  perf.data = data.frame(perf.data[-c(l),])
}

perf.data$sd=year(as.Date(perf.data$Planted))*10000+month(as.Date(perf.data$Planted))*100+day(as.Date(perf.data$Planted))
perf.data$ed=year(as.Date(perf.data$Harvested))*10000+month(as.Date(perf.data$Harvested))*100+day(as.Date(perf.data$Harvested))
perf.data$PollenDAP = DaysOfYearByDate(perf.data$Anthesis)-DaysOfYearByDate(perf.data$Planted)

# Read weather data
weather.data = read.csv(file.choose())
# Remove NA rows for column Yield, Plant height, Ear height and Pollen DAP
l = c()
l = unique(append(l, which(is.na(perf.data$Year))))
weather.data$dd=weather.data$Year*10000+weather.data$Month*100+weather.data$Day

# Compute cumulative GDD
weather.data = data.frame(weather.data%>%group_by(Year,Environment)%>%mutate(cGDD=cumsum(GDD)))

# get unique date and field location from perf data
perf.data.distDateLoc = data.frame(perf.data[,c(1:3,17,18)])%>%distinct

# Prepare g2fdataset
sr=1;er=1;
l=c()
for(i in 1:nrow(perf.data.distDateLoc)){
  # columns are: year, experiment, pedigree, pollendap, plantheight, earheight, yield
  tmp.perf = data.frame(perf.data[which(perf.data$Year == perf.data.distDateLoc$Year[i]
                                        & perf.data$Environment == perf.data.distDateLoc$Environment[i]
                                        & perf.data$Pedigree==perf.data.distDateLoc$Pedigree[i]
                                        & perf.data$sd==perf.data.distDateLoc$sd[i]
                                        & perf.data$ed==perf.data.distDateLoc$ed[i]),
                                  c(1:3,19,15,16,6)])
  
  # Columns are: Year, Environment, DAP, PPT, TMAX, TMIN, TAVG, TRNG, VPD, GDD, EDD, CDD, SR, PPD, PTT
  tmp.env = data.frame(weather.data[which(weather.data$Environment == perf.data.distDateLoc$Environment[i]
                                          & weather.data$dd>=perf.data.distDateLoc$sd[i] 
                                          & weather.data$dd<=perf.data.distDateLoc$ed[i]),
                                    c(1:2,5:12)])
  
  if(nrow(tmp.env)>0){
    
    for(j in 1:nrow(tmp.perf)){
      er = er + nrow(tmp.env)-1
      
      l = append(l, c(sr,er))
      
      sr = er + 1
      er = sr
    }
    
  }
  
  rm(tmp.env, tmp.perf)
}
a = max(l)

rm(g2f.data)
g2f.data = data.frame(Year=integer(a), Environment=rep(NA,a), Pedigree=rep(NA,a), DAP=integer(a), GDD=double(a), CDD=double(a), EDD=double(a), PPT=double(a), SR=double(a), VPD=double(a), cGDD=double(a), PollenDap=integer(a), PlantHeight=double(a), EarHeight=double(a), Yield=double(a))

sr=1;er=1;
for(i in 1:nrow(perf.data.distDateLoc)){
  # columns are: year, environment, pedigree, pollendap, plantheight, earheight, yield
  tmp.perf = data.frame(perf.data[which(perf.data$Year == perf.data.distDateLoc$Year[i]
                                        & perf.data$Environment == perf.data.distDateLoc$Environment[i]
                                        & perf.data$Pedigree==perf.data.distDateLoc$Pedigree[i]
                                        & perf.data$sd==perf.data.distDateLoc$sd[i]
                                        & perf.data$ed==perf.data.distDateLoc$ed[i]),
                                  c(1:3,19,15,16,6)])
  
  # Columns are: Year, Environment, DAP, GDD, CDD, EDD,PPT, SR, VPD, cGDD
  tmp.env = data.frame(weather.data[which(weather.data$Environment == perf.data.distDateLoc$Environment[i]
                                          & weather.data$dd>=perf.data.distDateLoc$sd[i] 
                                          & weather.data$dd<=perf.data.distDateLoc$ed[i]),
                                    c(1:2,5:12)])
  
  if(nrow(tmp.env)>0){
    
    for(j in 1:nrow(tmp.perf)){
      er = er + nrow(tmp.env)-1
      
      g2f.data$Year[sr:er]=tmp.perf$Year[j]
      g2f.data$Environment[sr:er]=as.character(tmp.perf$Environment[j])
      g2f.data$Pedigree[sr:er] = as.character(tmp.perf$Pedigree[j])
      g2f.data$PollenDap[sr:er] = tmp.perf$PollenDAP[j]
      g2f.data$PlantHeight[sr:er] = tmp.perf$PlantHeight[j]
      g2f.data$EarHeight[sr:er] = tmp.perf$EarHeight[j]
      g2f.data$Yield[sr:er] = tmp.perf$Yield[j]
      
      g2f.data[sr:er,4:11] = tmp.env[,3:10]
      
      sr = er + 1
      er = sr
    }
    
  }
  
  rm(tmp.env, tmp.perf)
}

write.csv(g2f.data, "/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Aaron_dataset/g2fData.csv")


# Compute cumulative GDD
g2f = data.frame(g2f%>%group_by(Year,Environment)%>%mutate(cGDD=cumsum(GDD)))
write.csv(g2f[,-c(1)], "/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Aaron_dataset/g2fData.csv")


x = seq(min(g2f$cGDD), max(g2f$cGDD), 36751.54)
sdb = c()
for (i in 2:length(x)){
  y = g2f$Yield[which(g2f$cGDD>=x[i-1] & g2f$cGDD<=x[i])]
  sdb = append(sdb, sd(y))
}

mean(sdb)

rm(perf.data, perf.data.distDateLoc, weather.data)

cdc = read.csv(file.choose())
write.csv(data.frame(cdc[,-c(1)]), "/Users/methun/self/Research/Dataset/Aaron_schnable_lab/mybox-selected/cdc.csv")

#176274,178522,178523,193681,194309,202055,203478,214057,525708,525709,525710

d = read.csv(file.choose())
xd = seq(min(d$DAP), max(d$DAP), diff(range(d$DAP))/30)
xg = seq(min(d$GDD), max(d$GDD), diff(range(d$GDD))/45)
gr = c()

for(i in 2:length(xd)){
  for(j in 2:length(xg)){
    y = d$GrowthRate[which(d$DAP>=xd[i-1] & d$DAP<=xd[i] & d$GDD>=xg[i-1] & d$GDD<=xg[i])]
    
    if(length(y)>0){
      gr = append(gr, sd(y))
    }
  }
}




ggplot(data = d, aes(x=log(ArraySize.int., base = 10), y=log(Time.ms., base = 10))) + geom_line(aes(colour=Operation))


for(i in 1:nrow(plant)){
  plant_new$GDD[which(plant_new$Gen==plant$Gen[i] & plant_new$DAP==plant$DAP[i] & plant_new$Loc==plant$Loc[i])]=plant$GDD[i]
}

# GDD vs Growth_rate
ggplot(plant_new, aes(x = GDD, y = GrowthRate,
                               colour = Gen,
                               shape = Loc)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='Location') +
  labs(shape='Genotype') +
  ggtitle("") +
  labs(x = expression(paste("Growing Degree Days (",degree,"C)")), y = "Growth Rate (cm./day)")

# GDD vs Humidity
ggplot(plant_new, aes(x = GDD, y = Humidity,
                      colour = Loc,
                      shape = Loc)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='Location') +
  labs(shape='Location') +
  ggtitle("") +
  labs(x = expression(paste("Growing Degree Days (",degree,"C)")), y = "Humidity (%)")


write.csv(data.frame(plant_new[,-c(1)]), "/Users/methun/self/Research/Publications/Code/TDA_Phenomics/Data/PlantHeight.csv")



# for power data
xd = seq(min(IM$Period), max(IM$Period), diff(range(IM$Period))/50)
gr = c()

for(i in 2:length(xd)){
  y = IM$Current[which(IM$Period>=xd[i-1] & IM$Period<=xd[i])]
  
  if(length(y)>0){
    gr = append(gr, sd(y))
  }
}

IM$Loc = as.integer(substr(IM$NodePoint_L1, 2, 4))
IM$Vol = as.integer(substr(IM$NodePoint_L4, 2, 4))

write.csv(data.frame(IM[,-c(1)]), "/Users/methun/self/Research/Publications/Code/TDA_Phenomics/Data/PowerGrid.csv")

## Nature paper data
aaron_data = read.table(file.choose(), header = T, sep = "\t")

# Stefens plant data
stephen_data = read.csv(file.choose())

# Compute days of year based on month and day information of year 2017
stephen_data$DaysOfYear = 0
for(i in 1:nrow(stephen_data)){
  stephen_data$DaysOfYear[i] = DaysOfYear(2017,stephen_data$Month[i], stephen_data$Day[i])
}

# Aggregate the height information by genotype,Irrigation,rep,daysofyear
sData_mean = stephen_data%>%group_by(Genotype,Irrigated,Rep,DaysOfYear)%>%summarise(Height.mean = mean(height))

# Plot mean height data for a specific genotype, rep; color by irrigated
d = data.frame(sData_mean[which(sData_mean$Genotype=="2369 x 3IIH6"&sData_mean$Rep==1),])

plot(d$DaysOfYear, d$Height.mean, col=ifelse(d$Irrigated==TRUE, yes="red", no="blue"))


# Testing the plant height 2016 dataset using dap, humidity as cluster
pData = read.csv(file.choose())
dap = seq(min(pData$DAP), max(pData$DAP), diff(range(pData$DAP))/30)
hum = seq(min(pData$Humidity), max(pData$Humidity), diff(range(pData$Humidity))/5)
l=c()
for(i in 2:length(dap)){
  for(j in 2:length(hum)){
    td = hData
  }
}


