source("header.R")

hData = read.csv("DryHeightWithAdjust.csv")

hData$DaysOfYear = DaysOfYearByDate(hData$date, frmat = "%m/%d/%y")
l = which(is.na(hData$height.adjust))
if(length(l)>0){
  hData = as.data.frame(hData[-c(l),])
}

sDayOfYear = DayOfAYear(2017,5,25)
hData$DAP = hData$DaysOfYear-sDayOfYear+1

## Assign genotype code
genList=unique(hData$genotype)
hData$genCode=""
i=1
for(g in genList){
  t = which(hData$genotype==as.character(g))
  hData$genCode[t] = paste("G_",i,sep = "")
  i = i + 1
}

##################################################################################################
############### Plot height curve
# For each genotype, plot the growth curve
imgPath = "spacialAdjust/ha/"
genList=unique(hData$genotype)

dev.set(which = dev.next())
for(g in genList){
  t = which(hData$genotype==as.character(g))
  
  while (!is.null(dev.list()))  dev.off()
  pdf(file=paste(imgPath,as.character(g),".pdf",sep=""))
  matplot(hData$DAP[t], cbind(hData$height.adjust[t], hData$height.adj.smooth[t]), type = 'l', main = paste(as.character(g)), col=c('red', 'blue'))
  while (!is.null(dev.list()))  dev.off()
}
while (!is.null(dev.list()))  dev.off()

##################################################################################################
######### Compute growth curve bases on smooth height data
# fit model for each genotype
mName=c("W14","LL5","L5","G4")
growthDS = NULL
genList=unique(hData$genotype)

for(g in genList){
  tmpSData = data.frame(hData%>%filter(genotype==as.character(g)))
  model_1=NULL;model_2=NULL;model_3=NULL;model_4=NULL
  
  # Create continuous DAC values
  sTime = 20 #min(tmpSData$DAP)
  eTime = 80
  total_obs = eTime-sTime+1
  x <- seq(sTime, eTime, length.out = total_obs)
  DAP <- seq(sTime, eTime, length.out = total_obs)
  
  if(nrow(tmpSData)>0){
    #par(mfrow=c(3,1))
    model_1=drm(height.adj.smooth~DAP, data = tmpSData, fct = W1.4())
    #plot(model_1,main="Weibull Model 1.4")
    
    model_2=drm(height.adj.smooth~DAP, data = tmpSData, fct = LL.5())
    #plot(model_2,main="Log-logistics model 5")
    
    model_3=drm(height.adj.smooth~DAP, data = tmpSData, fct = L.5())
    #plot(model_3,main="Logistic model")
    #par(mfrow=c(1,1))
    
    model_4=drm(height.adj.smooth~DAP, data = tmpSData, fct = G.4())
    #plot(model_4,main="Gompertz dose-response model")
    
    d = AIC(model_1,model_2,model_3,model_4)
    index = which(d$AIC==min(d$AIC))
    d$Status = TRUE
    d$Status[index] = FALSE
    
    # Compute height and growthRate
    par = NULL
    
    if(index==1){
      par = model_1$fit$par
    }else if(index==2){
      par = model_2$fit$par
    }else if(index==3){
      par = model_3$fit$par
    }else if(index==4){
      par = model_4$fit$par
    }
    
    grData = model_fit(x,DAP,par,index)
    colnames(grData)<-c("DAP","pHeight","GrowthRate")
    grData$Genotype = as.character(g)
    grData = as.data.frame(grData[,c(1,4,2,3)])
    
    if(is.null(growthDS)){
      growthDS = as.data.frame(grData)
    }else{
      growthDS = rbind(growthDS, grData)
    }
    rm(grData, model_1, model_2, model_3, model_4, par,d,index,tmpSData)
  }
}
rm(genList)

## Assign genotype code
genList=unique(growthDS$Genotype)
growthDS$genCode=""
i=1
for(g in genList){
  t = which(growthDS$Genotype==as.character(g))
  growthDS$genCode[t] = paste("G_",i,sep = "")
  i = i + 1
}

growthDS$pHeight = round(growthDS$pHeight, 6)
growthDS$GrowthRate = round(growthDS$GrowthRate, 6)

# For each genotype, compute cumulative growth rate
growthDS=as.data.frame(growthDS%>%group_by(Genotype)%>%mutate(cGrowthRate=cumsum(GrowthRate)))

##################################################################################################
############### Plot growth curve
# For each genotype, plot the growth curve
imgPath = "spacialAdjust/gr/"
genList=unique(growthDS$Genotype)

dev.set(which = dev.next())
for(g in genList){
  t = which(growthDS$Genotype==as.character(g))
  ga = unique(growthDS$genCode[t])
  
  while (!is.null(dev.list()))  dev.off()
  pdf(file=paste(imgPath,as.character(ga),".pdf",sep=""))
  matplot(growthDS$DAP[t], cbind(normalize(growthDS$GrowthRate[t], method = "range"), normalize(growthDS$cGrowthRate[t], method = "range")), type = 'l', main = paste(as.character(g),"(",as.character(ga),")",sep = ""), col=c('red', 'blue'), xlab = "DAP", ylab = "GrowthRate (red), cumGrowthRate (blue)")
  while (!is.null(dev.list()))  dev.off()
}
while (!is.null(dev.list()))  dev.off()

##################################################################################################
######### Remove genotypes based on growth curve
## G_=c(3,4,5,7,14,22,23,24,30,33,51,70,71,75,76,80,81,82,84,86,91,93.96)
l = setdiff(c(1:100),c(3:5,7,14,22:24,30,33,51,70,71,75,76,80:82,84,86,91,93,96))
excludeGL = paste("G_",l,sep = "")
growthDS = as.data.frame(growthDS%>%filter(genCode%in%excludeGL)%>%arrange(Genotype,DAP))

##################################################################################################
######### Compute weather information
weatherDS = read.csv("Weather.csv")
weatherDS$DaysOfYear = DaysOfYearByDate(weatherDS$Date)
sDayOfYear = DayOfAYear(2017,5,25)
weatherDS$DAP = weatherDS$DaysOfYear-sDayOfYear+1
#weatherDS = as.data.frame(weatherDS%>%filter(DAP>0))

# Create GDD
weatherDS = as.data.frame(weatherDS%>%group_by(Date,DaysOfYear,DAP)%>%summarise(SoilMoistInVWC=mean(Soil.Moist.VWC...VWC.),
                                                                                TemperatureMin=min(Temperature...F.),
                                                                                TemperatureMax = max(Temperature...F.),
                                                                                TemperatureAvg = mean(Temperature...F.),
                                                                                SolarRadInWatM2=mean(Solar.Rad..wat.m2.),
                                                                                HumidityInPerc=mean(RH.....HMD),
                                                                                WindGustInMph=mean(Wind.Gust..mph.),
                                                                                WindSpeedInMph=mean(Wind.Speed..mph.),
                                                                                DewPointInF=mean(Dew.Point...F.))%>%arrange(DaysOfYear))

# Compute the trend of the weather data
plot(weatherDS$DaysOfYear, weatherDS$TemperatureMin, type='l')

d = decompose(ts(weatherDS$SoilMoistInVWC, frequency = 7))
weatherDS$SoilMoist = d$trend

d = decompose(ts(weatherDS$TemperatureMin, frequency = 7))
weatherDS$TempMin = d$trend

d = decompose(ts(weatherDS$TemperatureMax, frequency = 7))
weatherDS$TempMax = d$trend

d = decompose(ts(weatherDS$TemperatureAvg, frequency = 7))
weatherDS$TempAvg = d$trend

d = decompose(ts(weatherDS$SolarRadInWatM2, frequency = 7))
weatherDS$SolarRad = d$trend

d = decompose(ts(weatherDS$HumidityInPerc, frequency = 7))
weatherDS$Humidity = d$trend

d = decompose(ts(weatherDS$WindGustInMph, frequency = 7))
weatherDS$WindGust = d$trend

d = decompose(ts(weatherDS$WindSpeedInMph, frequency = 7))
weatherDS$WindSpeed = d$trend

d = decompose(ts(weatherDS$DewPointInF, frequency = 7))
weatherDS$DewPoint = d$trend

# Remove NA valued rows
weatherDS = as.data.frame(weatherDS%>%filter(!is.na(TempMin)))

weatherDS$GDD = ComputeGDD(weatherDS$TempMax, weatherDS$TempMin, type = "F")

matplot(weatherDS$DaysOfYear, cbind(weatherDS$TempMax, weatherDS$TempMin, weatherDS$TempAvg, weatherDS$GDD), type='l', col=c('red','blue','darkgreen', 'darkorange'))

# Remove rows for DAP<=0
weatherDS = as.data.frame(weatherDS%>%filter(DAP>0))

weatherDS$cGDD = cumsum(weatherDS$GDD)

weatherDS = as.data.frame(weatherDS[,c(2,3,13,17,18,22,23)])

# Correlation analysis
corDS = abs(cor(weatherDS[,c(3:7)], use = "pairwise.complete.obs"))
pheatmap(corDS)

##################################################################################################
######### Combine growth data with weather data
phenoDS = as.data.frame(merge(growthDS, weatherDS, by.x = "DAP", by.y = "DAP")%>%arrange(Genotype, DAP))

# Save it to file
write.csv(phenoDS, "spacialAdjust/Dry_Growth_adjusted.csv")

##################################################################################################
######### Analysis

phenoDS = read.csv("spacialAdjust/Dry_Growth_adjusted.csv")

# Weather correlation
corDS = abs(cor(phenoDS[,c(9:13)], use = "pairwise.complete.obs"))
pheatmap(corDS)

# Finding the pick growth rate and create dataset using pick data only
phenoPickDS = NULL
gn=unique(phenoDS$genCode)
l=c()
for(g in gn){
  df = as.data.frame(phenoDS%>%filter(genCode==as.character(g)))
  
  p=which(diff(sign(diff(df$GrowthRate)))==-2)+1
  
  if(length(p)>0){
    l=append(l,p)
    if(is.null(phenoPickDS)){
      phenoPickDS = as.data.frame(df[c(1:p),])
    }else{
      phenoPickDS = as.data.frame(rbind(phenoPickDS, df[c(1:p),]))
    }
  }
}

corDS = (cor(phenoPickDS[,c(9:13)], use = "pairwise.complete.obs"))
pheatmap(corDS)

write.csv(phenoPickDS[,-c(1)], "spacialAdjust/Dry_adjusted_Growth_pick.csv")

##################################################################################################
######### Analysis

phenoDS = read.csv("spacialAdjust/Dry_adjusted_Growth_pick.csv")

corDS = (cor(phenoDS[,c(2,5,9:13)], use = "pairwise.complete.obs"))
pheatmap(corDS)




