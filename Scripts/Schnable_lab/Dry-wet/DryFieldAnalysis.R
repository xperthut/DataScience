source("header.R")

#### Preprocess #############
###### Read file
phenoRep1DS = read.csv("Dry_Field/rep1_height_est.csv")
phenoRep2DS = read.csv("Dry_Field/rep2_height_est.csv")
phenoRep1DS$DaysOfYear = DaysOfYearByDate(phenoRep1DS$date, frmat = "%Y-%m-%d")
phenoRep2DS$DaysOfYear = DaysOfYearByDate(phenoRep2DS$date, frmat = "%Y-%m-%d")
phenoRep1DS$TimeOfYear = SecondOfYearByDate(phenoRep1DS$time)
phenoRep2DS$TimeOfYear = SecondOfYearByDate(phenoRep2DS$time)

phenoRep1DS = as.data.frame(phenoRep1DS%>%arrange(DaysOfYear, TimeOfYear))
phenoRep2DS = as.data.frame(phenoRep2DS%>%arrange(DaysOfYear, TimeOfYear))

l = which(is.na(phenoRep1DS$height))
if(length(l)>0){
  phenoRep1DS = as.data.frame(phenoRep1DS[-c(l),])
}

l = which(is.na(phenoRep2DS$height))
if(length(l)>0){
  phenoRep2DS = as.data.frame(phenoRep2DS[-c(l),])
}

write.csv(phenoRep1DS[,-c(1)], "Dry_Field/rep1_height_est_dt.csv")
write.csv(phenoRep2DS[,-c(1)], "Dry_Field/rep2_height_est_dt.csv")

#### Don't run the above part again #############
#### Only run the following parts #############
#### preprocess Start from here #############

phenoRep1DS = read.csv("Dry_Field/rep1_height_est_dt.csv")
phenoRep2DS = read.csv("Dry_Field/rep2_height_est_dt.csv")

genDS = read.csv("Genotype.csv")
genDS$Irrigated = ifelse(genDS$Field=="Irrigated", yes=TRUE, no=FALSE)
genDS = as.data.frame(genDS%>%filter(Irrigated==FALSE))

# Dataset using unique cameraID and genotype for replica 1
rep1DS = as.data.frame(genDS[,c(2,4)]%>%distinct())

# Dataset using unique cameraID and genotype for replica 2
rep2DS = as.data.frame(genDS[,c(2,5)]%>%distinct())

rep1PhenoDS = merge(phenoRep1DS, rep1DS, by.x = "cam", by.y = "Rep1_Cameras")
rep2PhenoDS = merge(phenoRep2DS, rep2DS, by.x = "cam", by.y = "Rep2_Cameras")
rm(phenoRep1DS,phenoRep2DS, rep1DS, rep2DS, genDS)

rep1PhenoDS = as.data.frame(rep1PhenoDS[,c(11,3,4,9,10,5,6,8)]%>%arrange(Genotype,plant.no,DaysOfYear,TimeOfYear))
rep2PhenoDS = as.data.frame(rep2PhenoDS[,c(11,3,4,9,10,5,6,8)]%>%arrange(Genotype,plant.no,DaysOfYear,TimeOfYear))

# Compute DAP (Days After Planting)
# Start planting day of year
sDayOfYear = DayOfAYear(2017,5,25)
rep1PhenoDS$DAP = rep1PhenoDS$DaysOfYear-sDayOfYear+1
rep2PhenoDS$DAP = rep2PhenoDS$DaysOfYear-sDayOfYear+1

# For each [Genotype, DAP, date, TimeOfYear] group, compute average height per day
rep1PhenoDS = as.data.frame(rep1PhenoDS%>%group_by(Genotype, DAP, date, TimeOfYear)%>%summarise(meanH = mean(height), meanEstH=mean(estimation)))
rep2PhenoDS = as.data.frame(rep2PhenoDS%>%group_by(Genotype, DAP, date, TimeOfYear)%>%summarise(meanH = mean(height), meanEstH=mean(estimation)))

# Further computer Genotype wise estimated height as follows
rep1PhenoDS = as.data.frame(rep1PhenoDS%>%group_by(Genotype, DAP, date)%>%summarise(meanH = mean(meanH), meanEstH=mean(meanEstH)))
rep2PhenoDS = as.data.frame(rep2PhenoDS%>%group_by(Genotype, DAP, date)%>%summarise(meanH = mean(meanH), meanEstH=mean(meanEstH)))

# Save data
write.csv(rep1PhenoDS, "Dry_Field/rep1_height_est_new.csv")
write.csv(rep2PhenoDS, "Dry_Field/rep2_height_est_new.csv")
rm(rep1PhenoDS,rep2PhenoDS)

#### Preprocess Done #############
######## New start here ##########
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

phenoRep1DS = read.csv("Dry_Field/rep1_height_est_new.csv")
phenoRep2DS = read.csv("Dry_Field/rep2_height_est_new.csv")

################## Only genotype starts here ###################
#phenoRep1DS <-phenoRep1DS%>%group_by(DAP, date, Genotype)%>%summarise(meanH = mean(meanH), meanEstH = mean(meanEstH), weight=n())
#phenoRep2DS <-phenoRep2DS%>%group_by(DAP, date, Genotype)%>%summarise(meanH = mean(meanH), meanEstH = mean(meanEstH), weight=n())

# fit model for each genotype
mName=c("W14","LL5","L5","G4")
rep1DS = NULL
genList = unique(phenoRep1DS$Genotype)

for(g in genList){
  tmpSData = data.frame(phenoRep1DS%>%filter(Genotype==as.character(g)))
  model_1=NULL;model_2=NULL;model_3=NULL;model_4=NULL
  
  # Create continuous DAC values
  sTime = min(tmpSData$DAP)
  eTime = 80
  total_obs = eTime-sTime+1
  x <- seq(sTime, eTime, length.out = total_obs)
  DAP <- seq(sTime, eTime, length.out = total_obs)
  
  if(nrow(tmpSData)>0){
    #par(mfrow=c(3,1))
    model_1=drm(meanEstH~DAP, data = tmpSData, fct = W1.4())
    #plot(model_1,main="Weibull Model 1.4")
    
    model_2=drm(meanEstH~DAP, data = tmpSData, fct = LL.5())
    #plot(model_2,main="Log-logistics model 5")
    
    model_3=drm(meanEstH~DAP, data = tmpSData, fct = L.5())
    #plot(model_3,main="Logistic model")
    #par(mfrow=c(1,1))
    
    model_4=drm(meanEstH~DAP, data = tmpSData, fct = G.4())
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
    
    if(is.null(rep1DS)){
      rep1DS = as.data.frame(grData)
    }else{
      rep1DS = rbind(rep1DS, grData)
    }
    rm(grData, model_1, model_2, model_3, model_4, par,d,index,tmpSData)
  }
}

rm(genList)

# fit model for each genotype
mName=c("W14","LL5","L5","G4")
rep2DS = NULL
genList = unique(phenoRep2DS$Genotype)

for(g in genList){
  tmpSData = data.frame(phenoRep2DS%>%filter(Genotype==as.character(g)))
  model_1=NULL;model_2=NULL;model_3=NULL;model_4=NULL
  
  # Create continuous DAC values
  sTime = min(tmpSData$DAP)
  eTime = 80
  total_obs = eTime-sTime+1
  x <- seq(sTime, eTime, length.out = total_obs)
  DAP <- seq(sTime, eTime, length.out = total_obs)
  
  if(nrow(tmpSData)>0){
    #par(mfrow=c(3,1))
    model_1=drm(meanEstH~DAP, data = tmpSData, fct = W1.4())
    #plot(model_1,main="Weibull Model 1.4")
    
    model_2=drm(meanEstH~DAP, data = tmpSData, fct = LL.5())
    #plot(model_2,main="Log-logistics model 5")
    
    model_3=drm(meanEstH~DAP, data = tmpSData, fct = L.5())
    #plot(model_3,main="Logistic model")
    #par(mfrow=c(1,1))
    
    model_4=drm(meanEstH~DAP, data = tmpSData, fct = G.4())
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
    
    if(is.null(rep2DS)){
      rep2DS = as.data.frame(grData)
    }else{
      rep2DS = rbind(rep2DS, grData)
    }
    rm(grData, model_1, model_2, model_3, model_4, par,d,index,tmpSData)
  }
}
rm(genList)

# Combina weather data for replica 1
phenoRep1DS = as.data.frame(merge(rep1DS, weatherDS, by.x = "DAP", by.y = "DAP"))
phenoRep2DS = as.data.frame(merge(rep2DS, weatherDS, by.x = "DAP", by.y = "DAP"))

phenoRep1DS$Replica=1
phenoRep2DS$Replica=2

# Combine two replicas
grData = as.data.frame(rbind(phenoRep1DS, phenoRep2DS))
rm(phenoRep1DS, phenoRep2DS, rep1DS, rep2DS, weatherDS)

## Filter Genotype which has both replica data
repDS = as.data.frame(grData[,c(2,11)]%>%distinct())
repDS = as.data.frame(repDS%>%group_by(Genotype)%>%summarise(rc=n())%>%filter(rc>1))

grData = as.data.frame(grData%>%filter(Genotype%in%repDS$Genotype))
grData = as.data.frame(grData[,c(1,5,2,11,3,4,6:10)])

phenoRep1DS = as.data.frame(grData%>%filter(Replica==1))
phenoRep2DS = as.data.frame(grData%>%filter(Replica==2))

write.csv(phenoRep1DS%>%arrange(Genotype, DAP), "Dry_Field/dry_data_genotype_rep1.csv")
write.csv(phenoRep2DS%>%arrange(Genotype, DAP), "Dry_Field/dry_data_genotype_rep2.csv")
write.csv(grData%>%arrange(Genotype, Replica, DAP), "Dry_Field/dry_data_genotype.csv")


################################### Genotype, plantno Done ########################
# # Compute growth rate for each [genotype, plantno] combination
# df = as.data.frame(phenoRep1DS[,c(4,5)]%>%distinct())
# 
# # fit model for each genotype, plantno
# mName=c("W14","LL5","L5","G4")
# rep1DS = NULL
# 
# for(i in 1:nrow(df)){
#   tmpSData = data.frame(phenoRep1DS%>%filter(Genotype==as.character(df$Genotype[i]) & plant.no==df$plant.no[i])) # & Rep==tmp$Rep[i]))
#   model_1=NULL;model_2=NULL;model_3=NULL;model_4=NULL
#   
#   # Create continuous DAC values
#   sTime = min(tmpSData$DAP)
#   eTime = 80
#   total_obs = eTime-sTime+1
#   x <- seq(sTime, eTime, length.out = total_obs)
#   DAP <- seq(sTime, eTime, length.out = total_obs)
#   
#   if(nrow(tmpSData)>0){
#     #par(mfrow=c(3,1))
#     model_1=drm(meanEstH~DAP, data = tmpSData, fct = W1.4())
#     #plot(model_1,main="Weibull Model 1.4")
#     
#     model_2=drm(meanEstH~DAP, data = tmpSData, fct = LL.5())
#     #plot(model_2,main="Log-logistics model 5")
#     
#     model_3=drm(meanEstH~DAP, data = tmpSData, fct = L.5())
#     #plot(model_3,main="Logistic model")
#     #par(mfrow=c(1,1))
#     
#     model_4=drm(meanEstH~DAP, data = tmpSData, fct = G.4())
#     #plot(model_4,main="Gompertz dose-response model")
#     
#     d = AIC(model_1,model_2,model_3,model_4)
#     index = which(d$AIC==min(d$AIC))
#     d$Status = TRUE
#     d$Status[index] = FALSE
#     
#     # Compute height and growthRate
#     par = NULL
#     
#     if(index==1){
#       par = model_1$fit$par
#     }else if(index==2){
#       par = model_2$fit$par
#     }else if(index==3){
#       par = model_3$fit$par
#     }else if(index==4){
#       par = model_4$fit$par
#     }
#     
#     grData = model_fit(x,DAP,par,index)
#     colnames(grData)<-c("DAP","pHeight","GrowthRate")
#     grData$Genotype = as.character(df$Genotype[i])
#     grData$PlantNo = as.integer(df$plant.no[i])
#     grData = as.data.frame(grData[,c(1,4,5,2,3)])
#     
#     if(is.null(rep1DS)){
#       rep1DS = as.data.frame(grData)
#     }else{
#       rep1DS = rbind(rep1DS, grData)
#     }
#      rm(grData, model_1, model_2, model_3, model_4, par,d,index,tmpSData)
#   }
# }
# 
# rm(df)
# 
# # Assign colors plant no wise
# color = c("red","darkgreen", "darkorange", "purple","darkcyan", "blue","darkpink")
# rep1DS$pColor=color[rep1DS$PlantNo]
# 
# # Plot the growthrate to a file, all plants in a plot
# imgPath = "Dry_Field/img1/"
# df = unique(rep1DS$Genotype)
# for(i in df){
#   t = which(rep1DS$Genotype==as.character(i))
#   tdf=as.data.frame(rep1DS[t,c(3,6)]%>%distinct())
#   while (!is.null(dev.list()))  dev.off()
#   pdf(file=paste(imgPath,as.character(i),".pdf",sep=""))
#   plot(rep1DS$DAP[t], rep1DS$GrowthRate[t], col=rep1DS$pColor[t], 
#        xlab = "Days After Planting (DAP)", ylab = "Growth-rate", 
#        main = paste("Genotype: ", as.character(i),sep=""), pch=c(16))
#   legend(x=min(rep1DS$DAP),y=max(rep1DS$GrowthRate[t]), legend = tdf$PlantNo, col = tdf$pColor, pch=c(16))
#   while (!is.null(dev.list()))  dev.off()
#   rm(tdf)
# }
# while (!is.null(dev.list()))  dev.off()
# 
# # Create replica field
# rep1DS$Replica = 1
# 
# ################################
# # Compute growth rate for each [genotype, plantno] combination
# df = as.data.frame(phenoRep2DS[,c(4,5)]%>%distinct())
# 
# # fit model for each genotype, plantno
# mName=c("W14","LL5","L5","G4")
# rep2DS = NULL
# 
# for(i in 1:nrow(df)){
#   tmpSData = data.frame(phenoRep2DS%>%filter(Genotype==as.character(df$Genotype[i]) & plant.no==df$plant.no[i])) # & Rep==tmp$Rep[i]))
#   model_1=NULL;model_2=NULL;model_3=NULL;model_4=NULL
#   
#   # Create continuous DAC values
#   sTime = min(tmpSData$DAP)
#   eTime = 80
#   total_obs = eTime-sTime+1
#   x <- seq(sTime, eTime, length.out = total_obs)
#   DAP <- seq(sTime, eTime, length.out = total_obs)
#   
#   if(nrow(tmpSData)>0){
#     #par(mfrow=c(3,1))
#     model_1=drm(meanEstH~DAP, data = tmpSData, fct = W1.4())
#     #plot(model_1,main="Weibull Model 1.4")
#     
#     model_2=drm(meanEstH~DAP, data = tmpSData, fct = LL.5())
#     #plot(model_2,main="Log-logistics model 5")
#     
#     model_3=drm(meanEstH~DAP, data = tmpSData, fct = L.5())
#     #plot(model_3,main="Logistic model")
#     #par(mfrow=c(1,1))
#     
#     model_4=drm(meanEstH~DAP, data = tmpSData, fct = G.4())
#     #plot(model_4,main="Gompertz dose-response model")
#     
#     d = AIC(model_1,model_2,model_3,model_4)
#     index = which(d$AIC==min(d$AIC))
#     d$Status = TRUE
#     d$Status[index] = FALSE
#     
#     # Compute height and growthRate
#     par = NULL
#     
#     if(index==1){
#       par = model_1$fit$par
#     }else if(index==2){
#       par = model_2$fit$par
#     }else if(index==3){
#       par = model_3$fit$par
#     }else if(index==4){
#       par = model_4$fit$par
#     }
#     
#     grData = model_fit(x,DAP,par,index)
#     colnames(grData)<-c("DAP","pHeight","GrowthRate")
#     grData$Genotype = as.character(df$Genotype[i])
#     grData$PlantNo = as.integer(df$plant.no[i])
#     grData = as.data.frame(grData[,c(1,4,5,2,3)])
#     
#     if(is.null(rep2DS)){
#       rep2DS = as.data.frame(grData)
#     }else{
#       rep2DS = rbind(rep2DS, grData)
#     }
#     rm(grData, model_1, model_2, model_3, model_4, par,d,index,tmpSData)
#   }
# }
# 
# rm(df)
# 
# # Assign colors plant no wise
# color = c("red","darkgreen", "darkorange", "purple","darkcyan", "blue","darkpink")
# rep2DS$pColor=color[rep2DS$PlantNo]
# 
# # Plot the growthrate to a file, all plants in a plot
# imgPath = "Dry_Field/img2/"
# df = unique(rep2DS$Genotype)
# for(i in df){
#   t = which(rep2DS$Genotype==as.character(i))
#   tdf=as.data.frame(rep2DS[t,c(3,6)]%>%distinct())
#   while (!is.null(dev.list()))  dev.off()
#   pdf(file=paste(imgPath,as.character(i),".pdf",sep=""))
#   plot(rep2DS$DAP[t], rep2DS$GrowthRate[t], col=rep2DS$pColor[t], 
#        xlab = "Days After Planting (DAP)", ylab = "Growth-rate", 
#        main = paste("Genotype: ", as.character(i),sep=""), pch=c(16))
#   legend(x=min(rep2DS$DAP),y=max(rep2DS$GrowthRate[t]), legend = tdf$PlantNo, col = tdf$pColor, pch=c(16))
#   while (!is.null(dev.list()))  dev.off()
#   rm(tdf)
# }
# while (!is.null(dev.list()))  dev.off()
# 
# # Create replica field
# rep2DS$Replica = 2
# 
# # Combina weather data for replica 1
# phenoRep1DS = as.data.frame(merge(rep1DS, weatherDS, by.x = "DAP", by.y = "DAP"))
# phenoRep2DS = as.data.frame(merge(rep2DS, weatherDS, by.x = "DAP", by.y = "DAP"))
# 
# # Combine two replicas
# grData = as.data.frame(rbind(phenoRep1DS, phenoRep2DS))
# rm(phenoRep1DS, phenoRep2DS, rep1DS, rep2DS, weatherDS)
# 
# grData = as.data.frame(grData%>%group_by(Genotype,Replica,PlantNo)%>%mutate(cGDD=cumsum(GDDTrend)))
# grData = as.data.frame(grData%>%arrange(Genotype, Replica,PlantNo, DAP))
# 
# write.csv(grData, "Dry_Field/dry_data.csv")

################################### Genotype, plantno Done ########################


##################################################################################################
# For each genotype, plot the growth curve
imgPath = "Dry_Field/dry_gen/"
genList = unique(repDS$Genotype)

dev.set(which = dev.next())
for(g in genList){
  t= which(grData$Genotype==as.character(g))
  
  while (!is.null(dev.list()))  dev.off()
  pdf(file=paste(imgPath,as.character(g),".pdf",sep=""))
  plot(grData$DAP[t], grData$GrowthRate[t], col=ifelse(grData$Replica[t]==1, yes="blue", no='red'))
  legend("topleft", legend = c("rep-1","rep-2"), col = c("blue","red"), lwd = 2)
  while (!is.null(dev.list()))  dev.off()
}
while (!is.null(dev.list()))  dev.off()

##################################################################################################
# For each genotype, replica, how many observations are there
obsDS <-grData%>%group_by(Genotype, Replica)%>%summarise(obs=n(), mDAP=min(DAP), xDAP=max(DAP))
obsGDS <- obsDS%>%group_by(Genotype)%>%summarise(obs=n())%>%filter(obs>1)

phenoDS = NULL

for(i in 1:nrow(obsGDS)){
  t = as.data.frame(obsDS%>%filter(Genotype==as.character(obsGDS$Genotype[i])))
  tmpDS = as.data.frame(grData%>%filter(Genotype==as.character(obsGDS$Genotype[i]) & DAP>=max(t$mDAP))%>%group_by(Genotype,DAP)%>%summarise(GrowthRateDiff=diff(GrowthRate), HeightDiff=diff(pHeight)))
  
  if(is.null(phenoDS)){
    phenoDS=as.data.frame(tmpDS)
  }else{
    phenoDS = as.data.frame(rbind(phenoDS, tmpDS))
  }
  
  rm(tmpDS,t)
}
rm(obsDS, obsGDS)

phenoDS=phenoDS%>%arrange(Genotype, DAP)


################## Only genotype ends here ###################

# Check the growth-rate differences
r1DS = as.data.frame(grData%>%filter(Replica==1))
r2DS = as.data.frame(grData%>%filter(Replica==2))
g1 = sort(unique(r1DS$Genotype))
g2 = sort(unique(r2DS$Genotype))

gList = intersect(g1,g2)
df = as.data.frame(grData[,c(2,3,7)]%>%distinct())
t = which(df$Genotype%in%gList)
df = as.data.frame(df[t,])
x=df%>%group_by(Genotype,PlantNo)%>%summarise(r=n())
t = which(x$r==2)
df = as.data.frame(x[t,])

for(i in 1:nrow(df)){
  t = which(grData$Genotype==as.character(df$Genotype[i]) & grData$PlantNo==df$PlantNo[i])
  tdf = grData[t,c(1:5,7)]
  hv1 = tdf$pHeight[which(tdf$Replica==1)]
  hv2 = tdf$pHeight[which(tdf$Replica==2)]
}

## Read growth-rate data
grData = read.csv("Dry_Field/dry_data.csv")
grData1Rep = as.data.frame(grData%>%filter(Replica==1)%>%arrange(Genotype,PlantNo,DAP))
grData2Rep = as.data.frame(grData%>%filter(Replica==2)%>%arrange(Genotype,PlantNo,DAP))

write.csv(grData1Rep[,-c(1)],"Dry_Field/dry_data_rep1.csv")
write.csv(grData2Rep[,-c(1)],"Dry_Field/dry_data_rep2.csv")

#grData = as.data.frame(grData%>%arrange(Genotype, Replica,PlantNo, DAP))
#write.csv(grData[,-c(1)],"Dry_Field/dry_data.csv")

