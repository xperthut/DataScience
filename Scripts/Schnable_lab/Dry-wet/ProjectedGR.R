
# Author: Methun Kamruzzaman
# Date: July 25, 2020

source("header.R")
###########################################################

# Load the raw file with height information
sData = read.csv(file.choose())

# Start planting day of year
sDayOfYear = DayOfAYear(2017,5,25)

# This loop takes time, please wait
sData$DaysOfYear = 0
sData$DAP = 0
for(i in 1:nrow(sData)){
  sData$DaysOfYear[i] = DayOfAYear(2017,sData$Month[i], sData$Day[i])
  sData$DAP[i] = sData$DaysOfYear[i]-sDayOfYear+1
}

# Weather data
wData = read.csv(file.choose())

# Consider point=[genotype,rep,location,dap]

#########################################################################
sTime = 1; eTime = 80; adjust = TRUE
h_img_path="/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Stephen/himg/"
gr_img_path="/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Stephen/img/" 
file_path="/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Stephen/"

# Compute mean height based on TurkID,Genotype,Rep,Irrigated,DaysOfYear
mData = as.data.frame(sData%>%group_by(Genotype,Irrigated,Rep,DaysOfYear,DAP,TurkerID)%>%summarise(Height=mean(height, na.rm = TRUE), Obs=n()))

# Compute weighted height without considering rep
ooData = as.data.frame(mData%>%group_by(Genotype,Irrigated,DAP,Rep,DaysOfYear)%>%summarise(Height=weighted.mean(Height, Obs, na.rm=TRUE), obs=n()))
oData = as.data.frame(ooData%>%group_by(Genotype,Irrigated,DAP,DaysOfYear)%>%summarise(Height=weighted.mean(Height, obs, na.rm=TRUE)))

#oData = as.data.frame(mData%>%group_by(Genotype,Irrigated,Rep,DaysOfYear,DAP)%>%summarise(Height=mean(Height, na.rm = TRUE), obs=n()))
rm(mData, ooData)

# Without rep
obs.Data = as.data.frame(oData%>%group_by(Genotype,Irrigated)%>%summarise(minD=min(DAP), maxD=max(DAP), obs=length(unique(DAP)), DAPDiff=maxD-minD+1, miss=DAPDiff-obs))
th = ceiling(max(obs.Data$obs)/2)
obs.Data = as.data.frame(obs.Data%>%filter(obs>=th))# & miss<10))

## Filter [genotype,replicate] tested in both locations
# tmp = obs.Data[,c(1:3)]%>%distinct()
# tmp = tmp%>%group_by(Genotype,Rep)%>%summarise(Env=n())
# ftmp=as.data.frame(tmp%>%filter(Env>1))
# rm(tmp, obs.Data)

# Filter [genotype] tested in both locations
tmp = obs.Data%>%group_by(Genotype)%>%summarise(Env=n())
ftmp=as.data.frame(tmp%>%filter(Env>1))
rm(tmp, obs.Data)

# 83 genotype tested in both locations whose total DAP data >=20

# Pick index which has valid data point
gl = unique(ftmp$Genotype)
k=which(oData$Genotype%in%gl)
phData = as.data.frame(oData[k,])
#phData$Weight=phData$obs/6
phData$Weight=1.0
rm(ftmp, oData)

# Unique genotypes, environment, replicates
# tmp = as.data.frame(phData[,c(1:3)]%>%distinct())

# Unique genotypes, environment
tmp = as.data.frame(phData[,c(1:2)]%>%distinct())

# Min available DAP=28
# Max available DAP = 68
# Make GR from DAP 20 to DAP 70
# To avoid negative height, assume the height of the plant at DAP=20 is 0.0254m=1in;
# inclrement of height from DAP 20 to minDAP is linear
# for(i in 1:nrow(tmp)){
#   tmpData = data.frame(phData%>%filter(Genotype==as.character(tmp$Genotype[i]) & Irrigated==tmp$Irrigated[i]))
#   
#   if(nrow(tmpData)>0){
#     tmp1Data = as.data.frame(tmpData[which(tmpData$DAP==min(tmpData$DAP)),])
#     slope = (tmp1Data$Height-0.0254)/(tmp1Data$DAP-20)
#     mxDAP = tmp1Data$DAP-1
#     tmp1Data$DaysOfYear=tmp1Data$DaysOfYear-(tmp1Data$DAP-20)
#     tmp1Data$DAP=20
#     tmp1Data$Height=0.0254
#     #tmp1Data$Weight=1.0
#     phData = as.data.frame(rbind(phData, tmp1Data))
#     
#     for(al in 21:mxDAP){
#       tmp1Data$DAP=tmp1Data$DAP+1
#       tmp1Data$DaysOfYear=tmp1Data$DaysOfYear+1
#       tmp1Data$Height=slope*(al-20+1)
#       phData = as.data.frame(rbind(phData, tmp1Data))
#     }
#     
#     rm(tmp1Data)
#   }
#   rm(tmpData)
# }
# 
# # Sort data based on genotype, DAP, irrigation
# phData = as.data.frame(phData%>%arrange(Genotype, DAP, Irrigated))

######## Plot the height data
# Plotting [genotype, Env]
GenList = unique(phData$Genotype)

p = 0
pp = 2
pdf(file=paste(h_img_path,p,"_4.pdf",sep=""))
par(mfrow=c(pp,1))

for(i in GenList){
  tmpSData = phData%>%filter(Genotype==as.character(i))
  if(nrow(tmpSData)>0){
    tmpSData.dry = tmpSData%>%filter(Irrigated==FALSE)
    tmpSData.wet = tmpSData%>%filter(Irrigated==TRUE)
    
    if(nrow(tmpSData.dry)>0 & nrow(tmpSData.wet)>0){
      if(p%%pp==0){
        dev.off()
        
        pdf(file=paste(h_img_path,p,"_4.pdf",sep=""))
        par(mfrow=c(pp,1))
      }
      
      plot(tmpSData.dry$DAP, tmpSData.dry$Height, col="red", pch=16, xlab = "Days After Planting (DAP)", ylab = "Height", main = paste("No Irrigated:",as.character(i),sep = ""))
      plot(tmpSData.wet$DAP, tmpSData.wet$Height, col="blue", pch=16, xlab = "Days After Planting (DAP)", ylab = "Height", main = paste("Irrigated:",as.character(i),sep = ""))
      
      p = p +2
    }
    
    rm(tmpSData.dry,tmpSData.wet)
  }
  rm(tmpSData)
}
dev.off()

#########################################################################
# Fit data based on genotype and env
mName=c("W12","W13","W14","LL2","LL4","LL5","MM3","AR3","L5","G4")
heightData = NULL
sTime = 20
eTime = 80
total_obs = eTime-sTime+1

for(i in 1:nrow(tmp)){
  tmpSData = data.frame(phData%>%filter(Genotype==as.character(tmp$Genotype[i]) & Irrigated==tmp$Irrigated[i])) # & Rep==tmp$Rep[i]))
  
  model_1=NULL;model_2=NULL;model_3=NULL;model_4=NULL;model_5=NULL;model_6=NULL;model_7=NULL;model_8=NULL;model_9=NULL;model_10=NULL
  
  if(nrow(tmpSData)>0){
    model_1=drm(Height~DAP, data = tmpSData, fct = W1.2(), weights = Weight)
    #model_1=drm(Height~DAP, data = tmpSData, fct = W1.2())
    #plot(model_1,main="Weibull Model 1.2")
    
    model_2=drm(Height~DAP, data = tmpSData, fct = W1.3(), weights = Weight)
    #model_2=drm(Height~DAP, data = tmpSData, fct = W1.3())
    #plot(model_2,main="Weibull Model 1.3")
    
    model_3=drm(Height~DAP, data = tmpSData, fct = W1.4(), weights = Weight)
    #model_3=drm(Height~DAP, data = tmpSData, fct = W1.4())
    #plot(model_3,main="Weibull Model 1.4")
    
    model_4=drm(Height~DAP, data = tmpSData, fct = LL.2(), weights = Weight)
    #model_4=drm(Height~DAP, data = tmpSData, fct = LL.2())
    #plot(model_4,main="Log-logistics model 2")
    
    model_5=drm(Height~DAP, data = tmpSData, fct = LL.4(), weights = Weight)
    #model_5=drm(Height~DAP, data = tmpSData, fct = LL.4())
    #plot(model_5,main="Log-logistics model 4")
    
    model_6=drm(Height~DAP, data = tmpSData, fct = LL.5(), weights = Weight)
    #model_6=drm(Height~DAP, data = tmpSData, fct = LL.5())
    #plot(model_6,main="Log-logistics model 5")
    
    model_7=drm(Height~DAP, data = tmpSData, fct = MM.3(), weights = Weight)
    #model_7=drm(Height~DAP, data = tmpSData, fct = MM.3())
    #plot(model_7,main="Michaelis-Menten model")
    
    model_8=drm(Height~DAP, data = tmpSData, fct = AR.3(), weights = Weight)
    #model_8=drm(Height~DAP, data = tmpSData, fct = AR.3())
    #plot(model_8,main="Asymptotic regression model")
    
    model_9=drm(Height~DAP, data = tmpSData, fct = L.5(), weights = Weight)
    #model_9=drm(Height~DAP, data = tmpSData, fct = L.5())
    #plot(model_9,main="Logistic model")
    
    model_10=drm(Height~DAP, data = tmpSData, fct = G.4(), weights = Weight)
    #model_10=drm(Height~DAP, data = tmpSData, fct = G.4())
    #plot(model_10,main="Gompertz dose-response model")
    
    d = AIC(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,model_9,model_10)
    index = which(d$AIC==min(d$AIC))
    d$Status = TRUE
    d$Status[index] = FALSE
    
    # Create continuous DAC values
    x <- seq(sTime, eTime, length.out = total_obs)
    DAP <- seq(sTime, eTime, length.out = total_obs)
    
    # Compute Days of year value
    # Days of year for dap=1
    mi = which(tmpSData$DAP==min(tmpSData$DAP))
    if(length(mi)>1) mi = mi[1]
    sDoy = tmpSData$DaysOfYear[mi]-tmpSData$DAP[mi]+1
    DOY = DAP+sDoy-1
    
    rm(tmpSData)
    
    PrevIndx=index
    
    for(ix in c(2:10)){
      par = NULL
      
      if(index==1){
        par = model_1$fit$par
      }else if(index==2){
        par = model_2$fit$par
      }else if(index==3){
        par = model_3$fit$par
      }else if(index==4){
        par = model_4$fit$par
      }else if(index==5){
        par = model_5$fit$par
      }else if(index==6){
        par = model_6$fit$par
      }else if(index==7){
        par = model_7$fit$par
      }else if(index==8){
        par = model_8$fit$par
      }else if(index==9){
        par = model_9$fit$par
      }else if(index==10){
        par = model_10$fit$par
      }
      
      grData = model_fit(x,DAP,par,index)
      
      if((length(which(is.na(grData$GrowthRate)==TRUE))>0 | length(which(grData$Height<=0))>0) & length(which(d$Status==TRUE))>0){
        index = which(d$AIC==min(d$AIC[which(d$Status==TRUE)]))
        d$Status[index] = FALSE
      }else{
        break()
      }
      
    }
    
    rm(d)
    
    if((length(which(is.na(grData$GrowthRate)==TRUE))>0 | length(which(grData$Height<0))>0)==FALSE){
      grData$Genotype = as.character(tmp$Genotype[i])
      grData$Irrigated = tmp$Irrigated[i]
      #grData$Rep = tmp$Rep[i]
      grData$DaysOfYear = DOY
      grData$Date = DateByDaysOfYear(2017,doy = DOY)
      grData$Model = mName[index]
      grData$pi = PrevIndx
      grData$si = index
      
      if(is.null(heightData)){
        heightData = data.frame(grData)
      }else{
        heightData = as.data.frame(rbind(heightData, grData))
      }
    }
    
    rm(grData)
  }
}

rm(tmp, phData)

# Lifted the growth rate to 100%
heightData$GrowthRate = heightData$GrowthRate*100

#summary(heightData)

# Find genotype that shows anomaly growth rate
l = unique(heightData$Genotype[which(heightData$GrowthRate>10)])
if(length(l)>0){
  k=c()
  for(i in l){
    k = append(k, which(heightData$Genotype==as.character(i)))
  }
  heightData = as.data.frame(heightData[-k,])
}

################################ Plotting height and growthrate for [genotype, rep]
tmp = unique(heightData$Genotype)
p = 0
pp = 2
pdf(file=paste(gr_img_path,p,"_4.pdf",sep=""))
par(mfrow=c(pp,1))

for(i in tmp){
  tmpSData = heightData%>%filter(Genotype==as.character(i)) # & Rep==tmp$Rep[i])
  if(nrow(tmpSData)>0){
    tmpSData.dry = tmpSData%>%filter(Irrigated==FALSE)
    tmpSData.wet = tmpSData%>%filter(Irrigated==TRUE)
    
    if(nrow(tmpSData.dry)>0 & nrow(tmpSData.wet)>0){
      if(p%%pp==0){
        dev.off()
        
        pdf(file=paste(gr_img_path,p,"_4.pdf",sep=""))
        par(mfrow=c(pp,1))
      }
      
      matplot(tmpSData.dry$DAP, cbind(tmpSData.dry$Height, tmpSData.wet$Height), xlab = "Days After Planting (DAP)", ylab = "Height", main = paste("Height:",as.character(i),"-D:",as.character(tmpSData.dry$Model[1]),",I:",as.character(tmpSData.wet$Model[1]),sep = ""), pch = c(16,19), col = c("red","blue"))
      legend(x=0,y=max(max(tmpSData.dry$Height),max(tmpSData.wet$Height)),legend = c("Dry","Irrigated"), pch = c(16,19), col = c("red","blue"))
      matplot(tmpSData.dry$DAP, cbind(tmpSData.dry$GrowthRate, tmpSData.wet$GrowthRate), xlab = "Days After Planting (DAP)", ylab = "Growth Rate", main = paste("GrowthRate:",as.character(i),sep = ""), pch = c(16,19), col = c("red","blue"))
      legend(x=0,y=max(max(tmpSData.dry$GrowthRate),max(tmpSData.wet$GrowthRate)),legend = c("Dry","Irrigated"), pch = c(16,19), col = c("red","blue"))
      
      p = p +2
    }
    
    rm(tmpSData.dry,tmpSData.wet)
  }
  rm(tmpSData)
}
dev.off()

# Unique genotypes, environment, replicates
# Remove genotype and replica which growth rate 
tmp = as.data.frame(heightData[,c(4,5)]%>%distinct())
l=c()
for(i in 1:nrow(tmp)){
  tmpSData = as.data.frame(heightData%>%filter(Genotype==tmp$Genotype[i] & Irrigated==tmp$Irrigated[i]))# & Rep==tmp$Rep[i]))
  
  mdap = tmpSData$DAP[which(tmpSData$GrowthRate==max(tmpSData$GrowthRate))]
  
  if(mdap==max(tmpSData$DAP)){
    l = unique(append(l, which(heightData$Genotype==tmp$Genotype[i])))# & heightData$Rep==tmp$Rep[i])))
  }
  
  rm(tmpSData)
}

if(length(l)>0){
  heightData = as.data.frame(heightData[-l,])
}

rm(l)

################### Add weather data
#######################################
phData = merge(heightData, wData, by.x = "DaysOfYear", by.y = "DOY")
phData$Temperature = TempConvert(phData$Temperature.C, convertTo = "F")
phData$Temperature.min = TempConvert(phData$TemperatureMin.C, convertTo = "F")
phData$Temperature.max = TempConvert(phData$TemperatureMax.C, convertTo = "F")
phData = as.data.frame(phData%>%group_by(Genotype, Irrigated)%>%mutate(GDD = GDD(Temperature.max, Temperature.min, type = "F"),cGDD=cumsum(GDD)))

phData = as.data.frame(phData[,c(1:7,11:14,19,21:25)]%>%arrange(Genotype, Irrigated, DAP))

# Save data
write.csv(phData, paste(file_path,"IrrigationHeight.csv",sep = ""))

# Read data from file
phData = read.csv("/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Stephen/IrrigationHeight.csv")







