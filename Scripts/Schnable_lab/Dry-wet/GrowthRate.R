
# Author: Methun Kamruzzaman
# Date: December 10th, 2018

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
  
  # tmp$DAP=0
  # tmp$Height=0
  # 
  # for(i in 1:nrow(tmp)){
  #   tmpData = data.frame(phData%>%filter(Genotype==as.character(tmp$Genotype[i]) & Irrigated==tmp$Irrigated[i] & Rep==tmp$Rep[i]))
  #   tmp$DAP[which(tmp$Genotype==tmp$Genotype[i] & tmp$Rep==tmp$Rep[i] & tmp$Irrigated==tmp$Irrigated[i])]=min(tmpData$DAP)
  #   tmp$Height[which(tmp$Genotype==tmp$Genotype[i] & tmp$Rep==tmp$Rep[i] & tmp$Irrigated==tmp$Irrigated[i])]=tmpData$Height[which(tmpData$DAP==min(tmpData$DAP))]
  # }
  # tmp$HeightCM=tmp$Height*100
  
  # To avoid negative height, assume the height of the plant at DAP=1 is 0.0254m=1in;
  if(adjust==TRUE){
    for(i in 1:nrow(tmp)){
      tmpData = data.frame(phData%>%filter(Genotype==as.character(tmp$Genotype[i]) & Irrigated==tmp$Irrigated[i])) # & Rep==tmp$Rep[i]))
      
      if(nrow(tmpData)>0){
        tmp1Data = as.data.frame(tmpData[which(tmpData$DAP==min(tmpData$DAP)),])
        tmp1Data$DAP=0
        tmp1Data$Height=0.0
        tmp1Data$DaysOfYear=sDayOfYear-1
        tmp1Data$Weight=1.0
        for(al in 1:6){
          tmp1Data$DAP=tmp1Data$DAP+1
          tmp1Data$DaysOfYear=tmp1Data$DaysOfYear+1
          phData = as.data.frame(rbind(phData, tmp1Data))
        }
        
        # hm = 0.01
        # for(al in 1:9){
        #   tmp1Data$DAP=tmp1Data$DAP+1
        #   tmp1Data$DaysOfYear=tmp1Data$DaysOfYear+1
        #   tmp1Data$Height=hm
        #   phData = as.data.frame(rbind(phData, tmp1Data))
        #   hm = hm+.001
        # }
        
        rm(tmp1Data)
        
        tmp1Data = as.data.frame(tmpData[which(tmpData$DAP==max(tmpData$DAP)),])
        #mdap = 75-max(tmpData$DAP)
        ht = tmpData%>%filter(DAP>=max(tmpData$DAP)-3)%>%summarise(mh=max(Height))
        hm = ht$mh
        tmp1Data$Weight=1.0
        rm(ht)
        
        for(dmi in 1:10){
          tmp1Data$DAP=tmp1Data$DAP+1
          tmp1Data$DaysOfYear=tmp1Data$DaysOfYear+1
          tmp1Data$Height = hm
          
          phData = as.data.frame(rbind(phData, tmp1Data))
          #hm=hm*1.0001
        }
        
        rm(tmp1Data)
      }
      rm(tmpData)
    }
  }
  
  # ######## Plot the height data
  # # Plotting [genotype, Env, Rep]
  # GenList = unique(phData$Genotype)
  # IrrList = unique(phData$Irrigated)
  # RepList = unique(phData$Rep)
  # 
  # p = 0
  # pp = 2
  # pdf(file=paste(h_img_path,p,"_4.pdf",sep=""))
  # par(mfrow=c(pp,1))
  # 
  # for(i in GenList){
  #   for(k in RepList){
  #     tmpSData = phData%>%filter(Genotype==as.character(i)&Rep==k)
  #     if(nrow(tmpSData)>0){
  #       tmpSData.dry = tmpSData%>%filter(Irrigated==FALSE)
  #       tmpSData.wet = tmpSData%>%filter(Irrigated==TRUE)
  #       
  #       if(nrow(tmpSData.dry)>0 & nrow(tmpSData.wet)>0){
  #         if(p%%pp==0){
  #           dev.off()
  #           
  #           pdf(file=paste(h_img_path,p,"_4.pdf",sep=""))
  #           par(mfrow=c(pp,1))
  #         }
  #         
  #         plot(tmpSData.dry$DAP, tmpSData.dry$Height, col="red", pch=16, xlab = "Days After Planting (DAP)", ylab = "Height", main = paste("No Irrigated:",as.character(i),"--",k,sep = ""))
  #         plot(tmpSData.wet$DAP, tmpSData.wet$Height, col="blue", pch=16, xlab = "Days After Planting (DAP)", ylab = "Height", main = paste("Irrigated:",as.character(i),"--",k,sep = ""))
  #         
  #         p = p +2
  #       }
  #       
  #       rm(tmpSData.dry,tmpSData.wet)
  #     }
  #     rm(tmpSData)
  #   }
  # }
  # dev.off()
  
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
  
  ############################# Compute growth rate
  
  # Fit data based on genotype and env
  mName=c("W12","W13","W14","LL2","LL4","LL5","MM3","AR3","L5","G4")
  heightData = NULL
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
  
  # ################################ Plotting height and growthrate for [genotype, rep]
  # tmp = as.data.frame(heightData[,c(4,6)]%>%distinct())
  # p = 0
  # pp = 2
  # pdf(file=paste(gr_img_path,p,"_4.pdf",sep=""))
  # par(mfrow=c(pp,1))
  # 
  # for(i in 1:nrow(tmp)){
  #   tmpSData = heightData%>%filter(Genotype==tmp$Genotype[i] & Rep==tmp$Rep[i])
  #   if(nrow(tmpSData)>0){
  #     tmpSData.dry = tmpSData%>%filter(Irrigated==FALSE)
  #     tmpSData.wet = tmpSData%>%filter(Irrigated==TRUE)
  #     
  #     if(nrow(tmpSData.dry)>0 & nrow(tmpSData.wet)>0){
  #       if(p%%pp==0){
  #         dev.off()
  #         
  #         pdf(file=paste(gr_img_path,p,"_4.pdf",sep=""))
  #         par(mfrow=c(pp,1))
  #       }
  #       
  #       matplot(tmpSData.dry$DAP, cbind(tmpSData.dry$Height, tmpSData.wet$Height), xlab = "Days After Planting (DAP)", ylab = "Height", main = paste("Height:",as.character(tmp$Genotype[i]),"--",tmp$Rep[i],"-D:",as.character(tmpSData.dry$Model[1]),",I:",as.character(tmpSData.wet$Model[1]),sep = ""), pch = c(16,19), col = c("red","blue"))
  #       legend(x=0,y=max(max(tmpSData.dry$Height),max(tmpSData.wet$Height)),legend = c("Dry","Irrigated"), pch = c(16,19), col = c("red","blue"))
  #       matplot(tmpSData.dry$DAP, cbind(tmpSData.dry$GrowthRate, tmpSData.wet$GrowthRate), xlab = "Days After Planting (DAP)", ylab = "Growth Rate", main = paste("GrowthRate:",as.character(tmp$Genotype[i]),"--",tmp$Rep[i],sep = ""), pch = c(16,19), col = c("red","blue"))
  #       legend(x=0,y=max(max(tmpSData.dry$GrowthRate),max(tmpSData.wet$GrowthRate)),legend = c("Dry","Irrigated"), pch = c(16,19), col = c("red","blue"))
  #       
  #       p = p +2
  #     }
  #     
  #     rm(tmpSData.dry,tmpSData.wet)
  #   }
  #   rm(tmpSData)
  # }
  # dev.off()
  
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
  
  # Compute growth rate difference (wet-dry) for [genotype,rep,dap]
  tmpHD = as.data.frame(heightData%>%group_by(DAP,Genotype,DaysOfYear, Date)%>%summarise(HeightDiff=diff(Height), GRDiff=diff(GrowthRate)))
  
  # irrHD = as.data.frame(heightData%>%filter(Irrigated==TRUE))
  # nirrHD = as.data.frame(heightData%>%filter(Irrigated==FALSE))
  # tmp = as.data.frame(heightData[,c(1,4)]%>%distinct())
  # 
  # for(i in 1:nrow(tmp)){
  #   twet = as.data.frame(irrHD%>%filter(Genotype==tmp$Genotype[i] & DAP==tmp$DAP[i]))# & Rep==tmp$Rep[i]))
  #   tdry = as.data.frame(nirrHD%>%filter(Genotype==tmp$Genotype[i] & DAP==tmp$DAP[i]))# & Rep==tmp$Rep[i]))
  #   
  #   tt = as.data.frame(twet[,c(1,4,6,7)])
  #   tt$HeightDiff = twet$Height-tdry$Height
  #   tt$GRDiff = twet$GrowthRate-tdry$GrowthRate
  #   
  #   if(is.null(tmpHD)){
  #     tmpHD = as.data.frame(tt)
  #   }else{
  #     tmpHD = as.data.frame(rbind(tmpHD, tt))
  #   }
  #   
  #   rm(twet,tdry,tt) 
  # }
  # rm(irrHD,nirrHD,tmp)
  
  # plot(tmpHD$DAP, tmpHD$GRDiff, col=5)
  
  # Save data
  #write.csv(heightData, paste(file_path,"PlantHeight_GR.csv", sep="")
  
  # Adding weather data
  #tmp = as.data.frame(tmpHD[,c(2,3)]%>%distinct())
  tmp = unique(tmpHD$Genotype)
  phData = NULL
  for(i in tmp){
    tmpData = as.data.frame(tmpHD%>%filter(Genotype==as.character(i)))# & Rep == tmp$Rep[i]))
    
    if(nrow(tmpData)>0){
      rDOY = range(tmpData$DaysOfYear)
      tmpwData = as.data.frame(wData%>%filter(DOY>=rDOY[1] & DOY<=rDOY[2]))
      
      tmphData = as.data.frame(cbind(tmpData, as.data.frame(tmpwData[,c(5,7,9,10,12,13)])))
      
      # Convert temperature from C to F
      tmphData$Temperature = TempConvert(tmphData$Temperature.C, convertTo = "F")
      tmphData$Temperature.min = TempConvert(tmphData$TemperatureMin.C, convertTo = "F")
      tmphData$Temperature.max = TempConvert(tmphData$TemperatureMax.C, convertTo = "F")
      tmphData$GDD = GDD(tmphData$Temperature.max, tmphData$Temperature.min, type = "F")
      tmphData$CGDD = cumsum(tmphData$GDD)
      
      if(is.null(phData)){
        phData = as.data.frame(tmphData)
      }else{
        phData = as.data.frame(rbind(phData, tmphData))
      }
      
      rm(tmpwData,tmphData)
    }
    rm(tmpData)
  }
  rm(tmp, tmpHD)
  
  # Save data
  write.csv(phData, paste(file_path,"gxeHeight.csv",sep = ""))
  
  # Filter columns
  # gxeData = as.data.frame(phData[,c(1:9,13,14,17,18)])
  # colnames(gxeData) <- c("DAP","Genotype","Rep","DaysOfYear","Date","Height-Diff","GrowthRate-Diff","Precipitation","Humidity","SolarRadiation","Temperature","GDD","CGDD")
  # 
  
  # gxeData = as.data.frame(phData[,c(1,4,6,7,2,3,11,12,16,17,20,21)])
  # colnames(gxeData) <- c("DAP","Genotype","DaysOfYear","Date","Height","GrowthRate","Precipitation","Humidity","SolarRadiation","Temperature","GDD","CGDD")
  
  
  gxeData = as.data.frame(phData[,c(1:8,12,13,16,17)])
  colnames(gxeData) <- c("DAP","Genotype","DaysOfYear","Date","Height-Diff","GrowthRate-Diff","Precipitation","Humidity","SolarRadiation","Temperature","GDD","CGDD")
  
  # Filter columns
  #gxeData = as.data.frame(phData[,c(1:11,15,16,19,20)])
  #colnames(gxeData) <- c("DAP","Height","GrowthRate","Genotype","Irrigated","Rep","DaysOfYear","Date","Model","Precipitation","Humidity","SolarRadiation","Temperature","GDD","CGDD")
  
  # Free memory
  rm(heightData,phData)
  
  # Remove genotype ICI 441 x LH123HT as it shows anomaly result
  #gxeData = as.data.frame(gxeData[-which(gxeData$Genotype=="ICI 441 x LH123HT"),])
  
  # Remove genotype, rep which shows highest growthrate within 10 dap
  #rx = which(gxeData$GrowthRate>7 & gxeData$DAP<11)
  #gx = as.data.frame(gxeData[rx,c(4,5,6)]%>%distinct)
  #for(i in 1:nrow(gx)){
  #  tx = which(gxeData$Genotype==gx$Genotype[i] & gxeData$Irrigated==gx$Irrigated[i] & gxeData$Rep==gx$Rep[i])
  #  if(length(tx)>0){
  #    gxeData = as.data.frame(gxeData[-tx,])
  #  }
  #  rm(tx)
  #}
  #rm(rx, gx)
  
  # Add location information
  gxeData$State="NE"
  gxeData$Country="USA"
  #gxeData$County=ifelse(gxeData$Irrigated==TRUE, yes="Grant", no="Perkins")
  
  # Split parents
  gxeData$Female = ""
  gxeData$Male = ""
  
  GenList = unique(gxeData$Genotype)
  for(i in GenList){
    a = unlist(strsplit(as.character(i), " x "))
    k = which(gxeData$Genotype==as.character(i))
    gxeData$Female[k] = a[1]
    gxeData$Male[k] = a[2]
  }
  
  # Rearrange columns and save data
  gxeData = as.data.frame(gxeData[,c(1,2,15,16,3,4,13,14,5:12)])
  
  # Get color for [gen,rep]
  d =unique(gxeData$Genotype) #as.data.frame(gxeData[,c(2,5)]%>%distinct())
  n = length(d)
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  col = sample(color, n)
  
  # Assign color
  #  & gxeData$Rep==d$Rep[i]
  gxeData$col = ""
  for(i in 1:length(d)){
    gxeData$col[which(gxeData$Genotype==as.character(d[i]))] = col[i]
  }
  rm(d)
  
  plot(gxeData$DAP, gxeData$`GrowthRate-Diff`, col=gxeData$col)
  
  ## PLot all curves
  ## Under construction
  tmp = as.data.frame(gxeData[,c(2,5)]%>%distinct())
  tmpCurve = NULL
  col=c()
  leg=c()
  mgr=0
  for(i in 1:nrow(tmp)){
    tmpSData = as.data.frame(gxeData%>%filter(Genotype==tmp$Genotype[i] & Rep==tmp$Rep[i]))
    tmpSC = as.data.frame(tmpSData$`GrowthRate-Diff`)
    colnames(tmpSC)<-c(paste(tmp$Genotype[i],",",tmp$Rep[i],sep=""))
    col = unique(append(col, tmpSData$col))
    leg = unique(append(leg, as.character(paste(tmp$Genotype[i],"_",tmp$Rep[i],sep=""))))
    mgr = max(mgr, max(tmpSData$`GrowthRate-Diff`))
    
    if(is.null(tmpCurve)){
      tmpCurve = as.data.frame(tmpSData$DAP)%>%rename(DAP="tmpSData$DAP")
      tmpCurve = as.data.frame(cbind(tmpCurve, tmpSC))
    }else{
      
      tmpCurve = as.data.frame(cbind(tmpCurve, tmpSC))
    }
    
    rm(tmpSData, tmpSC)
  }
  rm(tmp)
  
  matplot(tmpCurve$DAP,as.data.frame(tmpCurve[,c(2:92)]), type=c("b"), xlab = "Days After Planting (DAP)", ylab = "Growth rate", main = "", pch = 1, col = col)
  legend(x=0,y=mgr,legend = leg, pch = 1, col = col)
  
  matplot()
  ###########################
  
  
  # d = as.data.frame(gxeData[which(gxeData$Genotype=="ICI 441 x LH123HT" & gxeData$Rep==2),])
  # plot(d$DAP, d$GrowthRate.Diff,col=d$col)
  # rm(d)
  
  # Save data
  # write.csv(gxeData, paste(file_path,"gxePlantHeight.csv",sep = ""))
  write.csv(gxeData, paste(file_path,"Irrigation.csv",sep = ""))
  
  rm(gxeData)
  
  #########################################################################

# Consider point=[genotype,location,dap]
#Compute_GR_Using_Gen(sData, wData)

######################
# Load from drive: gxePlantHeight.csv
gxeData = data.frame(read.csv(file.choose()))

## ## Plot DAP vs Growth rate
plot(gxeData$DAP, gxeData$GrowthRate.Diff, col=gxeData$col)


# number of individuals [genotype, rep]
nrow(as.data.frame(gxeData[,c(3,6)])%>%distinct)

# number of genotypes
length(unique(gxeData$Genotype))

# Compute correlation coefficient
#phCor = cor(gxeData[,c(2:4,14:17,19)])
phCor = cor(gxeData[,c(2,11:17)])
pheatmap(phCor)

################## create dap range for selected genotype ###########
tmp = read.csv(file.choose())
td = as.data.frame(tmp%>%group_by(Genotype)%>%summarise(startDAP=min(DAP), endDAP=max(DAP), startDaysOfYear=min(DaysOfYear),endDaysOfYear=max(DaysOfYear), Daydiff=endDAP-startDAP))
rm(tmp)
#td$DayDiff = td$maxDAP-td$minDAP
tdf = as.data.frame(td%>%filter(endDAP>55 & Daydiff>2))
rm(td)
tdf$startDate = ""
tdf$endDate = ""
for(i in 1:nrow(tdf)){
  tdf$startDate[i] = DateByDaysOfYear(2017, tdf$startDaysOfYear[i])
  tdf$endDate[i] = DateByDaysOfYear(2017, tdf$endDaysOfYear[i])
}

write.csv(tdf, paste(file_path,"rootworm_genotypes.csv",sep = ""))
rm(tdf)

#########################################################
########################## Finish here ##################
#########################################################



## Plot DAP vs Growth rate
plot(gxeData$DAP, gxeData$GrowthRate, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

# Compute growth rate diff for [gen,rep]: Irrigated(gr)-nonIrrigated(gr)
tmpGR = NULL


## Cluster point density
## Filter DAP
dap = seq(min(gxeData$DAP), max(gxeData$DAP), diff(range(gxeData$DAP))/30)
l = c()
for(i in 2:length(dap)){
  gr = gxeData$GrowthRate[which(gxeData$DAP>=dap[i-1] & gxeData$DAP<=dap[i])]
  if(length(gr)>0){
    l = append(l, sd(gr))
  }
}
l
mean(l)

## Filter CGDD
plot(gxeData$CGDD, gxeData$GrowthRate, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

cgdd = seq(min(gxeData$CGDD), max(gxeData$CGDD), diff(range(gxeData$CGDD))/50)
l = c()
for(i in 2:length(cgdd)){
  gr = gxeData$GrowthRate[which(gxeData$CGDD>=cgdd[i-1] & gxeData$CGDD<=cgdd[i])]
  if(length(gr)>0){
    l = append(l, sd(gr))
  }
}
l
mean(l)

## Filter Humidity
# Can't splid data based on Irrigated using humidity

plot(gxeData$Humidity, gxeData$GrowthRate, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

hum = seq(min(gxeData$Humidity), max(gxeData$Humidity), diff(range(gxeData$Humidity))/40)
l = c()
for(i in 2:length(hum)){
  gr = gxeData$GrowthRate[which(gxeData$Humidity>=hum[i-1] & gxeData$Humidity<=hum[i])]
  if(length(gr)>0){
    l = append(l, sd(gr))
  }
}
l
mean(l)

## Filter Precepitation

plot(gxeData$Precipitation, gxeData$GrowthRate, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

pr = seq(min(gxeData$Precipitation), max(gxeData$Precipitation), diff(range(gxeData$Precipitation))/30)
l = c()
for(i in 2:length(pr)){
  gr = gxeData$GrowthRate[which(gxeData$Precipitation>=pr[i-1] & gxeData$Precipitation<=pr[i])]
  if(length(gr)>0){
    l = append(l, sd(gr))
  }
}
l
mean(l)

## Filter Solar radiation

plot(gxeData$SolarRadiation, gxeData$GrowthRate, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

sr = seq(min(gxeData$SolarRadiation), max(gxeData$SolarRadiation), diff(range(gxeData$SolarRadiation))/20)
l = c()
for(i in 2:length(pr)){
  gr = gxeData$GrowthRate[which(gxeData$SolarRadiation>=sr[i-1] & gxeData$SolarRadiation<=sr[i])]
  if(length(gr)>0){
    l = append(l, sd(gr))
  }
}
l
mean(l)

## Filter DAP and humidity
plot(gxeData$DAP, gxeData$Humidity, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

dap = seq(min(gxeData$DAP), max(gxeData$DAP), diff(range(gxeData$DAP))/30)
hum = seq(min(gxeData$Humidity), max(gxeData$Humidity), diff(range(gxeData$Humidity))/30)
l = c()
for(i in 2:length(dap)){
  for(j in 2:length(hum)){
    m = which(gxeData$Humidity>=hum[j-1] & gxeData$Humidity<=hum[j] & gxeData$DAP>=dap[i-1] & gxeData$DAP<=dap[i])
    if(length(m)>0){
      gr = gxeData$GrowthRate[m]
      if(length(gr)>0){
        l = append(l, sd(gr))
      }
    }
  }
}
l
length(l)
mean(l)

# 43, 47, 51, 54, 50, 53, 56, 58, 60, 65, 67, 69, 71, 73, 75
# get the selected genotypes
gd = read.csv(file.choose())

# extract unique genotypes
glist = as.data.frame(gd%>%distinct())

# Crreate dataset for these selected genotypes only
sGxeData = NULL
for(i in 1:nrow(glist)){
  tmp = as.data.frame(gxeData[which(gxeData$Genotype==as.character(glist$Genotype[i]) & gxeData$Rep==glist$Rep[i]),])
  
  if(nrow(tmp)>0){
    if(is.null(sGxeData)){
      sGxeData = as.data.frame(tmp)
    }else{
      sGxeData = as.data.frame(rbind(sGxeData,tmp))
    }
  }
  
  rm(tmp)
}

# plot data
plot(sGxeData$DAP, sGxeData$GrowthRate, col=ifelse(gxeData$Irrigated==TRUE, yes="blue", no="red"))

# Save data
write.csv(as.data.frame(as.data.frame(sGxeData[,-c(1)])), "/Users/methun/self/Research/Dataset/pet(Schnable\ lab)dataset/Stephen/gxePlantHeight_gen_rep.csv")

l = c(1785,1786,1787,1788,1789,1790,1791,1792,1793,1794,1795,1796,1797,1798,1799,1800)
g=c()
j=1
for(i in 1:nrow(gxeData)){
  if(gxeData$X[i]==l[j]){
    g = append(g, as.character(gxeData$Genotype[i]))
    j = j + 1;
  }
}

tmp = as.data.frame(gxeData[which(gxeData$DAP>=36 & gxeData$DAP<=50),])
