# Author: Methun Kamruzzaman
# Date: December 10th, 2018

# Clear all data
rm(list=ls())

# Load library
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}else{
  library(dplyr)
}

if(!require(ISLR)){
  install.packages("ISLR")
  library(ISLR)
}else{
  library(ISLR)
}

if(!require(pheatmap)){
  install.packages("pheatmap")
  library(pheatmap)
}else{
  library(pheatmap)
}

if(!require(lme4)){
  install.packages("lme4")
  library(lme4)
}else{
  library(lme4)
}

if(!require(drc)){
  install.packages("drc")
  library(drc)
}else{
  library(drc)
}

if(!require(scales)){
  install.packages("scales")
  library(scales)
}else{
  library(scales)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}else{
  library(RColorBrewer)
}

# Custom functions
#######  Function to compute GDD #######
ComputeGDD<-function(tmax, tmin, type="C"){
  fmax = tmax
  fmin = tmin
  gdd = rep(0, length(fmax))
  
  if(type=="C"){
    fmax = 32 + ((tmax*9)/5)
    fmin = 32 + ((tmin*9)/5)
  }
  
  for(i in 1:length(fmax)){
    tl = fmin[i]
    th = fmax[i]
    
    if(th<50 | tl<50){
      tl = 50
    }
    
    if(th>86){
      th=86
    }
    
    gdd[i] = ((th+tl)/2)-50
  }
  
  return (gdd)
}

####### Function to convert temperature #########
TempConvert=function(t, convertTo="C"){
  m=NULL
  
  if(convertTo=="C"){
    m = ((t-32)*5)/9
  }else{
    m = ((t*9)/5)+32
  }
  
  return(m)
}

########## Function to check leap year #############

DaysOfFeb<-function(Year){
  if(Year%%4==0 | (Year%%100==0 & Year%%400==0)) return(29)
  return(28)
}

######### Function to compute the day of a year based on date ##########

DayOfAYear<-function(Year, Month, Day){
  
  if(Day==1 & Month==1) return(1)
  
  M = c(1:12)
  DR = c(31,DaysOfFeb(Year),31,30,31,30,31,31,30,31,30,31)
  
  # Check the range
  if(Month<1 | Month>12) return(-1)
  if(Day<1 | Day>DR[Month]) return(-1)
  
  if(Month==1){
    return(Day)
  }
  
  return(sum(DR[1:Month-1]) + Day)
}

# Get date based on Days of year of a specific year
DateByDaysOfYear<-function(year, doy){
  M = c(1:12)
  DR = c(31,DaysOfFeb(year),31,30,31,30,31,31,30,31,30,31)
  
  l = c()
  
  for(k in 1:length(doy)){
    if(doy[k]<=DR[1]){
      l = append(l, paste(year,"/01/",ifelse(doy[k]<10, yes=paste("0",doy[k],sep = ""), no=paste("",doy[k],sep = "")),sep = ""))
    }else{
      for(i in M){
        if(sum(DR[1:i-1])<doy[k] & sum(DR[1:i])>=doy[k]){
          mon  = ifelse(i<10, yes=paste("0",i,sep = ""), no=paste("",i,sep = ""))
          d = doy[k]- sum(DR[1:i-1])
          da = ifelse(d<10, yes=paste("0",d,sep = ""), no=paste("",d,sep = ""))
          l = append(l, paste(year,"/",mon,"/",da,sep = ""))
        }
      }
    }
  }
  
  return(l)
}

############ Compute the DaysOfYear By Dates ##########

DaysOfYearByDate<-function(someDate){
  d = as.Date(someDate)
  if(length(d)==1) return(DayOfAYear(year(d), month(d), day(d)))
  
  l=c(DayOfAYear(year(d[1]), month(d[1]), day(d[1])))
  for(i in 2:length(d)){
    l = append(l, DayOfAYear(year(d[i]), month(d[i]), day(d[i])))
  }
  
  return(l)
}

##################### Model functions
Model_W12=function(x,b,e){
  return(exp(-exp(b*(log(x)-e))))
}

Model_W12_D=function(x,b,e){
  return(-(exp(-exp(b * (log(x) - e))) * (exp(b * (log(x) - e)) * (b * (1/x)))))
}

Model_W13=function(x,b,d,e){
  return(0+(d-0)*exp(-exp(b*(log(x)-e))))
}

Model_W13_D=function(x,b,d,e){
  return(-((d - 0) * (exp(-exp(b * (log(x) - e))) * (exp(b * (log(x) - e)) * (b * (1/x))))))
}

Model_W14=function(x,b,c,d,e){
  return(c+(d-c)*exp(-exp(b*(log(x)-log(e)))))
}

Model_W14_D=function(x,b,c,d,e){
  return(-((d - c) * (exp(-exp(b * (log(x) - log(e)))) * (exp(b * (log(x) - log(e))) * (b * (1/x))))))
}

Model_LL2=function(x,b,e){
  return(1/(1+exp(b*(log(x)-log(e)))))
}

Model_LL2_D=function(x,b,e){
  return(-(exp(b * (log(x) - log(e))) * (b * (1/x))/(1 + exp(b * (log(x) - log(e))))^2))
}

Model_LL4=function(x,b,c,d,e){
  return(c+((d-c)/(1+exp(b*(log(x)-log(e))))))
}

Model_LL4_D=function(x,b,c,d,e){
  return(-((d - c) * (exp(b * (log(x) - log(e))) * (b * (1/x)))/(1 + exp(b * (log(x) - log(e))))^2))
}

Model_LL5=function(x,b,c,d,e,f){
  return(c+((d-c)/(1+exp(b*(log(x)-log(e))))^f))
}

Model_LL5_D=function(x,b,c,d,e,f){
  return(-((d - c) * ((1 + exp(b * (log(x) - log(e))))^(f - 1) * (f * (exp(b * (log(x) - log(e))) * (b * (1/x)))))/((1 + exp(b * (log(x) - log(e))))^f)^2))
}

Model_LL5_1=function(x,b,c,d,e,f){
  return(c+((d-c)/(1+exp(b*(log(x)-e)))^f))
}

Model_LL5_D_1=function(x,b,c,d,e,f){
  return(-((d - c) * ((1 + exp(b * (log(x) - e)))^(f - 1) * (f * (exp(b * (log(x) - e)) * (b * (1/x)))))/((1 + exp(b * (log(x) - e)))^f)^2))
}

Model_MM3=function(x,c,d,e){
  return(c+((d-c)/(1+(e/x))))
}

Model_MM3_D=function(x,c,d,e){
  return((d - c) * (e/x^2)/(1 + (e/x))^2)
}

Model_AR3=function(x,c,d,e){
  return(c+(d-c)*(1-exp(-x/e)))
}

Model_AR3_D=function(x,c,d,e){
  return((d - c) * (exp(-x/e) * (1/e)))
}

Model_L5 = function(x,b,c,d,e,f){
  return(c + ((d-c)/((1+exp(b*(x-e)))^f)))
}

# Derivative of Logistic Model
Model_L5_D = function(x,b,c,d,e,f){
  return((d - c) * (f * (exp(b * (x - e)) * -1* b * (1 + exp(b * (x - e)))^(f-1)))/((1 + exp(b * (x - e)))^f)^2)
}

# Gompertz Model
Model_G4<-function(x,b,c,d,e){
  return(c + (d-c)*(exp(-exp(b*(x-e)))))
}

# Derivative of Gompertz Model
Model_G4_D<-function(x,b,c,d,e){
  return(-((d - c) * (exp(-exp(b * (x - e))) * (exp(b * (x - e)) * b))))
}

#########################################################################
model_fit = function(x, y, par, index){
  grData = NULL
  
  if(index==1){
    grData <- as.data.frame(cbind(y, Model_W12(x,par[1],par[2]), Model_W12_D(x,par[1],par[2]) ))
  }else if(index==2){
    grData <- as.data.frame(cbind(y, Model_W13(x,par[1],par[2],par[3]), Model_W13_D(x,par[1],par[2],par[3]) ))
  }else if(index==3){
    grData <- as.data.frame(cbind(y, Model_W14(x,par[1],par[2],par[3],par[4]), Model_W14_D(x,par[1],par[2],par[3],par[4]) ))
  }else if(index==4){
    grData <- as.data.frame(cbind(y, Model_LL2(x,par[1],par[2]), Model_LL2_D(x,par[1],par[2]) ))
  }else if(index==5){
    grData <- as.data.frame(cbind(y, Model_LL4(x,par[1],par[2],par[3],par[4]), Model_LL4_D(x,par[1],par[2],par[3],par[4]) ))
  }else if(index==6){
    grData <- as.data.frame(cbind(y, Model_LL5(x,par[1],par[2],par[3],par[4],par[5]), Model_LL5_D(x,par[1],par[2],par[3],par[4], par[5]) ))
  }else if(index==7){
    grData <- as.data.frame(cbind(y, Model_MM3(x,par[1],par[2],par[3]), Model_MM3_D(x,par[1],par[2],par[3]) ))
  }else if(index==8){
    grData <- as.data.frame(cbind(y, Model_AR3(x,par[1],par[2],par[3]), Model_AR3_D(x,par[1],par[2],par[3]) ))
  }else if(index==9){
    grData <- as.data.frame(cbind(y, Model_L5(x,par[1],par[2],par[3],par[4],par[5]), Model_L5_D(x,par[1],par[2],par[3],par[4],par[5]) ))
  }else if(index==10){
    grData <- as.data.frame(cbind(y, Model_G4(x,par[1],par[2],par[3],par[4]), Model_G4_D(x,par[1],par[2],par[3],par[4]) ))
  }
  
  if(is.null(grData)==FALSE){
    colnames(grData) <- c("DAP", "Height", "GrowthRate")
  }
  
  return(grData)
}
###########################################################