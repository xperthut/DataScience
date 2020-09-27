

# Removing all environmental variables
rm(list=ls())
Sys.setenv(TZ="GMT")

# Install packages
if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}else{
  library("ggplot2")
}

if(!require("formattable")){
  install.packages("formattable")
  library("formattable")
}else{
  library("formattable")
}

if(!require("drc")){
  install.packages("drc")
  library("drc")
}else{
  library("drc")
}

if(!require("pheatmap")){
  install.packages("pheatmap")
  library("pheatmap")
}else{
  library("pheatmap")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}else{
  library("dplyr")
}

if(!require("hashmap")){
  install.packages("hashmap")
  library("hashmap")
}else{
  library("hashmap")
}

if(!require("tidyverse")){
  install.packages("tidyverse")
  library("tidyverse")
}else{
  library("tidyverse")
}

if(!require("vistime")){
  install.packages("vistime")
  library("vistime")
}else{
  library("vistime")
}

if(!require("BBmisc")){
  install.packages("BBmisc")
  library("BBmisc")
}else{
  library("BBmisc")
}

if(!require("RColorBrewer")){
  install.packages("RColorBrewer")
  library("RColorBrewer")
}else{
  library("RColorBrewer")
}

if(!require("stringr")){
  install.packages("stringr")
  library("stringr")
}else{
  library("stringr")
}

if(!require("rgl")){
  install.packages("rgl")
  library("rgl")
}else{
  library("rgl")
}

if(!require("car")){
  install.packages("car")
  library("car")
}else{
  library("car")
}

if(!require("plot3D")){
  install.packages("plot3D")
  library("plot3D")
}else{
  library("plot3D")
}

if(!require("scatterplot3d")){
  install.packages("scatterplot3d")
  library("scatterplot3d")
}else{
  library("scatterplot3d")
}

if(!require("colorRamps")){
  install.packages("colorRamps")
  library("colorRamps")
}else{
  library("colorRamps")
}

if(!require("lme4")){
  install.packages("lme4")
  library("lme4")
}else{
  library("lme4")
}

if(!require("ISLR")){
  install.packages("ISLR")
  library("ISLR")
}else{
  library("ISLR")
}

if(!require("rpart")){
  install.packages("rpart")
  library("rpart")
}else{
  library("rpart")
}

if(!require("randomForest")){
  install.packages("randomForest")
  library("randomForest")
}else{
  library("randomForest")
}

if(!require("randomcoloR")){
  install.packages("randomcoloR")
  library("randomcoloR")
}else{
  library("randomcoloR")
}

if(!require("forecast")){
  install.packages("forecast")
  library("forecast")
}else{
  library("forecast")
}

if(!require("trend")){
  install.packages("trend")
  library("trend")
}else{
  library("trend")
}

if(!require("tseries")){
  install.packages("tseries")
  library("tseries")
}else{
  library("tseries")
}

if(!require("ggmosaic")){
  install.packages("ggmosaic")
  library("ggmosaic")
}else{
  library("ggmosaic")
}

if(!require("treemapify")){
  install.packages("treemapify")
  library("treemapify")
}else{
  library("treemapify")
}

if(!require("treemap")){
  install.packages("treemap")
  library("treemap")
}else{
  library("treemap")
}

if(!require("lattice")){
  install.packages("lattice")
  library("lattice")
}else{
  library("lattice")
}

if(!require("ggrepel")){
  install.packages("ggrepel")
  library("ggrepel")
}else{
  library("ggrepel")
}

if(!require("combinat")){
  install.packages("combinat")
  library("combinat")
}else{
  library("combinat")
}

if(!require("gplots")){
  install.packages("gplots")
  library("gplots")
}else{
  library("gplots")
}

if(!require("astsa")){
  install.packages("astsa")
  library("astsa")
}else{
  library("astsa")
}

# if(!require("igraph")){
#   install.packages("igraph")
#   library("igraph")
# }else{
#   library("igraph")
# }

options(digits=10)
options(scipen = 999)    

### Functions definition
getColors<-function(n, cpal=c("#FF0000", "#777700", "#00FF00", "#007777", "#0000FF")){
  #colorRampPalette(c("red", "green", "blue"))(n)
  colorRampPalette(cpal)(n)
}

convertDatetoString<-function(d){
  xlt <- as.POSIXlt(as.character(d), "%m/%d/%Y")
  t = paste(xlt$year+1900,
            ifelse(xlt$mon<10, yes=(paste("0",xlt$mon,sep="")), no=(xlt$mon)),
            ifelse(xlt$mday<10, yes=(paste("0",xlt$mday,sep="")), no=(xlt$mday)),
            ifelse(xlt$hour<10, yes=(paste("0",xlt$hour,sep="")), no=(xlt$hour)),
            ifelse(xlt$min<10, yes=(paste("0",xlt$min,sep="")), no=(xlt$min)),
            ifelse(xlt$sec<10, yes=(paste("0",xlt$sec,sep="")), no=(xlt$sec)),
            sep="")
  
  t
}

convertToDateString<-function(d, frmt="%m/%d/%Y"){
  l=c()
  for(t in d){
    ifelse(length(l)==0, yes = (l=convertDatetoDateString(t, frmt)), no = (l=append(l, convertDatetoDateString(t, frmt))))
  }
  l
}

convertDatetoDateString<-function(d, frmt="%m/%d/%Y"){
  xlt <- as.POSIXlt(as.character(d), format = frmt)
  t = paste(xlt$year+1900,
            ifelse((xlt$mon+1)<10, yes=(paste("0",(xlt$mon+1),sep="")), no=(xlt$mon+1)),
            ifelse(xlt$mday<10, yes=(paste("0",xlt$mday,sep="")), no=(xlt$mday)),
            sep="")
  
  t
}

convertDatetoDate<-function(d){
  xlt <- as.POSIXlt(as.character(d))
  t = paste(xlt$year+1900,
            ifelse(xlt$mon<10, yes=(paste("0",xlt$mon,sep="")), no=(xlt$mon)),
            ifelse(xlt$mday<10, yes=(paste("0",xlt$mday,sep="")), no=(xlt$mday)),
            sep="")
  
  t
}

replaceStringUsingPattern<-function(d="", pattern="\\.", replace=""){
  gsub(pattern, replace, d)
}

getDays<-function(d1, d2, fmt="%Y-%m-%d"){
  da <- as.Date(as.character(d2), format=fmt)-
    as.Date(as.character(d1), format=fmt)
  
  da=da+1
  da
}

convertDatetoSec<-function(d){
  xlt <- as.POSIXlt(as.character(d))
  t = (xlt$hour*3600) + (xlt$min*60) + xlt$sec
  
  t
}

convertDatetoMin<-function(d){
  xlt <- as.POSIXlt(as.character(d))
  t = (xlt$hour*60) + xlt$min
  
  t
}


getTimeVariable<-function(d, t="year", format="%m/%d/%Y"){
  xlt <- as.POSIXlt(strptime(as.character(d), format))
  
  if(t=="year"){
    xlt$year+1900
  }else if(t=="month"){
    xlt$mon
  }else if(t=="day"){
    xlt$mday
  }else if(t=="hour"){
    xlt$hour
  }else if(t=="min"){
    xlt$min
  }else if(t=="sec"){
    xlt$sec
  }
}

toRadian<-function(v){
  (v*pi)/180
}

getDistance<-function(R=6371, lat1,lat2,long1,long2){
  lat1 = toRadian(as.numeric(lat1))
  lat2 = toRadian(as.numeric(lat2))
  long1 = toRadian(as.numeric(long1))
  long2 = toRadian(as.numeric(long2))
  
  latm = (lat1+lat2)/2
  
  a = (lat2-lat1)*(lat2-lat1)
  b = (long2-long1)*cos(latm)
  c = b*b
  
  R * sqrt(a+c)
}

getNormalizedData<-function(d){
  a = min(d)
  b=max(d)
  df = b-a
  
  if(df>0){
    k=(d-a)/df
  }else k=1
  
  k
}

getStandardizeddData<-function(d){
  a = min(d)
  b=max(d)
  df = b-a
  
  if(df>0){
    k=(d-mean(d))/sd(d)
  }else k=a
}

formatDecimal<-function(data, digits=6){
  formattable(as.numeric(data), digits = digits, format = "f")
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

model_fit = function(x, y, par, index, colName=c("x","y","rate")){
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
    colnames(grData) <- colName
  }
  
  return(grData)
}

model_fit_4 = function(x, y, par, index, colName=c("x","y","rate")){
  grData = NULL
  
  if(index==4){
    grData <- as.data.frame(cbind(y, Model_W14(x,par[1],par[2],par[3],par[4]), Model_W14_D(x,par[1],par[2],par[3],par[4]) ))
  }else if(index==3){
    grData <- as.data.frame(cbind(y, Model_LL5(x,par[1],par[2],par[3],par[4],par[5]), Model_LL5_D(x,par[1],par[2],par[3],par[4], par[5]) ))
  }else if(index==2){
    grData <- as.data.frame(cbind(y, Model_L5(x,par[1],par[2],par[3],par[4],par[5]), Model_L5_D(x,par[1],par[2],par[3],par[4],par[5]) ))
  }else if(index==1){
    grData <- as.data.frame(cbind(y, Model_G4(x,par[1],par[2],par[3],par[4]), Model_G4_D(x,par[1],par[2],par[3],par[4]) ))
  }
  
  if(is.null(grData)==FALSE){
    colnames(grData) <- colName
  }
  
  return(grData)
}

