source("header.R")

covid_data = read.csv("FromAnil/UNM/Write/covid_state_7MA_2.csv")

# Recalculate the growth
covid_data = as.data.frame(covid_data[,c(1:23)])

## Create growth rate (daily and weekly)
covid_data = as.data.frame(covid_data %>%group_by(STFIPS)%>%mutate(
  #daily_lag = DSFC-lag(DSFC),
  daily_growth = (Cum.New.cases.1000.people-lag(Cum.New.cases.1000.people))/lag(Cum.New.cases.1000.people),
  #daily_growth_rate = daily_growth/daily_lag,
  #weekly_lag = DSFC-lag(DSFC, n = 7),
  weekly_growth = (Cum.New.cases.1000.people-lag(Cum.New.cases.1000.people, n = 7))/lag(Cum.New.cases.1000.people, n = 7),
  weekly_growth_rate = weekly_growth/7
))

# Set the initial values with the first value
sid = unique(covid_data$STFIPS)
for(i in sid){
  t=which(covid_data$STFIPS==as.numeric(i))
  #covid_data_trend$daily_lag[t[1]]=covid_data_trend$daily_lag[t[2]]
  covid_data$daily_growth[t[1]]=covid_data$daily_growth[t[2]]
  #covid_data_trend$daily_growth_rate[t[1]]=covid_data_trend$daily_growth_rate[t[2]]
  
  #covid_data_trend$weekly_lag[t[1:7]]=covid_data_trend$weekly_lag[t[8]]
  covid_data$weekly_growth[t[1:7]]=covid_data$weekly_growth[t[8]]
  covid_data$weekly_growth_rate[t[1:7]]=covid_data$weekly_growth_rate[t[8]]
}

#######################################
############################# Plotting
#######################################
t=which(covid_data$STNAME=='GA')
par(mfrow=c(4,1))
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="Georgia State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$Cum.New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Cum.New Cases per 1k")
plot(covid_data$DSFC[t], covid_data$daily_growth[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth")
plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data$STNAME=='FL')
par(mfrow=c(3,1))
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="Florida State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$NewCasesTrend[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases per 1k")
plot(covid_data$DSFC[t], covid_data$New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases trend")
#plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data$STNAME=='TX')
par(mfrow=c(2,1))
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="Texas State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases per 1k")
#plot(covid_data$DSFC[t], covid_data$daily_growth[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth")
#plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))


t=which(covid_data$STNAME=='CA')
par(mfrow=c(2,1))
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="California State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases per 1k")
#plot(covid_data$DSFC[t], covid_data$daily_growth[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth")
#plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data$STNAME=='CA')
par(mfrow=c(2,1))
plot(covid_data$DSFC[t], covid_data$HospitalBedUtilizationTrend[t], type='o', main="California State", xlab="Date Since First Case (DSFC)", ylab="Hospital bed utilization")
plot(covid_data$DSFC[t], covid_data$New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases per 1k")
#plot(covid_data$DSFC[t], covid_data$daily_growth[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth")
#plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data$STNAME=='CA')
par(mfrow=c(2,1))
plot(covid_data$DSFC[t], covid_data$OutOfStateTripTrend[t], type='o', main="California State", xlab="Date Since First Case (DSFC)", ylab="Out of state trip")
plot(covid_data$DSFC[t], covid_data$New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases per 1k")
#plot(covid_data$DSFC[t], covid_data$daily_growth[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth")
#plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data$STNAME=='WA')
par(mfrow=c(2,1))
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="Washington State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases per 1k")
#plot(covid_data$DSFC[t], covid_data$daily_growth[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth")
#plot(covid_data$DSFC[t], covid_data$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

############ Lag
imgPath = "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/img/"
stNames = read.csv(file.choose())
cross_corrDS = data.frame(StateCode=as.character(),
                          Lags=as.integer(),
                          corrcoef=as.double(),
                          pp=as.character(),
                          stringsAsFactors = FALSE)

# Get all states IDs
stIds = unique(covid_data$STNAME)
#stIds = c("AL","AZ","CA","FL","GA","NY","NJ","TX","WA")
for (state in stIds) {
  t=which(covid_data$STNAME==as.character(state))
  stName = as.character(stNames$State_name[which(stNames$state_code==as.character(state))])
  title = paste(stName,": New case(t+h), SDI(t)",sep="")
  
  x=normalize(covid_data$NewCasesTrend[t], method="range")
  y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
  mdl=ccf(x,y, lag.max = 50, main=title)
  ppl = which(diff(sign(diff(mdl$acf)))==-2)+1
  npl = which(diff(sign(diff(mdl$acf)))==2)+1
  
  tmp = data.frame(StateCode=rep(as.character(state), length(mdl$lag)),
                   Lags=mdl$lag,
                   corrcoef=mdl$acf,
                   pp=rep("", length(mdl$lag)),
                   stringsAsFactors = FALSE)
  
  if(length(ppl)>0){
    tmp$pp[ppl]=as.character("U")
  }
  
  if(length(npl)>0){
    tmp$pp[npl]="D"
  }
  
  cross_corrDS = rbind(cross_corrDS, tmp)
  rm(tmp)
  
  # Save as image
  dev.off()
  pdf(file=paste(imgPath,stName,".pdf",sep=""))
  plot(mdl, xlab = "Lag(h)", main = title)
  abline(v=0, col="red")
  dev.off()
}
dev.off()
###################

## Cross correlation between ActiveCases(t+h), SDI(t)
imgPath = "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/img_SDI_acase/"
# Get all states IDs
stIds = unique(covid_data$STNAME)
#stIds = c("AL","AZ","CA","FL","GA","NY","NJ","TX","WA")
for (state in stIds) {
  t=which(covid_data$STNAME==as.character(state))
  stName = as.character(stNames$State_name[which(stNames$state_code==as.character(state))])
  title = paste(stName,": Active case(t+h), SDI(t)",sep="")
  
  x=normalize(covid_data$ActiveCasesTrend[t], method="range")
  y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
  mdl=ccf(x,y, lag.max = 50, main=title)
  # ppl = which(diff(sign(diff(mdl$acf)))==-2)+1
  # npl = which(diff(sign(diff(mdl$acf)))==2)+1
  # 
  # tmp = data.frame(StateCode=rep(as.character(state), length(mdl$lag)),
  #                  Lags=mdl$lag,
  #                  corrcoef=mdl$acf,
  #                  pp=rep("", length(mdl$lag)),
  #                  stringsAsFactors = FALSE)
  # 
  # if(length(ppl)>0){
  #   tmp$pp[ppl]=as.character("U")
  # }
  # 
  # if(length(npl)>0){
  #   tmp$pp[npl]="D"
  # }
  # 
  # cross_corrDS = rbind(cross_corrDS, tmp)
  # rm(tmp)
  
  # Save as image
  dev.off()
  pdf(file=paste(imgPath,stName,".pdf",sep=""))
  plot(mdl, xlab = "Lag(h)", main = title)
  abline(v=0, col="red")
  dev.off()
}
dev.off()
#################
################# Lag based data
t = which(cross_corrDS$Lags<0 & cross_corrDS$pp=="D" & cross_corrDS$Lags>-30)
ndf = as.data.frame(cross_corrDS[t,])

t = which(cross_corrDS$Lags>0 & cross_corrDS$pp=="D")
pdf = as.data.frame(cross_corrDS[t,])
pdf = pdf%>%group_by(StateCode)%>%summarise(Lag=min(Lags))%>%filter(Lag<=40)

covid_neg_lag = NULL
covid_pos_lag = NULL

for(i in 1:nrow(ndf)){
  h = ndf$Lags[i]
  
  t = which(covid_data$STNAME==as.character(ndf$StateCode[i]))
  df = as.data.frame(covid_data[t,c(2:5,11,12)])
  print(paste(as.character(ndf$StateCode[i]),"-",min(df$DSFC),"-",h,sep=""))
  df$SocialDistanceIndexTrend = lag(df$SocialDistanceIndexTrend, n = -h)
  tt = which(!is.na(df$SocialDistanceIndexTrend))
  df = as.data.frame(df[tt,])
  
  if(is.null(covid_neg_lag)){
    covid_neg_lag = as.data.frame(df)
  }else{
    covid_neg_lag = rbind(covid_neg_lag, df)
  }
}

for(i in 1:nrow(pdf)){
  h = pdf$Lag[i]
  
  t = which(covid_data$STNAME==as.character(pdf$StateCode[i]))
  df = as.data.frame(covid_data[t,c(2:5,11,12)])
  print(paste(as.character(pdf$StateCode[i]),"-",min(df$DSFC),"-",h,sep=""))
  df$NewCasesTrend = lag(df$NewCasesTrend, n = h)
  tt = which(!is.na(df$NewCasesTrend))
  df = as.data.frame(df[tt,])
  
  if(is.null(covid_pos_lag)){
    covid_pos_lag = as.data.frame(df)
  }else{
    covid_pos_lag = rbind(covid_pos_lag, df)
  } 
}

# Reset row index
row.names(covid_neg_lag)<-NULL
row.names(covid_pos_lag)<-NULL

# Save files
write.csv(covid_neg_lag,"/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/covid_neg_lag_all.csv")
write.csv(covid_pos_lag,"/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/covid_pos_lag_all.csv")

### Get two nearest down points of zero
covid_neg_lag = NULL
covid_pos_lag = NULL

stIds = c("AL","AZ","CA","FL","GA","TX","WA")
for(stn in stIds){
  stn = as.character(stn)
  t = which(cross_corrDS$StateCode==stn)
  df = as.data.frame(cross_corrDS[t,])
  a = min(t)-1
  zero = which(cross_corrDS$Lags[t]==0)+a
  p=c()
  i=zero
  while(i>=min(t)){
    if(cross_corrDS$pp[t][i-a]=="D"){
      p = append(p, i-a);
      break;
    }
    i = i-1
  }
  
  i=zero+1
  while(i<=max(t)){
    if(cross_corrDS$pp[t][i-a]=="D"){
      p = append(p, i-a);
      break;
    }
    i = i+1
  }
  
  h1 = cross_corrDS$Lags[t][p[1]]
  h2 = cross_corrDS$Lags[t][p[2]]
  
  t = which(covid_data$STNAME==stn)
  df = as.data.frame(covid_data[t,c(2:5,11,12)])
  print(paste(stn,"-",min(df$DSFC),sep=""))
  df$SocialDistanceIndexTrend = lag(df$SocialDistanceIndexTrend, n = -h1)
  tt = which(!is.na(df$SocialDistanceIndexTrend))
  df = as.data.frame(df[tt,])
  
  if(is.null(covid_neg_lag)){
    covid_neg_lag = as.data.frame(df)
  }else{
    covid_neg_lag = rbind(covid_neg_lag, df)
  }
  
  t = which(covid_data$STNAME==stn)
  df = as.data.frame(covid_data[t,c(2:5,11,12)])
  df$NewCasesTrend = lag(df$NewCasesTrend, n = h2)
  tt = which(!is.na(df$NewCasesTrend))
  df = as.data.frame(df[tt,])
  
  if(is.null(covid_pos_lag)){
    covid_pos_lag = as.data.frame(df)
  }else{
    covid_pos_lag = rbind(covid_pos_lag, df)
  } 
}

# Reset row index
row.names(covid_neg_lag)<-NULL
row.names(covid_pos_lag)<-NULL

# Save files
write.csv(covid_neg_lag,"/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/covid_neg_lag.csv")
write.csv(covid_pos_lag,"/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/covid_pos_lag.csv")

########### Cumulative cases
covid_data_testing = as.data.frame(covid_data%>%group_by(STFIPS,STNAME)%>%mutate(CumNewCasesTrend = cumsum(NewCasesTrend),
                                                                             CumTestingDoneTrend=cumsum(TestingDoneTrend),
                                                                             CumActiveCaseTrend=cumsum(ActiveCasesTrend)))
covid_data_testing = as.data.frame(covid_data_testing[,c(2:5,11:22,27:29)])
row.names(covid_data_testing)<-NULL
write.csv(covid_data_testing,"/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/covid_Data.csv")
######################################################
t=which(covid_data_testing$STNAME=='FL')
x=normalize(covid_data_testing$SocialDistanceIndexTrend[t], method="range")
y=normalize(covid_data_testing$ActiveCasesTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="Florida: SDI(t+h), ActiveCases(t)")
mdl
plot(mdl)
par(mfrow=c(3,1))
plot(mdl, xlab = "Lag(h)", main = "Florida:corr New case(t+h), SDI(t)")
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="Florida State", xlab="Days Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$NewCasesTrend[t], type='o', xlab="Days Since First Case (DSFC)", ylab="New Cases per 1k")
par(mfrow=c(1,1))



t=which(covid_data$STNAME=='FL')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="Florida: New case(t+h), SDI(t)")
mdl
par(mfrow=c(3,1))
plot(mdl, xlab = "Lag(h)", main = "Florida:corr New case(t+h), SDI(t)")
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o', main="Florida State", xlab="Days Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data$DSFC[t], covid_data$NewCasesTrend[t], type='o', xlab="Days Since First Case (DSFC)", ylab="New Cases per 1k")
par(mfrow=c(1,1))


x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
t=which(covid_data$STNAME=='TX')
mdl=ccf(x,y, lag.max = 50, main="Texas: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "Texas:corr New case(t+h), SDI(t)")
mdl

t=which(covid_data$STNAME=='CA')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="California: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "California:corr New case(t+h), SDI(t)")
mdl

t=which(covid_data$STNAME=='CA')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$HospitalBedUtilizationTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="California: New case(t+h), HBU(t)")
plot(mdl, xlab = "Lag(h)", main = "California:corr New case(t+h), HBU(t)")
mdl

t=which(covid_data$STNAME=='CA')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$OutOfStateTripTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="California: New case(t+h), OutState(t)")
plot(mdl, xlab = "Lag(h)", main = "California:corr New case(t+h), OutState(t)")
mdl

t=which(covid_data$STNAME=='WA')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="Washington: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "Washington:corr New case(t+h), SDI(t)")
mdl

t=which(covid_data$STNAME=='NY')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="New York: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "New York:corr New case(t+h), SDI(t)")
mdl

t=which(covid_data$STNAME=='AZ')
x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
mdl=ccf(x,y, lag.max = 50, main="Arizona: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "Arizona:corr New case(t+h), SDI(t)")
mdl

x=normalize(covid_data$NewCasesTrend[t], method="range")
y=normalize(covid_data$ICUUtilizationTrend[t], method="range")
t=which(covid_data$STNAME=='GA')
mdl=ccf(x,y, lag.max = 50, main="Georgia: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "Correlation between NewCases(t+h) and SDI(t)")
mdl

x=normalize(covid_data$ICUUtilizationTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
t=which(covid_data$STNAME=='GA')
mdl=ccf(x,y, lag.max = 50, main="Georgia: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "Correlation between IcuUtil(t+h) and SDI(t)")
mdl

x=normalize(covid_data$OutOfStateTripTrend[t], method="range")
y=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
t=which(covid_data$STNAME=='GA')
mdl=ccf(x,y, lag.max = 50, main="Georgia: New case(t+h), SDI(t)")
plot(mdl, xlab = "Lag(h)", main = "Correlation between OST(t+h) and SDI(t)")
mdl

#######################################################################
####################################### Regression analyzing
covid_data = read.csv("FromAnil/UNM/unm_state_trend.csv")
covid_data_regress = as.data.frame(covid_data[,c(2,5:7,9:23)])

# Correlation coefficient analysis
imgPath = "FromAnil/UNM/correlation_state/"
stids = sort(unique(covid_data$STFIPS))
dev.set(which = dev.next())
for(st in stids){
  t = which(covid_data$STFIPS==st)
  stName = covid_data$STNAME[t[1]]
  corcoef = abs(cor(covid_data[t,c(11,13:23)]))
  
  while (!is.null(dev.list()))  dev.off()
  pdf(file=paste(imgPath,stName,".pdf",sep=""))
  pheatmap(corcoef, treeheight_row = 0, treeheight_col = 0, cluster_rows = F, cluster_cols = F, main = stName)
  while (!is.null(dev.list()))  dev.off()
}

corcoef = abs(cor(covid_data[,c(11,13:23)]))
while (!is.null(dev.list()))  dev.off()
pdf(file=paste(imgPath,"All",".pdf",sep=""))
pheatmap(corcoef, treeheight_row = 0, treeheight_col = 0, cluster_rows = F, cluster_cols = F, main = "All states")
while (!is.null(dev.list()))  dev.off()

while (!is.null(dev.list()))  dev.off()

#######################################################################
####################################### Trend analyzing

covid_data = read.csv("FromAnil/UNM/unm_state_trend.csv")

covid_lag_data = data.frame(STFIPS=as.integer(),
                            stateCode=as.character(),
                            lags=as.integer(),
                            coef_NCase_SDI=as.numeric(),
                            coef_NCase_TstDone=as.numeric(),
                            coef_NCase_OSTrip = as.numeric(),
                            coef_NCase_WrkTrip=as.numeric(),
                            coef_NCase_ImportCase=as.numeric(),
                            coef_SDI_Death=as.numeric(),
                            coef_SDI_ICUUtil=as.numeric(),
                            coef_SDI_HospBedUtil=as.numeric(),
                            coef_SDI_VentilationShortage=as.numeric(),
                            coef_Death_ICUUtil=as.numeric(),
                            coef_Death_HospBedUtil=as.numeric(),
                            coef_Death_VentilationShortage=as.numeric())

stids = sort(unique(covid_data$STFIPS))

for(st in stids){
  t = which(covid_data$STFIPS==st)
  
  if(length(t)>0){
    xNewCasesTrend=normalize(covid_data$NewCasesTrend[t], method="range")
    xTestingDoneTrend=normalize(covid_data$TestingDoneTrend[t], method="range")
    xOutOfStateTripTrend=normalize(covid_data$OutOfStateTripTrend[t], method="range")
    xWorkTripTrend=normalize(covid_data$WorkTripTrend[t], method="range")
    xImportedCasesTrend=normalize(covid_data$ImportedCasesTrend[t], method="range")
    xDeathRateTrend=normalize(covid_data$DeathRateTrend[t], method="range")
    xICUUtilizationTrend=normalize(covid_data$ICUUtilizationTrend[t], method="range")
    xHospitalBedUtilizationTrend=normalize(covid_data$HospitalBedUtilizationTrend[t], method="range")
    xVentilationShortageTrend=normalize(covid_data$VentilationShortageTrend[t], method="range")
    xSocialDistanceIndexTrend=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
    
    
    m_NCase_SDI=ccf(xNewCasesTrend, xSocialDistanceIndexTrend, lag.max = 120, main="")
    m_NCase_TstDone=ccf(xNewCasesTrend, xTestingDoneTrend, lag.max = 120, main="")
    m_NCase_OSTrip = ccf(xNewCasesTrend, xOutOfStateTripTrend, lag.max = 120, main="")
    m_NCase_WrkTrip=ccf(xNewCasesTrend, xWorkTripTrend, lag.max = 120, main="")
    m_NCase_ImportCase=ccf(xNewCasesTrend, xImportedCasesTrend, lag.max = 120, main="")
    
    m_SDI_Death=ccf(xSocialDistanceIndexTrend, xDeathRateTrend, lag.max = 120, main="")
    m_SDI_ICUUtil=ccf(xSocialDistanceIndexTrend, xICUUtilizationTrend, lag.max = 120, main="")
    m_SDI_HospBedUtil=ccf(xSocialDistanceIndexTrend, xHospitalBedUtilizationTrend, lag.max = 120, main="")
    m_SDI_VentilationShortage=ccf(xSocialDistanceIndexTrend, xVentilationShortageTrend, lag.max = 120, main="")

    m_Death_ICUUtil=ccf(xDeathRateTrend, xICUUtilizationTrend, lag.max = 120, main="")
    m_Death_HospBedUtil=ccf(xDeathRateTrend, xHospitalBedUtilizationTrend, lag.max = 120, main="")
    m_Death_VentilationShortage=ccf(xDeathRateTrend, xVentilationShortageTrend, lag.max = 120, main="")
    
    tt = which(m_NCase_SDI$lag>=0)
    
    tmpDS = data.frame(STFIPS=rep(covid_data$STFIPS[t][1], 121),
                       stateCode=rep(as.character(covid_data$STNAME[t][1]), 121),
                       lags=c(0:120),
                       coef_NCase_SDI=m_NCase_SDI$acf[tt],
                       coef_NCase_TstDone=m_NCase_TstDone$acf[tt],
                       coef_NCase_OSTrip = m_NCase_OSTrip$acf[tt],
                       coef_NCase_WrkTrip=m_NCase_WrkTrip$acf[tt],
                       coef_NCase_ImportCase=m_NCase_ImportCase$acf[tt],
                       coef_SDI_Death=m_SDI_Death$acf[tt],
                       coef_SDI_ICUUtil=m_SDI_ICUUtil$acf[tt],
                       coef_SDI_HospBedUtil=m_SDI_HospBedUtil$acf[tt],
                       coef_SDI_VentilationShortage=m_SDI_VentilationShortage$acf[tt],
                       coef_Death_ICUUtil=m_Death_ICUUtil$acf[tt],
                       coef_Death_HospBedUtil=m_Death_HospBedUtil$acf[tt],
                       coef_Death_VentilationShortage=m_Death_VentilationShortage$acf[tt])
    
    covid_lag_data = as.data.frame(rbind(covid_lag_data, tmpDS))
    rm(xNewCasesTrend,xTestingDoneTrend,xOutOfStateTripTrend,xWorkTripTrend,xImportedCasesTrend,xDeathRateTrend,xICUUtilizationTrend,xHospitalBedUtilizationTrend,xVentilationShortageTrend,xSocialDistanceIndexTrend)
    rm(m_NCase_SDI,m_NCase_TstDone,m_NCase_OSTrip,m_NCase_WrkTrip,m_NCase_ImportCase,m_SDI_Death,m_SDI_ICUUtil,m_SDI_HospBedUtil,m_SDI_VentilationShortage,m_Death_ICUUtil,m_Death_HospBedUtil,m_Death_VentilationShortage)
    rm(tmpDS)
  }
}

stateDS = read.csv("FromAnil/UNM/state_code.csv")
covid_lag_data = as.data.frame(merge(covid_lag_data, stateDS, by.x = "stateCode", by.y = "state_code"))
covid_lag_data = as.data.frame(covid_lag_data[,c(2,1,16,3:15)])

write.csv(covid_lag_data, "FromAnil/UNM/unm_state_lag.csv")

############### Reshape lag data
covid_lag_data = read.csv("FromAnil/UNM/unm_state_lag.csv")
covid_lag_data = as.data.frame(covid_lag_data[,-c(1)])

# Create vertical data
covidLagDS <- covid_lag_data %>%
  #select(date, psavert, uempmed) %>%
  gather(key = "variable", value = "value", -c(STFIPS,stateCode,State_name,lags))%>%
  arrange(STFIPS)

covidLagDS$AtT=""
covidLagDS$AtTPlusDelta=""
covidLagDS$Color=""
uv = unique(covidLagDS$variable)
for(v in uv){
  t = which(covidLagDS$variable==as.character(v))
  k = strsplit(as.character(v), "_")[[1]]
  
  col=""
  if(k[2]=="NCase"){
    if(k[3]=="SDI"){
      col="red"
    }else if(k[3]=="TstDone"){
      col="darkgreen"
    }else if(k[3]=="OSTrip"){
      col="darkorange"
    }else if(k[3]=="WrkTrip"){
      col="purple"
    }else if(k[3]=="ImportCase"){
      col="blue"
    }
  }else if(k[2]=="SDI"){
    if(k[3]=="Death"){
      col="red"
    }else if(k[3]=="ICUUtil"){
      col="darkgreen"
    }else if(k[3]=="HospBedUtil"){
      col="darkorange"
    }else if(k[3]=="VentilationShortage"){
      col="blue"
    }
  }else if(k[2]=="Death"){
    if(k[3]=="ICUUtil"){
      col="red"
    }else if(k[3]=="HospBedUtil"){
      col="darkgreen"
    }else if(k[3]=="VentilationShortage"){
      col="blue"
    }
  }
  
  covidLagDS$AtT[t] = k[3]
  covidLagDS$AtTPlusDelta[t] = k[2]
  covidLagDS$Color[t]=col
}

write.csv(covidLagDS, "FromAnil/UNM/unm_state_lag_vertical.csv")

############### Plot lag data
covid_lag_data = read.csv("FromAnil/UNM/unm_state_lag.csv")
covid_lag_data = as.data.frame(covid_lag_data[,-c(1)])
covidLagDS = read.csv("FromAnil/UNM/unm_state_lag_vertical.csv")
covidLagDS = as.data.frame(covidLagDS[,-c(1)])

# GGplot
delta = c("NCase","SDI","Death")
deltaDesc = c("New cases", "Social distance index", "Death rate")
sids = unique(covidLagDS$STFIPS)
imgPath = "FromAnil/UNM/lag_state/"

for(st in sids){
  stName = as.character(unique(covidLagDS$State_name[which(covidLagDS$STFIPS==as.character(st))]))
  
  dev.set(which = dev.next())
  for(i in 1:length(delta)){
    ldesc = deltaDesc[i]
    
    tdf = covidLagDS[which(covidLagDS$AtTPlusDelta==delta[i] & covidLagDS$STFIPS==st),]
    p = ggplot(tdf, aes(x = lags)) + 
      geom_point(aes(y=value,color = AtT)) +
      ggtitle(as.character(unique(tdf$State_name))) +
      xlab(paste('Lags on ',as.character(ldesc),sep="")) +
      ylab('Correlation coefficient') + 
      theme(axis.title.x = element_text(size=rel(1.5)), 
            axis.text.x = element_text(size=rel(1.2)), 
            axis.title.y = element_text(size=rel(1.5)),
            axis.text.y = element_text(size=rel(1.2)), 
            legend.title = element_text(size = rel(2)),
            legend.text = element_text(size = 12), 
            plot.title = element_text(size=rel(3.0), hjust = 0.5), 
            strip.text = element_text(face="bold", size = rel(2)),
            strip.background = element_rect(fill="white", colour="black",size=2))
    
    while (!is.null(dev.list()))  dev.off()
    pdf(file=paste(imgPath,stName,"_",as.character(ldesc),".pdf",sep=""))
    print(p)
    while (!is.null(dev.list()))  dev.off()
  }
  while (!is.null(dev.list()))  dev.off()
}
#while (!is.null(dev.list()))  dev.off()

tdf = covidLagDS[which(covidLagDS$AtTPlusDelta=="NCase" & (covidLagDS$AtT=="SDI" | covidLagDS$AtT=="TstDone")),]
p = ggplot(tdf, aes(x = lags,y=value, color=AtT)) + 
  geom_point() +
  #scale_color_manual(values = Color)+
  #ggtitle(as.character(unique(tdf$State_name))) +
  xlab(paste('Lags on ',as.character(unique(tdf$AtTPlusDelta,sep="")))) +
  ylab('Correlation coefficient') + 
  theme(axis.title.x = element_text(size=rel(1.5)), 
        axis.text.x = element_text(size=rel(1.2)), 
        axis.title.y = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.2)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2))
#p = p + scale_color_manual(values = c("blue","darkgreen","darkorange","purple","red"))
p

#p<-p+geom_label_repel(aes(label=AtT), size=2, data=tdf) + theme(legend.position = "none");p


covid_lag_data = as.data.frame(covid_lag_data%>%arrange(STFIPS, lags))
lblDS = data.frame(lagsOn=c(rep("NewCases",5), rep("SDI", 4), rep("DeathRate",3)), 
                 atT=c("SDI","TestDone","OutStateTrip","WorkTrip","ImportCase","DeathRate","ICUUtilization","HospitalBedUtil","VentilationShortage","ICUUtilization","HospitalBedUtil","VentilationShortage"))

t = which(covid_lag_data$STFIPS==1)
matplot(covid_lag_data$lags[t], covid_lag_data[t,c(5:9)], col = c("red","darkgreen","darkorange","purple","blue"), type = "l", lty = 1)
legend("topright",legend = c("SDI","TestDone","OutStateTrip","WorkTrip","ImportCase"), col = c("red","darkgreen","darkorange","purple","blue"), lwd = 2)

p = ggplot() + 
  geom_line(data = covid_lag_data[t,], aes(x = lags, y = coef_NCase_SDI), color = "red") +
  geom_line(data = covid_lag_data[t,], aes(x = lags, y = coef_NCase_TstDone), color = "darkgreen") +
  geom_line(data = covid_lag_data[t,], aes(x = lags, y = coef_NCase_OSTrip), color = "darkorange") +
  geom_line(data = covid_lag_data[t,], aes(x = lags, y = coef_NCase_WrkTrip), color = "purple") +
  geom_line(data = covid_lag_data[t,], aes(x = lags, y = coef_NCase_ImportCase), color = "blue") +
  ggtitle("") +
  xlab('Lags on NewCases') +
  ylab('Correlation coefficient') + 
  theme(axis.title.x = element_text(size=rel(1.5)), 
        axis.text.x = element_text(size=rel(1.2)), 
        axis.title.y = element_text(size=rel(1.5)),
        axis.text.y = element_text(size=rel(1.2)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2));
p<-p+geom_label_repel(aes(label=atT), size=2, data=lblDS[c(1:5),]) + theme(legend.position = "none")

ggl2<-ggplot(tmpDS, aes(x = lags, y = LogTwo, colour = State)) + 
  geom_line() +
  scale_color_manual(values = unique(df$color)) + 
  #geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  #labs(color='USA States') +
  ggtitle("") +
  labs(x = "Days Since First Case (DSFC)", y = "Log2(cases)")
#gg+geom_label_repel(aes(label=State), size=2, data=df_sub) + theme(legend.position = "Right")
ggtl2<-ggl2+geom_label_repel(aes(label=State), size=2, data=df_sub) + theme(legend.position = "none")



### Lag for CA and FL
t=which(covid_data$STNAME=="FL")
stName = as.character(covid_data$STNAMELONG[t[1]])
title = paste(stName,": New case(t+h), SDI(t)",sep="")

x=normalize(covid_data$NewCases[t], method="range")
y=normalize(covid_data$SDI[t], method="range")
mdl=ccf(x,y, lag.max = 120, main=title)

ppl = which(diff(sign(diff(mdl$acf)))==-2)+1
npl = which(diff(sign(diff(mdl$acf)))==2)+1


### Lag for NY and NJ
par(mfrow=c(2,2))
t=which(covid_data$STNAME=="NY")
stName = as.character(covid_data$STNAMELONG[t[1]])
title = paste(stName,": New case(t+h), SDI(t)",sep="")

x=normalize(covid_data$NewCases[t], method="range")
y=normalize(covid_data$CovidExposure[t], method="range")
mdl=ccf(x,y, lag.max = 120, main=title)

t=which(covid_data$STNAME=="NJ")
stName = as.character(covid_data$STNAMELONG[t[1]])
title = paste(stName,": New case(t+h), SDI(t)",sep="")

x=normalize(covid_data$NewCases[t], method="range")
y=normalize(covid_data$CovidExposure[t], method="range")
mdl=ccf(x,y, lag.max = 120, main=title)

t=which(covid_data$STNAME=="UT")
stName = as.character(covid_data$STNAMELONG[t[1]])
title = paste(stName,": New case(t+h), SDI(t)",sep="")

x=normalize(covid_data$NewCases[t], method="range")
y=normalize(covid_data$CovidExposure[t], method="range")
mdl=ccf(x,y, lag.max = 120, main=title)

t=which(covid_data$STNAME=="NM")
stName = as.character(covid_data$STNAMELONG[t[1]])
title = paste(stName,": New case(t+h), SDI(t)",sep="")

x=normalize(covid_data$NewCases[t], method="range")
y=normalize(covid_data$CovidExposure[t], method="range")
mdl=ccf(x,y, lag.max = 120, main=title)
par(mfrow=c(1,1))



