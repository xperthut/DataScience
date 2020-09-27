source("header.R")

covid_data = read.csv("FromAnil/UNM/County_2020_8_10.csv")

####################################### Preprocessing
# Summary statistics
summary(covid_data)

# Drop columns with missing data
covid_data = as.data.frame(covid_data %>%select_if(~ !any(is.na(.))))

# Compute cumulative new cases per 1000 people
covid_data = as.data.frame(covid_data%>%group_by(STFIPS,CTFIPS)%>%mutate(Cum.New.cases.1000.people=cumsum(New.cases.1000.people)))
covid_data = as.data.frame(covid_data%>%filter(Cum.New.cases.1000.people>0))

# Sort by date
covid_data$dts = as.numeric(convertToDateString(covid_data$date, frmt = "%m/%d/%Y"))
covid_data = as.data.frame(covid_data%>%arrange(STFIPS,CTFIPS,dts))

# Rearrange columns
covid_data = as.data.frame(covid_data[,c(60,15,1:4,59,8,10,12,25,28,33:36,38:43)]%>%arrange(STFIPS,CTFIPS,dts))

# Generate Days Since First Case numbers
covid_data$DSFC = 0
df = as.data.frame(covid_data[,c(3,5)]%>%distinct())

for(i in 1:nrow(df)){
  t = which(covid_data$CTFIPS==df$CTFIPS[i] & covid_data$STFIPS==df$STFIPS[i])
  if(length(t) > 0){
    d = as.character(covid_data$date[t[1]])
    # %m/%d/%Y
    covid_data$DSFC[t] = getDays(d, covid_data$date[t], fmt = "%m/%d/%Y")
    rm(d)
  }
  rm(t)
}
rm(df)

# Rearrange columns
covid_data = as.data.frame(covid_data[,c(23,2,5,3,4,6:22)]%>%arrange(STFIPS,CTFIPS,DSFC))

################################################################################################
################################### Trend ###################################################
################################################################################################
covid_data_SIC = as.data.frame(covid_data)

## Create trend and use it as social distance
covid_data_SIC$SocialDistanceIndexTrend=0.0
covid_data_SIC$NewCasesTrend=0.0
covid_data_SIC$DeathRateTrend=0.0
covid_data_SIC$CovidExposerTrend=0.0
covid_data_SIC$ActiveCasesTrend=0.0

covid_data_SIC$OutOfStateTripTrend=0.0
covid_data_SIC$WorkTripTrend=0.0
covid_data_SIC$ICUUtilizationTrend=0.0
covid_data_SIC$HospitalBedUtilizationTrend=0.0
covid_data_SIC$TestingCapacityTrend=0.0
covid_data_SIC$TestingDoneTrend=0.0
covid_data_SIC$VentilationShortageTrend=0.0
covid_data_SIC$ImportedCasesTrend=0.0

tdf = as.data.frame(covid_data_SIC[,c(3,4)]%>%distinct())

for(i in 1:nrow(tdf)){
  t = which(covid_data_SIC$CTFIPS==tdf$CTFIPS[i] & covid_data_SIC$STFIPS==tdf$STFIPS[i])
  
  trend_sdi = ma(as.ts(covid_data_SIC$Social.distancing.index[t]), order = 7, centre = T)
  trend_nwc = ma(as.ts(covid_data_SIC$New.cases.1000.people[t]), order = 7, centre = T)
  trend_death = ma(as.ts(covid_data_SIC$COVID.death.rate[t]), order = 7, centre = T)
  trend_exp = ma(as.ts(covid_data_SIC$COVID.exposure.1000.people[t]), order = 7, centre = T)
  trend_act = ma(as.ts(covid_data_SIC$Active.cases.1000.people[t]), order = 7, centre = T)
  
  trend_1 = ma(as.ts(covid_data_SIC$X..out.of.state.trips[t]), order = 7, centre = T)
  trend_2 = ma(as.ts(covid_data_SIC$Work.trips.person[t]), order = 7, centre = T)
  trend_3 = ma(as.ts(covid_data_SIC$X..ICU.utilization[t]), order = 7, centre = T)
  trend_4 = ma(as.ts(covid_data_SIC$X..hospital.bed.utilization[t]), order = 7, centre = T)
  trend_5 = ma(as.ts(covid_data_SIC$Testing.capacity[t]), order = 7, centre = T)
  trend_6 = ma(as.ts(covid_data_SIC$Tests.done.1000.people[t]), order = 7, centre = T)
  trend_7 = ma(as.ts(covid_data_SIC$Ventilator.shortage[t]), order = 7, centre = T)
  trend_8 = ma(as.ts(covid_data_SIC$Imported.COVID.cases[t]), order = 7, centre = T)
  
  df = data.frame("DSFC"=covid_data_SIC$DSFC[t],
                  "SocialDistanceIndexTrend"=as.ts(trend_sdi),
                  "NewCasesTrend"=as.ts(trend_nwc),
                  "DeathRateTrend"=as.ts(trend_death),
                  "CovidExposerTrend"=as.ts(trend_exp),
                  "ActiveCasesTrend"=as.ts(trend_act),
                  "OutOfStateTripTrend"=as.ts(trend_1),
                  "WorkTripTrend"=as.ts(trend_2),
                  "ICUUtilizationTrend"=as.ts(trend_3),
                  "HospitalBedUtilizationTrend"=as.ts(trend_4),
                  "TestingCapacityTrend"=as.ts(trend_5),
                  "TestingDoneTrend"=as.ts(trend_6),
                  "VentilationShortageTrend"=as.ts(trend_7),
                  "ImportedCasesTrend"=as.ts(trend_8)
  )
  
  df$SocialDistanceIndexTrend[1:3]=df$SocialDistanceIndexTrend[4]
  df$SocialDistanceIndexTrend[(length(df$SocialDistanceIndexTrend)-3):length(df$SocialDistanceIndexTrend)]=df$SocialDistanceIndexTrend[length(df$SocialDistanceIndexTrend)-4]
  
  df$NewCasesTrend[1:3]=df$NewCasesTrend[4]
  df$NewCasesTrend[(length(df$NewCasesTrend)-3):length(df$NewCasesTrend)]=df$NewCasesTrend[length(df$NewCasesTrend)-4]
  
  df$DeathRateTrend[1:3]=df$DeathRateTrend[4]
  df$DeathRateTrend[(length(df$DeathRateTrend)-3):length(df$DeathRateTrend)]=df$DeathRateTrend[length(df$DeathRateTrend)-4]
  
  df$CovidExposerTrend[1:3]=df$CovidExposerTrend[4]
  df$CovidExposerTrend[(length(df$CovidExposerTrend)-3):length(df$CovidExposerTrend)]=df$CovidExposerTrend[length(df$CovidExposerTrend)-4]
  
  df$ActiveCasesTrend[1:3]=df$ActiveCasesTrend[4]
  df$ActiveCasesTrend[(length(df$ActiveCasesTrend)-3):length(df$ActiveCasesTrend)]=df$ActiveCasesTrend[length(df$ActiveCasesTrend)-4]
  
  #
  df$OutOfStateTripTrend[1:3]=df$OutOfStateTripTrend[4]
  df$OutOfStateTripTrend[(length(df$OutOfStateTripTrend)-3):length(df$OutOfStateTripTrend)]=df$OutOfStateTripTrend[length(df$OutOfStateTripTrend)-4]
  
  df$WorkTripTrend[1:3]=df$WorkTripTrend[4]
  df$WorkTripTrend[(length(df$WorkTripTrend)-3):length(df$WorkTripTrend)]=df$WorkTripTrend[length(df$WorkTripTrend)-4]
  
  df$ICUUtilizationTrend[1:3]=df$ICUUtilizationTrend[4]
  df$ICUUtilizationTrend[(length(df$ICUUtilizationTrend)-3):length(df$ICUUtilizationTrend)]=df$ICUUtilizationTrend[length(df$ICUUtilizationTrend)-4]
  
  df$HospitalBedUtilizationTrend[1:3]=df$HospitalBedUtilizationTrend[4]
  df$HospitalBedUtilizationTrend[(length(df$HospitalBedUtilizationTrend)-3):length(df$HospitalBedUtilizationTrend)]=df$HospitalBedUtilizationTrend[length(df$HospitalBedUtilizationTrend)-4]
  
  df$TestingCapacityTrend[1:3]=df$TestingCapacityTrend[4]
  df$TestingCapacityTrend[(length(df$TestingCapacityTrend)-3):length(df$TestingCapacityTrend)]=df$TestingCapacityTrend[length(df$TestingCapacityTrend)-4]
  
  df$TestingDoneTrend[1:3]=df$TestingDoneTrend[4]
  df$TestingDoneTrend[(length(df$TestingDoneTrend)-3):length(df$TestingDoneTrend)]=df$TestingDoneTrend[length(df$TestingDoneTrend)-4]
  
  df$VentilationShortageTrend[1:3]=df$VentilationShortageTrend[4]
  df$VentilationShortageTrend[(length(df$VentilationShortageTrend)-3):length(df$VentilationShortageTrend)]=df$VentilationShortageTrend[length(df$VentilationShortageTrend)-4]
  
  df$ImportedCasesTrend[1:3]=df$ImportedCasesTrend[4]
  df$ImportedCasesTrend[(length(df$ImportedCasesTrend)-3):length(df$ImportedCasesTrend)]=df$ImportedCasesTrend[length(df$ImportedCasesTrend)-4]
  
  
  covid_data_SIC$SocialDistanceIndexTrend[t] = formatDecimal(df$SocialDistanceIndexTrend)
  covid_data_SIC$NewCasesTrend[t] = formatDecimal(df$NewCasesTrend)
  covid_data_SIC$DeathRateTrend[t] = formatDecimal(df$DeathRateTrend)
  covid_data_SIC$CovidExposerTrend[t] = formatDecimal(df$CovidExposerTrend)
  covid_data_SIC$ActiveCasesTrend[t] = formatDecimal(df$ActiveCasesTrend)
  
  covid_data_SIC$OutOfStateTripTrend[t] = formatDecimal(df$OutOfStateTripTrend)
  covid_data_SIC$WorkTripTrend[t] = formatDecimal(df$WorkTripTrend)
  covid_data_SIC$ICUUtilizationTrend[t] = formatDecimal(df$ICUUtilizationTrend)
  covid_data_SIC$HospitalBedUtilizationTrend[t] = formatDecimal(df$HospitalBedUtilizationTrend)
  covid_data_SIC$TestingCapacityTrend[t] = formatDecimal(df$TestingCapacityTrend)
  covid_data_SIC$TestingDoneTrend[t] = formatDecimal(df$TestingDoneTrend)
  covid_data_SIC$VentilationShortageTrend[t] = formatDecimal(df$VentilationShortageTrend)
  covid_data_SIC$ImportedCasesTrend[t] = formatDecimal(df$ImportedCasesTrend)
  
  rm(df,trend_sdi,trend_nwc,trend_death,trend_exp,trend_act,trend_1,trend_2,trend_3,trend_4,trend_5,trend_6,trend_7,trend_8)
}
rm(tdf)

covid_data_SIC = as.data.frame(covid_data_SIC%>%group_by(CTFIPS,STFIPS)%>%mutate(CumNewCasesTrend=cumsum(NewCasesTrend)))
covid_data_SIC = as.data.frame(covid_data_SIC[,c(1:5,23,24,36,25:35)])

# Load state data
covid_state_data = read.csv(file.choose())
stCode_data = read.csv(file.choose())

covid_state_data = as.data.frame(covid_state_data[,c(1,2)]%>%distinct()%>%arrange(STFIPS))
covid_state_DS = as.data.frame(merge(covid_state_data, stCode_data, by.x = "STNAME", by.y = "state_code"))
rm(covid_state_data,stCode_data,covid_data)

covid_data = as.data.frame(merge(covid_data_SIC, covid_state_DS, by.x = "STFIPS", by.y = "STFIPS"))
rm(covid_data_SIC, covid_state_DS)

covid_data = as.data.frame(covid_data[,c(1,20,21,4,5,2,3,6:19)]%>%arrange(STFIPS, CTFIPS, DSFC))

write.csv(covid_data, "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/unm_county_trend.csv")

####################################### Preprocessing Done
#######################################################################
####################################### Trend analyzing

covid_data = read.csv("FromAnil/UNM/unm_county_trend.csv")

covid_lag_data = data.frame(STFIPS=as.integer(),
                            stateCode=as.character(),
                            stateName=as.character(),
                            CTFIPS=as.integer(),
                            countyName=as.character(),
                            lags=as.integer(),
                            coef_NCase_SDI=as.numeric(),
                            coef_TstDone_NCase=as.numeric())
df = as.data.frame(covid_data[,c(2,5)]%>%distinct())

for(i in 1:nrow(df)){
  t = which(covid_data$STFIPS==df$STFIPS[i] & covid_data$CTFIPS==df$CTFIPS[i])
  
  if(length(t)>0){
    x1=normalize(covid_data$NewCasesTrend[t], method="range")
    x2=normalize(covid_data$SocialDistanceIndexTrend[t], method="range")
    x3=normalize(covid_data$TestingDoneTrend[t], method="range")
    x4=normalize(covid_data$DeathRateTrend[t], method="range")
    x5=normalize(covid_data$VentilationShortageTrend[t], method="range")
    x6=normalize(covid_data$OutOfStateTripTrend[t], method="range")
    
    mdl_1=ccf(x,y, lag.max = 120, main="")
    tp_1 = which(mdl_1$lag>=0)
    
    matplot(covid_data$DSFC[t], cbind(x1,x2,x3,x4,x5,x6), col = c("red", "blue","purple","black","darkcyan","darkgreen"), type = 'l')
    
    x=normalize(covid_data$TestingDoneTrend[t], method="range")
    y=normalize(covid_data$NewCasesTrend[t], method="range")
    mdl_2=ccf(x,y, lag.max = 120, main="")
    tp_2 = which(mdl_2$lag>=0)
    
    tmpDS = data.frame(STFIPS=rep(covid_data$STFIPS[t][1], 121),
                       stateCode=rep(as.character(covid_data$STNAME[t][1]), 121),
                       stateName=rep(as.character(covid_data$State_name[t][1]), 121),
                       CTFIPS=rep(covid_data$CTFIPS[t][1], 121),
                       countyName=rep(covid_data$CTNAME[t][1], 121),
                       lags=mdl_1$lag[tp_1],
                       coef_NCase_SDI=mdl_1$acf[tp_1],
                       coef_TstDone_NCase=mdl_2$acf[tp_2])
  }
  
  
  matplot(tmpDS$lags, cbind(tmpDS$coef_NCase_SDI, tmpDS$coef_TstDone_NCase), pch = c(16,18), col = c("red", "blue"))
}






