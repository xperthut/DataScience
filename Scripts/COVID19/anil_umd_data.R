source("header.R")

##########################################################################################################################################################
## State wise preprocessing
##########################################################################################################################################################

covid_data = read.csv("FromAnil/UNM/Read/State_2020_9_9.csv")

# df = as.data.frame(covid_data[which(covid_data$STFIPS==48),])
# cor_df = cor(df[,c(5:10,12,25:40)], use = "pairwise.complete.obs")
# cor_df_abs = abs(cor_df)
# pheatmap(cor_df)
# pheatmap(cor_df_abs)

#stDS = as.data.frame(covid_data[,c(1,2)]%>%distinct()%>%arrange(STFIPS))


##########################################################################################################################################################
# Summary statistics
summary(covid_data)

# Drop columns with missing data
covid_data = as.data.frame(covid_data %>%select_if(~ !any(is.na(.))))

# Sort by date
covid_data$dts = as.numeric(convertToDateString(covid_data$date, frmt = "%m/%d/%Y"))
covid_data = as.data.frame(covid_data%>%arrange(STFIPS,dts))

# Generate Days Since Case Recorded (DSCR) numbers
covid_data$DSCR = 0
l = sort(unique(covid_data$STFIPS))
for(i in l){
  t = which(covid_data$STFIPS==as.numeric(i))
  
  if(length(t) > 0){
    d = as.character(covid_data$date[t[1]])
    # %m/%d/%Y
    covid_data$DSCR[t] = getDays(d, covid_data$date[t], fmt = "%m/%d/%Y")
    rm(d)
  }
  rm(t)
}

# Compute cumulative new cases per 1000 people
covid_data = as.data.frame(covid_data%>%group_by(STFIPS)%>%mutate(cumCases=cumsum(New.cases.1000.people))%>%arrange(cumCases))
covid_data = as.data.frame(covid_data%>%filter(cumCases>0))

# Generate Days Since First Case numbers
covid_data$DSFC = 0
l = sort(unique(covid_data$STFIPS))
for(i in l){
  t = which(covid_data$STFIPS==as.numeric(i))
  
  if(length(t) > 0){
    #d = as.character(covid_data$date[t[1]])
    # %m/%d/%Y
    #covid_data$DSFC[t] = getDays(d, covid_data$date[t], fmt = "%m/%d/%Y")
    #rm(d)
    
    d = min(as.numeric(covid_data$DSCR[t]))
    covid_data$DSFC[t] = covid_data$DSCR[t]-d+1
  }
  rm(t)
}

# par(mfrow=c(4,2))
# t=which(covid_data$STNAME=="NY");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "NY", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="NJ");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "NJ", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="VA");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "VA", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="GA");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "GA", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="CA");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "CA", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="AZ");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "AZ", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="TX");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "TX", xlab = "DSFC", ylab = "Exposure")
# t=which(covid_data$STNAME=="FL");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "FL", xlab = "DSFC", ylab = "Exposure")
# par(mfrow=c(1,1))
# t=which(covid_data$STNAME=="NM");plot(covid_data$DSFC[t], covid_data$COVID.exposure.1000.people[t], type='l', main = "NM", xlab = "DSFC", ylab = "Exposure")


# List all column names
colnames(covid_data)

# Rearrange columns
covid_data = as.data.frame(covid_data[,c(1,2,14,59,61,60,3,7,9,27,32:35,37,39,40:42)]%>%arrange(STFIPS,DSFC))

# Weekdays
covid_data$WeekDay=1+as.numeric(format(as.Date(covid_data$date, format = "%m/%d/%Y"),"%w"))
covid_data$WeekDayName=as.character(format(as.Date(covid_data$date, format = "%m/%d/%Y"),"%A"))
covid_data$Weeks=0

tmpState = unique(covid_data$STFIPS)
for(i in tmpState){
  t = which(covid_data$STFIPS==as.numeric(i))
  
  w=1
  for(j in t){
    covid_data$Weeks[j] = w
    if(covid_data$WeekDay[j]==7){
      w=w+1
    }
  }
}

#covid_data_w = as.data.frame(covid_data%>%group_by(STFIPS,STNAME,Weeks)%>%summarise(SocialIndex=median(Social.distancing.index), 
 #                                                                                   NewCases1000People=mean(New.cases.1000.people)))
#covid_data_w = as.data.frame(covid_data_w%>%group_by(STFIPS,STNAME)%>%mutate(cumSI=cumsum(SocialIndex),cumNewCases1000People=cumsum(NewCases1000People)))

# Separate weekdays with weekends
# 1: Saturday, Sunday
# 2: Monday
# 3: Tuesday-Friday
# covid_data$WeekDay.rank=0
# covid_data$WeekDay.rank[which(covid_data$WeekDay==7)] = 1 # Saturday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==1)] = 1 # Sunday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==2)] = 2 # Monday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==3)] = 3 # Tuesday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==4)] = 3 # Wednesday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==5)] = 3 # Thursday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==6)] = 3 # Friday

### Create exposour data from Covid_Exposer variable
covid_data$Daily.COVID.exposure.1000.people=covid_data$COVID.exposure.1000.people

stids = unique(covid_data$STFIPS)
for(st in stids){
  t = which(covid_data$STFIPS==as.numeric(st))
  
  prev = covid_data$COVID.exposure.1000.people[t][1]
  
  for(i in 2:length(covid_data$COVID.exposure.1000.people[t])){
    covid_data$Daily.COVID.exposure.1000.people[t][i] = covid_data$COVID.exposure.1000.people[t][i]-prev
    if(covid_data$Daily.COVID.exposure.1000.people[t][i]<0){
      covid_data$Daily.COVID.exposure.1000.people[t][i] = 0
    }else{
      prev = covid_data$COVID.exposure.1000.people[t][i]
    }
  }
}

t = which(covid_data$STFIPS==as.numeric(36))
matplot(covid_data$DSFC[t], cbind(covid_data$COVID.exposure.1000.people[t],cumsum(covid_data$Daily.COVID.exposure.1000.people[t])), type='l')

covid_data=as.data.frame(covid_data[,c(1:5,20:23,6:19)]%>%arrange(STFIPS,DSFC))
covid_data_SIC = as.data.frame(covid_data)

#covid_data_SIC = as.data.frame(covid_data_SIC%>%group_by(STFIPS, Weeks)%>%mutate(wdc=n(),norm.SI = getNormalizedData(Social.distancing.index))
#                               #%>%filter(wdc>5)
#                               )
#covid_data_SIC$norm.SI.rank=ifelse(covid_data_SIC$norm.SI>0.5, yes = 1, no = 2)

############ Seasonality check
# SDI
# t = which(covid_data_SIC$STNAME=="TX")
# sdits = ts(covid_data_SIC$Social.distancing.index[t], frequency = 7)
# plot.ts(sdits)
# plot(covid_data_SIC$DSFC[t[1:14]], covid_data_SIC$Social.distancing.index[t[1:14]], type='l')
# 
# t = which(covid_data_SIC$STNAME=="AZ")
# d=decompose(ts(covid_data_SIC$Social.distancing.index[t], frequency = 7))
# plot(d)
# 
# d=decompose(ts(covid_data_SIC$New.cases.1000.people[t], frequency = 7))
# plot(d)
# 
# d=decompose(ts(covid_data_SIC$COVID.death.rate[t], frequency = 7))
# plot(d)
# 
# plot(covid_data_SIC$DSCR[t], covid_data_SIC$COVID.death.rate[t], type = 'l')
# 
# ### Correlation coefficient analysis
# cor_df = cor(covid_data_SIC[,c(5,6,8:22)], use = "pairwise.complete.obs")
# cor_df_abs = abs(cor_df)
# pheatmap(cor_df)
# pheatmap(cor_df_abs)

################################################################################################
################################### Trend ###################################################
################################################################################################
## Create trend and use it as social distance

covid_data_trend = NULL

sid = unique(covid_data_SIC$STFIPS)
for(i in sid){
  t=which(covid_data_SIC$STFIPS==as.numeric(i))
  
  trend_sdi = decompose(ts(covid_data_SIC$Social.distancing.index[t], frequency = 7))
  trend_nwc = decompose(ts(covid_data_SIC$New.cases.1000.people[t], frequency = 7))
  trend_death = decompose(ts(covid_data_SIC$COVID.death.rate[t], frequency = 7))
  trend_exp = decompose(ts(cumsum(covid_data_SIC$Daily.COVID.exposure.1000.people[t]), frequency = 7))
  trend_exp_daily = decompose(ts(covid_data_SIC$Daily.COVID.exposure.1000.people[t], frequency = 7))
  trend_act = decompose(ts(covid_data_SIC$Active.cases.1000.people[t], frequency = 7))
  
  trend_1 = decompose(ts(covid_data_SIC$X..out.of.state.trips[t], frequency = 7))
  trend_2 = decompose(ts(covid_data_SIC$Work.trips.person[t], frequency = 7))
  trend_3 = decompose(ts(covid_data_SIC$X..ICU.utilization[t], frequency = 7))
  trend_4 = decompose(ts(covid_data_SIC$X..hospital.bed.utilization[t], frequency = 7))
  #trend_5 = decompose(ts(covid_data_SIC$Testing.capacity[t], frequency = 7))
  trend_6 = decompose(ts(covid_data_SIC$Tests.done.1000.people[t], frequency = 7))
  trend_7 = decompose(ts(covid_data_SIC$Ventilator.shortage[t], frequency = 7))
  trend_8 = decompose(ts(covid_data_SIC$Imported.COVID.cases[t], frequency = 7))
  
  df = data.frame("STFIPS"=covid_data_SIC$STFIPS[t[4:(length(t)-3)]],
                  "STNAME"=covid_data_SIC$STNAME[t[4:(length(t)-3)]],
                  "date"=covid_data_SIC$date[t[4:(length(t)-3)]],
                  "DSCR"=covid_data_SIC$DSCR[t[4:(length(t)-3)]],
                  "DSFC"=covid_data_SIC$DSFC[t[4:(length(t)-3)]],
                  "WeekDay"=covid_data_SIC$WeekDay[t[4:(length(t)-3)]],
                  "WeekDayName"=covid_data_SIC$WeekDayName[t[4:(length(t)-3)]],
                  "Weeks"=covid_data_SIC$Weeks[t[4:(length(t)-3)]],
                  "SDI"=trend_sdi$trend[4:(length(trend_sdi$trend)-3)],
                  "NewCases"=trend_nwc$trend[4:(length(trend_nwc$trend)-3)],
                  "DeathRate"=trend_death$trend[4:(length(trend_death$trend)-3)],
                  "CovidExposure"=trend_exp_daily$trend[4:(length(trend_exp_daily$trend)-3)],
                  "cCovidExposure"=trend_exp$trend[4:(length(trend_exp$trend)-3)],
                  "ActiveCases"=trend_act$trend[4:(length(trend_act$trend)-3)],
                  "OutOfStateTrip"=trend_1$trend[4:(length(trend_1$trend)-3)],
                  "WorkTrip"=trend_2$trend[4:(length(trend_2$trend)-3)],
                  "ICUUtilization"=trend_3$trend[4:(length(trend_3$trend)-3)],
                  "HospitalBedUtilization"=trend_4$trend[4:(length(trend_4$trend)-3)],
                  #"TestingCapacityTrend"=as.ts(trend_5),
                  "TestingDone"=trend_6$trend[4:(length(trend_6$trend)-3)],
                  "VentilationShortage"=trend_7$trend[4:(length(trend_7$trend)-3)],
                  "ImportedCases"=trend_8$trend[4:(length(trend_8$trend)-3)]
  )
  
  if(is.null(covid_data_trend)){
    covid_data_trend = as.data.frame(df)
  }else{
    covid_data_trend = as.data.frame(rbind(covid_data_trend, df))
  }
  
  rm(df,trend_sdi,trend_nwc,trend_death,trend_act,trend_exp,trend_exp_daily,trend_1,trend_2,trend_3,trend_4,trend_6,trend_7,trend_8)
  #rm(trend_5)
}

covid_data_trend = as.data.frame(covid_data_trend%>%group_by(STFIPS)%>%mutate(cSDI=cumsum(SDI/100), cumCases=cumsum(NewCases)))

cor_df = (cor(covid_data_trend[,c(5,6,8:23)], use = "pairwise.complete.obs"))
pheatmap(abs(cor_df))

#write.csv(covid_data_trend, "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/unm_state_trend.csv")

##################################################################################################################
## Split states into regions and divisions
stReg=read.csv("FromAnil/UNM/Read/state_code.csv")

covid_data = as.data.frame(merge(covid_data_trend, stReg, by = c("STFIPS","STNAME")))

covid_data = as.data.frame(covid_data[,c(1,2,24:28,3:23)]%>%arrange(STFIPS,DSFC))

write.csv(covid_data, "FromAnil/UNM/Write/Covid_state.csv")

# stReg$rColor=""
# stReg$dColor=""
# 
# # Region wise color
# t = which(stReg$Region=="South")
# stReg$rColor[t] = "red"
# 
# t = which(stReg$Region=="West")
# stReg$rColor[t] = "green3"
# 
# t = which(stReg$Region=="NorthEast")
# stReg$rColor[t] = "blue"
# 
# t = which(stReg$Region=="MidWest")
# stReg$rColor[t] = "orange4"
# 
# # Division wise color
# t = which(stReg$Division=="East South Central")
# stReg$dColor[t] = "red"
# 
# t = which(stReg$Division=="West South Central")
# stReg$dColor[t] = "red4"
# 
# t = which(stReg$Division=="East North Central")
# stReg$dColor[t] = "orange"
# 
# t = which(stReg$Division=="West North Central")
# stReg$dColor[t] = "orange4"
# 
# t = which(stReg$Division=="Pacific")
# stReg$dColor[t] = "green3"
# 
# t = which(stReg$Division=="Mountain")
# stReg$dColor[t] = "pink4"
# 
# t = which(stReg$Division=="New England")
# stReg$dColor[t] = "blue3"
# 
# t = which(stReg$Division=="South Atlantic")
# stReg$dColor[t] = "cyan4"
# 
# t = which(stReg$Division=="Middle Atlantic")
# stReg$dColor[t] = "purple2"
# 
# write.csv(stReg, "FromAnil/UNM/Read/state_code.csv")

##################################################################################################################
# Add state wise color
# stids = unique(covid_data_trend$STFIPS)
# cn = getColors(length(stids))
# covid_data_trend$stColor=""
# for(i in 1:length(stids)){
#   t = which(covid_data_trend$STFIPS==stids[i])
#   covid_data_trend$stColor[t] = cn[i]
# }
# 
# ## Split states based on time zoone
# covid_data_trend$TimeZone=""
# covid_data_trend$TimeZoneName=""
# covid_data_trend$TZColor=""
# 
# t = which(covid_data_trend$STNAME%in%c("WA","OR","CA","NV"))
# covid_data_trend$TimeZone[t]="PDT"
# covid_data_trend$TimeZoneName[t]="Pacific"
# covid_data_trend$TZColor[t]="blue"
# 
# t = which(covid_data_trend$STNAME%in%c("MT","ID","WY","UT","CO","AZ","NM"))
# covid_data_trend$TimeZone[t]="MDT"
# covid_data_trend$TimeZoneName[t]="Mountain"
# covid_data_trend$TZColor[t]="darkgreen"
# 
# t = which(covid_data_trend$STNAME%in%c("ND","SD","NE","KS","OK","TX","MN","IA","MO","AR","LA","WI","IL","TN","MS","AL"))
# covid_data_trend$TimeZone[t]="CDT"
# covid_data_trend$TimeZoneName[t]="Central"
# covid_data_trend$TZColor[t]="darkorange"
# 
# t = which(covid_data_trend$STNAME%in%c("MI","IN","OH","PA","NY","VT","ME","NH","MA","RI","CT","KY","NJ","DE","MD","WV","VA","NC","SC","GA","FL","DC"))
# covid_data_trend$TimeZone[t]="EST"
# covid_data_trend$TimeZoneName[t]="Eastern"
# covid_data_trend$TZColor[t]="red"
# 
# t = which(covid_data_trend$STNAME=="AK")
# covid_data_trend$TimeZone[t]="AKDT"
# covid_data_trend$TimeZoneName[t]="Alaska"
# covid_data_trend$TZColor[t]="purple"
# 
# t = which(covid_data_trend$STNAME=="HI")
# covid_data_trend$TimeZone[t]="HST"
# covid_data_trend$TimeZoneName[t]="Hawaii"
# covid_data_trend$TZColor[t]="darkcyan"
# 
# write.csv(covid_data_trend, "FromAnil/UNM/unm_state_trend.csv")
# 
# rm(covid_data, covid_data_SIC, covid_data_trend, cor_df, cor_df_abs)

##########################################################################################################################################################
##########################################################################################################################################################
### County wise preprocessing
##########################################################################################################################################################
##########################################################################################################################################################

covid_data = read.csv("FromAnil/UNM/Read/County_2020_9_1.csv")

# df = as.data.frame(covid_data[which(covid_data$STFIPS==48),])
# cor_df = cor(df[,c(5:10,12,25:40)], use = "pairwise.complete.obs")
# cor_df_abs = abs(cor_df)
# pheatmap(cor_df)
# pheatmap(cor_df_abs)

#stDS = as.data.frame(covid_data[,c(1,2)]%>%distinct()%>%arrange(STFIPS))


##########################################################################################################################################################
# Summary statistics
summary(covid_data)

# Drop columns with missing data
covid_data = as.data.frame(covid_data %>%select_if(~ !any(is.na(.))))

# Sort by date
#covid_data$dts = as.numeric(convertToDateString(covid_data$date, frmt = "%m/%d/%Y"))
#covid_data = as.data.frame(covid_data%>%arrange(STFIPS,CTFIPS,dts))

# Generate Days Since Case Recorded (DSCR) numbers
covid_data$DSCR = covid_data$date
# l = sort(unique(covid_data$STFIPS))
# for(i in l){
#   t = which(covid_data$STFIPS==as.numeric(i))
#   
#   if(length(t) > 0){
#     d = as.character(covid_data$date[t[1]])
#     # %m/%d/%Y
#     covid_data$DSCR[t] = getDays(d, covid_data$date[t], fmt = "%m/%d/%Y")
#     rm(d)
#   }
#   rm(t)
# }

# Compute cumulative new cases per 1000 people
covid_data = as.data.frame(covid_data%>%group_by(STFIPS,CTFIPS)%>%mutate(cumCases=cumsum(New.cases.1000.people))%>%arrange(cumCases))
covid_data = as.data.frame(covid_data%>%filter(cumCases>0))

# Generate Days Since First Case numbers
#covid_data$DSFC = 0
df=as.data.frame(covid_data[,c(2,4,63)]%>%distinct()%>%arrange(STFIPS, CTFIPS, DSCR))
df$DSFC=0

tmp = as.data.frame(df[,c(1,2)]%>%distinct())
for(i in 1:nrow(tmp)){
  t = which(df$STFIPS==tmp$STFIPS[i] & df$CTFIPS==tmp$CTFIPS[i])
  md = min(df$DSCR[t])
  
  df$DSFC[t]=df$DSCR[t]-md+1
}
rm(tmp)

## Merge
covid_data = as.data.frame(merge(covid_data, df, by=c("STFIPS", "CTFIPS", "DSCR")))

# List all column names
colnames(covid_data)

# # Weekdays
# covid_data$WeekDay=1+as.numeric(format(as.Date(covid_data$date, format = "%m/%d/%Y"),"%w"))
# covid_data$WeekDayName=as.character(format(as.Date(covid_data$date, format = "%m/%d/%Y"),"%A"))
# covid_data$Weeks=0
# 
# tmpState = unique(covid_data$STFIPS)
# for(i in tmpState){
#   t = which(covid_data$STFIPS==as.numeric(i))
#   
#   w=1
#   for(j in t){
#     covid_data$Weeks[j] = w
#     if(covid_data$WeekDay[j]==7){
#       w=w+1
#     }
#   }
# }

#covid_data_w = as.data.frame(covid_data%>%group_by(STFIPS,STNAME,Weeks)%>%summarise(SocialIndex=median(Social.distancing.index), 
#                                                                                   NewCases1000People=mean(New.cases.1000.people)))
#covid_data_w = as.data.frame(covid_data_w%>%group_by(STFIPS,STNAME)%>%mutate(cumSI=cumsum(SocialIndex),cumNewCases1000People=cumsum(NewCases1000People)))

# Separate weekdays with weekends
# 1: Saturday, Sunday
# 2: Monday
# 3: Tuesday-Friday
# covid_data$WeekDay.rank=0
# covid_data$WeekDay.rank[which(covid_data$WeekDay==7)] = 1 # Saturday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==1)] = 1 # Sunday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==2)] = 2 # Monday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==3)] = 3 # Tuesday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==4)] = 3 # Wednesday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==5)] = 3 # Thursday
# covid_data$WeekDay.rank[which(covid_data$WeekDay==6)] = 3 # Friday

covid_data=as.data.frame(covid_data[,c(1,2,5,61,62,64,3,6, 10, 12, 30, 36, 37, 38, 40, 42, 43, 44, 45)]%>%arrange(STFIPS,CTFIPS,DSFC))
covid_data_SIC = as.data.frame(covid_data)

#covid_data_SIC = as.data.frame(covid_data_SIC%>%group_by(STFIPS, Weeks)%>%mutate(wdc=n(),norm.SI = getNormalizedData(Social.distancing.index))
#                               #%>%filter(wdc>5)
#                               )
#covid_data_SIC$norm.SI.rank=ifelse(covid_data_SIC$norm.SI>0.5, yes = 1, no = 2)

############ Seasonality check
# SDI
# t = which(covid_data_SIC$STNAME=="TX")
# sdits = ts(covid_data_SIC$Social.distancing.index[t], frequency = 7)
# plot.ts(sdits)
# plot(covid_data_SIC$DSFC[t[1:14]], covid_data_SIC$Social.distancing.index[t[1:14]], type='l')
# 
# t = which(covid_data_SIC$STNAME=="AZ")
# d=decompose(ts(covid_data_SIC$Social.distancing.index[t], frequency = 7))
# plot(d)
# 
# d=decompose(ts(covid_data_SIC$New.cases.1000.people[t], frequency = 7))
# plot(d)
# 
# d=decompose(ts(covid_data_SIC$COVID.death.rate[t], frequency = 7))
# plot(d)
# 
# plot(covid_data_SIC$DSCR[t], covid_data_SIC$COVID.death.rate[t], type = 'l')
# 
# ### Correlation coefficient analysis
# cor_df = cor(covid_data_SIC[,c(5,6,8:22)], use = "pairwise.complete.obs")
# cor_df_abs = abs(cor_df)
# pheatmap(cor_df)
# pheatmap(cor_df_abs)

################################################################################################
################################### Trend ###################################################
################################################################################################
## Create trend and use it as social distance

covid_data_trend = NULL

# County which has at least 2 months data
tmp = as.data.frame(covid_data_SIC%>%group_by(STFIPS,CTFIPS)%>%summarise(obj=n())%>%filter(obj>59))

for(i in 1:nrow(tmp)){
  t = which(covid_data_SIC$STFIPS==tmp$STFIPS[i] & covid_data_SIC$CTFIPS==tmp$CTFIPS[i])
  
  trend_sdi = decompose(ts(covid_data_SIC$Social.distancing.index[t], frequency = 7))
  trend_nwc = decompose(ts(covid_data_SIC$New.cases.1000.people[t], frequency = 7))
  trend_death = decompose(ts(covid_data_SIC$COVID.death.rate[t], frequency = 7))
  trend_exp = decompose(ts(covid_data_SIC$COVID.exposure.1000.people[t], frequency = 7))
  trend_act = decompose(ts(covid_data_SIC$Active.cases.1000.people[t], frequency = 7))
  
  trend_1 = decompose(ts(covid_data_SIC$X..out.of.state.trips[t], frequency = 7))
  trend_2 = decompose(ts(covid_data_SIC$Work.trips.person[t], frequency = 7))
  trend_3 = decompose(ts(covid_data_SIC$X..ICU.utilization[t], frequency = 7))
  trend_4 = decompose(ts(covid_data_SIC$X..hospital.bed.utilization[t], frequency = 7))
  #trend_5 = decompose(ts(covid_data_SIC$Testing.capacity[t], frequency = 7))
  trend_6 = decompose(ts(covid_data_SIC$Tests.done.1000.people[t], frequency = 7))
  trend_7 = decompose(ts(covid_data_SIC$Ventilator.shortage[t], frequency = 7))
  trend_8 = decompose(ts(covid_data_SIC$Imported.COVID.cases[t], frequency = 7))
  
  df = data.frame("STFIPS"=covid_data_SIC$STFIPS[t[4:(length(t)-3)]],
                  "CTFIPS"=covid_data_SIC$CTFIPS[t[4:(length(t)-3)]],
                  "CTNAME"=covid_data_SIC$CTNAME[t[4:(length(t)-3)]],
                  "Latitude"=covid_data_SIC$Latitude[t[4:(length(t)-3)]],
                  "Longitude"=covid_data_SIC$Longitude[t[4:(length(t)-3)]],
                  "DSCR"=covid_data_SIC$DSCR[t[4:(length(t)-3)]],
                  "DSFC"=covid_data_SIC$DSFC[t[4:(length(t)-3)]],
                  "SDI"=trend_sdi$trend[4:(length(trend_sdi$trend)-3)],
                  "NewCases"=trend_nwc$trend[4:(length(trend_nwc$trend)-3)],
                  "DeathRate"=trend_death$trend[4:(length(trend_death$trend)-3)],
                  "CovidExposer"=trend_exp$trend[4:(length(trend_exp$trend)-3)],
                  "ActiveCases"=trend_act$trend[4:(length(trend_act$trend)-3)],
                  "OutOfStateTrip"=trend_1$trend[4:(length(trend_1$trend)-3)],
                  "WorkTrip"=trend_2$trend[4:(length(trend_2$trend)-3)],
                  "ICUUtilization"=trend_3$trend[4:(length(trend_3$trend)-3)],
                  "HospitalBedUtilization"=trend_4$trend[4:(length(trend_4$trend)-3)],
                  #"TestingCapacityTrend"=as.ts(trend_5),
                  "TestingDone"=trend_6$trend[4:(length(trend_6$trend)-3)],
                  "VentilationShortage"=trend_7$trend[4:(length(trend_7$trend)-3)],
                  "ImportedCases"=trend_8$trend[4:(length(trend_8$trend)-3)]
  )
  
  if(is.null(covid_data_trend)){
    covid_data_trend = as.data.frame(df)
  }else{
    covid_data_trend = as.data.frame(rbind(covid_data_trend, df))
  }
  
  rm(df,trend_sdi,trend_nwc,trend_death,trend_act,trend_exp,trend_1,trend_2,trend_3,trend_4,trend_6,trend_7,trend_8)
  #rm(trend_5)
}

#write.csv(covid_data_trend, "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/unm_state_trend.csv")

covid_data_trend = as.data.frame(covid_data_trend%>%group_by(STFIPS)%>%mutate(cSDI=cumsum(SDI), cumCases=cumsum(NewCases)))

#cor_df = (cor(covid_data_trend[,c(5,6,8:22)], use = "pairwise.complete.obs"))
#pheatmap(cor_df)

##################################################################################################################
## Split states into regions and divisions
stReg=read.csv("FromAnil/UNM/Read/state_code.csv")

covid_data = as.data.frame(merge(covid_data_trend, stReg, by = c("STFIPS")))

covid_data = as.data.frame(covid_data[,c(1,22:27,2:21)]%>%arrange(STFIPS,CTFIPS,DSFC))

write.csv(covid_data, "FromAnil/UNM/Write/Covid_county.csv")


################ PLot
p<-ggplot(covid_data, aes(x = Longitude, y = Latitude,
                         colour = Division#,shape = Region
                         )) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=25), 
        axis.text.x = element_text(size=rel(1)), 
        axis.title.y = element_text(size=25),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(3.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  #ggtitle("USA counties ") +
  labs(x = "Longitude", y = "Latitude")

dev.set(which = dev.next())
while (!is.null(dev.list()))  dev.off()
pdf(file=paste("FromAnil/UNM/Write/allCountiesDivision.pdf",sep=""))
print(p)
while (!is.null(dev.list()))  dev.off()







############################################################################################################
################## Read trend data
covid_data = read.csv("FromAnil/UNM/unm_state_trend.csv")
covid_data = as.data.frame(covid_data[,-c(1)])

# Weekly data
covid_data_weekly=as.data.frame(covid_data%>%group_by(STFIPS, STNAME, Weeks)%>%summarise(NewCases=mean(NewCasesTrend),
                                                                                         SDI=mean(SocialDistanceIndexTrend)/100,
                                                                                         CovidExposer=mean(CovidExposerTrend),
                                                                                         Death=mean(DeathRateTrend),
                                                                                         TestingDone=mean(TestingDoneTrend)))
# Create cumulative new cases and SDI
covid_data_weekly = as.data.frame(covid_data_weekly%>%group_by(STFIPS)%>%mutate(cNC=cumsum(NewCases), cSDI=cumsum(SDI)))


# Plot cumulative new case VS cumulative SDI
t=which(covid_data$STNAME=="AL")
matplot(covid_data$Weeks[t], cbind(covid_data$cumCases[t],covid_data$cSDI[t]), type = 'l', col=c("red","blue"))

t=which(covid_data_weekly$STNAME=="AL")
plot(covid_data_weekly$Weeks[t], covid_data_weekly$cNC[t]/covid_data_weekly$cSDI[t], type='l')
matplot(covid_data_weekly$Weeks[t], cbind(covid_data_weekly$cNC[t],covid_data_weekly$cSDI[t]), type = 'l', col=c("red","blue"))

df = covid_data_weekly%>%select(STNAME, cNC, cSDI)%>%filter(STNAME=="AL")
mdl<-lm(cNC~cSDI, df)
plot(mdl)

plot(covid_data$cSDI, covid_data$cumCases, col=covid_data$TZColor)


matplot(covid_data$Weeks[t], cbind(normalize(covid_data$cumCases[t], method = "range"), normalize(covid_data$cSDI[t], method = "range")), type = 'l', col=c("red","blue"))

scatterplot3d(covid_data$Weeks[t], covid_data$cSDI[t], covid_data$cumCases[t], type = 'l')


##################################################################################################################################################################
## Create growth rate (daily and weekly)
covid_data_trend = as.data.frame(covid_data_trend %>%group_by(STFIPS)%>%mutate(
  daily_lag = DSFC-lag(DSFC),
  daily_growth = (Cum.New.cases.1000.people-lag(Cum.New.cases.1000.people))/ifelse(lag(Cum.New.cases.1000.people)==0.0, yes = 1, no=lag(Cum.New.cases.1000.people)),
  daily_growth_rate = daily_growth/daily_lag,
  weekly_lag = DSFC-lag(DSFC, n = 7),
  weekly_growth = (Cum.New.cases.1000.people-lag(Cum.New.cases.1000.people, n = 7))/ifelse(lag(Cum.New.cases.1000.people, n = 7)==0.0, yes = 1, no=lag(Cum.New.cases.1000.people, n = 7)),
  weekly_growth_rate = weekly_growth/weekly_lag,
))

# Set the initial values with the first value
sid = unique(covid_data_trend$STFIPS)
for(i in sid){
  t=which(covid_data_trend$STFIPS==as.numeric(i))
  covid_data_trend$daily_lag[t[1]]=covid_data_trend$daily_lag[t[2]]
  covid_data_trend$daily_growth[t[1]]=covid_data_trend$daily_growth[t[2]]
  covid_data_trend$daily_growth_rate[t[1]]=covid_data_trend$daily_growth_rate[t[2]]
  
  covid_data_trend$weekly_lag[t[1:7]]=covid_data_trend$weekly_lag[t[8]]
  covid_data_trend$weekly_growth[t[1:7]]=covid_data_trend$weekly_growth[t[8]]
  covid_data_trend$weekly_growth_rate[t[1:7]]=covid_data_trend$weekly_growth_rate[t[8]]
}

rm(covid_data, covid_data_SIC, covid_data_trend)

################################################################################################
##################################### End ####################################################
################################################################################################

covid_data_trend = read.csv("FromAnil/UNM/unm_state_trend.csv")

t=which(covid_data_trend$STNAME=='WA')
par(mfrow=c(2,1))
plot(covid_data_trend$DSFC[t], covid_data_trend$SocialDistanceIndexTrend[t], type='o', main="Washington State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data_trend$DSFC[t], covid_data_trend$NewCasesTrend[t], type='o', xlab="Date Since First Case (DSFC)", ylab="New Cases daily growth rate")
par(mfrow=c(1,1))

t=which(covid_data_trend$STNAME=='GA')
par(mfrow=c(4,1))
plot(covid_data_trend$DSFC[t], covid_data_trend$SocialDistanceIndexTrend[t], type='o', main="Georgia State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data_trend$DSFC[t], covid_data_trend$Cum.New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Cum.New Cases per 1k")
plot(covid_data_trend$DSFC[t], covid_data_trend$daily_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth rate")
plot(covid_data_trend$DSFC[t], covid_data_trend$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data_trend$STNAME=='TX')
par(mfrow=c(4,1))
plot(covid_data_trend$DSFC[t], covid_data_trend$SocialDistanceIndexTrend[t], type='o', main="Texas State", xlab="Date Since First Case (DSFC)", ylab="Social Distance Index")
plot(covid_data_trend$DSFC[t], covid_data_trend$Cum.New.cases.1000.people[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Cum.New Cases per 1k")
plot(covid_data_trend$DSFC[t], covid_data_trend$daily_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Daily cum.new case growth rate")
plot(covid_data_trend$DSFC[t], covid_data_trend$weekly_growth_rate[t], type='o', xlab="Date Since First Case (DSFC)", ylab="Weekly cum.new case growth rate")
par(mfrow=c(1,1))

t=which(covid_data_trend$STNAME=='GA');ccf(covid_data_trend$NewCasesTrend[t],covid_data_trend$SocialDistanceIndexTrend[t], lag.max = 150, main="Georgia: New case(t-a), SDI(t)")
t=which(covid_data_trend$STNAME=='GA');ccf(covid_data_trend$ICUUtilizationTrend[t],covid_data_trend$SocialDistanceIndexTrend[t], lag.max = 150, main="Georgia: ICU utilization(t-a), SDI(t)")

t=which(covid_data_trend$STNAME=='TX');ccf(covid_data_trend$NewCasesTrend[t],covid_data_trend$SocialDistanceIndexTrend[t], lag.max = 150, main="Texas: New case(t-a), SDI(t)")
t=which(covid_data_trend$STNAME=='TX');ccf(covid_data_trend$ICUUtilizationTrend[t],covid_data_trend$SocialDistanceIndexTrend[t], lag.max = 150, main="Texas: ICU utilization(t-a), SDI(t)")

t=which(covid_data_trend$STNAME=='AZ');ccf(as.ts(covid_data_trend$SocialDistanceIndexTrend[t]), as.ts(covid_data_trend$daily_growth_rate[t]))

dfccf=ccf(as.ts(covid_data_trend$SocialDistanceIndexTrend[t]), as.ts(covid_data_trend$HospitalBedUtilizationTrend[t]))
plot(dfccf$lag, dfccf$acf)

t=which(covid_data_SIC$STNAME=='WA' & covid_data_SIC$norm.SI.rank>1);plot(covid_data_SIC$DSFC[t], covid_data_SIC$Social.distancing.index[t], type = 'o')
t=which(covid_data_SIC$STNAME=='WA');plot(covid_data_SIC$Cum.New.cases.1000.people[t], covid_data_SIC$norm.SI[t], type = 'o')
plot(covid_data_SIC$DSFC, covid_data_SIC$norm.SI)

# Timeseries analysis of social distance
s = "Social.distancing.index"
st="TX"
t=which(covid_data$STNAME==st)
a = covid_data$Social.distancing.index[t]
trend_df = ma(as.ts(a), order = 7, centre = T)
par(mfrow=c(2,1))
plot(as.ts(a), main=st, xlab="Date Since First Case (DSFC)", ylab=s)
lines(trend_df, col="blue")
plot(as.ts(trend_df), main="Trend", xlab="Date Since First Case (DSFC)", ylab=s)
par(mfrow=c(1,1))

scatter3D(covid_data_SIC$DSFC, covid_data_SIC$Social.distancing.trend, covid_data_SIC$Cum.New.cases.1000.people,bty = "g",theta = 75, phi = 20)
scatter3D(covid_data_SIC$DSFC, covid_data_SIC$Social.distancing.trend, covid_data_SIC$New.cases.1000.people,bty = "g")

t=which(covid_data_SIC$STNAME=='WA')
df = data.frame("DSFC"=covid_data$DSFC[t],"Social.index"=as.ts(covid_data$Social.distancing.index[t]), "trend"=as.ts(trend_df))
df$trend[1:3]=df$trend[4]
df$trend[(length(df$trend)-3):length(df$trend)]=df$trend[length(df$trend)-4]



df = read.csv(file.choose())
cr = cor(df[,c(5,6,8:19)], use = "pairwise.complete.obs")
cr_abs =abs(cr)
pheatmap(cr)
pheatmap(cr_abs)

trend_df = ma(as.ts(covid_data$Social.distancing.index), order = 7, centre = T)
par(mfrow=c(2,1))
plot(as.ts(covid_data$Social.distancing.index))
lines(trend_df, col="blue")
plot(as.ts(trend_df))
par(mfrow=c(1,1))

# Plot
# Timeseries analysis of social distance
st = ""
par(mfrow=c(2,1))
plot(covid_data_SIC$DSFC, covid_data_SIC$NewCasesTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="New.Cases.Trend")
plot(covid_data_SIC$DSFC, covid_data_SIC$SocialDistanceIndexTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Social.Distance.Index.Trend")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(covid_data_SIC$DSFC, covid_data_SIC$SocialDistanceIndexTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Social.Distance.Index.Trend")
plot(covid_data_SIC$DSFC, covid_data_SIC$DeathRateTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Death.Rate.Trend")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(covid_data_SIC$DSFC, covid_data_SIC$SocialDistanceIndexTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Social.Distance.Index.Trend")
plot(covid_data_SIC$DSFC, covid_data_SIC$SocialDistanceIndexTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Social.Distance.Index.Trend")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(covid_data_SIC$DSFC, covid_data_SIC$SocialDistanceIndexTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Social.Distance.Index.Trend")
plot(covid_data_SIC$DSFC, covid_data_SIC$CovidExposerTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Covid.Exposer.Trend")
par(mfrow=c(1,1))

par(mfrow=c(2,1))
plot(covid_data_SIC$DSFC, covid_data_SIC$SocialDistanceIndexTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Social.Distance.Index.Trend")
plot(covid_data_SIC$DSFC, covid_data_SIC$ActiveCasesTrend, main=st, xlab="Date Since First Case (DSFC)", ylab="Active.Cases.Trend")
par(mfrow=c(1,1))



# cn=colnames(covid_data)
# ncn=c()
# for(i in cn){
#   r = replaceStringUsingPattern(d=as.character(i), pattern = "\\.\\.", replace = "")
#   r = replaceStringUsingPattern(d=as.character(r), pattern = "\\.", replace = "")
#   
#   if(length(ncn)==0){
#     ncn = c(r)
#   }else ncn = append(ncn, r)
# }
# colnames(covid_data)<-ncn
write.csv(covid_data[,-1], "/Users/methun/self/Research/Dataset/COVID-19/FromAnil/UNM/unm_state.csv")

par(mfrow=c(3,1))
df = covid_data%>%filter(STFIPS==53)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Washington")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

df = covid_data%>%filter(STFIPS==53 & DSFC>28 & DSFC<71)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social distance index", main = as.character(df$STNAME[1]), type='o')

par(mfrow=c(3,1))
df = covid_data%>%filter(STFIPS==53 & DSFC>65)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Washington")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
df = covid_data%>%filter(STFIPS==13 & DSFC>65)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Georgia")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
df = covid_data%>%filter(STFIPS==36 & DSFC>65)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "New York")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
df = covid_data%>%filter(STNAME=="NJ" & DSFC>65)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "New Jersey")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
df = covid_data%>%filter(STNAME=="FL" & DSFC>65)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Florida")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
df = covid_data%>%filter(STNAME=="TX" & DSFC>65)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Texas")
plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
df = covid_data%>%filter(STFIPS==12)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Florida")
#plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000", main = "Florida")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000", main = "Florida")

df = covid_data%>%filter(STFIPS==48)
plot(df$DSFC, df$Social.distancing.index, xlab = "Days since first case", ylab = "Social index", main = "Texas")
#plot(df$DSFC, df$Cum.New.cases.1000.people, xlab = "Days since first case", ylab = "cum.New cases per 1000", main = "Texas")
plot(df$DSFC, df$New.cases.1000.people, xlab = "Days since first case", ylab = "New cases per 1000", main = "Texas")
par(mfrow=c(1,1))


par(mfrow=c(2,1))
t = which(covid_data$STNAME=="CA")
plot(covid_data$DSFC[t], covid_data$SocialDistanceIndexTrend[t], type='o')
plot(covid_data$DSFC[t], covid_data$NewCasesTrend[t], type='o')
par(mfrow=c(1,1))

####################################
## Parameter estimation
# Parameter estimation for DBSCAN
nw = c(1:15)
ac=c()
m = max(covid_data$DSFC)

for(j in nw){
  w = ceiling(m/j)
  rt = seq(1, m+w, w)
  
  # cases
  l=c()
  for(i in 1:length(rt)){
    y = covid_data$Cum.New.cases.1000.people[which(covid_data$DSFC>= rt[i-1] & covid_data$DSFC<rt[i])]
    if(length(l)==0) l = sd(y)
    else l = append(l, sd(y))
  }
  l = na.exclude(l)
  if(length(ac)==0) ac=mean(l)
  else ac = append(ac, mean(l))
}

plot(nw, ac)

dt = seq(min(covid_data_SIC$DSFC), max(covid_data_SIC$DSFC)+5, 5)
td = seq(min(covid_data_SIC$Social.distancing.trend), max(covid_data_SIC$Social.distancing.trend)+6, 6)
l=c()

for(i in 2:length(dt)){
  for(j in 2:length(td)){
    t = which(covid_data_SIC$DSFC>=dt[i-1] & covid_data_SIC$DSFC<dt[i] & covid_data_SIC$Social.distancing.trend>=td[j-1] & covid_data_SIC$Social.distancing.trend<td[j])
    if(length(t)>0){
      k = sd(covid_data_SIC$Cum.New.cases.1000.people[t])
      if(!is.na(k)){
        if(length(l)==0){
          l = c(k)
        }
        else{
          l=append(l, k)
        } 
      }
    }
  }
}




####################################
i=2;plot(covid_data$Social.distancing.index[which(covid_data$STFIPS==i)], covid_data$Cum.New.cases.1000.people[which(covid_data$STFIPS==i)], 
         #col=covid_data$STFIPS[which(covid_data$STFIPS==i)],
         xlab = "Social index", ylab = "Cum.New cases per 1000")

tmp = as.data.frame(covid_data[,c(1,2)]%>%distinct()%>%arrange(STFIPS))

# State: WA
df = as.data.frame(covid_data[which(covid_data$STFIPS==53),c(3:10)])
pairs(df, pch = 19)
cordf = cor(df, method = "pearson")
cordf_abs = abs(cordf)
pheatmap(cordf)
pheatmap(cordf_abs)

# State: GA
df = as.data.frame(covid_data[which(covid_data$STFIPS==13),c(3:10)])
pairs(df, pch = 19)
cordf = cor(df, method = "pearson")
cordf_abs = abs(cordf)
pheatmap(cordf)
pheatmap(cordf_abs)

# State: NY
df = as.data.frame(covid_data[which(covid_data$STFIPS==36),c(3:10)])
pairs(df, pch = 19)
cordf = cor(df, method = "pearson")
cordf_abs = abs(cordf)
pheatmap(cordf)
pheatmap(cordf_abs)

# State: All
df = as.data.frame(covid_data[,c(3:10)])
pairs(df, pch = 19)
cordf = cor(df, method = "pearson")
cordf_abs = abs(cordf)
pheatmap(cordf)
pheatmap(cordf_abs)

covid_data$County = ""
covid_data$State = ""

for(i in 1:nrow(covid_data)){
  d = strsplit(as.character(covid_data$CTNAME[i]), " ")[[1]]
  k = length(d)-1
  cn = ""
  for(j in 1:k){
    if(j>1) cn = paste(cn, " ", sep = "")
    cn = paste(cn, d[i], sep = "")
  }
  covid_data$County[i] = cn
}

# King county, WA
df1 = as.data.frame(covid_data%>%filter(covid_data$CTFIPS==53033))
pairs(df1[,c(4:11)], pch = 19)

# Flton county, GA
df1 = as.data.frame(covid_data%>%filter(covid_data$CTFIPS==13121))
pairs(df1[,c(4:11)], pch = 19)

# another dataset
tmp = read.csv(file.choose())

# List of FIPS, county and State
tmp = as.data.frame(tmp[-which(is.na(tmp$fips)),])
dt = as.data.frame(tmp[,c(4,2,3)]%>%distinct()%>%filter(county!="unknown")%>%arrange(fips))

# Merge county name and state name based on FIPS
df = merge(covid_data, dt, by.x = "CTFIPS", by.y = "fips")

# Non-temporal variables
dntf = as.data.frame(df[,c(1,59,60,)])

a = sort(unique(covid_data$STFIPS))
b = sort(unique(t$fips))



x=seq(1,11,1)
a=0
for(i in x){
  a = a + ((i*choose(11,i))/(choose(20, i)))
}
a

b=0
for(i in x){
  b = b + ((i*i*choose(11,i))/(choose(20, i)))
}
b



########################################################################################################################################
############################################ Data Incubator exam ############################################################################################
########################################################################################################################################

r = read.csv(file.choose())
r$sdts=convertToDateString(r$StartDate)
r$edts=convertToDateString(r$EndDate)
df = as.data.frame(r%>%filter(sdts>=20100101 & edts<=20191231 & !is.na(ContractAmount) & ContractAmount>0))
rm(r)


# what is the total sum of contract amounts?
sca = sum(df$ContractAmount)

temp = as.data.frame(df[which(df$AgencyName=="Citywide Administrative Services"),])
tmp_ag = as.data.frame(temp%>%group_by(VendorName)%>%summarize(sumCA = sum(ContractAmount), freq=n())%>%arrange(-sumCA))

total_contract = sum(tmp_ag$freq)
top_50_contracts = sum(tmp_ag$freq[1:50])

# What proportion of the total number of contracts in the data set were awarded to the top 50 vendors?
top_50_contracts/total_contract

# Consider only contracts with in the categories of Construction Related Services and Construction/Construction Services.
tmp_cat = as.data.frame(df
                        %>%filter(CategoryDescription=="Construction Related Services" | CategoryDescription=="Construction/Construction Services")
                        %>%filter(grepl("(CENTRAL|WASHINGTON SQUARE) PARK\\s*", ShortTitle))
                        )

# What is the ratio of total construction and contruction-related expenditure for the Central Park contracts compared to the Washington Square Park contracts?
central_park_expenditure = 12864125.60 + 200000.00 = 13064125.6
wa_square_park_expenditure = 8939669.24
ratio = 13064125.6/8939669.24=1.461365656

##
temp = as.data.frame(df[which(df$AgencyName=="Citywide Administrative Services" & df$CategoryDescription=="Goods"),])
temp$year = as.integer(as.integer(temp$sdts)/10000)
temp_df = as.data.frame(temp%>%group_by(year)%>%summarise(expenditure=sum(ContractAmount)))
model = lm(expenditure~year, temp_df)
summary(model)$r.squared

## Contract awared by each agency
df_agency_contract = as.data.frame(df%>%group_by(AgencyName)%>%summarise(contracts=n(), contractAmount=sum(ContractAmount), meanCAperContract=contractAmount/contracts)%>%arrange(-contracts))
df_agency_contract_t5 = sort(df_agency_contract$meanCAperContract[1:5], decreasing = T)
df_agency_contract_t5[1]/df_agency_contract_t5[2]

# Do agencies publish procurement notices uniformly throughout the week? 
# As an example, consider the agency of Parks and Recreation. For this agency, 
# compute the weekday for which each notice was published, and perform a Chi-squared test on
# the null hypothesis that each weekday occurs equally often. Report the value of the test statistic.

df$weekcday=as.numeric(format(as.Date(df$StartDate, format = "%m/%d/%Y"),"%w"))
df$weekday=as.character(format(as.Date(df$StartDate, format = "%m/%d/%Y"),"%A"))
df_park_recreation = as.data.frame(df%>%filter(AgencyName=="Parks and Recreation")%>%arrange(sdts))
df_agw=as.data.frame(df%>%group_by(AgencyName, weekcday, weekday)%>%summarise(freq=n()))

# Create new data frame based on frequency
aname = unique(df$AgencyName)
# Create table like:
# AgencyName | Monday | Tuesday | Wednesday | Thursday | Friday
#---------------------------------------------------------------
# abc        | 34     |45       |65         |0         |2
#---------------------------------------------------------------
df_freq = data.frame(AgencyName=sort(aname), Monday=rep(0, length(aname)), Tuesday=rep(0, length(aname)), Wednesday=rep(0, length(aname)), Thursday=rep(0, length(aname)), Friday=rep(0, length(aname)))

for(i in 1:nrow(df_freq)){
  t = df_agw%>%filter(AgencyName==df_freq$AgencyName[i])
  for(j in 1:nrow(t)){
    df_freq[i,1+t$weekcday[j]] = t$freq[j]
  }
  rm(t)
}

# Chi2 test
chisq <- chisq.test(df_freq[2:6], simulate.p.value = TRUE)
chisq

# Get month from start date
df$month=as.numeric(format(as.Date(df$StartDate, format = "%m/%d/%Y"),"%m"))
df$year=as.numeric(format(as.Date(df$StartDate, format = "%m/%d/%Y"),"%Y"))

# Compute monthly expenditure of an agency
df_agency_monthly_expenditure = as.data.frame(df%>%group_by(AgencyName, year, month)%>%summarise(Expenditure=sum(ContractAmount)))

# Auto correlation analysis
m<-acf(df_agency_monthly_expenditure$Expenditure, lag.max = 12)
m$acf[13]

# Extract zip codes
df$zip=str_extract(df$VendorAddress, "\\d{5}")

# NY city zip
NY_zip = as.character(unique(c(10453, 10457, 10460, 10458, 10467, 10468, 10451, 10452, 10456, 10454, 10455, 10459, 10474, 10463, 10471,10466, 
           10469, 10470, 10475,10461, 10462,10464, 10465, 10472, 10473,11212, 11213, 11216, 11233, 11238,11209, 11214, 11228,
           11204, 11218, 11219, 11230,11234, 11236, 11239,11223, 11224, 11229, 11235,11201, 11205, 11215, 11217, 11231,
           11203, 11210, 11225, 11226,11207, 11208,11211, 11222,11220, 11232,11206, 11221, 11237,10026, 10027, 10030, 10037, 10039,
           10001, 10011, 10018, 10019, 10020, 10036,10029, 10035,10010, 10016, 10017, 10022,10012, 10013, 10014,10004, 10005, 10006, 10007, 10038, 10280,
           10002, 10003, 10009,10021, 10028, 10044, 10065, 10075, 10128,10023, 10024, 10025,10031, 10032, 10033, 10034, 10040,
           11361, 11362, 11363, 11364,11354, 11355, 11356, 11357, 11358, 11359, 11360,11365, 11366, 11367,11412, 11423, 11432, 11433, 11434, 11435, 11436,
           11101, 11102, 11103, 11104, 11105, 11106,11374, 11375, 11379, 11385,11691, 11692, 11693, 11694, 11695, 11697,
           11004, 11005, 11411, 11413, 11422, 11426, 11427, 11428, 11429,11414, 11415, 11416, 11417, 11418, 11419, 11420, 11421,
           11368, 11369, 11370, 11372, 11373, 11377, 11378,10302, 10303, 10310,10306, 10307, 10308, 10309, 10312,10301, 10304, 10305,10314)))


df$InNY = df$zip%in%NY_zip
df_2018 = as.data.frame(df%>%filter(year==2018)%>%group_by(InNY)%>%summarise(TotalExpenditure=sum(ContractAmount)))

# Proportional expenditure of NY vs out of NY companies
df_2018$TotalExpenditure[which(df_2018$InNY==T)]/df_2018$TotalExpenditure[which(df_2018$InNY==F)]



## CREATE NEWIGHBOR COUNTIES
ca = read.csv("FromAnil/UNM/Read/county_adjacency.csv", sep = '|', header = F)
adjCnty = matrix(0L, nrow = nrow(ca), ncol = ncol(ca)+1)

for(i in 1:nrow(ca)){
  for(j in 1:ncol(ca)){
    a = as.character(ca[i,j])
    b=strsplit(a, '\t')[[1]]
    if(length(b)>0){
      if(j==1){
        if(length(b)==2){
          adjCnty[i,1]=as.numeric(b[2])
          adjCnty[i,2]=as.numeric(b[2])
        }else if(length(b)==4){
          adjCnty[i,1]=as.numeric(b[2])
          adjCnty[i,2]=as.numeric(b[4])
        }
      }else{
        adjCnty[i,(j+1)]=as.numeric(b[length(b)])
      } 
    }
  }
}

write.csv(adjCnty, "FromAnil/UNM/Read/CTFIPS_adjacency.csv")

l=c()
for(i in 1:nrow(adjCnty)){
  for(j in 1:ncol(adjCnty)){
    l = append(l, adjCnty[i,j])
  }
}
length(l)
l = sort(unique(l))
l = l[-c(1)]

adjMat = matrix(0L, nrow = length(l)+1, ncol = length(l)+1)
for(i in 2:nrow(adjMat)){
  adjMat[i,1]=l[i-1]
}

for(i in 2:ncol(adjMat)){
  adjMat[1,i]=l[i-1]
}

for(i in 2:nrow(adjMat)){
  adjMat[i,i]=1
}

adjCntyMat = as.matrix(adjCnty)
for(i in 1:nrow(adjCntyMat)){
  for(j in 1:ncol(adjCntyMat)){
    if(adjCntyMat[i,j]>0){
      adjCntyMat[i,j] = which(l==adjCntyMat[i,j])
    }
  }
}

for(i in 1:nrow(adjCntyMat)){
  for(j in 2:ncol(adjCntyMat)){
    if(adjCntyMat[i,j]>0){
      adjMat[adjCntyMat[i,1]+1, adjCntyMat[i,j]+1]=1
    }
  }
}

write.csv(adjMat, "FromAnil/UNM/Read/CTFIPS_adjacency_matrix.csv")

g = graph.adjacency(adjMat, mode = "undirected")
tkplot(g)


par(mfrow=c(3,2))
ax=append(seq(96,227,20),227)
t=which(covid_county$CTFIPS==1001);plot(covid_county$DSCR[t], covid_county$NewCases[t]*1000, type='l', xaxt="n", main="1001");axis(1, at = ax, labels = ax)
t=which(covid_county$CTFIPS==1021);plot(covid_county$DSCR[t], covid_county$NewCases[t]*1000, type='l', xaxt="n", main="1021");axis(1, at = ax, labels = ax)
t=which(covid_county$CTFIPS==1047);plot(covid_county$DSCR[t], covid_county$NewCases[t]*1000, type='l', xaxt="n", main="1047");axis(1, at = ax, labels = ax)
t=which(covid_county$CTFIPS==1051);plot(covid_county$DSCR[t], covid_county$NewCases[t]*1000, type='l', xaxt="n", main="1051");axis(1, at = ax, labels = ax)
t=which(covid_county$CTFIPS==1085);plot(covid_county$DSCR[t], covid_county$NewCases[t]*1000, type='l', xaxt="n", main="1085");axis(1, at = ax, labels = ax)
t=which(covid_county$CTFIPS==1101);plot(covid_county$DSCR[t], covid_county$NewCases[t]*1000, type='l', xaxt="n", main="1101");axis(1, at = ax, labels = ax)
par(mfrow=c(1,1))

df = as.data.frame(covid_county[which(covid_county$CTFIPS%in%c(1001,1021,1047,1051,1085,1101)),])

plot(df$Longitude, df$Latitude);text(df$Longitude, df$Latitude, df$CTFIPS, cex=0.6, pos=4, col="red")

adjMat = 









