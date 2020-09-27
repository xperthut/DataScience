source("header.R")

# us-counties.csv
covid_us = read.csv(file.choose())

# Remove data where county is Unknown
covid_us = as.data.frame(covid_us%>%filter(state!="Unknown"))

# Select states
covid_us$color=""
scp = as.data.frame(covid_us[,c(2,6)]%>%distinct()%>%arrange(state, color))
scp$color = getColors(nrow(scp))

# Convert date to DaysSinceFirstCase relative to each county
covid_us$DaysSinceFirstCase = 0
covid_us$DailyCase = covid_us$cases
covid_us$DailyDead = covid_us$deaths
covid_us$dts = as.numeric(convertToDateString(covid_us$date, frmt = "%Y-%m-%d"))

# Create daily case
covid_us = as.data.frame(covid_us%>%arrange(state,dts))

# Remove leading 1's and adjust DaysSince
# l=c()
# for(i in 1:nrow(scp)){
#   t = which(covid_us$county==as.character(scp$county[i]) & covid_us$state==as.character(scp$state[i]) & covid_us$cases==1)
#   if(length(t)>1){
#     for(j in 1:(length(t)-1)){
#       if(length(l)==0) l=c(t[j])
#       else l = append(l, t[j])
#     }
#   }
# }
# covid_us_filter = as.data.frame(covid_us[-l,])
# covid_us = as.data.frame(covid_us_filter%>%arrange(state,county,dts))
# rm(covid_us_filter)

# Generate Date Since First Case numbers
for(i in 1:nrow(scp)){
  t = which(covid_us$state==as.character(scp$state[i]))
  
  if(length(t) > 0){
    covid_us$color[t] = scp$color[i]
    d = as.character(covid_us$date[t[1]])
    # %m/%d/%Y
    covid_us$DaysSinceFirstCase[t] = getDays(d, covid_us$date[t], fmt = "%Y-%m-%d")
    rm(d)
  }
  rm(t)
}

# Remove unwanted data
covid_us = as.data.frame(covid_us%>%filter(DaysSinceFirstCase>0))

# Fix cumulative issue
# 0,1,2,3,2,4 should be fixed to
# 0,1,2,3,3,4
for(i in 1:nrow(scp)){
  t = which(covid_us$state==as.character(scp$state[i]))
  
  if(length(t) > 1){
    for(i in 2:length(covid_us$cases[t])){
      if(covid_us$cases[t][i]<covid_us$cases[t][i-1]){
        covid_us$cases[t][i] = covid_us$cases[t][i-1]
      }
      
      if(covid_us$deaths[i]<covid_us$deaths[i-1]){
        covid_us$deaths[t][i] = covid_us$deaths[t][i-1]
      }
    }
  }
}


# Compute daily case and daily death
for(i in 1:nrow(scp)){
  t = which(covid_us$state==as.character(scp$state[i]))
  
  if(length(t) > 1){
    for(i in length(covid_us$cases[t]):2){
      covid_us$DailyCase[t][i] = covid_us$cases[t][i]-covid_us$cases[t][i-1]
      covid_us$DailyDead[t][i] = covid_us$deaths[t][i]-covid_us$deaths[t][i-1]
    }
  }
}

# Death rate
covid_us$DeathRate = (covid_us$deaths/covid_us$cases)*100

# Save wa data
write.csv(covid_us, file = "/Users/methun/self/Research/Dataset/COVID-19/Data/covid_us_state.csv")


# Plots
plot(covid_us$DaysSinceFirstCase, covid_us$cases, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative cases", col=covid_us$color)
plot(covid_us$DaysSinceFirstCase, covid_us$deaths, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative deaths", col=covid_us$color)
plot(covid_us$DaysSinceFirstCase, covid_us$DeathRate, xlab = "Days Since First Case (DSFC)", ylab = "Death rate (%)", col=covid_us$color)

plot(covid_us$cases, covid_us$deaths, xlab = "Cumulative cases", ylab = "Cumulative deaths", col=covid_us$color)
plot(covid_us$cases, covid_us$DeathRate, xlab = "Cumulative cases", ylab = "Death rate (%)", col=covid_us$color, type="l")


rm(scp)






# Selected WA state analysis
# King, Snohomish, Pierce, Chelan, Thurston, Spokane, Clark, Benton, Yakima
# red,  darkgreen, blue,  orange,   purple3,  black,    tomato3, salmon4, gold3
t = which(covid_us$state == "Washington" & (covid_us$county=="King"|covid_us$county=="Snohomish"|covid_us$county=="Pierce"|covid_us$county=="Chelan"|covid_us$county=="Thurston"|covid_us$county=="Spokane"|covid_us$county=="Clark"|covid_us$county=="Benton"|covid_us$county=="Yakima"))
covid_WA = as.data.frame(covid_us[t,])
covid_WA$color = ""
covid_WA$color[which(covid_WA$county=="King")] = "red"
covid_WA$color[which(covid_WA$county=="Snohomish")] = "darkgreen"
covid_WA$color[which(covid_WA$county=="Pierce")] = "blue"
covid_WA$color[which(covid_WA$county=="Chelan")] = "orange"
covid_WA$color[which(covid_WA$county=="Thurston")] = "purple3"
covid_WA$color[which(covid_WA$county=="Spokane")] = "black"
covid_WA$color[which(covid_WA$county=="Clark")] = "tomato3"
covid_WA$color[which(covid_WA$county=="Benton")] = "salmon4"
covid_WA$color[which(covid_WA$county=="Yakima")] = "gold3"

covid_WA = as.data.frame(covid_WA)

#plot(covid_WA$DaysSinceFirstCase, covid_WA$cases, col=covid_WA$color, xlab = "Days since start the case", ylab = "Cummulative cases")
ggplot(covid_WA, aes(x = DaysSinceFirstCase, y = cases, colour = county)) + 
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
  labs(color='County') +
  ggtitle("") +
  labs(x = "Days Since First Case (DSFC)", y = "Cumulative cases")

# Save wa data
write.csv(covid_WA, file = "/Users/methun/self/Research/Dataset/Dummy/COVID-19/covid_WA_9_counties.csv")

t=which(covid_WA$county=="King")
plot(covid_WA$DaysSinceFirstCase[t], covid_WA$DailyCase[t], col=covid_WA$color[t], xlab = "Days since first case", ylab = "Daily cases", type = "o")

###################################################################################################
###################################################################################################
# Create periods (3 Days, 5 Days, 7 Days)
covid_WA_period = as.data.frame(covid_WA[1,c(2:4)])
covid_WA_period$term=0
covid_WA_period$period_3=0
covid_WA_period$period_5=0
covid_WA_period$period_7=0

# Remove row
covid_WA$grp3 = 0
covid_WA$grp5 = 0
covid_WA$grp7 = 0
covid_WA_period = as.data.frame(covid_WA[1,])
covid_WA_period = as.data.frame(covid_WA_period[-1,])

for(i in c("King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima")){
  t = which(covid_WA$county == as.character(i))
  a=1
  b=1
  c=1
  d=1
  
  tmp = as.data.frame(covid_WA[t,])
  
  for(j in 1:nrow(tmp)){
    tmp$grp3[j] = b
    tmp$grp5[j] = c
    tmp$grp7[j] = d
    
    if(a%%3==0){
      b = b+1
    }
    
    if(a%%5==0){
      c = c+1
    }
    
    if(a%%7==0){
      d = d+1
    }
    
    a=a+1
  }
  covid_WA_period = rbind(covid_WA_period, tmp)
}

# Save the file
write.csv(covid_WA_period, file = "/Users/methun/self/Research/Dataset/Dummy/COVID-19/covid_WA_9_counties_periods.csv")

# Group by period
covid_WA_period3 = as.data.frame(covid_WA_period[,c(2:10,11)])
covid_WA_period5 = as.data.frame(covid_WA_period[,c(2:10,12)])
covid_WA_period7 = as.data.frame(covid_WA_period[,c(2:10,13)])

# Create 3 days interval data
covid_WA_period3_grp = covid_WA_period3%>%group_by(county, state, fips, color,grp3)%>%summarise(total_case=sum(DailyCase), total_death=sum(DailyDead), avg_case=mean(DailyCase), avg_death=mean(DailyDead))

# Create 5 days interval data
covid_WA_period5_grp = covid_WA_period5%>%group_by(county, state, fips, color,grp5)%>%summarise(total_case=sum(DailyCase), total_death=sum(DailyDead) , avg_case=mean(DailyCase), avg_death=mean(DailyDead))

# Create 7 days interval data
covid_WA_period7_grp = covid_WA_period7%>%group_by(county, state, fips, color,grp7)%>%summarise(total_case=sum(DailyCase), total_death=sum(DailyDead) , avg_case=mean(DailyCase), avg_death=mean(DailyDead))


covid_WA_period3_grp$total_case_norm = getNormalizedData(covid_WA_period3_grp$total_case)

ct = c("King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima")
par(mfrow=c(3,1))
i=1
t = which(covid_WA_period3_grp$county==as.character(ct[i]))
plot(covid_WA_period3_grp$grp3[t], covid_WA_period3_grp$total_case[t], col=covid_WA_period3_grp$color[t], type="o", ylab = "Total cases", xlab = "3 Days interval")
t = which(covid_WA_period5_grp$county==as.character(ct[i]))
plot(covid_WA_period5_grp$grp5[t], covid_WA_period5_grp$total_case[t], col=covid_WA_period5_grp$color[t], type="o", ylab = "Total cases", xlab = "5 Days interval")
t = which(covid_WA_period7_grp$county==as.character(ct[i]))
plot(covid_WA_period7_grp$grp7[t], covid_WA_period7_grp$total_case[t], col=covid_WA_period7_grp$color[t], type="o", ylab = "Total cases", xlab = "7 Days interval")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
i=2
t = which(covid_WA_period3_grp$county==as.character(ct[i]))
plot(covid_WA_period3_grp$total_case[t], col=covid_WA_period3_grp$color[t], type="o", ylab = "Total cases", xlab = "3 Days interval")
t = which(covid_WA_period5_grp$county==as.character(ct[i]))
plot(covid_WA_period5_grp$total_case[t], col=covid_WA_period5_grp$color[t], type="o", ylab = "Total cases", xlab = "5 Days interval")
t = which(covid_WA_period7_grp$county==as.character(ct[i]))
plot(covid_WA_period7_grp$total_case[t], col=covid_WA_period7_grp$color[t], type="o", ylab = "Total cases", xlab = "7 Days interval")
par(mfrow=c(1,1))

par(mfrow=c(3,1))
i=3
t = which(covid_WA_period3_grp$county==as.character(ct[i]))
plot(covid_WA_period3_grp$total_case[t], col=covid_WA_period3_grp$color[t], type="o", ylab = "Total cases", xlab = "3 Days interval")
t = which(covid_WA_period5_grp$county==as.character(ct[i]))
plot(covid_WA_period5_grp$total_case[t], col=covid_WA_period5_grp$color[t], type="o", ylab = "Total cases", xlab = "5 Days interval")
t = which(covid_WA_period7_grp$county==as.character(ct[i]))
plot(covid_WA_period7_grp$total_case[t], col=covid_WA_period7_grp$color[t], type="o", ylab = "Total cases", xlab = "7 Days interval")
par(mfrow=c(1,1))

###################################################################################################
###################################################################################################


####################################################################################################################################
# Model on cummulative cases
##fit curve for cummulative data
## "King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima"
####################################################################################################################################
# King county
t=which(covid_WA$county=="King")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, dm, length.out = dm)
covid_king_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_king_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_king_case_rate$county="King"
covid_king_case_rate$state="Washington"
covid_king_case_rate$color="red"

plot(covid_king_case_rate$DaysSinceFirstCase, covid_king_case_rate$CaseRate, type="o", col=covid_king_case_rate$color)


####################################################################################################################################
# Snohomish county
t=which(covid_WA$county=="Snohomish")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_4)

par = result_4$fit$par
x = seq(1, dm, length.out = dm)
covid_Snohomish_case_rate=as.data.frame(cbind(x, Model_LL5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Snohomish_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Snohomish_case_rate$county="Snohomish"
covid_Snohomish_case_rate$state="Washington"
covid_Snohomish_case_rate$color="darkgreen"

plot(covid_Snohomish_case_rate$DaysSinceFirstCase, covid_Snohomish_case_rate$CaseRate, type="o", col=covid_Snohomish_case_rate$color)

####################################################################################################################################
# Pierce county
t=which(covid_WA$county=="Pierce")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
#result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
#plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, dm, length.out = dm)
covid_Pierce_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Pierce_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Pierce_case_rate$county="Pierce"
covid_Pierce_case_rate$state="Washington"
covid_Pierce_case_rate$color="blue"

plot(covid_Pierce_case_rate$DaysSinceFirstCase, covid_Pierce_case_rate$CaseRate, type="o", col=covid_Pierce_case_rate$color)

####################################################################################################################################
# Chelan county
t=which(covid_WA$county=="Chelan")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1,result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, dm, length.out = dm)
covid_Chelan_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Chelan_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Chelan_case_rate$county="Chelan"
covid_Chelan_case_rate$state="Washington"
covid_Chelan_case_rate$color="orange"


plot(covid_Chelan_case_rate$DaysSinceFirstCase, covid_Chelan_case_rate$CaseRate, type="o", col=covid_Chelan_case_rate$color)

####################################################################################################################################
# Thurston county
t=which(covid_WA$county=="Thurston")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, dm, length.out = dm)
covid_Thurston_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Thurston_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Thurston_case_rate$county="Thurston"
covid_Thurston_case_rate$state="Washington"
covid_Thurston_case_rate$color="purple3"


plot(covid_Thurston_case_rate$DaysSinceFirstCase, covid_Thurston_case_rate$CaseRate, type="o",col=covid_Thurston_case_rate$color)


####################################################################################################################################
# Spokane county
t=which(covid_WA$county=="Spokane")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, dm, length.out = dm)
covid_Spokane_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Spokane_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Spokane_case_rate$county="Spokane"
covid_Spokane_case_rate$state="Washington"
covid_Spokane_case_rate$color="black"


plot(covid_Spokane_case_rate$DaysSinceFirstCase, covid_Spokane_case_rate$CaseRate, type="o", col=covid_Spokane_case_rate$color)

####################################################################################################################################
# Clark county
t=which(covid_WA$county=="Clark")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, dm, length.out = dm)
covid_Clark_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Clark_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Clark_case_rate$county="Clark"
covid_Clark_case_rate$state="Washington"
covid_Clark_case_rate$color="tomato3"

plot(covid_Clark_case_rate$DaysSinceFirstCase, covid_Clark_case_rate$CaseRate, type="o", col=covid_Clark_case_rate$color)


####################################################################################################################################
# Benton county
t=which(covid_WA$county=="Benton")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, dm, length.out = dm)
covid_Benton_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Benton_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Benton_case_rate$county="Benton"
covid_Benton_case_rate$state="Washington"
covid_Benton_case_rate$color="salmon4"

plot(covid_Benton_case_rate$DaysSinceFirstCase, covid_Benton_case_rate$CaseRate, type="o", col=covid_Benton_case_rate$color)

####################################################################################################################################
# Yakima county
t=which(covid_WA$county=="Yakima")
covid_d=as.data.frame(covid_WA[t,])
dm = max(covid_d$DaysSinceFirstCase)
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, dm, length.out = dm)
covid_Yakima_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Yakima_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Yakima_case_rate$county="Yakima"
covid_Yakima_case_rate$state="Washington"
covid_Yakima_case_rate$color="gold3"

plot(covid_Yakima_case_rate$DaysSinceFirstCase, covid_Yakima_case_rate$CaseRate, type="o", col=covid_Yakima_case_rate$color)

## Accumulate all results
## "King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima"
covid_WA_case_rate = as.data.frame(rbind(covid_king_case_rate, covid_Snohomish_case_rate, covid_Pierce_case_rate, 
                           covid_Chelan_case_rate, covid_Thurston_case_rate, covid_Spokane_case_rate, 
                           covid_Clark_case_rate, covid_Benton_case_rate, covid_Yakima_case_rate))

rm(covid_king_case_rate, covid_Snohomish_case_rate, covid_Pierce_case_rate, 
      covid_Chelan_case_rate, covid_Thurston_case_rate, covid_Spokane_case_rate, 
      covid_Clark_case_rate, covid_Benton_case_rate, covid_Yakima_case_rate)
# Format to 6 decimal places
covid_WA_case_rate$CaseRate = formattable(covid_WA_case_rate$CaseRate, digits = 6, format = "f")

# SAve it
write.csv(covid_WA_case_rate, file = "/Users/methun/self/Research/Dataset/Dummy/COVID-19/covid_WA_cumcase_rate.csv")

plot(covid_WA_case_rate$DaysSinceFirstCase, covid_WA_case_rate$CaseRate, col=covid_WA_case_rate$color, type = "o")

# Plot with label
ggplot(covid_WA_case_rate, aes(x = DaysSinceFirstCase, y = CaseRate, colour = county)) + 
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
  labs(color='County') +
  ggtitle("") +
  labs(x = "Days Since First Case (DSFC)", y = "Covid-19 case growth-rate")

####################################################################################################################################
#             Model building ends Here
####################################################################################################################################




## Analysis for Hyppo-X
l=c()
r=c()
for(i in c("King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima")){
  t = which(covid_WA_case_rate$county == as.character(i))
  if(length(l)==0) l = sd(covid_WA_case_rate$Case_rate[t])
  else l = append(l, sd(covid_WA_case_rate$Case_rate[t]))
  
  t = which(covid_WA$county == as.character(i))
  if(length(r)==0) r = sd(covid_WA$cases[t])
  else r = append(r, sd(covid_WA$cases[t]))
}
mean(l)
mean(r)


####################################################################################################################################
# Model on total cases in 3 days interval
##fit curve for cummulative data
## "King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima"
####################################################################################################################################
# King county
t=which(covid_WA$county=="King")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$DailyCase, col=covid_d$color, xlab = "Days since first case", ylab = "Cumulative cases", type = "o")

par(mfrow=c(2,2))
result_1<-drm(DailyCase~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_1,main="Gompertz Model")
result_2<-drm(DailyCase~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_2,main="Logistic Model")
result_3<-drm(DailyCase~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_3,main="Log-logistic")
result_4<-drm(DailyCase~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_4,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
daic=AIC(result_1, result_2, result_3, result_4,result_5)
daic
index=which(daic$AIC==min(daic$AIC))
index

#obtain the coefficients from the best fitting model
summary(result_2)

par = result_2$fit$par
x = seq(1, 100, length.out = 100)

covid_king_case_rate=model_fit_4(x,x,par,index,c("DaysSinceFirstCase", "DailyCases", "CaseRate"))
covid_king_case_rate$county="King"
covid_king_case_rate$state="Washington"
covid_king_case_rate$color="red"

plot(covid_king_case_rate$DaysSinceFirstCase, covid_king_case_rate$DailyCases, type="o", col=covid_king_case_rate$color)


####################################################################################################################################
# Snohomish county
t=which(covid_WA$county=="Snohomish")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_4)

par = result_4$fit$par
x = seq(1, 100, length.out = 100)
covid_Snohomish_case_rate=as.data.frame(cbind(x, Model_LL5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Snohomish_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Snohomish_case_rate$county="Snohomish"
covid_Snohomish_case_rate$state="Washington"
covid_Snohomish_case_rate$color="darkgreen"

plot(covid_Snohomish_case_rate$DaysSinceFirstCase, covid_Snohomish_case_rate$CaseRate, type="o", col=covid_Snohomish_case_rate$color)

####################################################################################################################################
# Pierce county
t=which(covid_WA$county=="Pierce")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
#result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
#plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, 100, length.out = 100)
covid_Pierce_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Pierce_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Pierce_case_rate$county="Pierce"
covid_Pierce_case_rate$state="Washington"
covid_Pierce_case_rate$color="blue"

plot(covid_Pierce_case_rate$DaysSinceFirstCase, covid_Pierce_case_rate$CaseRate, type="o", col=covid_Pierce_case_rate$color)

####################################################################################################################################
# Chelan county
t=which(covid_WA$county=="Chelan")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1,result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, 100, length.out = 100)
covid_Chelan_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Chelan_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Chelan_case_rate$county="Chelan"
covid_Chelan_case_rate$state="Washington"
covid_Chelan_case_rate$color="orange"


plot(covid_Chelan_case_rate$DaysSinceFirstCase, covid_Chelan_case_rate$CaseRate, type="o", col=covid_Chelan_case_rate$color)

####################################################################################################################################
# Thurston county
t=which(covid_WA$county=="Thurston")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_3)

par = result_3$fit$par
x = seq(1, 100, length.out = 100)
covid_Thurston_case_rate=as.data.frame(cbind(x, Model_L5_D(x,par[1],par[2],par[3],par[4],par[5])))
colnames(covid_Thurston_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Thurston_case_rate$county="Thurston"
covid_Thurston_case_rate$state="Washington"
covid_Thurston_case_rate$color="purple3"


plot(covid_Thurston_case_rate$DaysSinceFirstCase, covid_Thurston_case_rate$CaseRate, type="o",col=covid_Thurston_case_rate$color)


####################################################################################################################################
# Spokane county
t=which(covid_WA$county=="Spokane")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, 100, length.out = 100)
covid_Spokane_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Spokane_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Spokane_case_rate$county="Spokane"
covid_Spokane_case_rate$state="Washington"
covid_Spokane_case_rate$color="black"


plot(covid_Spokane_case_rate$DaysSinceFirstCase, covid_Spokane_case_rate$CaseRate, type="o", col=covid_Spokane_case_rate$color)

####################################################################################################################################
# Clark county
t=which(covid_WA$county=="Clark")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, 100, length.out = 100)
covid_Clark_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Clark_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Clark_case_rate$county="Clark"
covid_Clark_case_rate$state="Washington"
covid_Clark_case_rate$color="tomato3"

plot(covid_Clark_case_rate$DaysSinceFirstCase, covid_Clark_case_rate$CaseRate, type="o", col=covid_Clark_case_rate$color)


####################################################################################################################################
# Benton county
t=which(covid_WA$county=="Benton")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, 100, length.out = 100)
covid_Benton_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Benton_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Benton_case_rate$county="Benton"
covid_Benton_case_rate$state="Washington"
covid_Benton_case_rate$color="salmon4"

plot(covid_Benton_case_rate$DaysSinceFirstCase, covid_Benton_case_rate$CaseRate, type="o", col=covid_Benton_case_rate$color)

####################################################################################################################################
# Yakima county
t=which(covid_WA$county=="Yakima")
covid_d=as.data.frame(covid_WA[t,])
plot(covid_d$DaysSinceFirstCase, covid_d$cases, col=covid_d$color, type="o")

par(mfrow=c(2,3))
result_1<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = EXD.3())
plot(result_1,main="Exponential Model")
result_2<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = G.4())
plot(result_2,main="Gompertz Model")
result_3<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = L.5())
plot(result_3,main="Logistic Model")
result_4<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = LL.5())
plot(result_4,main="Log-logistic")
result_5<-drm(cases~DaysSinceFirstCase, data = covid_d, fct = W1.4())
plot(result_5,main="Weibull Model")
par(mfrow=c(1,1))

#identify the best fitting model
AIC(result_1, result_2, result_3, result_4,result_5)

#obtain the coefficients from the best fitting model
summary(result_5)

par = result_5$fit$par
x = seq(1, 100, length.out = 100)
covid_Yakima_case_rate=as.data.frame(cbind(x, Model_W14_D(x,par[1],par[2],par[3],par[4])))
colnames(covid_Yakima_case_rate) <- c("DaysSinceFirstCase", "CaseRate")
covid_Yakima_case_rate$county="Yakima"
covid_Yakima_case_rate$state="Washington"
covid_Yakima_case_rate$color="gold3"

plot(covid_Yakima_case_rate$DaysSinceFirstCase, covid_Yakima_case_rate$CaseRate, type="o", col=covid_Yakima_case_rate$color)

## Accumulate all results
## "King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima"
covid_WA_case_rate = as.data.frame(rbind(covid_king_case_rate, covid_Snohomish_case_rate, covid_Pierce_case_rate, 
                                         covid_Chelan_case_rate, covid_Thurston_case_rate, covid_Spokane_case_rate, 
                                         covid_Clark_case_rate, covid_Benton_case_rate, covid_Yakima_case_rate))

####################################################################################################################################
#             Model building ends Here
####################################################################################################################################


dp = data.frame(c(1:10000),c(1:10000),c(1:10000),c(1:10000),c(1:10000),c(1:10000),c(1:10000),c(1:10000),c(1:10000))
colnames(dp)<-c("5", "10", "25", "50", "75", '100', '150', "200", "1000")
d = data.frame(c(5, 10, 25, 50, 75, 100, 150, 200, 1000), c(5, 10, 25, 50, 75, 100, 150, 200, 1000), c(5, 10, 25, 50, 75, 100, 150, 200, 1000), c(5, 10, 25, 50, 75, 100, 150, 200, 1000))
colnames(d)<-c("N", "mean p", "variance p", "standard dev p")
#l=""
for(i in 1:nrow(d)){
  x = rbinom(10000, size = d$N[i], prob = 0.2)
  p = x/d$N[i]
  dp[,i] = p
  d$`mean p`[i] = formatDecimal(mean(p), digits = 2)
  d$`standard dev p`[i]=formatDecimal(sd(p), digits = 2)
  d$`variance p`[i]=formatDecimal(var(p), digits = 2)
  #l = paste(l,d$N[i],"&",d$`mean p`[i],"&",d$`variance p`[i],"&",d$`standard dev p`[i],"\\",sep = "")
}

boxplot(dp, names = c(5, 10, 25, 50, 75, 100, 150, 200, 1000),col = "gray90")

par(mfrow = c(3,3))
l=c(5, 10, 25, 50, 75, 100, 150, 200, 1000)
for(i in 1:length(l)){
  hist(dp[,i], breaks = seq(0,1,0.05), xlab = "Extimated p", main = paste("Sample size: ",l[i],sep=""))
}
par(mfrow = c(1,1))

par(mfrow = c(3,3))
l=c(5, 10, 25, 50, 75, 100, 150, 200, 1000)
for(i in 1:length(l)){
  hist(dp[,i], xlab = "Extimated p", main = paste("Sample size: ",l[i],sep=""))
}
par(mfrow = c(1,1))

plot(d$N, d$`variance p`, type="o", xlab = "Sample size", ylab = "Variance of estimated p")


####################
m=10000
N=5

fd = data.frame(c(1,5,10,25,50,100,1000),c(1,5,10,25,50,100,1000),c(1,5,10,25,50,100,1000),c(1,5,10,25,50,100,1000))
colnames(fd) = c("N", "Mean", "Var", "Sd")
spm = NULL

for(i in 1:nrow(fd)){
  xmat = as.data.frame(replicate(m, rnorm(n = fd$N[i], mean = 50, sd = 5)))
  if(is.null(spm)){
    spm = plyr::colwise(mean)(xmat)
  }else{
    spm = rbind(spm, plyr::colwise(mean)(xmat))
  }
  
  rd = t(plyr::colwise(mean)(xmat))
  rm(xmat)
  
  fd$Mean[i] = mean(rd)
  fd$Var[i] = var(rd)
  fd$Sd[i] = sd(rd)
  
  rm(rd)
}

fd$VbyN = fd$Var/fd$N
spm = as.data.frame(t(spm))
colnames(spm)<-c("c5","c10","c25","c50","c100","c1000")

par(mfrow=c(3,2))
hist(spm$c5, xlab = "Mean X", main = paste("N=5",sep=""))
hist(spm$c10, xlab = "Mean X", main = paste("N=10",sep=""))
hist(spm$c25, xlab = "Mean X", main = paste("N=25",sep=""))
hist(spm$c50, xlab = "Mean X", main = paste("N=50",sep=""))
hist(spm$c100, xlab = "Mean X", main = paste("N=100",sep=""))
hist(spm$c1000, xlab = "Mean X", main = paste("N=1000",sep=""))
par(mfrow=c(1,1))

boxplot(spm, names = c(5, 10, 25, 50, 100, 1000),col = "gray90")

plot(fd$N,  fd$Var, type="o", xlab = "Sample size", ylab = "Variance (X)")

fd$c25bn = 25/fd$N
ggplot(fd, aes(N)) +                    # basic graphical object
  geom_line(aes(y=Var), colour="red") +  # first layer
  geom_line(aes(y=c25bn), colour="green")  # second layer

# Exponential distribution
fd = data.frame(c(5,10,25,50,100,1000),c(5,10,25,50,100,1000),c(5,10,25,50,100,1000),c(5,10,25,50,100,1000))
colnames(fd) = c("N", "Mean", "Var", "Sd")
spm = NULL

for(i in 1:nrow(fd)){
  xmat = as.data.frame(replicate(m, rexp(n = fd$N[i], rate = 1/50)))
  if(is.null(spm)){
    spm = plyr::colwise(mean)(xmat)
  }else{
    spm = rbind(spm, plyr::colwise(mean)(xmat))
  }
  
  rd = t(plyr::colwise(mean)(xmat))
  rm(xmat)
  
  fd$Mean[i] = formatDecimal(mean(rd), digits = 3)
  fd$Var[i] = formatDecimal(var(rd), digits = 3)
  fd$Sd[i] = formatDecimal(sd(rd), digits = 3)
  
  rm(rd)
}

fd$VbyN = formatDecimal(fd$Var/fd$N, digits = 3)
spm = as.data.frame(t(spm))
colnames(spm)<-c("c5","c10","c25","c50","c100","c1000")

par(mfrow=c(3,2))
hist(spm$c5, xlab = "Mean X", main = paste("N=5",sep=""))
hist(spm$c10, xlab = "Mean X", main = paste("N=10",sep=""))
hist(spm$c25, xlab = "Mean X", main = paste("N=25",sep=""))
hist(spm$c50, xlab = "Mean X", main = paste("N=50",sep=""))
hist(spm$c100, xlab = "Mean X", main = paste("N=100",sep=""))
hist(spm$c1000, xlab = "Mean X", main = paste("N=1000",sep=""))
par(mfrow=c(1,1))

boxplot(spm, names = c(5, 10, 25, 50, 100, 1000),col = "gray90")

plot(fd$N,  fd$Var, type="o", xlab = "Sample size", ylab = "Variance (X)")

fd$c25bn = 2500/fd$N
ggplot(fd, aes(N)) +                    # basic graphical object
  geom_line(aes(y=Var), colour="red") +  # first layer
  geom_line(aes(y=c25bn), colour="green")  # second layer


k=replicate(1, rnorm(n = 1000, mean = 50, sd = 5))
l=replicate(1000, rnorm(n = 5, mean = 50, sd = 5))
k1=c()
for(i in 1:ncol(l)){
  if(length(k1)==0) k1 = mean(l[,i])
  else k1 = append(k1, l[,i])
}

hist(k1)

k=c(13.0,18.5,16.4,14.8,20.8,19.3,18.8,23.1,25.1,16.8,20.4,17.4,16.0,21.7,15.2,21.3,19.4,17.3,15.2,19.9,25.2,23.1,21.5,16.8,23.2,24.9,19.1,18.1,15.3,19.4,15.6,17.6)

k1 = k-mean(k)
k2 = k1*k1
sdk = sum(k2)

# 7.3
#part c)
#set the random seed
set.seed(123)
#set the sample size
n<- 25
#set number of samples
R<-200
#set n*R samples from normal(50,5^2)
x<-rnorm(n*R,mean=50,sd=5)
#convert this into a matrix with 200 rows
x<-matrix(x,nrow=200)
#get R=200 sample means
xbar<- apply(x,1,mean)
#get R=200 sample sds
s<-apply(x,1,sd)
#get 200 standard errors of mean
se<-s/sqrt(n)
#get the significance level for 95% confidence
alpha<- 1-95/100
#set the degrees of freedom for t
d.f<- n-1
#get the right tail critical value using t distribution
tc<- qt(1-alpha/2,df=d.f)
#get the 95% confidence intervals
lci<-xbar-tc*se
uci<-xbar+tc*se
#count number of confidence intervals that contain the true mean=50
count<-sum(lci<50 &50< uci)
sprintf('Number of confidence intervals that contain the true mean is %g',count)








