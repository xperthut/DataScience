source("header.R")

covid_state = read.csv("FromAnil/UNM/Write/covid_state_7MA_2.csv")
covid_state = read.csv("FromAnil/UNM/Write/Covid_state.csv")
covid_county=read.csv("FromAnil/UNM/Write/Covid_county.csv")

################################################################################################
# Correlation
corDS = cor(covid_state[,c(2,11,15:29)], use="pairwise.complete.obs")
pheatmap(abs(corDS))

# Plot
plot(covid_state$CovidExposer, covid_state$cumCases, main = "", xlab = "Covid exposer", ylab = "Cum cases per 1k people")
plot(covid_state$TestingDone, covid_state$cumCases, main = "", xlab = "TestingDone", ylab = "Cum cases per 1k people")

plot(covid_state$SDI, covid_state$cumCases, main = "", xlab = "Social distancing index", ylab = "Cum cases per 1k people")
plot(covid_state$SDI, covid_state$NewCases, main = "", xlab = "Social distancing index", ylab = "New cases per 1k people")
plot(covid_state$cSDI, covid_state$NewCases, main = "", xlab = "Cum.Social distancing index", ylab = "New cases per 1k people")

plot(covid_state$HospitalBedUtilization, covid_state$cumCases, main = "", xlab = "Hospital bed utilization", ylab = "Cum cases per 1k people")

plot(covid_state$CovidExposer, covid_state$NewCases, main = "", xlab = "Covid exposer", ylab = "New cases per 1k people")

plot(covid_state$TestingDone, covid_state$NewCases, main = "", xlab = "Testing done", ylab = "New cases per 1k people")

# Regression
m<-lm(formula = cumCases~CovidExposer, data = covid_state)
summary(m)

ms<-lm(formula = cumCases~CovidExposer+STFIPS, data = covid_state)
summary(ms)

m3<-lm(formula = cumCases~CovidExposer+TestingDone+STFIPS, data = covid_state)
summary(m3)

m4<-lm(formula = cumCases~CovidExposer+TestingDone, data = covid_state)
summary(m4)

## Check statewise date
dfc = as.data.frame(covid_county[,c(2,3,13)]%>%distinct()%>%arrange(STFIPS,STNAME,DSCR))
dfs = as.data.frame(covid_state[,c(2,3,10,14)]%>%distinct()%>%arrange(STFIPS,STNAME,DSCR,Weeks))
df = as.data.frame(merge(dfc,dfs,by = c("STFIPS", "STNAME", "DSCR"), all.x = TRUE))



## Plot
df = as.data.frame(covid_county[which(covid_county$STNAME=="GA"),c(3,9:12)]%>%distinct()%>%arrange(Longitude, Latitude))

tc=unique(covid_county$CTFIPS[which(covid_county$STNAME=="GA")])
t = which(covid_county$CTFIPS==tc[1])

plot(covid_county$DSFC[t], covid_county$NewCases[t], type="l")

covid_state$stdPR = 0.00
st = unique(covid_state$STNAME)
for(sn in st){
  t = which(covid_state$STNAME==as.character(sn))
  covid_state$stdPR[t] = normalize(covid_state$PositivityRate[t], method="standardize")
}

t=which(covid_state$STNAME=="AZ");matplot(covid_state$DSCR[t], cbind(covid_state$stdPR[t],covid_state$PositivityRate[t]), type='l', col=c('darkgreen','purple'))


####### Time distribution plot covid exposure
tdDS = read.csv("FromAnil/UNM/Analysis/covid_state_14MA_1600061998862.csv")
tdDS<-tdDS%>%arrange(ClusteID, DSFC)

# number of points based on cluster ids, stateids
tdDSgr = tdDS%>%group_by(STNAME,ClusteID)%>%summarise(mDR=min(DSCR), xDR=max(DSCR), pt=n())%>%arrange(ClusteID, -pt)

k=tdDSgr%>%filter(mDR>=89 & xDR<=107)%>%arrange(-pt)
k1=tdDSgr%>%filter(mDR>=169 & xDR<=218)%>%arrange(-pt)
k2=tdDSgr%>%filter(mDR>=218 & xDR<=231 & ClusteID%in%c(20,29))%>%arrange(-pt)
k3=tdDSgr%>%filter(mDR>=218 & xDR<=231 & ClusteID%in%c(44,47))%>%arrange(-pt)

tdDS$grp=0
tdDS$grp[which(tdDS$ClusteID==33 | tdDS$ClusteID==25)] = 1
tdDS$grp[which(tdDS$ClusteID==20 | tdDS$ClusteID==29)] = 2
tdDS$grp[which(tdDS$ClusteID==44 | tdDS$ClusteID==47)] = 3

k = tdDS%>%group_by(grp,ClusteID)%>%summarise(mDAY=min(DSCR), xDAY=max(DSCR), meanDay=mean(DSCR), sdDay=sd(DSCR), mDSFC=min(DSFC), xDSFC=max(DSFC))

sort(tdDS$DSFC[which(tdDS$grp==1)])
sort(tdDS$DSFC[which(tdDS$grp==2)])
sort(tdDS$DSFC[which(tdDS$grp==3)])

cIDs = sort(unique(tdDS$grp))
#p=c()
par(mfrow=c(3,2))
for(i in cIDs){
  t = which(tdDS$grp==as.numeric(i))
  p=ggplot(tdDS[t,], aes(x=grp, y=DSCR, fill="megenta"))+
    #geom_violin()+
  geom_density()+
    theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
  print(p)
}
par(mfrow=c(1,1))

ggplot(tdDS[t,],aes(DSCR))+geom_density()

tdDS%>%group_by(ClusteID)%>%ggplot(aes(DSCR))+geom_density()

ggplot(tdDS[t,], aes(x=ClusteID, y=DSCR))+geom_violin()



## Time distribution plot for SDI
tmpDS = read.csv("FromAnil/UNM/Analysis/covid_state_7MA_2_SDI.csv")
tmpDS<-tmpDS%>%arrange(ClusterID, DSCR)


tmpDS$grp=0
tmpDS$grp[which(tmpDS$ClusterID%in%c(17,19,22))] = 1
tmpDS$grp[which(tmpDS$ClusterID%in%c(49,52,57,59))] = 2
tmpDS$grp[which(tmpDS$ClusterID%in%c(62,63,65,66,68))] = 3

k = tmpDS%>%group_by(grp,STNAME)%>%summarise(mDAY=min(DSCR), xDAY=max(DSCR), meanDay=mean(DSCR), sdDay=sd(DSCR), mDSFC=min(DSFC), xDSFC=max(DSFC))

sort(tmpDS$DSFC[which(tmpDS$grp==1)])
sort(tmpDS$DSFC[which(tmpDS$grp==2)])
sort(tmpDS$DSFC[which(tmpDS$grp==3)])


tmpDS = read.csv("FromAnil/UNM/Analysis/covid_state_7MA_2_DSFC_NY_NJ.csv")
tmpDS<-tmpDS%>%arrange(ClusterID, DSCR)
sort(tmpDS$DSFC)


## Correlation test
x = tdDS%>%filter(STNAME%in%c("NY","NJ"))
y = tdDS%>%filter(STNAME%in%c("UT","NM"))

corx = cor(x[,c(9,12)], use = "pairwise.complete.obs");pheatmap(corx)
cory = cor(y[,c(9,12)], use = "pairwise.complete.obs");pheatmap(cory)





