source("header.R")

# us-counties.csv
covid_us = read.csv("/Users/methun/self/Research/Dataset/COVID-19/covid-19-data/us-counties.csv")

# Remove data where county is Unknown
covid_us = as.data.frame(covid_us%>%filter(county!="Unknown"))

# Filter state and county
cs = c("Washington", "Georgia")
ccwa = c("King", "Snohomish", "Pierce", "Chelan", "Thurston", "Spokane", "Clark", "Benton", "Yakima")
ccga = c("Fulton","Heard","Carroll","Douglas","Cobb","Cherokee","Forsyth","Gwinnett","Dekalb","Fayette",
       "Clayton","Chatham","Screven","Grady","Dougherty", "DeKalb", "Coweta")

covid_us = as.data.frame(rbind(covid_us%>%filter(state=="Washington")%>%filter(county%in%ccwa), covid_us%>%filter(state=="Georgia")%>%filter(county%in%ccga)))

# Select state, county pairs
scp = as.data.frame(covid_us[,c(2,3)]%>%distinct()%>%arrange(state, county))

# Convert date to DaysSinceFirstCase relative to each county
covid_us$DaysSinceFirstCase = 0
covid_us$DailyCase = covid_us$cases
covid_us$DailyDead = covid_us$deaths
covid_us$dts = as.numeric(convertToDateString(covid_us$date, frmt = "%Y-%m-%d"))

# Create daily case
covid_us = as.data.frame(covid_us%>%arrange(state,county,dts))

# Generate Date Since First Case numbers
for(i in 1:nrow(scp)){
  t = which(covid_us$county==as.character(scp$county[i]) & covid_us$state==as.character(scp$state[i]))
  
  if(length(t) > 0){
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
  t = which(covid_us$county==as.character(scp$county[i]) & covid_us$state==as.character(scp$state[i]))
  
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
  t = which(covid_us$county==as.character(scp$county[i]) & covid_us$state==as.character(scp$state[i]))
  
  if(length(t) > 1){
    for(i in length(covid_us$cases[t]):2){
      covid_us$DailyCase[t][i] = covid_us$cases[t][i]-covid_us$cases[t][i-1]
      covid_us$DailyDead[t][i] = covid_us$deaths[t][i]-covid_us$deaths[t][i-1]
    }
  }
}

# Color by state
sl = unique(covid_us$state)
stateColor = getColors(length(sl))
covid_us$StateColor = ""
for(i in 1:length(sl)){
  t = which(covid_us$state==as.character(sl[i]))
  covid_us$StateColor[t] = stateColor[i]
}

# Color by county within state
covid_us$CountyColor = ""
# Washington
cl = unique(covid_us$county[which(covid_us$state=="Washington")])
countyColor = getColors(length(cl))
covid_us$CountyColor[which(covid_us$state=="Washington")] = ""
for(i in 1:length(cl)){
  t = which(covid_us$county==as.character(cl[i]) & covid_us$state=="Washington")
  covid_us$CountyColor[t] = countyColor[i]
}
# Georgia
cl = unique(covid_us$county[which(covid_us$state=="Georgia")])
countyColor = getColors(length(cl))
covid_us$CountyColor[which(covid_us$state=="Georgia")] = ""
for(i in 1:length(cl)){
  t = which(covid_us$county==as.character(cl[i]) & covid_us$state=="Georgia")
  covid_us$CountyColor[t] = countyColor[i]
}

# Normalize the cumulative data based on state, county
covid_us = as.data.frame(
  covid_us%>%group_by(state, county)%>%mutate(
    normcases=getNormalizedData(cases), 
    stdcases=getStandardizeddData(cases), 
    normdeaths=getNormalizedData(deaths), 
    stddeaths=getStandardizeddData(deaths)))

# Save wa data
write.csv(covid_us, file = "/Users/methun/self/Research/Dataset/COVID-19/Data/covid_us_county.csv")

scp = as.data.frame(covid_us%>%group_by(county, state)%>%summarise(records=n()))

## 
covid_us_wa = as.data.frame(covid_us%>%filter(state=="Washington"))
covid_us_ga = as.data.frame(covid_us%>%filter(state=="Georgia"))

# colors and death rate
scp$color = getColors(nrow(scp))
covid_us$color = ""
for(i in 1:nrow(scp)){
  t = which(covid_us$county==as.character(scp$county[i]) & covid_us$state==as.character(scp$state[i]))
  
  if(length(t) > 0){
    covid_us$color[t] = scp$color[i]
  }
}
covid_us$DeathRate = (covid_us$deaths/covid_us$cases)*100
rm(scp)

# Save wa data
write.csv(covid_us, file = "/Users/methun/self/Research/Dataset/COVID-19/Data/covid_us_county.csv")
write.csv(covid_us_wa, file = "/Users/methun/self/Research/Dataset/COVID-19/Data/covid_WA_county.csv")
write.csv(covid_us_ga, file = "/Users/methun/self/Research/Dataset/COVID-19/Data/covid_GA_county.csv")

################################
# Plots
plot(covid_us$DaysSinceFirstCase, covid_us$cases, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative cases", col=covid_us$color)
plot(covid_us$DaysSinceFirstCase, covid_us$deaths, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative deaths", col=covid_us$color)
plot(covid_us$DaysSinceFirstCase, covid_us$DeathRate, xlab = "Days Since First Case (DSFC)", ylab = "Death rate (%)", col=covid_us$color)

#plot(covid_WA$DaysSinceFirstCase, covid_WA$cases, col=covid_WA$color, xlab = "Days since start the case", ylab = "Cummulative cases")
# cumulatice cases in WA
gcase<-ggplot(covid_us_wa, aes(x = DaysSinceFirstCase, y = cases, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Cumulative cases in WA") +
  labs(x = "Days Since First Case (DSFC)", y = "Cumulative cases")
gcase + scale_color_manual(values=unique(covid_us_wa$CountyColor))

# normalized cumulatice cases in WA
ggplot(covid_us_wa, aes(x = DaysSinceFirstCase, y = normcases, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Normalized cum. cases in WA") +
  labs(x = "Days Since First Case (DSFC)", y = "Norm. Cum. cases")

# standardize cumulatice cases in WA
ggplot(covid_us_wa, aes(x = DaysSinceFirstCase, y = stdcases, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Standardize cum. cases in WA") +
  labs(x = "Days Since First Case (DSFC)", y = "Std. Cum. cases")

# cumulatice deaths in WA
ggplot(covid_us_wa, aes(x = DaysSinceFirstCase, y = deaths, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Cumulative deaths in WA") +
  labs(x = "Days Since First Case (DSFC)", y = "Cumulative deaths")

# normalized cumulatice deaths in WA
ggplot(covid_us_wa, aes(x = DaysSinceFirstCase, y = normdeaths, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Normalized cum. deaths in WA") +
  labs(x = "Days Since First Case (DSFC)", y = "Norm. Cum. deaths")

# standardize cumulatice deaths in WA
ggplot(covid_us_wa, aes(x = DaysSinceFirstCase, y = stddeaths, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Standardized cum. deaths in WA") +
  labs(x = "Days Since First Case (DSFC)", y = "Std. Cum. deaths")


############# GA states
# cumulatice cases in GA
gcase<-ggplot(covid_us_ga, aes(x = DaysSinceFirstCase, y = cases, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Cumulative cases in GA") +
  labs(x = "Days Since First Case (DSFC)", y = "Cumulative cases")
gcase + scale_color_manual(values=unique(covid_us_ga$CountyColor))

# normalized cumulatice cases in GA
ggplot(covid_us_ga, aes(x = DaysSinceFirstCase, y = normcases, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Normalized cum. cases in GA") +
  labs(x = "Days Since First Case (DSFC)", y = "Norm. Cum. cases")

# standardize cumulatice cases in GA
ggplot(covid_us_ga, aes(x = DaysSinceFirstCase, y = stdcases, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Standardize cum. cases in GA") +
  labs(x = "Days Since First Case (DSFC)", y = "Std. Cum. cases")

# cumulatice deaths in GA
ggplot(covid_us_ga, aes(x = DaysSinceFirstCase, y = deaths, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Cumulative deaths in GA") +
  labs(x = "Days Since First Case (DSFC)", y = "Cumulative deaths")

# normalized cumulatice deaths in GA
ggplot(covid_us_ga, aes(x = DaysSinceFirstCase, y = normdeaths, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Normalized cum. deaths in GA") +
  labs(x = "Days Since First Case (DSFC)", y = "Norm. Cum. deaths")

# standardize cumulatice deaths in GA
ggplot(covid_us_ga, aes(x = DaysSinceFirstCase, y = stddeaths, colour = county)) + 
  geom_point() + 
  theme(axis.title.x = element_text(size=rel(2)), 
        axis.text.x = element_text(size=rel(2.0)), 
        axis.title.y = element_text(size=rel(2)),
        axis.text.y = element_text(size=rel(2.0)), 
        legend.title = element_text(size = rel(2)),
        legend.text = element_text(size = 12), 
        plot.title = element_text(size=rel(2.0), hjust = 0.5), 
        strip.text = element_text(face="bold", size = rel(2)),
        strip.background = element_rect(fill="white", colour="black",size=2)) + 
  labs(color='County') +
  ggtitle("Standardized cum. deaths in GA") +
  labs(x = "Days Since First Case (DSFC)", y = "Std. Cum. deaths")

# Plot, color by state
plot(covid_us$DaysSinceFirstCase, covid_us$cases, col=covid_us$StateColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative cases")
plot(covid_us$DaysSinceFirstCase, covid_us$deaths, col=covid_us$StateColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative deaths")

# Normalize Plot, color by state
plot(covid_us$DaysSinceFirstCase, covid_us$normcases, col=covid_us$StateColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative cases")
plot(covid_us$DaysSinceFirstCase, covid_us$normdeaths, col=covid_us$StateColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative deaths")

# Standardize Plot, color by state
plot(covid_us$DaysSinceFirstCase, covid_us$stdcases, col=covid_us$StateColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative cases")
plot(covid_us$DaysSinceFirstCase, covid_us$stddeaths, col=covid_us$StateColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative deaths")


# Plot, color by county
plot(covid_us$DaysSinceFirstCase, covid_us$cases, col=covid_us$CountyColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative cases")
plot(covid_us$DaysSinceFirstCase, covid_us$deaths, col=covid_us$CountyColor, xlab = "Days Since First Case (DSFC)", ylab = "Cumulative deaths")


###########################
curve(0+0*x, -4, 0, xlim = c(-4,10), ylim=c(0,1), xlab = "x", ylab = "f(x)", col="blue")
curve((3/16)*sqrt(x), 0, 4, add = T, col="blue")
curve(0+0*x, 4, 10, add = T, col="blue")

curve(0+0*x, -4, 0, xlim = c(-4,10), ylim=c(0,1), xlab = "x", ylab = "F(x)", col="blue")
curve((1/8)*sqrt(x*x*x), 0, 4, add = T, col="blue")
curve(0+0*x, 4, 10, add = T, col="blue")

# 3.1.14
curve(0+0*x, -2, 0, xlim = c(-2,5), ylim=c(0,1), xlab = "x", ylab = "f(x)", col="blue")
curve(0.5+0*x, 0, 1, add = T, col="blue")
curve(0+0*x, 1, 2, add = T, col="blue")
curve(0.5+0*x, 2, 3, add = T, col="blue")
curve(0+0*x, 3, 5, add = T, col="blue")

curve(0+0*x, -2, 0, xlim = c(-2,5), ylim=c(0,1.1), xlab = "x", ylab = "F(x)", col="blue")
curve(0.5*x, 0, 1, add = T, col="blue")
curve(0.5+0*x, 1, 2, add = T, col="blue")
curve(0.5*(x-1), 2, 3, add = T, col="blue")
curve(1+0*x, 3, 5, add = T, col="blue")






